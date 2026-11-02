import functools
import itertools
import math
from typing import Any

from flamapy.core.models.ast import AST, ASTOperation, Node
from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature, Relation, Constraint
from flamapy.metamodels.fm_metamodel.transformations.refactorings import (
    FMRefactoring,
    RefactoringException
)


class CardinalityGroupRefactoring(FMRefactoring):
    """It changes the cardinality group to an and-group where all sub-features are optionals and
    add a new complex constraint with all feature combinations of the sub-features where each
    combination has at least 'a' and at most 'b' elements.

    With `clauses=True` the group is instead encoded with one constraint per clause, which grows
    with C(n, n-a+1) + C(n, b+1) instead of with all combinations:
      * at least a: the feature implies one sub-feature of every set of n-a+1 sub-features;
      * at most b: no b+1 sub-features are selected together.
    Groups needing more than MAX_CLAUSES clauses are rejected.
    """

    MAX_CLAUSES = 100_000

    def __init__(self, source_model: Any, clauses: bool = False) -> None:
        super().__init__(source_model)
        self.clauses = clauses

    def get_name(self) -> str:
        return 'Cardinality group refactoring'

    def get_instances(self) -> list[Feature]:
        if self.feature_model is None:
            return []
        return [feat for feat in self.feature_model.get_features() if feat.is_cardinality_group()]

    def is_applicable(self) -> bool:
        if self.feature_model is None:
            return False
        return any(feat.is_cardinality_group() for feat in self.feature_model.get_features())

    def apply(self, instance: Any) -> FeatureModel | None:
        if self.feature_model is None:
            raise RefactoringException('Feature model is None.')
        if instance is None:
            raise RefactoringException(f'Invalid instance for {self.get_name()}.')
        if not isinstance(instance, Feature):
            raise RefactoringException(f'Invalid instance for {self.get_name()}.'
                                       f'Expected Feature, got {type(instance)} for {instance}.')
        if not instance.is_cardinality_group():
            raise RefactoringException(f'Feature {instance.name} is not a cardinality group.')

        r_card = next((r for r in instance.get_relations() if r.is_cardinal()), None)
        if r_card is None:
            raise RefactoringException(f'Feature {instance.name} \
                                       does not have a cardinality relation.')
        if self.clauses:
            return self._apply_clauses(instance, r_card)
        instance.get_relations().remove(r_card)

        for child in r_card.children:
            r_opt = Relation(instance, [child], 0, 1)  # optional
            instance.add_relation(r_opt)

        ast = get_ast_constraint_for_cardinality_group(instance, r_card)
        constraint = Constraint(FMRefactoring.get_new_constraint_name(self.feature_model, 'CG'),
                                ast)
        self.feature_model.ctcs.append(constraint)
        return self.feature_model

    def _apply_clauses(self, instance: Feature, r_card: Relation) -> FeatureModel | None:
        assert self.feature_model is not None
        n_clauses = count_clauses_for_cardinality_group(r_card)
        if n_clauses > CardinalityGroupRefactoring.MAX_CLAUSES:
            raise RefactoringException(
                f'Cardinality group of {instance.name} needs {n_clauses} constraints; at most '
                f'{CardinalityGroupRefactoring.MAX_CLAUSES} are supported.')
        instance.get_relations().remove(r_card)
        for child in r_card.children:
            instance.add_relation(Relation(instance, [child], 0, 1))  # optional
        used = {ctc.name for ctc in self.feature_model.get_constraints()}
        for ast in get_clause_asts_for_cardinality_group(instance, r_card):
            name, count = 'CG', 1
            while name in used:
                name = f'CG_{count}'
                count += 1
            used.add(name)
            self.feature_model.ctcs.append(Constraint(name, ast))
        return self.feature_model


def count_clauses_for_cardinality_group(relation: Relation) -> int:
    n_children = len(relation.children)
    card_min = relation.card_min
    card_max = n_children if relation.card_max < 0 else min(relation.card_max, n_children)
    at_least = math.comb(n_children, n_children - card_min + 1) if 0 < card_min <= n_children \
        else int(card_min > n_children)
    at_most = math.comb(n_children, card_max + 1) if card_max < n_children else 0
    return at_least + at_most


def get_clause_asts_for_cardinality_group(feature: Feature, relation: Relation) -> list[AST]:
    """Clauses for a [card_min..card_max] group (see CardinalityGroupRefactoring)."""
    children = [child.name for child in relation.children]
    n_children = len(children)
    card_min = relation.card_min
    card_max = n_children if relation.card_max < 0 else min(relation.card_max, n_children)
    if card_min > n_children:
        return [AST.create_simple_unary_operation(ASTOperation.NOT, feature.name)]
    asts: list[AST] = []
    if card_min > 0:
        for subset in itertools.combinations(children, n_children - card_min + 1):
            disjunction = functools.reduce(lambda left, right: Node(ASTOperation.OR, left, right),
                                           [Node(name) for name in subset])
            asts.append(AST(Node(ASTOperation.IMPLIES, Node(feature.name), disjunction)))
    if card_max < n_children:
        for subset in itertools.combinations(children, card_max + 1):
            conjunction = functools.reduce(lambda left, right: Node(ASTOperation.AND, left, right),
                                           [Node(name) for name in subset])
            asts.append(AST(Node(ASTOperation.NOT, conjunction)))
    return asts


def create_and_constraints_for_cardinality_group(positives: tuple[Feature, ...],
                                                 negatives: set[Feature]) -> Node:
    elements = [Node(f.name) for f in positives]
    elements += [AST.create_unary_operation(ASTOperation.NOT, Node(f.name)).root
                 for f in negatives]
    result = functools.reduce(lambda left, right:
                              AST.create_binary_operation(ASTOperation.AND, left, right).root,
                              elements)
    return result


def get_or_constraints_for_cardinality_group(relation: Relation) -> Node:
    card_min = relation.card_min
    # An unbounded maximum (-1) means up to all sub-features (it used to make the range empty).
    card_max = len(relation.children) if relation.card_max < 0 else relation.card_max
    children = set(relation.children)
    and_nodes = []
    for k in range(card_min, card_max + 1):
        combi_k = list(itertools.combinations(relation.children, k))
        for positives in combi_k:
            negatives = children - set(positives)

            and_ctc = create_and_constraints_for_cardinality_group(positives, negatives)
            and_nodes.append(and_ctc)
    result = functools.reduce(lambda left, right: Node(ASTOperation.OR, left, right), and_nodes)
    return result


def get_ast_constraint_for_cardinality_group(feature: Feature, relation: Relation) -> AST:
    return AST.create_binary_operation(ASTOperation.IMPLIES,
                                       Node(feature.name),
                                       get_or_constraints_for_cardinality_group(relation))
