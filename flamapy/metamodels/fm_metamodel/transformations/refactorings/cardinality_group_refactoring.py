import functools
import itertools
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
    combination has at least 'a' and at most 'b' elements."""

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
        instance.get_relations().remove(r_card)

        for child in r_card.children:
            r_opt = Relation(instance, [child], 0, 1)  # optional
            instance.add_relation(r_opt)

        ast = get_ast_constraint_for_cardinality_group(instance, r_card)
        constraint = Constraint(FMRefactoring.get_new_constraint_name(self.feature_model, 'CG'),
                                ast)
        self.feature_model.ctcs.append(constraint)
        return self.feature_model


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
    card_max = relation.card_max
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
