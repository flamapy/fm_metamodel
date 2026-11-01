import itertools
import math
from enum import Enum

from flamapy.core.models.ast import ASTOperation
from flamapy.core.transformations import ModelToText
from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature, Relation, Constraint
from flamapy.metamodels.fm_metamodel.transformations.fm_secure_features_names import (
    FMSecureFeaturesNames,
)
from flamapy.metamodels.fm_metamodel.transformations.uvl_writer import xor_as_negated_equivalence


# Reserved words of the Logic2BDD expression scanner (synExpScanner.lpp).
PL_KEYWORDS = frozenset({
    'synTrue', 'synFalse', 'synModule', 'if', 'IF', 'then', 'THEN', 'else', 'ELSE',
    'and', 'AND', 'or', 'OR', 'not', 'NOT', 'XOR', 'Xor', 'xor', 'MUX', 'Mux', 'mux',
})


class PLWriter(ModelToText):
    """Propositional logic writer for feature models.

    It transforms the feature tree of the feature model into a propositional logic formula,
    while maintaining the original constraints that are already in propositional logic.

    Typed features and multi-features are considered as simple (boolean) features.
    Arithmetic, and other complex constraints are ommited, and thus, eliminated from the output.

    The output follows the Logic2BDD expression grammar: one expression per line, every line
    ending in a newline. Feature names are written through FMSecureFeaturesNames (letters,
    digits and '_') and never as one of the scanner's keywords (PL_KEYWORDS), so every name is a
    single symbol. The mapping from original to written names is available in `mapping_names`
    after `transform()`. XOR constraints are written as `not (a <-> b)`, because the grammar's
    XOR is a prefix function over a list.

    This writer is used by the Logic2BDD tool.
    """

    class LogicConnective(Enum):
        NOT = 'not'
        OR = 'or'
        AND = 'and'
        IMPLIES = '->'
        EQUIVALENCE = '<->'
        XOR = 'XOR'
        MUX = 'MUX'

        def __str__(self) -> str:
            # The connectives are interpolated in f-strings: write the symbol, not the member.
            return str(self.value)

    @staticmethod
    def get_destination_extension() -> str:
        return 'exp'

    def __init__(self, path: str, source_model: FeatureModel):
        self.path: str = path
        self.source_model: FeatureModel = source_model
        self.mapping_names: dict[str, str] = {}

    def transform(self) -> str:
        secure_names = FMSecureFeaturesNames(self.source_model)
        secure_names.reserved_names |= PL_KEYWORDS
        feature_model = secure_names.transform()
        self.mapping_names = secure_names.mapping_names
        expressions = to_exp(feature_model)
        expressions_str = ''.join(f'{expression}\n' for expression in expressions)
        if self.path is not None:
            with open(self.path, 'w', encoding='utf8') as file:
                file.write(expressions_str)
        return expressions_str


def to_exp(feature_model: FeatureModel) -> list[str]:
    """Traverse the feature tree and constraints and return a list of propositional formulas."""
    formulas: list[str] = []
    formulas.append(feature_model.root.name)  # The root is always present
    features: list[Feature] = []
    features.append(feature_model.root)
    while features:
        feature = features.pop()
        for relation in feature.get_relations():
            formulas.append(get_relation_formula(relation))
            features.extend(relation.children)
    for constraint in feature_model.get_logical_constraints():
        formulas.append(get_constraint_formula(constraint))
    return formulas


def get_relation_formula(relation: Relation) -> str:
    if relation.is_mandatory():
        return get_mandatory_formula(relation)
    elif relation.is_optional():
        return get_optional_formula(relation)
    elif relation.is_or():
        return get_or_formula(relation)
    elif relation.is_alternative():
        return get_alternative_formula(relation)
    elif relation.is_mutex():
        return get_mutex_formula(relation)
    elif relation.is_cardinal():
        return get_cardinality_formula(relation)

    raise ValueError(f"Unknown relation type: {relation}")


def get_mandatory_formula(relation: Relation) -> str:
    parent = relation.parent.name
    child = relation.children[0].name
    return f'{parent} {PLWriter.LogicConnective.EQUIVALENCE} {child}'


def get_optional_formula(relation: Relation) -> str:
    parent = relation.parent.name
    child = relation.children[0].name
    return f'{child} {PLWriter.LogicConnective.IMPLIES} {parent}'


def get_or_formula(relation: Relation) -> str:
    parent = relation.parent.name
    children = f" {PLWriter.LogicConnective.OR} ".join(child.name for child in relation.children)
    return f'{parent} {PLWriter.LogicConnective.EQUIVALENCE} ({children})'


def get_alternative_formula(relation: Relation) -> str:
    """Each child is selected iff its parent is and no sibling is: exactly one child."""
    formula = []
    parent = relation.parent.name
    children = [child.name for child in relation.children]
    for child in children:
        children_neg_str = [f"{PLWriter.LogicConnective.NOT} " + ch
                            for ch in children if ch != child]
        formula.append(f'{child} {PLWriter.LogicConnective.EQUIVALENCE} '
                       f'({f" {PLWriter.LogicConnective.AND} ".join(children_neg_str)} '
                       f'{PLWriter.LogicConnective.AND} {parent})')
    return f" {PLWriter.LogicConnective.AND} ".join(f'({f})' for f in formula)


def get_mutex_formula(relation: Relation) -> str:
    """At most one child: the parent with no child, or exactly one child (as in alternative)."""
    parent = relation.parent.name
    children = [child.name for child in relation.children]
    or_children = f" {PLWriter.LogicConnective.OR} ".join(children)
    return f'({parent} {PLWriter.LogicConnective.AND} ' \
           f'{PLWriter.LogicConnective.NOT} ({or_children})) ' \
           f'{PLWriter.LogicConnective.OR} ({get_alternative_formula(relation)})'


# Cardinality groups are written as clauses (see cardinality_clauses); groups that would need
# more clauses than this are rejected instead of producing a huge formula.
MAX_CARDINALITY_CLAUSES = 100_000


def cardinality_clauses(card_min: int, card_max: int, n_children: int) -> int:
    """Number of clauses cardinality_formula writes for a [card_min..card_max] group."""
    at_least = math.comb(n_children, n_children - card_min + 1) if 0 < card_min <= n_children else 0
    at_most = math.comb(n_children, card_max + 1) if card_max < n_children else 0
    return n_children + at_least + at_most


def get_cardinality_formula(relation: Relation) -> str:
    """A group cardinality [m..k] as a conjunction of clauses:

    * every child implies the parent;
    * at least m: the parent implies at least one child of every set of n-m+1 children;
    * at most k: of every set of k+1 children, at least one is not selected.

    Every clause is parenthesized, because in the Logic2BDD grammar `<->` and `->` bind
    differently from `and`/`or` than in most languages.
    """
    parent = relation.parent.name
    children = [child.name for child in relation.children]
    n_children = len(children)
    card_min = relation.card_min
    card_max = n_children if relation.card_max < 0 else min(relation.card_max, n_children)
    n_clauses = cardinality_clauses(card_min, card_max, n_children)
    if n_clauses > MAX_CARDINALITY_CLAUSES:
        raise ValueError(
            f'Group cardinality [{relation.card_min}..{relation.card_max}] of {parent} over '
            f'{n_children} children needs {n_clauses} clauses; the PL writer supports at most '
            f'{MAX_CARDINALITY_CLAUSES}.')
    conn = PLWriter.LogicConnective
    clauses = [f'({child} {conn.IMPLIES} {parent})' for child in children]
    if card_min > n_children:
        clauses.append(f'({conn.NOT} {parent})')  # no set of children is large enough
    elif card_min > 0:
        for subset in itertools.combinations(children, n_children - card_min + 1):
            clauses.append(f'({parent} {conn.IMPLIES} ({f" {conn.OR} ".join(subset)}))')
    if card_max < n_children:
        for subset in itertools.combinations(children, card_max + 1):
            clauses.append(f'({f" {conn.OR} ".join(f"{conn.NOT} {child}" for child in subset)})')
    return f' {conn.AND} '.join(clauses)


PL_OPERATORS = {
    ASTOperation.NOT: PLWriter.LogicConnective.NOT.value,
    ASTOperation.AND: PLWriter.LogicConnective.AND.value,
    ASTOperation.OR: PLWriter.LogicConnective.OR.value,
    ASTOperation.IMPLIES: PLWriter.LogicConnective.IMPLIES.value,
    ASTOperation.REQUIRES: PLWriter.LogicConnective.IMPLIES.value,
    ASTOperation.EQUIVALENCE: PLWriter.LogicConnective.EQUIVALENCE.value,
    ASTOperation.EXCLUDES: f'{PLWriter.LogicConnective.IMPLIES.value} '
                           f'{PLWriter.LogicConnective.NOT.value}',
}


def get_constraint_formula(ctc: Constraint) -> str:
    """Write a logical constraint with the PL connectives (operators replaced node by node)."""
    return xor_as_negated_equivalence(ctc.ast).pretty_str(PL_OPERATORS)
