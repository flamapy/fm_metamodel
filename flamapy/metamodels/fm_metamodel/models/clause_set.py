"""A solver-agnostic CNF (clause set) of a feature model.

`ClauseSet` is a lightweight data holder — the clauses plus the variable/feature maps — that any
plugin can build from a feature model without depending on a SAT solver. It is deliberately NOT a
``VariabilityModel`` (it is not a discoverable metamodel) and the encoder lives here as a
classmethod builder rather than as a transformation (flamapy transformations are m2m/t2m/m2t).

The SAT plugin (`FmToPysat`) and the compilation/counting plugins (sdd, dnnf, sharpsat) all build
their representation from this single encoding.
"""
import itertools
from dataclasses import dataclass, field
from typing import Any, Callable, Optional

from flamapy.core.exceptions import FlamaException
from flamapy.core.models.ast import AST, ASTOperation, Node

from flamapy.metamodels.fm_metamodel.models.feature_model import (
    FeatureModel,
    Feature,
    Relation,
    Constraint,
)


@dataclass
class ClauseSet:
    """CNF encoding of a feature model as signed-integer clauses plus name/id maps."""

    clauses: list[list[int]] = field(default_factory=list)
    variables: dict[str, int] = field(default_factory=dict)   # feature name -> variable id
    features: dict[int, str] = field(default_factory=dict)    # variable id -> feature name
    auxiliary_variables: set[int] = field(default_factory=set)  # Tseytin aux ids (non-feature)
    original_model: Optional[FeatureModel] = None
    # Provenance: the clauses grouped by their source (root feature / relation / constraint),
    # labelled by ``str(source)``. Needed by diagnosis (clause → constraint mapping); ignored by
    # consumers that only want the flat clause list.
    clause_groups: list[tuple[str, list[list[int]]]] = field(default_factory=list)

    def feature_variables(self) -> list[int]:
        """Variable ids that correspond to features (excludes Tseytin auxiliaries)."""
        return list(self.features.keys())

    @classmethod
    def from_feature_model(
        cls, feature_model: FeatureModel, cnf_method: str = 'distributive'
    ) -> 'ClauseSet':
        """Encode ``feature_model`` into CNF.

        ``cnf_method`` controls only how cross-tree constraints are turned into clauses:
        ``'distributive'`` (default) expands them directly, ``'tseytin'`` introduces auxiliary
        variables for a compact, model-count-preserving encoding, and ``'auto'`` decides per
        constraint: it expands a constraint directly unless the expansion would be larger than
        its Tseytin encoding (see `distributive_clause_count`), so only constraints that blow up
        (e.g. disjunctions of conjunctions, wide equivalences) get auxiliary variables.
        """
        if cnf_method not in CNF_METHODS:
            raise FlamaException(f'Unknown cnf_method {cnf_method!r}; expected one of '
                                 f'{", ".join(CNF_METHODS)}.')
        return _Encoder(cnf_method).encode(feature_model)


CNF_METHODS = ('distributive', 'tseytin', 'auto')
BINARY = 2


# Clause counts of (P op Q) and of its negation, from the counts of P and Q in both polarities,
# following flamapy's rewriting (simplify_formula): P => Q as !P | Q, P excludes Q as !P | !Q,
# P <=> Q as (P => Q) & (Q => P), P xor Q as (P & !Q) | (!P & Q). A conjunction adds the clauses
# of its operands, a disjunction multiplies them.
_COUNT_RULES: dict[ASTOperation, Callable[[int, int, int, int], tuple[int, int]]] = {
    ASTOperation.AND: lambda ap, an, bp, bn: (ap + bp, an * bn),
    ASTOperation.OR: lambda ap, an, bp, bn: (ap * bp, an + bn),
    ASTOperation.IMPLIES: lambda ap, an, bp, bn: (an * bp, ap + bn),
    ASTOperation.REQUIRES: lambda ap, an, bp, bn: (an * bp, ap + bn),
    ASTOperation.EXCLUDES: lambda ap, an, bp, bn: (an * bn, ap + bp),
    ASTOperation.EQUIVALENCE: lambda ap, an, bp, bn: (an * bp + ap * bn, ap * bp + an * bn),
    ASTOperation.XOR: lambda ap, an, bp, bn: ((ap + bn) * (an + bp), an * bp + ap * bn),
}


def distributive_clause_count(ast: AST) -> Optional[int]:
    """Number of clauses the distributive CNF expansion of `ast` produces, computed without
    building it (duplicates and subsumed clauses are not removed, as in the expansion itself).

    Each node is counted for both polarities (see _COUNT_RULES), in one iterative pass, so deep
    constraints do not hit the recursion limit. Returns None for non-propositional operators.
    """
    pos: dict[int, int] = {}
    neg: dict[int, int] = {}
    stack: list[tuple[Node, bool]] = [(ast.root, False)]
    while stack:
        node, expanded = stack.pop()
        if node.is_term():
            pos[id(node)] = neg[id(node)] = 1
            continue
        children = [child for child in (node.left, node.right) if child is not None]
        if not expanded:
            stack.append((node, True))
            stack.extend((child, False) for child in children)
            continue
        if any(id(child) not in pos for child in children):
            return None
        if node.data == ASTOperation.NOT:
            pos[id(node)], neg[id(node)] = neg[id(children[0])], pos[id(children[0])]
        elif node.data in _COUNT_RULES and len(children) == BINARY:
            left, right = children
            pos[id(node)], neg[id(node)] = _COUNT_RULES[node.data](
                pos[id(left)], neg[id(left)], pos[id(right)], neg[id(right)])
        else:
            return None
    return pos[id(ast.root)]


def tseytin_clause_estimate(ast: AST) -> int:
    """Approximate size of the Tseytin encoding: about three clauses per operator, plus one."""
    return 3 * sum(1 for _ in ast.get_operators()) + 1


def use_tseytin(ast: AST) -> bool:
    """The `auto` rule: Tseytin when the distributive expansion would be larger."""
    count = distributive_clause_count(ast)
    return count is not None and count > tseytin_clause_estimate(ast)


class _Encoder:
    """Ported ~1:1 from the SAT plugin's FmToPysat so the produced clauses are identical."""

    def __init__(self, cnf_method: str) -> None:
        self.cnf_method = cnf_method
        self.counter = 1
        self.variables: dict[str, int] = {}
        self.features: dict[int, str] = {}
        self.clauses: list[list[int]] = []
        self.auxiliary_variables: set[int] = set()
        self.clause_groups: list[tuple[str, list[list[int]]]] = []

    def encode(self, feature_model: FeatureModel) -> ClauseSet:
        for feature in feature_model.get_features():
            self._add_feature(feature)
        self._add_root(feature_model.root)
        for relation in feature_model.get_relations():
            self._add_relation(relation)
        for constraint in feature_model.get_logical_constraints():
            self._add_constraint(constraint)
        return ClauseSet(
            clauses=self.clauses,
            variables=self.variables,
            features=self.features,
            auxiliary_variables=self.auxiliary_variables,
            original_model=feature_model,
            clause_groups=self.clause_groups,
        )

    def _get_variable(self, name: str) -> int:
        if name not in self.variables:
            raise FlamaException(f'Feature {name} is not in the model')
        return self.variables[name]

    def _add_feature(self, feature: Feature) -> None:
        if feature.name not in self.variables:
            self.variables[feature.name] = self.counter
            self.features[self.counter] = feature.name
            self.counter += 1

    def _add_root(self, feature: Feature) -> None:
        clause = [self._get_variable(feature.name)]
        self.clauses.append(clause)
        self.clause_groups.append((str(feature), [clause]))

    def _add_mandatory_relation(self, relation: Relation) -> list[list[int]]:
        parent = self._get_variable(relation.parent.name)
        child = self._get_variable(relation.children[0].name)
        return [[-parent, child], [-child, parent]]

    def _add_optional_relation(self, relation: Relation) -> list[list[int]]:
        parent = self._get_variable(relation.parent.name)
        child = self._get_variable(relation.children[0].name)
        return [[-child, parent]]

    def _add_or_relation(self, relation: Relation) -> list[list[int]]:
        parent = self._get_variable(relation.parent.name)
        alt = [-parent] + [self._get_variable(c.name) for c in relation.children]
        clauses = [alt]
        for child in relation.children:
            clauses.append([-self._get_variable(child.name), parent])
        return clauses

    def _add_alternative_relation(self, relation: Relation) -> list[list[int]]:
        parent = self._get_variable(relation.parent.name)
        alt = [-parent] + [self._get_variable(c.name) for c in relation.children]
        clauses = [alt]
        for i, _ in enumerate(relation.children):
            for j in range(i + 1, len(relation.children)):
                if i != j:
                    clauses.append([
                        -self._get_variable(relation.children[i].name),
                        -self._get_variable(relation.children[j].name),
                    ])
            clauses.append([-self._get_variable(relation.children[i].name), parent])
        return clauses

    def _add_constraint_relation(self, relation: Relation) -> list[list[int]]:
        parent = self._get_variable(relation.parent.name)
        card_min = relation.card_min
        card_max = relation.card_max if relation.card_max != -1 else len(relation.children)
        clauses = []
        for val in range(len(relation.children) + 1):
            if val < card_min or val > card_max:
                for combination in itertools.combinations(relation.children, val):
                    cnf = [-parent]
                    for feat in relation.children:
                        if feat in combination:
                            cnf.append(-self._get_variable(feat.name))
                        else:
                            cnf.append(self._get_variable(feat.name))
                    clauses.append(cnf)
        for val in range(1, len(relation.children) + 1):
            for combination in itertools.combinations(relation.children, val):
                cnf = [parent]
                for feat in relation.children:
                    if feat in combination:
                        cnf.append(-self._get_variable(feat.name))
                    else:
                        cnf.append(self._get_variable(feat.name))
                clauses.append(cnf)
        return clauses

    def _add_relation(self, relation: Relation) -> None:
        if relation.is_mandatory():
            clauses = self._add_mandatory_relation(relation)
        elif relation.is_optional():
            clauses = self._add_optional_relation(relation)
        elif relation.is_or():
            clauses = self._add_or_relation(relation)
        elif relation.is_alternative():
            clauses = self._add_alternative_relation(relation)
        else:
            clauses = self._add_constraint_relation(relation)
        self.clauses.extend(clauses)
        self.clause_groups.append((str(relation), clauses))

    def _allocate_auxiliary(self, aux_names: list[str]) -> dict[str, int]:
        aux_map: dict[str, int] = {}
        for name in aux_names:
            var = self.counter
            self.counter += 1
            aux_map[name] = var
            self.auxiliary_variables.add(var)
        return aux_map

    def _add_constraint(self, ctc: Constraint) -> None:
        method = self.cnf_method
        if method == 'auto':
            method = 'tseytin' if use_tseytin(ctc.ast) else 'distributive'
        if method == 'tseytin':
            clauses, aux_names = ctc.ast.get_clauses_with_aux(method='tseytin')
            aux_map = self._allocate_auxiliary(aux_names)
        else:
            clauses = ctc.ast.get_clauses()
            aux_map = {}

        def term_to_variable(term: Any) -> int:
            negated = term.startswith('-')
            name = term[1:] if negated else term
            var = aux_map[name] if name in aux_map else self._get_variable(name)
            return -var if negated else var

        group = []
        for clause in clauses:
            resolved = [term_to_variable(term) for term in clause]
            self.clauses.append(resolved)
            group.append(resolved)
        self.clause_groups.append((str(ctc), group))
