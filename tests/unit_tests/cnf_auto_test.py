"""ClauseSet cnf_method='auto': Tseytin only for constraints whose distributive expansion would
be larger, and the same configurations as the other methods."""
import functools

import pytest

from flamapy.core.exceptions import FlamaException
from flamapy.core.models.ast import AST, ASTOperation, Node
from flamapy.metamodels.fm_metamodel.models.clause_set import (
    ClauseSet,
    distributive_clause_count,
    use_tseytin,
)
from flamapy.metamodels.fm_metamodel.transformations import UVLReader


def _dnf(n_terms: int, width: int) -> AST:
    terms = [functools.reduce(lambda a, b: Node(ASTOperation.AND, a, b),
                              [Node(f'x{i}_{j}') for j in range(width)]) for i in range(n_terms)]
    return AST(functools.reduce(lambda a, b: Node(ASTOperation.OR, a, b), terms))


@pytest.mark.parametrize('ast', [
    AST(Node(ASTOperation.IMPLIES, Node('A'), Node('B'))),
    AST(Node(ASTOperation.EXCLUDES, Node('A'), Node('B'))),
    AST(Node(ASTOperation.EQUIVALENCE, Node(ASTOperation.OR, Node('a'), Node('b')),
             Node(ASTOperation.AND, Node('c'), Node('d')))),
    AST(Node(ASTOperation.XOR, Node('a'), Node(ASTOperation.OR, Node('b'), Node('c')))),
    AST(Node(ASTOperation.NOT, Node(ASTOperation.AND, Node('a'), Node(ASTOperation.OR, Node('b'),
                                                                      Node('c'))))),
    _dnf(3, 2),
])
def test_clause_count_matches_the_expansion(ast: AST) -> None:
    assert distributive_clause_count(ast) == len(ast.get_clauses())


def test_auto_rule() -> None:
    assert not use_tseytin(AST(Node(ASTOperation.IMPLIES, Node('A'), Node('B'))))
    assert use_tseytin(_dnf(11, 4))        # 4**11 clauses expanded vs ~130 with Tseytin


def _model(tmp_path, constraints: str):
    leaves = ''.join(f'            {name}\n' for name in ['A', 'B', 'C', 'D', 'E', 'F'])
    src = tmp_path / 'model.uvl'
    src.write_text(f'features\n    R\n        optional\n{leaves}constraints\n{constraints}')
    return UVLReader(str(src)).transform()


def test_auto_uses_tseytin_only_where_needed(tmp_path) -> None:
    fm = _model(tmp_path, '    A => B\n    (A & B & C) | (D & E & F) | (A & D) | (B & E)\n')
    assert not ClauseSet.from_feature_model(fm, 'distributive').auxiliary_variables
    auto = ClauseSet.from_feature_model(fm, 'auto')
    simple = next(g for label, g in auto.clause_groups if 'IMPLIES' in label)
    assert all(abs(lit) in auto.features for clause in simple for lit in clause)
    assert auto.auxiliary_variables


@pytest.mark.parametrize('constraints', [
    '    A => B\n',
    '    (A & B & C) | (D & E & F) | (A & D) | (B & E)\n',
    '    (A <=> B) <=> (C <=> D)\n    !(E & F)\n',
])
def test_auto_keeps_the_configurations(tmp_path, constraints: str) -> None:
    from flamapy.metamodels.pysat_metamodel.operations import PySATConfigurationsNumber
    from flamapy.metamodels.pysat_metamodel.transformations import FmToPysat
    fm = _model(tmp_path, constraints)
    counts = {method: PySATConfigurationsNumber().execute(
        FmToPysat(fm, cnf_method=method).transform()).get_result()
        for method in ('distributive', 'tseytin', 'auto')}
    assert len(set(counts.values())) == 1, counts


def test_unknown_method_is_rejected(tmp_path) -> None:
    with pytest.raises(FlamaException, match='cnf_method'):
        ClauseSet.from_feature_model(_model(tmp_path, '    A => B\n'), 'fast')


def test_default_is_auto(tmp_path) -> None:
    # Simple constraints: the default produces exactly the distributive clauses.
    fm = _model(tmp_path, '    A => B\n    !(E & F)\n')
    assert ClauseSet.from_feature_model(fm).clauses == \
        ClauseSet.from_feature_model(fm, 'distributive').clauses
    assert not ClauseSet.from_feature_model(fm).auxiliary_variables
    # A constraint whose expansion blows up: the default switches that one to Tseytin.
    fm = _model(tmp_path, '    (A & B & C) | (D & E & F) | (A & D) | (B & E)\n')
    default = ClauseSet.from_feature_model(fm)
    assert default.clauses == ClauseSet.from_feature_model(fm, 'auto').clauses
    assert default.auxiliary_variables
    assert len(default.clauses) < len(ClauseSet.from_feature_model(fm, 'distributive').clauses)
