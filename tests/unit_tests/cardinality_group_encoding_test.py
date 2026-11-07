"""Group cardinalities [a..b] in the CNF encoder: the forbidden counts are enumerated, the
child-implies-parent clauses are linear in the number of children."""
import time

from flamapy.metamodels.fm_metamodel.models.clause_set import ClauseSet
from flamapy.metamodels.fm_metamodel.transformations import UVLReader


def _model(tmp_path, n_children: int, lo: int, hi: int):
    children = ''.join(f'            C{i}\n' for i in range(n_children))
    src = tmp_path / 'model.uvl'
    src.write_text(f'features\n    R\n        [{lo}..{hi}]\n{children}')
    return UVLReader(str(src)).transform()


def _count(clauses, n_vars: int) -> int:
    from itertools import product
    total = 0
    for bits in product((False, True), repeat=n_vars):
        if all(any((lit > 0) == bits[abs(lit) - 1] for lit in clause) for clause in clauses):
            total += 1
    return total


def test_small_group_keeps_its_semantics(tmp_path) -> None:
    fm = _model(tmp_path, 4, 2, 3)
    cs = ClauseSet.from_feature_model(fm)
    # root selected and 2 or 3 of the 4 children: C(4,2) + C(4,3) = 10 configurations
    assert _count(cs.clauses, len(cs.variables)) == 10
    parent = cs.variables['R']
    assert all([parent, -cs.variables[f'C{i}']] in cs.clauses for i in range(4))


def test_wide_group_encodes_in_linear_time(tmp_path) -> None:
    fm = _model(tmp_path, 27, 4, 27)
    start = time.perf_counter()
    cs = ClauseSet.from_feature_model(fm)
    assert time.perf_counter() - start < 5
    assert len(cs.clauses) < 5000
