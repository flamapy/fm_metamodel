"""Group cardinalities [a..b] in the CNF encoder: the forbidden counts are enumerated, the
child-implies-parent clauses are one per child (the 2.8 encoder enumerated every non-empty subset
of the children for them, 2^n - 1 clauses, and hung on a 27-child group of the UVLHub corpus)."""
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
    clauses = {frozenset(clause) for clause in cs.clauses}
    assert all(frozenset((-cs.variables[f'C{i}'], parent)) in clauses for i in range(4))


def test_wide_group_child_implies_parent_is_linear(tmp_path) -> None:
    # [4..27]: the forbidden counts are 0-3, so their enumeration stays small too.
    fm = _model(tmp_path, 27, 4, 27)
    start = time.perf_counter()
    cs = ClauseSet.from_feature_model(fm)
    assert time.perf_counter() - start < 5
    assert len(cs.clauses) < 5000
