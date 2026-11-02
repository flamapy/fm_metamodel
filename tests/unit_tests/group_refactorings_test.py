"""MultipleGroupDecomposition, MutexGroup and CardinalityGroup refactorings keep the
configurations of the original features."""
import copy

import pytest

from flamapy.metamodels.fm_metamodel.transformations import UVLReader
from flamapy.metamodels.fm_metamodel.transformations.refactorings import (
    CardinalityGroupRefactoring,
    MultipleGroupDecompositionRefactoring,
    MutexGroupRefactoring,
    RefactoringException,
)
from flamapy.metamodels.fm_metamodel.transformations.refactorings.\
    multiple_group_decomposition_refactoring import needs_group_decomposition


def _count(fm) -> int:
    # BDD: the default cardinality refactoring writes one disjunction of conjunctions, which the
    # distributive CNF conversion of the SAT backend expands exponentially.
    from flamapy.metamodels.bdd_metamodel.operations import BDDConfigurationsNumber
    from flamapy.metamodels.bdd_metamodel.transformations import FmToBDD
    return BDDConfigurationsNumber().execute(FmToBDD(fm).transform()).get_result()


def _read(tmp_path, uvl: str):
    src = tmp_path / 'model.uvl'
    src.write_text(uvl)
    return UVLReader(str(src)).transform()


MIXED = [
    # one group and mandatory/optional children (not covered before)
    'features\n    R\n        mandatory\n            B\n        optional\n            E\n'
    '        alternative\n            C\n            D\n',
    # two groups
    'features\n    R\n        or\n            A\n            B\n        alternative\n            C\n'
    '            D\n',
    # three groups between other relations
    'features\n    R\n        or\n            A\n            B\n        optional\n            E\n'
    '        alternative\n            C\n            D\n        [0..1]\n            F\n            G\n',
]


@pytest.mark.parametrize('uvl', MIXED)
def test_multiple_group_decomposition(tmp_path, uvl: str) -> None:
    fm = _read(tmp_path, uvl)
    refactored = MultipleGroupDecompositionRefactoring(copy.deepcopy(fm)).transform()
    assert not any(needs_group_decomposition(f) for f in refactored.get_features())
    assert _count(refactored) == _count(fm)
    original = {f.name for f in fm.get_features()}
    assert original <= {f.name for f in refactored.get_features()}


def test_multiple_group_decomposition_previous_case_unchanged(tmp_path) -> None:
    # two groups and nothing else: the case the refactoring already handled
    fm = _read(tmp_path, MIXED[1])
    refactored = MultipleGroupDecompositionRefactoring(copy.deepcopy(fm)).transform()
    root = refactored.root
    assert len(root.get_relations()) == 2
    assert all(r.is_mandatory() and r.children[0].is_abstract for r in root.get_relations())


def test_mutex_group(tmp_path) -> None:
    fm = _read(tmp_path, 'features\n    R\n        [0..1]\n            A\n            B\n'
                         '            C\n')
    refactored = MutexGroupRefactoring(copy.deepcopy(fm)).transform()
    assert not any(f.is_mutex_group() for f in refactored.get_features())
    assert _count(refactored) == _count(fm)


@pytest.mark.parametrize('clauses', [False, True])
@pytest.mark.parametrize('card', ['[2..3]', '[2..*]', '[0..2]', '[3..3]', '[1..2]', '[4..5]'])
def test_cardinality_group(tmp_path, card: str, clauses: bool) -> None:
    fm = _read(tmp_path, f'features\n    R\n        optional\n            P\n                {card}\n'
                         '                    A\n                    B\n                    C\n'
                         '                    D\n')
    refactored = CardinalityGroupRefactoring(copy.deepcopy(fm), clauses=clauses).transform()
    assert not any(f.is_cardinality_group() for f in refactored.get_features())
    assert _count(refactored) == _count(fm)


def test_cardinality_group_default_is_one_combination_constraint(tmp_path) -> None:
    fm = _read(tmp_path, 'features\n    R\n        [2..3]\n            A\n            B\n'
                         '            C\n            D\n')
    refactored = CardinalityGroupRefactoring(copy.deepcopy(fm)).transform()
    assert len(refactored.get_constraints()) == 1


def test_cardinality_group_clauses_scale(tmp_path) -> None:
    # [4..*] over 27 children needs 134 million combinations, but only C(27, 24) clauses
    children = ''.join(f'            F{i}\n' for i in range(27))
    fm = _read(tmp_path, f'features\n    R\n        [4..*]\n{children}')
    refactored = CardinalityGroupRefactoring(copy.deepcopy(fm), clauses=True).transform()
    assert len(refactored.get_constraints()) == 2925


def test_cardinality_group_clauses_reject_huge_groups(tmp_path) -> None:
    children = ''.join(f'            F{i}\n' for i in range(30))
    fm = _read(tmp_path, f'features\n    R\n        [15..*]\n{children}')
    with pytest.raises(RefactoringException, match='constraints'):
        CardinalityGroupRefactoring(copy.deepcopy(fm), clauses=True).transform()
