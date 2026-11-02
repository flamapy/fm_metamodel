"""FeatureIDEWriter must write elements FeatureIDE reads (XmlFeatureModelFormat) and keep the
semantics: every case is written, read back and its configurations counted."""
import pytest

from flamapy.core.exceptions import FlamaException
from flamapy.metamodels.fm_metamodel.transformations.refactorings import RefactoringException
from flamapy.metamodels.fm_metamodel.transformations import (
    FeatureIDEReader,
    FeatureIDEWriter,
    UVLReader,
)


def _count(fm) -> int:
    from flamapy.metamodels.pysat_metamodel.operations import PySATConfigurationsNumber
    from flamapy.metamodels.pysat_metamodel.transformations import FmToPysat
    return PySATConfigurationsNumber().execute(FmToPysat(fm).transform()).get_result()


def _write_read(tmp_path, uvl: str, apply_refactorings: bool = False):
    src = tmp_path / 'model.uvl'
    src.write_text(uvl)
    fm = UVLReader(str(src)).transform()
    out = tmp_path / 'model.xml'
    text = FeatureIDEWriter(str(out), fm, apply_refactorings=apply_refactorings).transform()
    return fm, FeatureIDEReader(str(out)).transform(), text


@pytest.mark.parametrize('uvl', [
    # a group and other relations on the same feature
    'features\n    R\n        mandatory\n            B\n        optional\n            E\n'
    '        alternative\n            C\n            D\n',
    # two groups
    'features\n    R\n        or\n            A\n            B\n        alternative\n            C\n'
    '            D\n',
    # mutex group: none or exactly one
    'features\n    R\n        [0..1]\n            A\n            B\n            C\n',
    # cardinality groups: bounded, lower bound only, upper bound only, impossible
    'features\n    R\n        [2..3]\n            A\n            B\n            C\n            D\n',
    'features\n    R\n        optional\n            P\n                [2..*]\n'
    '                    A\n                    B\n                    C\n',
    'features\n    R\n        optional\n            P\n                [0..2]\n'
    '                    A\n                    B\n                    C\n',
    'features\n    R\n        optional\n            P\n                [4..5]\n'
    '                    A\n                    B\n                    C\n',
    # constraints: excludes-like, xor, equivalence
    'features\n    R\n        optional\n            A\n            B\n            C\n'
    'constraints\n    A => !B\n    !(A <=> C)\n    B <=> C\n',
])
def test_configurations_are_preserved_with_refactorings(tmp_path, uvl: str) -> None:
    original, back, _ = _write_read(tmp_path, uvl, apply_refactorings=True)
    assert _count(back) == _count(original)


@pytest.mark.parametrize('uvl', [
    'features\n    R\n        mandatory\n            B\n        alternative\n            C\n'
    '            D\n',
    'features\n    R\n        [0..1]\n            A\n            B\n',
    'features\n    R\n        [2..3]\n            A\n            B\n            C\n',
])
def test_unrepresentable_groups_are_rejected_by_default(tmp_path, uvl: str) -> None:
    with pytest.raises(FlamaException, match='apply_refactorings'):
        _write_read(tmp_path, uvl)


def test_original_model_is_not_modified(tmp_path) -> None:
    src = tmp_path / 'model.uvl'
    src.write_text(MIXED_UVL)
    fm = UVLReader(str(src)).transform()
    before = sorted(f.name for f in fm.get_features())
    FeatureIDEWriter(str(tmp_path / 'out.xml'), fm, apply_refactorings=True).transform()
    assert sorted(f.name for f in fm.get_features()) == before


def test_names_are_not_quoted(tmp_path) -> None:
    _, back, text = _write_read(tmp_path, 'features\n    R\n        optional\n            "5 MP"\n'
                                          'constraints\n    "5 MP" => R\n')
    assert 'name="5 MP"' in text
    assert '<var>5 MP</var>' in text
    assert '&quot;' not in text
    assert '5 MP' in {f.name for f in back.get_features()}


def test_only_featureide_constraint_elements(tmp_path) -> None:
    _, _, text = _write_read(tmp_path, 'features\n    R\n        optional\n            A\n'
                                       '            B\nconstraints\n    !(A <=> B)\n')
    assert '<impn' not in text and '<alt>' not in text


MIXED_UVL = ('features\n    R\n        mandatory\n            B\n'
             '        alternative\n            C\n            D\n')


def test_refactoring_adds_an_abstract_feature_for_the_group(tmp_path) -> None:
    _, _, text = _write_read(tmp_path, MIXED_UVL, apply_refactorings=True)
    assert '<alt mandatory="true" abstract="true" name="R_1">' in text


def test_huge_cardinality_group_is_rejected(tmp_path) -> None:
    children = ''.join(f'            F{i}\n' for i in range(30))
    with pytest.raises(RefactoringException, match='constraints'):
        _write_read(tmp_path, f'features\n    R\n        [15..*]\n{children}', apply_refactorings=True)


def test_repeated_names_are_rejected(tmp_path) -> None:
    with pytest.raises(FlamaException, match='repeated'):
        _write_read(tmp_path, 'features\n    R\n        optional\n            A\n'
                              '                optional\n                    X\n'
                              '            B\n                optional\n                    X\n')


def test_large_cardinality_group_is_written(tmp_path) -> None:
    # [4..*] over 27 children (as in a UVLHub model) used to need 134 million combinations
    children = ''.join(f'            F{i}\n' for i in range(27))
    _, _, text = _write_read(tmp_path, f'features\n    R\n        [4..*]\n{children}',
                             apply_refactorings=True)
    assert text.count('<rule>') == 2925           # C(27, 24) at-least clauses


def test_arithmetic_constraints_are_omitted(tmp_path) -> None:
    _, back, text = _write_read(tmp_path, 'features\n    R\n        optional\n            A {Price 3}\n'
                                          '            B\nconstraints\n    A => B\n    A.Price > 2\n')
    assert len(back.get_constraints()) == 1
    assert '<imp>' in text
