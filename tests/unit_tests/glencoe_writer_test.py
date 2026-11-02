"""GlencoeWriter: names, groups and configurations survive a round-trip."""
import pytest

from flamapy.core.exceptions import FlamaException

from flamapy.metamodels.fm_metamodel.transformations import (
    GlencoeReader,
    GlencoeWriter,
    UVLReader,
)


def _count(fm) -> int:
    from flamapy.metamodels.pysat_metamodel.operations import PySATConfigurationsNumber
    from flamapy.metamodels.pysat_metamodel.transformations import FmToPysat
    return PySATConfigurationsNumber().execute(FmToPysat(fm).transform()).get_result()


def _read_uvl(tmp_path, uvl: str):
    src = tmp_path / 'model.uvl'
    src.write_text(uvl)
    return UVLReader(str(src)).transform()


GROUP_SHAPES = [
    # a group and other relations on the same feature
    'features\n    R\n        mandatory\n            B\n        optional\n            E\n'
    '        alternative\n            C\n            D\n',
    # two groups
    'features\n    R\n        or\n            A\n            B\n        alternative\n            C\n'
    '            D\n',
    # mutex and cardinality groups
    'features\n    R\n        [0..1]\n            A\n            B\n            C\n',
    'features\n    R\n        optional\n            P\n                [2..*]\n'
    '                    A\n                    B\n                    C\n',
]


@pytest.mark.parametrize('uvl', GROUP_SHAPES)
def test_configurations_are_preserved(tmp_path, uvl) -> None:
    fm = _read_uvl(tmp_path, uvl)
    out = tmp_path / 'out.gfm.json'
    GlencoeWriter(str(out), fm, apply_refactorings=True).transform()
    assert _count(GlencoeReader(str(out)).transform()) == _count(fm)


def test_names_are_written_as_they_are(tmp_path) -> None:
    fm = _read_uvl(tmp_path, 'features\n    R\n        optional\n            "5 MP"\n'
                             '            "Wi-Fi"\nconstraints\n    "5 MP" => "Wi-Fi"\n')
    out = tmp_path / 'out.gfm.json'
    GlencoeWriter(str(out), fm).transform()
    back = GlencoeReader(str(out)).transform()
    assert {f.name for f in back.get_features()} == {'R', '5 MP', 'Wi-Fi'}
    assert _count(back) == _count(fm)


def test_arithmetic_constraints_are_omitted(tmp_path) -> None:
    fm = _read_uvl(tmp_path, 'features\n    R\n        optional\n            A {Price 3}\n'
                             '            B\nconstraints\n    A => B\n    A.Price > 2\n')
    text = GlencoeWriter(str(tmp_path / 'out.gfm.json'), fm).transform()
    assert 'GREATER' not in text


@pytest.mark.parametrize('uvl', GROUP_SHAPES[:2])
def test_mixed_decompositions_are_rejected_by_default(tmp_path, uvl) -> None:
    with pytest.raises(FlamaException, match='apply_refactorings'):
        GlencoeWriter(str(tmp_path / 'out.gfm.json'), _read_uvl(tmp_path, uvl)).transform()


@pytest.mark.parametrize('uvl', GROUP_SHAPES[2:])
def test_mutex_and_cardinality_groups_need_no_refactoring(tmp_path, uvl) -> None:
    fm = _read_uvl(tmp_path, uvl)
    out = tmp_path / 'out.gfm.json'
    GlencoeWriter(str(out), fm).transform()
    assert _count(GlencoeReader(str(out)).transform()) == _count(fm)
