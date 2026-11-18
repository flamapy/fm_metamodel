"""A single child under a cardinality other than [1..1] and [0..1] is a cardinality group.

Such a relation used to match no relation shape (flamapy/bdd_metamodel#37,
flamapy/z3_metamodel#2), so writers and operations dropped or mislabelled its constraint.
"""
import pytest

from flamapy.core.exceptions import FlamaException

from flamapy.metamodels.fm_metamodel.models import Feature, Relation
from flamapy.metamodels.fm_metamodel.operations import (
    FMAtomicSets,
    FMCoreFeatures,
    FMEstimatedConfigurationsNumber,
    FMLanguageLevel,
)
from flamapy.metamodels.fm_metamodel.transformations import (
    ClaferWriter,
    FeatureIDEReader,
    FeatureIDEWriter,
    GlencoeReader,
    GlencoeWriter,
    JSONReader,
    JSONWriter,
    PLWriter,
    SPLOTWriter,
    UVLReader,
    UVLWriter,
)
from flamapy.metamodels.fm_metamodel.transformations.refactorings import (
    CardinalityGroupRefactoring,
)

# cardinality of the group over one child, and the number of configurations of the model
ONE_CHILD_GROUPS = [
    ('0..0', 1),  # the child can never be selected
    ('1..2', 1),  # the child is required
    ('2..2', 0),  # impossible: the model is void
    ('0..3', 2),  # the child is optional
    ('1..*', 1),  # unbounded upper bound
]


def _count(fm) -> int:
    from flamapy.metamodels.pysat_metamodel.operations import PySATConfigurationsNumber
    from flamapy.metamodels.pysat_metamodel.transformations import FmToPysat
    return PySATConfigurationsNumber().execute(FmToPysat(fm).transform()).get_result()


def _read(tmp_path, cardinality: str):
    src = tmp_path / 'model.uvl'
    src.write_text(f'features\n    Root\n        [{cardinality}]\n            F0\n')
    return UVLReader(str(src)).transform()


def _relation(card_min: int, card_max: int, children: int = 1) -> Relation:
    parent = Feature('Root')
    return Relation(parent, [Feature(f'F{i}', parent=parent) for i in range(children)],
                    card_min, card_max)


@pytest.mark.parametrize('card_min, card_max', [(0, 0), (1, 2), (2, 2), (0, 3), (1, -1)])
def test_one_child_cardinality_is_a_cardinality_group(card_min, card_max) -> None:
    relation = _relation(card_min, card_max)
    assert relation.is_group() and relation.is_cardinal()
    assert not relation.is_mandatory() and not relation.is_optional()
    assert str(relation).startswith('(cardinality)')


@pytest.mark.parametrize('card_min, card_max, children', [(1, 1, 1), (0, 1, 1), (0, 0, 0)])
def test_mandatory_optional_and_empty_relations_are_not_groups(card_min, card_max,
                                                               children) -> None:
    relation = _relation(card_min, card_max, children)
    assert not relation.is_group() and not relation.is_cardinal()


@pytest.mark.parametrize('cardinality, expected', ONE_CHILD_GROUPS)
def test_feature_and_language_level(tmp_path, cardinality, expected) -> None:
    fm = _read(tmp_path, cardinality)
    assert fm.root.is_cardinality_group() and fm.root.is_group()
    assert 'GROUP_CARDINALITY' in str(FMLanguageLevel().execute(fm).get_result())


@pytest.mark.parametrize('cardinality, expected', ONE_CHILD_GROUPS)
def test_estimated_configurations_number(tmp_path, cardinality, expected) -> None:
    fm = _read(tmp_path, cardinality)
    assert FMEstimatedConfigurationsNumber().execute(fm).get_result() == expected


def test_estimated_configurations_number_of_an_unbounded_group(tmp_path) -> None:
    src = tmp_path / 'model.uvl'
    src.write_text('features\n    Root\n        [1..*]\n            A\n            B\n')
    fm = UVLReader(str(src)).transform()
    assert FMEstimatedConfigurationsNumber().execute(fm).get_result() == 3


@pytest.mark.parametrize('cardinality, expected', ONE_CHILD_GROUPS)
@pytest.mark.parametrize('writer, reader, extension', [
    (UVLWriter, UVLReader, 'uvl'),
    (JSONWriter, JSONReader, 'json'),
    (GlencoeWriter, GlencoeReader, 'gfm.json'),
])
def test_round_trip_keeps_the_configurations(tmp_path, writer, reader, extension,
                                             cardinality, expected) -> None:
    out = tmp_path / f'out.{extension}'
    writer(str(out), _read(tmp_path, cardinality)).transform()
    assert _count(reader(str(out)).transform()) == expected


@pytest.mark.parametrize('cardinality, expected', ONE_CHILD_GROUPS)
def test_compact_uvl_keeps_the_group(tmp_path, cardinality, expected) -> None:
    out = tmp_path / 'out.uvl'
    writer = UVLWriter(str(out), _read(tmp_path, cardinality))
    writer.set_compact_relations(True)
    writer.transform()
    assert _count(UVLReader(str(out)).transform()) == expected


@pytest.mark.parametrize('cardinality, expected', ONE_CHILD_GROUPS)
def test_featureide_rejects_or_refactors_the_group(tmp_path, cardinality, expected) -> None:
    out = tmp_path / 'out.xml'
    with pytest.raises(FlamaException, match='CardinalityGroupRefactoring'):
        FeatureIDEWriter(str(out), _read(tmp_path, cardinality)).transform()
    FeatureIDEWriter(str(out), _read(tmp_path, cardinality), apply_refactorings=True).transform()
    assert _count(FeatureIDEReader(str(out)).transform()) == expected


@pytest.mark.parametrize('cardinality, expected', ONE_CHILD_GROUPS)
def test_cardinality_group_refactoring_keeps_the_configurations(tmp_path, cardinality,
                                                                expected) -> None:
    fm = CardinalityGroupRefactoring(_read(tmp_path, cardinality), clauses=True).transform()
    assert not any(feature.is_cardinality_group() for feature in fm.get_features())
    assert _count(fm) == expected


@pytest.mark.parametrize('cardinality, formula', [
    ('0..0', 'Root\n(F0 -> Root) and (not F0)'),
    ('1..2', 'Root\n(F0 -> Root) and (Root -> (F0))'),
    ('2..2', 'Root\n(F0 -> Root) and (not Root)'),
    ('0..3', 'Root\n(F0 -> Root)'),
])
def test_pl_writer_writes_the_group(tmp_path, cardinality, formula) -> None:
    out = tmp_path / 'out.txt'
    PLWriter(str(out), _read(tmp_path, cardinality)).transform()
    assert out.read_text().strip() == formula


@pytest.mark.parametrize('cardinality, group', [
    ('0..0', ':g [0,0]'), ('1..2', ':g [1,2]'), ('1..*', ':g [1,*]'),
])
def test_splot_writer_writes_the_group(tmp_path, cardinality, group) -> None:
    lines = SPLOTWriter(None, _read(tmp_path, cardinality)).transform().splitlines()
    assert [line.strip() for line in lines[4:6]] == [group, ': F0 (F0)']


def test_splot_writer_writes_mutex_and_cardinality_groups(tmp_path) -> None:
    src = tmp_path / 'model.uvl'
    src.write_text('features\n    Root\n        [0..1]\n            A\n            B\n'
                   '        [2..3]\n            C\n            D\n            E\n')
    text = SPLOTWriter(None, UVLReader(str(src)).transform()).transform()
    assert ':g [0,1]' in text and ':g [2,3]' in text
    assert all(f': {name} ({name})' in text for name in 'ABCDE')


@pytest.mark.parametrize('cardinality, expected', ONE_CHILD_GROUPS)
def test_clafer_writer_writes_the_group_cardinality(tmp_path, cardinality, expected) -> None:
    out = tmp_path / 'out.txt'
    ClaferWriter(str(out), _read(tmp_path, cardinality)).transform()
    assert f'abstract {cardinality} Root\n\tF0 ?' in out.read_text()


def _names(features) -> list[str]:
    return sorted(feature.name for feature in features)


def _sat_core(fm) -> list[str]:
    from flamapy.metamodels.pysat_metamodel.operations import PySATCoreFeatures
    from flamapy.metamodels.pysat_metamodel.transformations import FmToPysat
    return sorted(PySATCoreFeatures().execute(FmToPysat(fm).transform()).get_result())


def _sat_atomic_sets(fm) -> list[list[str]]:
    from flamapy.metamodels.pysat_metamodel.operations import PySATAtomicSets
    from flamapy.metamodels.pysat_metamodel.transformations import FmToPysat
    result = PySATAtomicSets().execute(FmToPysat(fm).transform()).get_result()
    return sorted(sorted(atomic_set) for atomic_set in result)


# groups that need all their children, with a mandatory grandchild under the first one
REQUIRED_CHILDREN = [
    ('[1..2]', 'F0'), ('[1..*]', 'F0'), ('[2..2]', 'F0\n            F1'),
    ('[2..*]', 'F0\n            F1'), ('[2..3]', 'F0\n            F1'),
]


@pytest.mark.parametrize('card_min, card_max, children, expected', [
    (1, 1, 1, True), (1, 2, 1, True), (1, -1, 1, True), (2, 2, 2, True), (2, -1, 2, True),
    (0, 1, 1, False), (0, 0, 1, False), (1, 1, 2, False), (1, 2, 2, False),
    (2, 2, 1, False),  # impossible: the parent cannot be selected
    (2, 1, 2, False),  # inconsistent bounds
    (0, 0, 0, False),
])
def test_requires_all_children(card_min, card_max, children, expected) -> None:
    assert _relation(card_min, card_max, children).requires_all_children() is expected


@pytest.mark.parametrize('cardinality, children', REQUIRED_CHILDREN)
def test_core_features_and_atomic_sets_follow_required_children(tmp_path, cardinality,
                                                                children) -> None:
    src = tmp_path / 'model.uvl'
    src.write_text(f'features\n    Root\n        {cardinality}\n            {children}\n'
                   '        optional\n            Opt\n                mandatory\n'
                   '                    Sub\n')
    read = lambda: UVLReader(str(src)).transform()  # noqa: E731
    expected = sorted(['Root'] + children.split())
    assert _names(FMCoreFeatures().execute(read()).get_result()) == expected == _sat_core(read())
    atomic_sets = sorted(_names(s) for s in FMAtomicSets().execute(read()).get_result())
    assert atomic_sets == sorted([['Opt', 'Sub'], expected]) == _sat_atomic_sets(read())


@pytest.mark.parametrize('cardinality', ['[0..0]', '[0..3]', '[0..1]'])
def test_an_optional_or_forbidden_child_is_not_core(tmp_path, cardinality) -> None:
    fm = _read(tmp_path, cardinality.strip('[]'))
    assert _names(FMCoreFeatures().execute(fm).get_result()) == ['Root']
    assert sorted(_names(s) for s in FMAtomicSets().execute(fm).get_result()) == [['F0'],
                                                                                  ['Root']]


@pytest.mark.parametrize('cardinality, expected', ONE_CHILD_GROUPS + [('3..3', 0)])
@pytest.mark.parametrize('clauses', [False, True])
def test_cardinality_group_refactoring_of_any_group(tmp_path, cardinality, expected,
                                                    clauses) -> None:
    """Both encodings, an impossible cardinality included (it makes the feature dead)."""
    fm = CardinalityGroupRefactoring(_read(tmp_path, cardinality), clauses=clauses).transform()
    assert _count(fm) == expected
