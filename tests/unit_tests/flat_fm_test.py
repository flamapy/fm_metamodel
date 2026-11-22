"""Flattening an imported model re-parents the imported relations and features.

The relations copied from the imported root used to keep that discarded root as
their parent, so renaming the features afterwards left stale names behind
(flamapy/bdd_metamodel#39).
"""
import pytest

from flamapy.metamodels.fm_metamodel.transformations import FMSecureFeaturesNames, UVLReader
from flamapy.metamodels.fm_metamodel.transformations.flat_fm import FlatFM

MAIN = 'imports\n    sub as s\nfeatures\n    Root\n        optional\n            s.A\n'
SUB = 'features\n    A\n        optional\n            B\n        alternative\n            C\n            D\n'


def _flat(tmp_path, maintain_namespaces: bool):
    (tmp_path / 'sub.uvl').write_text(SUB)
    (tmp_path / 'main.uvl').write_text(MAIN)
    flat_fm = FlatFM(UVLReader(str(tmp_path / 'main.uvl')).transform())
    flat_fm.set_maintain_namespaces(maintain_namespaces)
    return flat_fm.transform()


@pytest.mark.parametrize('maintain_namespaces', [True, False])
def test_flat_fm_relations_belong_to_their_feature(tmp_path, maintain_namespaces: bool) -> None:
    fm = _flat(tmp_path, maintain_namespaces)
    assert len(fm.get_features()) == 5
    for feature in fm.get_features():
        for relation in feature.get_relations():
            assert relation.parent is feature
            for child in relation.children:
                assert child.parent is feature


def test_flat_fm_secure_names_renames_relation_parents(tmp_path) -> None:
    fm = FMSecureFeaturesNames(_flat(tmp_path, maintain_namespaces=True)).transform()
    assert 's_A' in [feature.name for feature in fm.get_features()]
    for relation in fm.get_relations():
        assert '.' not in relation.parent.name


SUB_CTCS = ("features\n    A\n        optional\n            B {Price 3}\n            C {Name 'x.y'}\n"
            "constraints\n    B => C\n    B.Price > 2\n    sum(Price, B) < 4\n    C.Name == 'x.y'\n")
SUB_NESTED = 'imports\n    leaf as l\nfeatures\n    A\n        optional\n            l.X\n'
LEAF = 'features\n    X\n        optional\n            Y\n            Z\nconstraints\n    Y => Z\n'


def _write_and_read(tmp_path, sub: str, main: str = MAIN):
    (tmp_path / 'main.uvl').write_text(main)
    (tmp_path / 'sub.uvl').write_text(sub)
    (tmp_path / 'leaf.uvl').write_text(LEAF)
    return UVLReader(str(tmp_path / 'main.uvl')).transform()


@pytest.mark.parametrize('maintain_namespaces, expected', [
    (True, ['IMPLIES[s.B][s.C]', 'GREATER[s.B.Price][2]', 'LOWER[SUM[Price][s.B]][4]',
            'EQUALS[s.C.Name][x.y]']),
    (False, ['IMPLIES[B][C]', 'GREATER[B.Price][2]', 'LOWER[SUM[Price][B]][4]',
             'EQUALS[C.Name][x.y]']),
])
def test_flat_fm_imported_constraints_follow_the_feature_names(tmp_path, maintain_namespaces: bool,
                                                               expected: list[str]) -> None:
    flat_fm = FlatFM(_write_and_read(tmp_path, SUB_CTCS))
    flat_fm.set_maintain_namespaces(maintain_namespaces)
    assert [str(ctc.ast) for ctc in flat_fm.transform().get_constraints()] == expected


@pytest.mark.parametrize('maintain_namespaces, expected', [
    (True, ['IMPLIES[s.l.Y][s.l.Z]']),
    (False, ['IMPLIES[Y][Z]']),
])
def test_flat_fm_keeps_constraints_of_nested_imports(tmp_path, maintain_namespaces: bool,
                                                     expected: list[str]) -> None:
    flat_fm = FlatFM(_write_and_read(tmp_path, SUB_NESTED))
    flat_fm.set_maintain_namespaces(maintain_namespaces)
    fm = flat_fm.transform()
    assert [str(ctc.ast) for ctc in fm.get_constraints()] == expected
    feature_names = {feature.name for feature in fm.get_features()}
    for ctc in fm.get_constraints():
        assert set(ctc.ast.get_operands()) <= feature_names


@pytest.mark.parametrize('maintain_namespaces', [True, False])
def test_flat_fm_does_not_mutate_the_source_model(tmp_path, maintain_namespaces: bool) -> None:
    source_model = _write_and_read(tmp_path, SUB_CTCS, main=MAIN + 'constraints\n    s.B => s.C\n')
    sub_model = source_model.imports['sub']
    source_ctcs = [str(ctc.ast) for ctc in source_model.ctcs]
    sub_ctcs = [str(ctc.ast) for ctc in sub_model.ctcs]
    sub_features = [feature.name for feature in sub_model.get_features()]

    flat_fm = FlatFM(source_model)
    flat_fm.set_maintain_namespaces(maintain_namespaces)
    fm = flat_fm.transform()

    assert [str(ctc.ast) for ctc in source_model.ctcs] == source_ctcs
    assert [str(ctc.ast) for ctc in sub_model.ctcs] == sub_ctcs
    assert [feature.name for feature in sub_model.get_features()] == sub_features
    assert source_model.imports and source_model.alias_namespace
    assert not {id(ctc) for ctc in fm.get_constraints()} & {id(ctc) for ctc in sub_model.ctcs}


SPIRITS = 'features\n    Spirits\n        alternative\n            Gin\n            Rum\n'
TWO_PARENTS = ('imports\n{imports}features\n    Mix\n        optional\n'
               '            Three\n                mandatory\n                    {first}.Spirits\n'
               '            Two\n                mandatory\n                    {second}.Spirits\n')


def _flat_names(tmp_path, main: str) -> list[str]:
    (tmp_path / 'Spirits.uvl').write_text(SPIRITS)
    (tmp_path / 'Mix.uvl').write_text(main)
    fm = FlatFM(UVLReader(str(tmp_path / 'Mix.uvl')).transform()).transform()
    for feature in fm.get_features():
        for relation in feature.get_relations():
            assert relation.parent is feature
    return [feature.name for feature in fm.get_features()]


def test_flat_fm_expands_every_alias_of_the_same_model(tmp_path) -> None:
    # Regression: only the first alias of a model was registered, so the second stayed a leaf.
    names = _flat_names(tmp_path, TWO_PARENTS.format(
        imports='    Spirits as S\n    Spirits as T\n', first='S', second='T'))
    assert sorted(names) == sorted(['Mix', 'Three', 'Two', 'S.Spirits', 'S.Gin', 'S.Rum',
                                    'T.Spirits', 'T.Gin', 'T.Rum'])


def test_flat_fm_copies_a_model_referenced_twice(tmp_path) -> None:
    # Regression (flamapy/fm_metamodel#110): the two references shared one subtree, which
    # was namespaced twice ('S.S.Gin') and re-parented to the last reference.
    names = _flat_names(tmp_path, TWO_PARENTS.format(
        imports='    Spirits as S\n', first='S', second='S'))
    assert sorted(names) == sorted(['Mix', 'Three', 'Two'] + ['S.Spirits', 'S.Gin', 'S.Rum'] * 2)
