"""A global constraint that names no feature, such as `sum(P) == 3`, must stay global under the
feature cardinality refactoring. Before, `all(...)` over its empty feature list was vacuously true,
so the constraint was replaced by one copy per clone, each guarded by that clone, and a model with
no clone selected escaped it. Regression test for issue #111."""
from flamapy.metamodels.fm_metamodel.transformations import UVLReader
from flamapy.metamodels.fm_metamodel.transformations.refactorings.feature_cardinality_refactoring_paper import (  # noqa: E501
    FeatureCardinalityRefactoring,
)


def _refactor(tmp_path, text: str):
    src = tmp_path / 'model.uvl'
    src.write_text(text)
    fm = UVLReader(str(src)).transform()
    return FeatureCardinalityRefactoring(fm).transform()


def test_constraint_without_features_stays_global(tmp_path) -> None:
    fm = _refactor(tmp_path,
                   'features\n    R\n        mandatory\n            B {P 5}\n'
                   '        optional\n            A cardinality [1..2] {P 1}\n'
                   'constraints\n    sum(P) == 3\n')
    assert [str(c.ast) for c in fm.get_constraints()] == ['EQUALS[SUM[P][]][3]']


def test_constraint_inside_clone_is_still_contextualized(tmp_path) -> None:
    fm = _refactor(tmp_path,
                   'features\n    R\n        optional\n            A cardinality [1..2]\n'
                   '                optional\n                    C\n                    D\n'
                   'constraints\n    C => D\n')
    assert len(fm.get_constraints()) == 2
