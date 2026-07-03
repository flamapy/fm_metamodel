import pytest

from flamapy.core.exceptions import FlamaException
from flamapy.metamodels.fm_metamodel.models import FeatureModel
from flamapy.metamodels.fm_metamodel.operations import GenerateRandomFeatureModel


def _generate(level: str, seed: int = 3, num_features: int = 12) -> FeatureModel:
    op = GenerateRandomFeatureModel()
    op.set_num_features(num_features)
    op.set_seed(seed)
    op.set_language_level(level)
    return op.execute().get_result()


def test_generates_a_feature_model() -> None:
    op = GenerateRandomFeatureModel()
    op.set_num_features(8)
    op.set_seed(0)
    result = op.execute().get_result()
    assert isinstance(result, FeatureModel)
    assert len(result.get_features()) == 8


def test_seed_is_deterministic() -> None:
    def build() -> FeatureModel:
        op = GenerateRandomFeatureModel()
        op.set_num_features(10)
        op.set_seed(42)
        return op.execute().get_result()

    first = {f.name for f in build().get_features()}
    second = {f.name for f in build().get_features()}
    assert first == second


def test_void_flag_adds_constraints() -> None:
    # Verifying unsatisfiability needs a solver (not an fm_metamodel dependency); here we just
    # check that the void flag injects the extra contradictory constraints into the model.
    def constraint_count(void: bool) -> int:
        op = GenerateRandomFeatureModel()
        op.set_num_features(8)
        op.set_seed(1)
        op.set_max_constraints(0)
        op.set_void(void)
        return len(op.execute().get_result().get_constraints())

    assert constraint_count(True) > constraint_count(False)


def test_boolean_level_is_default_and_purely_boolean() -> None:
    fm = _generate('boolean')
    assert all(feature.is_boolean() for feature in fm.get_features())
    assert all(not ctc.is_arithmetic_constraint() for ctc in fm.get_constraints())


def test_typed_level_has_non_boolean_features() -> None:
    fm = _generate('typed')
    assert any(not feature.is_boolean() for feature in fm.get_features())


def test_arithmetic_level_has_arithmetic_constraints_over_boolean_features() -> None:
    fm = _generate('arithmetic')
    assert all(feature.is_boolean() for feature in fm.get_features())
    assert any(ctc.is_arithmetic_constraint() for ctc in fm.get_constraints())


def test_unknown_language_level_raises() -> None:
    op = GenerateRandomFeatureModel()
    with pytest.raises(FlamaException):
        op.set_language_level('nope')
