import pytest

from flamapy.core.exceptions import FlamaException
from flamapy.metamodels.fm_metamodel.models import FeatureModel
from flamapy.metamodels.fm_metamodel.transformations import FMSecureFeaturesNames, UVLReader


def _read_uvl(tmp_path, content: str) -> FeatureModel:
    path = tmp_path / "model.uvl"
    path.write_text(content)
    return UVLReader(str(path)).transform()


def test_safe_names_are_kept(tmp_path) -> None:
    feature_model = _read_uvl(tmp_path, """features
    Root
        optional
            A
            B
constraints
    A => B
""")
    op = FMSecureFeaturesNames(feature_model)
    secured = op.transform()
    assert op.mapping_names == {"Root": "Root", "A": "A", "B": "B"}
    assert [f.name for f in secured.get_features()] == ["Root", "A", "B"]


def test_names_sanitizing_to_the_same_string_stay_distinct(tmp_path) -> None:
    # 'A B' sanitizes to 'A_B', which is also the name of another feature.
    feature_model = _read_uvl(tmp_path, """features
    Root
        optional
            "A B"
            A_B
constraints
    "A B" => !A_B
""")
    op = FMSecureFeaturesNames(feature_model)
    secured = op.transform()
    secured_names = [f.name for f in secured.get_features()]
    assert len(set(secured_names)) == len(secured_names) == 3
    assert op.mapping_names["A B"] != op.mapping_names["A_B"]
    # Each constraint term is renamed to its own feature's secure name
    constraint = secured.get_constraints()[0].ast
    assert constraint.root.left.data == op.mapping_names["A B"]
    assert constraint.root.right.left.data == op.mapping_names["A_B"]


def test_repeated_feature_names_are_rejected(tmp_path) -> None:
    feature_model = _read_uvl(tmp_path, """features
    Root
        optional
            A
                optional
                    X
            B
                optional
                    X
""")
    with pytest.raises(FlamaException, match="repeated feature name.*X"):
        FMSecureFeaturesNames(feature_model).transform()
