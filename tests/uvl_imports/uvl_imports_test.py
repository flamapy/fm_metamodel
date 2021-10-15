from pathlib import Path

from famapy.metamodels.fm_metamodel.models.feature_model import Constraint, Feature, FeatureModel, Relation
from famapy.metamodels.fm_metamodel.transformations.uvl_transformation import UVLTransformation


def normalize_path(path: str) -> str:
    return str(Path(__file__).parent.resolve()) + "/" + path


def test_model_imports() -> None:
    raw_model_path = normalize_path("models/raw_model.uvl")

    raw_model_transformation = UVLTransformation(raw_model_path)
    raw_model_transformation.transform()
    raw_model = raw_model_transformation.model

    imports_model_path = normalize_path("models/imports_model.uvl")

    imports_model_transformation = UVLTransformation(imports_model_path)
    imports_model_transformation.transform()
    imports_model = imports_model_transformation.model

    assert(raw_model == imports_model)
