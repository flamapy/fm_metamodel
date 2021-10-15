from pathlib import Path

from famapy.metamodels.fm_metamodel.models.feature_model import Constraint, Feature, FeatureModel, Relation
from famapy.metamodels.fm_metamodel.transformations.uvl_transformation import UVLTransformation


def run(path) -> None:

    raw_model_transformation = UVLTransformation(path, "raw_model.uvl")
    raw_model_transformation.transform()
    raw_model = raw_model_transformation.model
    print(raw_model)

    imports_model_transformation = UVLTransformation(path, "imports_model.uvl")
    imports_model_transformation.transform()
    imports_model = imports_model_transformation.model
    print(imports_model)

    assert(raw_model == imports_model)


def normalize_path(path: str) -> str:
    return str(Path(__file__).parent.resolve()) + "/" + path


def test_case_1() -> None:
    path = normalize_path("models/case_1")
    run(path)


# TODO: Add more tests, testing ctcs imports and iterative imports
