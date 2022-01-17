from pathlib import Path
import pytest

from famapy.metamodels.fm_metamodel.transformations.uvl_reader import UVLReader


def run(path) -> None:

    raw_model_transformation = UVLReader(path + "/raw_model.uvl")
    raw_model_transformation.transform()
    raw_model = raw_model_transformation.model
    print(raw_model)

    imports_model_transformation = UVLReader(path + "/imports_model.uvl")
    imports_model_transformation.transform()
    imports_model = imports_model_transformation.model
    print(imports_model)

    assert(raw_model == imports_model)


def normalize_path(path: str) -> str:
    return str(Path(__file__).parent.resolve()) + "/" + path


def test_case_1() -> None:
    # Standard use case, testing a few imports with and without "as" statement
    path = normalize_path("models/case_1")
    run(path)


def test_case_2() -> None:
    # Tests imported submodels specified in features block
    path = normalize_path("models/case_2")
    run(path)


def test_case_3() -> None:
    # Tests a missing import inside the feature chain is detected as an error at feature block level
    path = normalize_path("models/case_3")
    with pytest.raises(AssertionError):
        run(path)


def test_case_4() -> None:
    # Tests a missing import inside the feature chain is detected as an error at imports block level
    path = normalize_path("models/case_4")
    with pytest.raises(AssertionError):
        run(path)


def test_case_5() -> None:
    # Tests that only constraints from imported features are imported
    path = normalize_path("models/case_5")
    run(path)


def test_case_6() -> None:
    # Bigger model containing recursive imports
    path = normalize_path("models/case_6")
    run(path)
