from pathlib import Path

from famapy.metamodels.fm_metamodel.transformations.uvl_reader import UVLReader
from famapy.metamodels.fm_metamodel.transformations.uvl_writter import UVLWriter

def run(path) -> None:

    model_reader = UVLReader(path)
    model_reader.transform()
    model = model_reader.model
    
    model_writer = UVLWriter(model, path + "_writen") 
    model_writer.transform()

    model_reader_2 = UVLReader(path + "_writen")
    model_reader_2.transform()
    model_2 = model_reader_2.model

    assert(model == model_2)

def normalize_path(path: str) -> str:
    return str(Path(__file__).parent.resolve()) + "/" + path

def test_case_1() -> None:
    # Standard use case, testing a few imports with and without "as" statement
    path = normalize_path("models/pizzas.uvl")
    run(path)

