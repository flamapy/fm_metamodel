import os
import filecmp
import tempfile
import pytest


from flamapy.metamodels.fm_metamodel.transformations import XMLReader
from flamapy.metamodels.fm_metamodel.transformations import UVLWriter
from flamapy.metamodels.fm_metamodel.transformations import UVLReader


# Directory containing your .uvl files
XML_FILES_DIRECTORY = 'resources/models'

def get_models():
    """Function to recursively retrieve file paths and fm's"""
    models = []
    for root, dirs, files in os.walk(XML_FILES_DIRECTORY):
        for file in files:
            if file.endswith('.xml'):
                path = os.path.join(root, file)
                fm = XMLReader(path=path).transform()
                models.append(fm)
    return models

def pytest_generate_tests(metafunc):
    if "model" in metafunc.fixturenames:
        metafunc.parametrize("model", get_models())
        
def test_write_compare_uvl(model):
    assert model, "File could not be read returned empty content"
    
    with tempfile.NamedTemporaryFile(mode='w+', delete=False) as uvl_file:
        uvl_file_path = uvl_file.name
        UVLWriter(path=uvl_file_path, source_model=model).transform()
        
        fm_from_uvl=UVLReader(path=uvl_file_path).transform()
        with tempfile.NamedTemporaryFile(mode='w+', delete=False) as uvl_file_second:
            uvl_file_path_second = uvl_file_second.name
            UVLWriter(path=uvl_file_path_second,source_model=fm_from_uvl).transform()
            
            fm_from_uvl_2=UVLReader(path=uvl_file_path).transform()

            assert str(fm_from_uvl_2)==str(fm_from_uvl), "the two fms are not equal"
            # Step 3: Compare the original and new file
            assert filecmp.cmp(uvl_file_path, uvl_file_path_second), f"Files {uvl_file_path} and {uvl_file_path_second} are not identical"

    # File deletion should be here, outside of the 'with' block
    os.remove(uvl_file_path)
    os.remove(uvl_file_path_second)