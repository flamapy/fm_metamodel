import os
import filecmp
import tempfile


from flamapy.metamodels.fm_metamodel.transformations import XMLReader, UVLWriter, UVLReader, GlencoeWriter, GlencoeReader, AFMReader, AFMWriter


# Directory containing your .uvl files
XML_FILES_DIRECTORY = 'resources/models/fama_test_suite'

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
    
def test_write_compare_glencoe(model):
    assert model, "File could not be read returned empty content"
    
    with tempfile.NamedTemporaryFile(mode='w+', delete=False) as glencoe_file:
        glencoe_file_path = glencoe_file.name
        GlencoeWriter(path=glencoe_file_path, source_model=model).transform()
        
        fm_from_glencoe=GlencoeReader(path=glencoe_file_path).transform()
        with tempfile.NamedTemporaryFile(mode='w+', delete=False) as glencoe_file_second:
            glencoe_file_path_second = glencoe_file_second.name
            GlencoeWriter(path=glencoe_file_path_second,source_model=fm_from_glencoe).transform()
            
            fm_from_glencoe_2=GlencoeReader(path=glencoe_file_path_second).transform()

            assert (len(fm_from_glencoe_2.get_features())==len(fm_from_glencoe.get_features())
                    and len(fm_from_glencoe_2.get_relations()) == len(fm_from_glencoe.get_relations()) 
                    and len(fm_from_glencoe_2.get_constraints()) == len(fm_from_glencoe.get_constraints()) 
                    ), "the two fms are not equal"
            # Step 3: Compare the original and new file
            assert filecmp.cmp(glencoe_file_path, glencoe_file_path_second), f"Files {glencoe_file_path} and {glencoe_file_path_second} are not identical."

    # File deletion should be here, outside of the 'with' block
    os.remove(glencoe_file_path)
    os.remove(glencoe_file_path_second)

def test_write_compare_afm(model):
    assert model, "File could not be read returned empty content"
    
    with tempfile.NamedTemporaryFile(mode='w+', delete=False) as afm_file:
        afm_file_path = afm_file.name
        AFMWriter(path=afm_file_path, source_model=model).transform()
        
        fm_from_afm=AFMReader(path=afm_file_path).transform()
        with tempfile.NamedTemporaryFile(mode='w+', delete=False) as afm_file_second:
            afm_file_path_second = afm_file_second.name
            AFMWriter(path=afm_file_path_second,source_model=fm_from_afm).transform()
            
            fm_from_afm_2=AFMReader(path=afm_file_path).transform()

            assert str(fm_from_afm_2)==str(fm_from_afm), "the two fms are not equal"
            # Step 3: Compare the original and new file
            assert filecmp.cmp(afm_file_path, afm_file_path_second), f"Files {afm_file_path} and {afm_file_path_second} are not identical"

    # File deletion should be here, outside of the 'with' block
    os.remove(afm_file_path)
    os.remove(afm_file_path_second)