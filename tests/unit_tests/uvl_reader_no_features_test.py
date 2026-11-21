"""A UVL model without a 'features' section is rejected with a FlamaException.

The grammar makes the section optional, so the reader used to crash with an
AttributeError on such files (flamapy/fm_metamodel#108).
"""
import pytest

from flamapy.core.exceptions import FlamaException
from flamapy.metamodels.fm_metamodel.transformations import UVLReader


@pytest.mark.parametrize('content', ['', 'namespace X\n'], ids=['empty', 'namespace_only'])
def test_model_without_features_section_raises_flama_exception(tmp_path, content: str) -> None:
    src = tmp_path / 'model.uvl'
    src.write_text(content)
    with pytest.raises(FlamaException, match="no 'features' section"):
        UVLReader(str(src)).transform()
