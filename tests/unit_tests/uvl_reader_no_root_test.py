"""A UVL file with no 'features' section (an empty file, or one that is only a namespace
declaration) must be rejected with a clean FlamaException, not an uncaught AttributeError
('NoneType' object has no attribute 'feature'). Regression test for issue #108."""
import pytest

from flamapy.core.exceptions import FlamaException
from flamapy.metamodels.fm_metamodel.transformations import UVLReader


def _read(tmp_path, text: str):
    src = tmp_path / 'model.uvl'
    src.write_text(text)
    return UVLReader(str(src)).transform()


def test_empty_file_raises_flama_exception(tmp_path) -> None:
    with pytest.raises(FlamaException):
        _read(tmp_path, '')


def test_namespace_only_file_raises_flama_exception(tmp_path) -> None:
    with pytest.raises(FlamaException):
        _read(tmp_path, 'namespace X\n')
