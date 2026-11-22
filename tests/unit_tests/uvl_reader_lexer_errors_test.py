"""A character the UVL lexer cannot tokenize must make the reader fail, as a parser error does,
instead of being skipped so that an invalid model is read as a different one (a feature `_A` read
as `A`). Regression test for issue #112."""
import pytest

from flamapy.core.exceptions import FlamaException
from flamapy.metamodels.fm_metamodel.transformations import UVLReader


def _read(tmp_path, text: str):
    src = tmp_path / 'model.uvl'
    src.write_text(text)
    return UVLReader(str(src)).transform()


def test_leading_underscore_raises_flama_exception(tmp_path) -> None:
    with pytest.raises(FlamaException):
        _read(tmp_path, 'features\n    Root\n        optional\n            _A\n            B\n')


def test_dot_in_quoted_feature_name_raises_flama_exception(tmp_path) -> None:
    with pytest.raises(FlamaException):
        _read(tmp_path, 'features\n    "Fun.Fun"\n')


def test_valid_model_still_reads(tmp_path) -> None:
    fm = _read(tmp_path, 'features\n    Root\n        optional\n            A\n            B\n')
    assert sorted(f.name for f in fm.get_features()) == ['A', 'B', 'Root']
