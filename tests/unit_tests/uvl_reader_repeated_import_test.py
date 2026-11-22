"""A model that references the same imported model under two parents gave both references the
same feature names (`S.Spirits` twice), so the sat and z3 backends encoded them as one feature and
answered a satisfiable model as unsatisfiable, while bdd rejected it for a repeated name. The reader
now rejects such a model with a FlamaException on every backend. Regression test for issue #110."""
import os

import pytest

from flamapy.core.exceptions import FlamaException
from flamapy.metamodels.fm_metamodel.transformations import UVLReader

SPIRITS = 'features\n    Spirits\n        alternative\n            Gin\n            Rum\n'


def _read(tmp_path, text: str):
    (tmp_path / 'Spirits.uvl').write_text(SPIRITS)
    (tmp_path / 'Mix.uvl').write_text(text)
    cwd = os.getcwd()
    os.chdir(tmp_path)
    try:
        return UVLReader(str(tmp_path / 'Mix.uvl')).transform()
    finally:
        os.chdir(cwd)


def test_same_import_under_two_parents_raises(tmp_path) -> None:
    with pytest.raises(FlamaException, match='referenced more than once'):
        _read(tmp_path,
              'imports\n    Spirits as S\n'
              'features\n    Mix\n        optional\n'
              '            Three\n                mandatory\n                    S.Spirits\n'
              '            Two\n                mandatory\n                    S.Spirits\n')


def test_single_reference_still_reads(tmp_path) -> None:
    fm = _read(tmp_path,
               'imports\n    Spirits as S\n'
               'features\n    Mix\n        optional\n'
               '            Three\n                mandatory\n                    S.Spirits\n')
    assert 'S.Spirits' in [f.name for f in fm.get_features()]
