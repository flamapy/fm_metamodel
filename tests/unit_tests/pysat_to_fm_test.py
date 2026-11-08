"""PysatToFM on SAT models with and without Tseytin auxiliary variables."""
import pytest

from flamapy.core.exceptions import FlamaException
from flamapy.metamodels.fm_metamodel.transformations import UVLReader

pysat = pytest.importorskip('flamapy.metamodels.pysat_metamodel')
from flamapy.metamodels.fm_metamodel.transformations.pysat_to_fm import PysatToFM  # noqa: E402
from flamapy.metamodels.pysat_metamodel.transformations import FmToPysat  # noqa: E402

UVL = '''features
    R
        optional
            A
            B
            C
            D
constraints
    (A & B) | (C & D)
'''


def _fm(tmp_path):
    path = tmp_path / 'model.uvl'
    path.write_text(UVL)
    return UVLReader(str(path)).transform()


def test_distributive_sat_model_converts_back(tmp_path) -> None:
    sat = FmToPysat(_fm(tmp_path), cnf_method='distributive').transform()
    fm = PysatToFM(sat).transform()
    assert {feature.name for feature in fm.get_features()} == {'R', 'A', 'B', 'C', 'D'}


def test_auxiliary_variables_are_refused_with_an_explanation(tmp_path) -> None:
    sat = FmToPysat(_fm(tmp_path), cnf_method='tseytin').transform()
    assert sat.auxiliary_variables
    with pytest.raises(FlamaException, match="cnf_method='distributive'"):
        PysatToFM(sat).transform()
