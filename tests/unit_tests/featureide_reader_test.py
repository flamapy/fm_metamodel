"""FeatureIDEReader must read the constraint elements FeatureIDE writes (XmlFeatureModelFormat)."""
from flamapy.metamodels.fm_metamodel.transformations import FeatureIDEReader


def _count(fm) -> int:
    from flamapy.metamodels.pysat_metamodel.operations import PySATConfigurationsNumber
    from flamapy.metamodels.pysat_metamodel.transformations import FmToPysat
    return PySATConfigurationsNumber().execute(FmToPysat(fm).transform()).get_result()


def test_reader_folds_nary_conjunctions_and_atmost1(tmp_path) -> None:
    path = tmp_path / 'fide.xml'
    path.write_text('<featureModel><struct><and name="R"><feature name="A"/><feature name="B"/>'
                    '<feature name="C"/></and></struct><constraints>'
                    '<rule><disj><conj><var>A</var><var>B</var><var>C</var></conj>'
                    '<not><var>A</var></not></disj></rule>'
                    '<rule><atmost1><var>A</var><var>B</var><var>C</var></atmost1></rule>'
                    '</constraints></featureModel>')
    fm = FeatureIDEReader(str(path)).transform()
    # A,B,C optional; (A and B and C) or not A; at most one of A, B, C: {}, {B}, {C}
    assert _count(fm) == 3
