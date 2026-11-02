"""XMLWriter writes every binary constraint FaMa XML can express and reports the rest."""
import pytest

from flamapy.metamodels.fm_metamodel.transformations import (
    UVLReader,
    XMLReader,
    XMLWriter,
)


def _count(fm) -> int:
    from flamapy.metamodels.pysat_metamodel.operations import PySATConfigurationsNumber
    from flamapy.metamodels.pysat_metamodel.transformations import FmToPysat
    return PySATConfigurationsNumber().execute(FmToPysat(fm).transform()).get_result()


def _read_uvl(tmp_path, uvl: str):
    src = tmp_path / 'model.uvl'
    src.write_text(uvl)
    return UVLReader(str(src)).transform()


@pytest.mark.parametrize('constraint', ['A => !B', '!(A & B)', 'A <=> B', 'A => B'])
def test_xml_writer_expresses_binary_constraints(tmp_path, constraint) -> None:
    fm = _read_uvl(tmp_path, 'features\n    R\n        optional\n            A\n            B\n'
                             f'constraints\n    {constraint}\n')
    out = tmp_path / 'out.xml'
    writer = XMLWriter(str(out), fm)
    writer.transform()
    assert writer.skipped_constraints == []
    assert _count(XMLReader(str(out)).transform()) == _count(fm)


def test_xml_writer_reports_what_it_skips(tmp_path) -> None:
    fm = _read_uvl(tmp_path, 'features\n    R\n        optional\n            A\n            B\n'
                             '            C\nconstraints\n    A => B | C\n')
    writer = XMLWriter(str(tmp_path / 'out.xml'), fm)
    writer.transform()
    assert len(writer.skipped_constraints) == 1
