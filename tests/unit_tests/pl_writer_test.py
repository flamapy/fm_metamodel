"""PLWriter must follow the Logic2BDD expression grammar (synExpScanner.lpp / synExpParser.ypp)."""
import pytest

from flamapy.metamodels.fm_metamodel.transformations import PLWriter, UVLReader
from flamapy.metamodels.fm_metamodel.transformations.pl_writer import PL_KEYWORDS

MODEL = """features
    R
        or
            "A x"
            OR
        alternative
            C
            D
constraints
    "A x" => !C
    OR => C
"""


def _read(tmp_path, uvl: str = MODEL):
    path = tmp_path / 'model.uvl'
    path.write_text(uvl)
    return UVLReader(str(path)).transform()


# ---------------------------------------------------------------- PLWriter

def test_pl_writer_writes_connective_symbols(tmp_path) -> None:
    writer = PLWriter(None, _read(tmp_path))
    text = writer.transform()
    or_name = writer.mapping_names['OR']
    assert 'LogicConnective' not in text
    assert f'R <-> (A_x or {or_name})' in text
    assert '(C <-> (not D and R))' in text      # a space after not
    assert 'A_x -> not C' in text
    assert text.endswith('\n')                  # every expression ends with a newline


def test_pl_writer_renames_scanner_keywords(tmp_path) -> None:
    writer = PLWriter(None, _read(tmp_path))
    writer.transform()
    assert writer.mapping_names['OR'] not in PL_KEYWORDS


def test_pl_writer_xor_is_negated_equivalence(tmp_path) -> None:
    text = PLWriter(None, _read(tmp_path, 'features\n    R\n        optional\n            A\n            B\n'
                                          'constraints\n    !(A <=> B)\n')).transform()
    assert 'not (A <-> B)' in text


def test_pl_writer_cardinality_as_clauses(tmp_path) -> None:
    text = PLWriter(None, _read(tmp_path, 'features\n    R\n        [2..*]\n            A\n'
                                          '            B\n            C\n')).transform()
    line = next(l for l in text.splitlines() if l.startswith('(A -> R)'))
    # every child implies the parent; at least 2 of 3: R implies one of every pair
    assert '(B -> R)' in line and '(C -> R)' in line
    assert '(R -> (A or B))' in line and '(R -> (A or C))' in line and '(R -> (B or C))' in line
    assert 'not A or not B' not in line          # no upper bound


def test_pl_writer_cardinality_upper_bound(tmp_path) -> None:
    text = PLWriter(None, _read(tmp_path, 'features\n    R\n        [0..2]\n            A\n'
                                          '            B\n            C\n')).transform()
    assert '(not A or not B or not C)' in text   # not all three
    assert 'R -> (' not in text                   # no lower bound


def test_pl_writer_rejects_huge_cardinality_groups(tmp_path) -> None:
    children = ''.join(f'            F{i}\n' for i in range(30))
    with pytest.raises(ValueError, match='clauses'):
        PLWriter(None, _read(tmp_path, f'features\n    R\n        [15..*]\n{children}')).transform()


def test_pl_writer_mutex_allows_no_child_without_parent(tmp_path) -> None:
    text = PLWriter(None, _read(tmp_path, 'features\n    R\n        optional\n            P\n'
                                          '                [0..1]\n                    A\n'
                                          '                    B\n')).transform()
    assert '(P and not (A or B)) or (' in text
