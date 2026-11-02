"""ClaferWriter must follow the Clafer grammar (gsdlab/clafer, src/clafer.cf)."""
import pytest

from flamapy.core.exceptions import FlamaException
from flamapy.metamodels.fm_metamodel.transformations import ClaferWriter, UVLReader
from flamapy.metamodels.fm_metamodel.transformations.clafer_writer import CLAFER_KEYWORDS

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


# ---------------------------------------------------------------- ClaferWriter

def _clafer(tmp_path, uvl: str, apply_refactorings: bool = False) -> tuple[str, dict[str, str]]:
    writer = ClaferWriter(None, _read(tmp_path, uvl), apply_refactorings=apply_refactorings)
    return writer.transform(), writer.mapping_names


def test_clafer_names_are_identifiers_not_strings(tmp_path) -> None:
    text, names = _clafer(tmp_path, MODEL, apply_refactorings=True)
    assert '"' not in text
    assert names['A x'] == 'A_x'
    assert names['OR'] == 'OR'                   # Clafer keywords are lower case
    assert '[A_x => ! C]' in text                # ! is negation; `not` is a quantifier in Clafer


def test_clafer_keywords_and_leading_digits_are_renamed(tmp_path) -> None:
    text, names = _clafer(tmp_path, 'features\n    R\n        optional\n            xor\n'
                                    '            "5G"\n            X\n')
    assert names['xor'] not in CLAFER_KEYWORDS
    assert names['X'] not in CLAFER_KEYWORDS
    assert names['5G'][0].isalpha()


def test_clafer_single_group_keeps_the_keyword_on_the_feature(tmp_path) -> None:
    text, _ = _clafer(tmp_path, 'features\n    R\n        alternative\n            C\n            D\n')
    assert 'abstract xor R\n\tC ?\n\tD ?\n' in text


def test_clafer_mixed_decomposition_is_rejected_by_default(tmp_path) -> None:
    with pytest.raises(FlamaException, match='apply_refactorings'):
        _clafer(tmp_path, MODEL)


def test_clafer_two_groups_with_refactoring(tmp_path) -> None:
    text, _ = _clafer(tmp_path, MODEL, apply_refactorings=True)
    assert 'abstract R\n' in text
    assert '\tor R_1\n\t\tA_x ?\n\t\tOR ?\n' in text
    assert '\txor R_2\n\t\tC ?\n\t\tD ?\n' in text


def test_clafer_mandatory_child_is_not_put_in_the_group(tmp_path) -> None:
    text, _ = _clafer(tmp_path, 'features\n    R\n        mandatory\n            B\n'
                                '        optional\n            E\n'
                                '        alternative\n            C\n            D\n',
                      apply_refactorings=True)
    assert 'abstract R\n\tB\n\tE ?\n\txor R_1\n\t\tC ?\n\t\tD ?\n' in text


def test_clafer_arithmetic_constraints_are_omitted(tmp_path) -> None:
    text, _ = _clafer(tmp_path, 'features\n    R\n        optional\n            A {Price 3}\n'
                                '            B\nconstraints\n    A => B\n    A.Price > 2\n')
    assert '[A => B]' in text
    assert 'GREATER' not in text and '> 2' not in text
