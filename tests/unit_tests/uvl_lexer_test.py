import pytest
from antlr4 import CommonTokenStream, InputStream
from uvl.UVLCustomLexer import UVLCustomLexer
from uvl.UVLPythonLexer import UVLPythonLexer

from flamapy.core.exceptions import FlamaException
from flamapy.metamodels.fm_metamodel.transformations import UVLReader
from flamapy.metamodels.fm_metamodel.transformations._uvl_lexer import (
    NEEDS_WORKAROUND,
    _DECISIONS_TO_DFA,
    uvl_lexer,
)

MODELS = {
    "groups_and_constraints": """namespace Shop

features
    Shop {abstract}
        mandatory
            Catalog
            Payment
                or
                    PayPal
                    "Credit Card"
        optional
            Search
                alternative
                    Basic
                    Advanced
constraints
    "Credit Card" => Advanced
    !(Basic & PayPal) | Search
""",
    "attributes_and_types": """include
    Arithmetic.*
    Type.*
features
    Root {Price 1.5, Name 'root', Vegan, Tags [1, 2, 3], Nested {A 1}}
        optional
            Integer Scoops {Cost 0.10}
            String Label
            Leaf cardinality [1..3]
constraints
    Scoops > 1 & Scoops < 5
    sum(Cost) < 10
    len(Label) > 2
""",
    "comments_and_blank_lines": """// leading comment

features
    /* block
       comment */
    A
        optional   // trailing comment
            B

            C
""",
    "crlf": "features\r\n    A\r\n        optional\r\n            B\r\n",
    "tabs": "features\n\tA\n\t\toptional\n\t\t\tB\n",
    "indented_first_line": "  features\n      A\n",
    "indented_first_line_tab": "\tfeatures\n\tA\n",
    "leading_spaces_then_comment": "  // c\nfeatures\n    A\n",
    "leading_blank_line_with_spaces": "   \nfeatures\n    A\n",
    "empty": "",
    "only_spaces": "    ",
}


def _tokens(lexer: UVLCustomLexer) -> list[tuple]:
    stream = CommonTokenStream(lexer)
    stream.fill()
    return [(t.type, t.text, t.line, t.column, t.start, t.stop) for t in stream.tokens]


@pytest.mark.parametrize("name", sorted(MODELS))
def test_patched_lexer_produces_the_same_tokens(name):
    text = MODELS[name]
    # Twice: on the second pass the patched lexer's cached start state is already filled
    # when the model's first token is lexed.
    for _ in range(2):
        assert _tokens(uvl_lexer(InputStream(text))) == _tokens(UVLCustomLexer(InputStream(text)))


@pytest.mark.skipif(not NEEDS_WORKAROUND, reason="installed uvlparser has no NEWLINE predicate")
def test_patched_lexer_caches_its_start_state_privately():
    _tokens(uvl_lexer(InputStream(MODELS["groups_and_constraints"])))
    assert _DECISIONS_TO_DFA[UVLPythonLexer.DEFAULT_MODE].s0 is not None
    # The DFA cache shared by stock UVL lexers is left alone.
    assert UVLPythonLexer.decisionsToDFA[UVLPythonLexer.DEFAULT_MODE].s0 is None


def test_reader_still_rejects_an_indented_first_line(tmp_path):
    path = tmp_path / "indented.uvl"
    path.write_text(MODELS["indented_first_line"])
    with pytest.raises(FlamaException):
        UVLReader(str(path)).transform()


def test_reader_reads_models_with_the_patched_lexer(tmp_path):
    path = tmp_path / "shop.uvl"
    path.write_text(MODELS["groups_and_constraints"])
    feature_model = UVLReader(str(path)).transform()
    assert [f.name for f in feature_model.get_features()] == [
        "Shop", "Catalog", "Payment", "PayPal", "Credit Card", "Search", "Basic", "Advanced"]
    assert len(feature_model.get_constraints()) == 2
