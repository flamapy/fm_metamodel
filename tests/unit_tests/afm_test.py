"""AFM writer and reader: names rewritten to AFM words, every group shape, attributes, the
logical constraints (fully parenthesised, XOR expanded) and clear errors."""
import os
import tempfile

import pytest

from flamapy.core.exceptions import FlamaException
from flamapy.metamodels.fm_metamodel.transformations import AFMReader, AFMWriter, UVLReader
from flamapy.metamodels.fm_metamodel.transformations.afm_writer import afm_word, value_text

MODEL = '''features
    "my root 1"
        mandatory
            AND
            Screen
                alternative
                    Analog
                    "High-Res"
                    EInk {cost 12, weight 1.5, flag true, label 'x y'}
        optional
            Connector
                or
                    USB
                    Wireless
            "2fast"
            camera
            camera_2
            Camera2
            Fancy
                optional
                    Child
                alternative
                    G1
                    G2
                [1..1]
                    H1
                    H2
            Mutex
                [0..1]
                    M1
                    M2
constraints
    camera => "High-Res"
    !Wireless | USB
    USB <=> Wireless
    Child => !(G1 & H1)
    Analog | EInk
    !Fancy
    !(!Child | !Mutex) => M1
'''

TYPED = '''features
    Root
        mandatory
            Integer Size
'''


def _read_uvl(text):
    handle, path = tempfile.mkstemp(suffix='.uvl')
    try:
        with os.fdopen(handle, 'w') as file:
            file.write(text)
        return UVLReader(path).transform()
    finally:
        os.remove(path)


def _read_afm(text):
    handle, path = tempfile.mkstemp(suffix='.afm')
    try:
        with os.fdopen(handle, 'w') as file:
            file.write(text)
        return AFMReader(path).transform()
    finally:
        os.remove(path)


def _round_trip(fm):
    writer = AFMWriter(None, fm)
    text = writer.transform()
    return writer, text, _read_afm(text)


def _relations(fm, mapping):
    return {(mapping[r.parent.name], frozenset(mapping[c.name] for c in r.children),
             r.card_min, r.card_max)
            for f in fm.get_features() for r in f.get_relations()}


def test_names_become_afm_words():
    writer, text, back = _round_trip(_read_uvl(MODEL))
    mapping = writer.mapping_names
    assert mapping['my root 1'] == 'Myroot1'      # letters and digits, capital first
    assert mapping['2fast'] == 'F2fast'           # a leading digit gets a prefix
    assert mapping['AND'] == 'AND1'               # keywords get a suffix
    assert mapping['camera'] == 'Camera'
    assert {mapping['camera_2'], mapping['Camera2']} == {'Camera2', 'Camera21'}  # unique
    assert sorted(mapping.values()) == sorted(f.name for f in back.get_features())
    assert text.startswith('%Relationships\nMyroot1 : AND1 Screen [Connector]')


def test_relations_and_groups_round_trip():
    fm = _read_uvl(MODEL)
    writer, _, back = _round_trip(fm)
    identity = {f.name: f.name for f in back.get_features()}
    assert _relations(back, identity) == _relations(fm, writer.mapping_names)
    assert 'Fancy : [Child] [1,1]{G1 G2} [1,1]{H1 H2};' in writer.transform()
    assert 'Mutex : [0,1]{M1 M2};' in writer.transform()


def test_attributes_round_trip():
    _, text, back = _round_trip(_read_uvl(MODEL))
    assert 'EInk.cost: [12],12,12;' in text
    assert 'EInk.label: ["x y"],"x y","x y";' in text
    attributes = {a.name: a.default_value for f in back.get_features() for a in f.get_attributes()}
    assert attributes == {'cost': '12', 'weight': '1.5', 'flag': 'true', 'label': '"x y"'}


def test_constraints_are_equivalent():
    pytest.importorskip('flamapy.metamodels.pysat_metamodel')
    from pysat.solvers import Solver
    from flamapy.metamodels.pysat_metamodel.transformations import FmToPysat

    def clauses(fm, mapping):
        sat = FmToPysat(fm).transform()
        return [[(mapping.get(sat.features[abs(lit)], sat.features[abs(lit)]), lit > 0)
                 for lit in clause] for clause in sat.get_all_clauses()]

    def entails(premise, conclusion):
        ids = {}

        def lit(name, positive):
            return ids.setdefault(name, len(ids) + 1) * (1 if positive else -1)

        with Solver(name='glucose3') as solver:
            for clause in premise:
                solver.add_clause([lit(n, p) for n, p in clause])
            return all(not solver.solve(assumptions=[-lit(n, p) for n, p in clause])
                       for clause in conclusion)

    fm = _read_uvl(MODEL)
    writer, text, back = _round_trip(fm)
    assert len(back.get_constraints()) == len(fm.get_constraints())
    assert '(Child IMPLIES (NOT (G1 AND H1)));' in text
    original, read_back = clauses(fm, writer.mapping_names), clauses(back, {})
    assert entails(original, read_back) and entails(read_back, original)


def test_xor_is_expanded_and_read_back():
    from flamapy.core.models.ast import AST, ASTOperation, Node
    from flamapy.metamodels.fm_metamodel.models import Constraint

    fm = _read_uvl('features\n    Root\n        optional\n            A\n            B\n')
    fm.ctcs.append(Constraint('xor', AST(Node(ASTOperation.XOR, Node('A'), Node('B')))))
    _, text, back = _round_trip(fm)
    assert '((A OR B) AND (NOT (A AND B)));' in text
    expanded = _read_uvl('features\n    Root\n        optional\n            A\n            B\n'
                         'constraints\n    (A | B) & !(A & B)\n')
    assert str(back.get_constraints()[0].ast) == str(expanded.get_constraints()[0].ast)


def test_reader_supports_implies_and_reports_syntax_errors():
    back = _read_afm('%Relationships\nR : [A] [B];\n%Constraints\nA IMPLIES B;\n')
    assert str(back.get_constraints()[0].ast) == 'IMPLIES[A][B]'
    with pytest.raises(FlamaException, match='syntax error'):
        _read_afm('%Relationships\nR : [a] [B];\n')     # a lowercase word is not a feature
    with pytest.raises(FlamaException, match='syntax error'):
        _read_afm('%Relationships\nR : [A] [B\n')


def test_typed_features_are_rejected():
    with pytest.raises(FlamaException, match='cannot represent'):
        AFMWriter(None, _read_uvl(TYPED)).transform()


def test_words_and_values():
    assert afm_word('my root 1', capital=True) == 'Myroot1'
    assert afm_word('cost_total', capital=False) == 'costtotal'
    assert afm_word('allergenFish', capital=False) == 'allergenfish'   # LOWERCASE token
    assert afm_word('2fast', capital=True) == 'F2fast'
    assert afm_word('', capital=True) == 'F'
    assert value_text(True) == 'true' and value_text(3) == '3' and value_text(1.5) == '1.5'
    assert value_text(0.75) == '"0.75"' and value_text(-3) == '"-3"'   # not lexed as numbers
    assert value_text('word') == 'word' and value_text('two words') == '"two words"'
    assert value_text('Integer') == '"Integer"'    # a keyword is quoted
