"""UVLWriter must quote names that are not valid unquoted UVL identifiers, in the feature tree
and in constraints, so that UVLReader can read back what UVLWriter writes."""
import pytest

from flamapy.core.models.ast import AST, ASTOperation, Node
from flamapy.metamodels.fm_metamodel.models import Constraint, Feature, FeatureModel, Relation
from flamapy.metamodels.fm_metamodel.transformations import UVLReader, UVLWriter
from flamapy.metamodels.fm_metamodel.transformations.uvl_writer import safe_simple_name


def serialize_node(node: Node) -> str:
    return UVLWriter.serialize_constraint(Constraint('c', AST(node)))


@pytest.mark.parametrize('name, expected', [
    ('Camera', 'Camera'),
    ('MP3_Recording', 'MP3_Recording'),
    ('5 MP', '"5 MP"'),
    ('2_1MP', '"2_1MP"'),              # starts with a digit: the lexer would read a number
    ("Bob's Hemp", '"Bob\'s Hemp"'),
    ('or', '"or"'),                    # UVL keywords
    ('features', '"features"'),
    ('Integer', '"Integer"'),
    ('OR', 'OR'),                      # not a keyword (case-sensitive)
])
def test_safe_simple_name(name: str, expected: str) -> None:
    assert safe_simple_name(name) == expected


def _model(child_names: list[str], ctcs: list[AST]) -> FeatureModel:
    root = Feature('Root', is_abstract=True)
    children = [Feature(n, parent=root) for n in child_names]
    root.add_relation(Relation(root, children, 0, len(children)))
    return FeatureModel(root, [Constraint(f'c{i}', ast) for i, ast in enumerate(ctcs)])


def _round_trip(fm: FeatureModel, tmp_path) -> FeatureModel:
    path = str(tmp_path / 'model.uvl')
    UVLWriter(path, fm).transform()
    return UVLReader(path).transform()


def _count(fm: FeatureModel) -> int:
    from flamapy.metamodels.pysat_metamodel.operations import PySATConfigurationsNumber
    from flamapy.metamodels.pysat_metamodel.transformations import FmToPysat
    return PySATConfigurationsNumber().execute(FmToPysat(fm).transform()).get_result()


def test_round_trip_with_names_that_need_quotes(tmp_path) -> None:
    names = ['5 MP', '2_1MP', "Bob's Hemp", 'or', 'OR', 'plain']
    ctcs = [
        AST.create_simple_binary_operation(ASTOperation.IMPLIES, '5 MP', '2_1MP'),
        AST.create_simple_binary_operation(ASTOperation.EXCLUDES, "Bob's Hemp", 'or'),
        AST.create_unary_operation(ASTOperation.NOT, Node(ASTOperation.AND, Node('OR'), Node('plain'))),
    ]
    fm = _model(names, ctcs)
    back = _round_trip(fm, tmp_path)
    assert sorted(f.name for f in back.get_features()) == sorted(f.name for f in fm.get_features())
    assert len(back.get_constraints()) == 3
    assert _count(back) == _count(fm)


def test_feature_named_like_an_operator_is_not_rewritten() -> None:
    node = Node(ASTOperation.OR, Node('OR'), Node('AND'))
    assert serialize_node(node) == 'OR | AND'


def test_xor_is_written_as_negated_equivalence(tmp_path) -> None:
    fm = _model(['A', 'B'], [AST.create_simple_binary_operation(ASTOperation.XOR, 'A', 'B')])
    assert serialize_node(fm.get_constraints()[0].ast.root) == '! (A <=> B)'
    back = _round_trip(fm, tmp_path)
    assert _count(back) == _count(fm)


def test_layout_unchanged_for_plain_constraints() -> None:
    node = Node(ASTOperation.IMPLIES, Node('A'), Node(ASTOperation.OR, Node('B'), Node(ASTOperation.NOT, Node('C'))))
    assert serialize_node(node) == 'A => (B | ! C)'
