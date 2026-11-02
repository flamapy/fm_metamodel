import string
import pathlib
from typing import Any, Union

from flamapy.core.models.ast import AST, ASTOperation, Node
from flamapy.core.transformations import ModelToText
from flamapy.metamodels.fm_metamodel.models import (
    Constraint,
    Feature,
    FeatureModel,
    Relation
)


UVL_OPERATORS: dict[ASTOperation, str] = {ASTOperation.AND: "&",
                                          ASTOperation.OR: "|",
                                          ASTOperation.NOT: "!",
                                          ASTOperation.IMPLIES: "=>",
                                          ASTOperation.EQUIVALENCE: "<=>",
                                          ASTOperation.REQUIRES: "=>",
                                          ASTOperation.EXCLUDES: "=> !",
                                          ASTOperation.EQUALS: '==',
                                          ASTOperation.LOWER: '<',
                                          ASTOperation.GREATER: '>',
                                          ASTOperation.LOWER_EQUALS: '<=',
                                          ASTOperation.GREATER_EQUALS: '>=',
                                          ASTOperation.NOT_EQUALS: '!=',
                                          ASTOperation.ADD: '+',
                                          ASTOperation.SUB: '-',
                                          ASTOperation.MUL: '*',
                                          ASTOperation.DIV: '/',
                                          ASTOperation.SUM: 'sum',
                                          ASTOperation.AVG: 'avg',
                                          ASTOperation.LEN: 'len',
                                          ASTOperation.FLOOR: 'floor',
                                          ASTOperation.CEIL: 'ceil',
                                          }

# Words the UVL lexer reserves; a feature or attribute with one of these names must be quoted.
UVL_KEYWORDS = frozenset({
    'namespace', 'imports', 'as', 'include', 'features', 'constraints', 'constraint',
    'cardinality', 'mandatory', 'optional', 'alternative', 'or',
    'Boolean', 'Integer', 'Real', 'String', 'Type', 'Arithmetic',
    'sum', 'avg', 'len', 'floor', 'ceil', 'true', 'false',
})


class UVLWriter(ModelToText):
    @staticmethod
    def get_destination_extension() -> str:
        return "uvl"

    def __init__(self, path: str, source_model: FeatureModel):
        self.path = path
        self.model = source_model
        self._compact_relations = False

    def set_compact_relations(self, compact_relations: bool) -> None:
        """Set whether to compact relations in the UVL output."""
        self._compact_relations = compact_relations

    def transform(self) -> str:
        model = self.model
        root = model.root
        self._constraints_attributes: list[Constraint] = []

        result = ''
        if self.model.imports:
            result += "imports\n"
            for alias, namespace in self.model.alias_namespace.items():
                result += f'\t{namespace}'
                if alias != namespace:
                    result += f' as {alias}'
                result += '\n'
        result += 'features'
        serialized_model = (
            self.read_features(root, result, 0) + '\n' + self.read_constraints()
        )

        if self.path is not None:
            with open(self.path, "w", encoding="utf8") as file:
                file.write(serialized_model)

        for namespace, submodel in self.model.imports.items():
            path = f'{"/".join(namespace.split("."))}.uvl'
            pathlib.Path(path).parent.mkdir(parents=True, exist_ok=True)
            submodel_str = UVLWriter(path, submodel).transform()
            serialized_model += f'\n\n{submodel_str}'
        return serialized_model

    def read_features(self, feature: Feature, result: str, tab_count: int) -> str:
        tab_count = tab_count + 1
        feature_type = f'{feature.feature_type.value} ' if not feature.is_boolean() else ''
        fmincard = feature.feature_cardinality.min
        fmaxcard: Union[int, str] = feature.feature_cardinality.max
        fmaxcard = '*' if fmaxcard == -1 else fmaxcard
        feature_cardinality = f'cardinality [{fmincard}..{fmaxcard}] '
        feature_cardinality = feature_cardinality if feature.is_multifeature() else ''
        result = (
            result
            + "\n"
            + tab_count * "\t"
            + feature_type
            + safename(feature.name)
            + " "
            + feature_cardinality
            + self.read_attributes(feature)
        )
        tab_count = tab_count + 1
        if not self._compact_relations:
            result = self.serialize_relations_with_children(feature.relations, result, tab_count)
        else:
            mandatory_relations = [rel for rel in feature.relations if rel.is_mandatory()]
            optional_relations = [rel for rel in feature.relations if rel.is_optional()]
            group_relations = [rel for rel in feature.relations if rel.is_group()]
            result = self.serialize_compact_relations(mandatory_relations, result, tab_count)
            result = self.serialize_compact_relations(optional_relations, result, tab_count)
            result = self.serialize_relations_with_children(group_relations, result, tab_count)
        return result

    def serialize_relations_with_children(self,
                                          relations: list[Relation],
                                          result: str,
                                          tab_count: int) -> str:
        for relation in relations:
            relation_name = self.serialize_relation(relation)
            result += "\n" + tab_count * "\t" + relation_name
            for feature_node in relation.children:
                result = self.read_features(feature_node, result, tab_count)
        return result

    def serialize_compact_relations(self,
                                    relations: list[Relation],
                                    result: str,
                                    tab_count: int) -> str:
        if not relations:
            return result
        relation_name = self.serialize_relation(relations[0])
        result += "\n" + tab_count * "\t" + relation_name
        children = [child for relation in relations for child in relation.children]
        for child in children:
            result = self.read_features(child, result, tab_count)
        return result

    def read_attributes(self, feature: Feature) -> str:
        attributes = []
        if feature.is_abstract:
            attributes.append("abstract")
        attributes.extend(serialize_attributes(feature))
        feature_constraints_str = self.read_feature_constraints(feature)
        if feature_constraints_str:
            attributes.append(feature_constraints_str)
        return f'{{{", ".join(attributes)}}}' if attributes else ""

    def read_feature_constraints(self, feature: Feature) -> str:
        result = ''
        feature_constraints = []
        if feature.constraints_attributes:
            for constraint in feature.constraints_attributes:
                if isinstance(constraint, Constraint):
                    feature_constraints.append(self.serialize_constraint(constraint))
                    self._constraints_attributes.append(constraint)
                else:
                    raise TypeError(f"Unsupported type for constraint: {type(constraint)}")
            if len(feature_constraints) == 1:
                result += f'constraint {feature_constraints[0]}'
            elif len(feature_constraints) > 1:
                result += f'constraints [{", ".join(feature_constraints)}]'
        return result

    @staticmethod
    def serialize_relation(rel: Relation) -> str:
        result = ""

        if rel.is_alternative():
            result = "alternative"
        elif rel.is_mandatory():
            result = "mandatory"
        elif rel.is_optional():
            result = "optional"
        elif rel.is_or():
            result = "or"
        else:
            min_value = rel.card_min
            max_value: Union[int, str] = rel.card_max

            if min_value == max_value:
                result = "[" + str(min_value) + "]"
            else:
                max_value = '*' if max_value == -1 else max_value
                result = "[" + str(min_value) + ".." + str(max_value) + "]"

        return result

    def read_constraints(self) -> str:
        result = ""
        constraints = self.model.get_constraints()
        if len(constraints) > len(self._constraints_attributes):
            result = "constraints"
            for constraint in constraints:
                if constraint in self._constraints_attributes:
                    self._constraints_attributes.remove(constraint)
                else:
                    constraint_text = self.serialize_constraint(constraint)
                    result = result + "\n\t" + constraint_text
        return result

    @staticmethod
    def serialize_constraint(ctc: Constraint) -> str:
        """Serialize a constraint in UVL syntax.

        `AST.pretty_str` writes the operators with their UVL symbols and every term through
        `serialize_term`, so names are quoted when UVL requires it and a feature called, e.g.,
        `OR` is never taken for an operator.
        """
        return xor_as_negated_equivalence(ctc.ast).pretty_str(UVL_OPERATORS, serialize_term)


def xor_as_negated_equivalence(ast: AST) -> AST:
    """UVL has no XOR operator: return an AST where every A XOR B is !(A <=> B).

    The constraint's own AST is not modified.
    """
    def rewrite(node: Node) -> Node:
        if node is None or not node.is_op():
            return node
        left = rewrite(node.left) if node.left is not None else node.left
        right = rewrite(node.right) if node.right is not None else node.right
        if node.data == ASTOperation.XOR:
            return Node(ASTOperation.NOT, Node(ASTOperation.EQUIVALENCE, left, right))
        if left is node.left and right is node.right:
            return node
        return Node(node.data, left, right, node.node_type)

    root = rewrite(ast.root)
    return ast if root is ast.root else AST(root)


def serialize_term(node: Node) -> str:
    """A constant is written as it is; a feature or attribute reference is quoted if needed."""
    term = node.data
    if isinstance(term, bool):
        return str(term).lower()
    if isinstance(term, (int, float)):
        return str(term)
    text = str(term)
    if len(text) > 1 and text.startswith("'") and text.endswith("'"):
        return text  # string constant
    return safename(text)


class _AttributeNode:
    """An attribute key with its value or its nested attributes (UVLReader flattens nested
    attributes such as `Allergen {Gluten, Egg}` into `Allergen.Gluten`, `Allergen.Egg`)."""

    def __init__(self) -> None:
        self.value: Any = None
        self.children: dict[str, '_AttributeNode'] = {}


def serialize_attributes(feature: Feature) -> list[str]:
    """The feature's attributes in UVL syntax, with dotted names written back as nested blocks."""
    root = _AttributeNode()
    for attribute in feature.get_attributes():
        node = root
        for part in attribute.name.split('.'):
            node = node.children.setdefault(part, _AttributeNode())
        node.value = attribute.default_value
    return _serialize_attribute_nodes(root)


def _serialize_attribute_nodes(node: _AttributeNode) -> list[str]:
    result = []
    for key, child in node.children.items():
        text = safe_simple_name(key)
        if child.children:
            text += f' {{{", ".join(_serialize_attribute_nodes(child))}}}'
        elif child.value is not None:
            text += f' {_attribute_value(child.value)}'
        result.append(text)
    return result


def _attribute_value(value: Any) -> str:
    if isinstance(value, str):
        return f"'{value}'"
    if isinstance(value, bool):
        return str(value).lower()
    return str(value)


def safename(name: str) -> str:
    if '.' in name:
        return '.'.join([safe_simple_name(simple_name) for simple_name in name.split('.')])
    return safe_simple_name(name)


def safe_simple_name(name: str) -> str:
    """Quote a name when it is not a valid unquoted UVL identifier.

    Unquoted identifiers may only contain letters, digits and '_', must start with a letter (the
    lexer reads a leading digit as a number and drops a leading '_') and may not be a UVL keyword.
    """
    if name.startswith("'") and name.endswith("'"):
        return name
    needs_quotes = (
        not name
        or any(char not in safecharacters() for char in name)
        or not name[0].isalpha()
        or name in UVL_KEYWORDS
    )
    return f'"{name}"' if needs_quotes else name


def safecharacters() -> str:
    return string.ascii_letters + string.digits + '_'
