import re
from typing import Any, Optional

from flamapy.core.exceptions import FlamaException
from flamapy.core.models.ast import ASTOperation, Node
from flamapy.core.transformations import ModelToText
from flamapy.metamodels.fm_metamodel.transformations.uvl_writer import xor_as_negated_equivalence
from flamapy.metamodels.fm_metamodel.models import (
    Attribute,
    Feature,
    FeatureModel,
    FeatureType,
    Relation,
)

# Words the AFM lexer reads as keywords rather than names.
AFM_KEYWORDS = frozenset({'Integer', 'AND', 'OR', 'NOT', 'IFF', 'IMPLIES', 'REQUIRES', 'EXCLUDES',
                          'to', 'abs', 'max', 'min', 'cos', 'sin', 'sum'})
BINARY_OPERATORS = {
    ASTOperation.AND: 'AND',
    ASTOperation.OR: 'OR',
    ASTOperation.IMPLIES: 'IMPLIES',
    ASTOperation.REQUIRES: 'REQUIRES',
    ASTOperation.EXCLUDES: 'EXCLUDES',
    ASTOperation.EQUIVALENCE: 'IFF',
}
LETTER_PREFIX = 'F'
MAX_SHOWN = 5  # features listed in the error message


class AFMWriter(ModelToText):
    """Write a feature model in the AFM text format of FaMa (attributed feature models).

    AFM feature names are a letter followed by letters and digits, starting with a capital;
    attribute names are lowercase letters and digits. Names are rewritten to that shape (every
    other character dropped, a prefix or suffix added when needed, keywords avoided) and the
    mapping from original to written names is in `mapping_names` after `transform()`, with
    the attribute names in `mapping_attribute_names`. The renaming does not go through
    `FMSecureFeaturesNames` because AFM needs a capital first letter and cannot lex ``_``,
    which that helper cannot express.

    The relationships are the tree with mandatory (``A``), optional (``[A]``) and group
    (``[min,max]{A B}``) children; a feature can have several. Abstract features are written
    as plain features. Typed features and feature cardinalities cannot be written: the writer
    rejects them. Attributes are written with an Integer range domain, a discrete domain or,
    without a domain, the single value they hold; attributes with other values (lists,
    nested attributes) are omitted. Only the logical constraints are written, as in the other
    text writers; AFM has no XOR, so ``A XOR B`` is written as ``NOT (A IFF B)`` (the rewrite
    the UVL and FeatureIDE writers share).
    """

    @staticmethod
    def get_destination_extension() -> str:
        return 'afm'

    def __init__(self, path: Optional[str], source_model: FeatureModel):
        self.path = path
        self.model = source_model
        self.mapping_names: dict[str, str] = {}
        # (feature name, attribute name) -> the attribute name written.
        self.mapping_attribute_names: dict[tuple[str, str], str] = {}

    def transform(self) -> str:
        self._check_representable()
        self.mapping_names = feature_names(self.model)
        serialized_model = (self.serialize_relationships() + self.serialize_attributes()
                            + self.serialize_constraints())
        if self.path is not None:
            with open(self.path, 'w', encoding='utf8') as file:
                file.write(serialized_model)
        return serialized_model

    def _check_representable(self) -> None:
        problems = []
        for feature in self.model.get_features():
            if feature.feature_type != FeatureType.BOOLEAN:
                problems.append(f'{feature.name} (typed feature: {feature.feature_type.value})')
            cardinality = feature.feature_cardinality
            if (cardinality.min, cardinality.max) != (1, 1):
                problems.append(f'{feature.name} (feature cardinality '
                                f'[{cardinality.min}..{cardinality.max}])')
        if problems:
            shown = '; '.join(problems[:MAX_SHOWN]) + (f'; ... ({len(problems)} in total)'
                                                       if len(problems) > MAX_SHOWN else '')
            raise FlamaException(f'AFM cannot represent these features: {shown}.')

    def _name(self, feature: Feature) -> str:
        return self.mapping_names[feature.name]

    def serialize_relationships(self) -> str:
        root_feature = self.model.root
        return '%Relationships\n' + self.recursive_relationship_read(root_feature) + '\n'

    def recursive_relationship_read(self, feature: Feature) -> str:
        relations = feature.get_relations()
        result = f'{self._name(feature)} : ' + \
            ' '.join(self.read_relation(relation) for relation in relations) + ';\n'
        for relation in relations:
            for child in relation.children:
                if child.get_relations():
                    result += self.recursive_relationship_read(child)
        return result

    def read_relation(self, relation: Relation) -> str:
        children = relation.children
        if len(children) == 1 and (relation.card_min, relation.card_max) == (1, 1):
            return self._name(children[0])
        if len(children) == 1 and (relation.card_min, relation.card_max) == (0, 1):
            return f'[{self._name(children[0])}]'
        names = ' '.join(self._name(child) for child in children)
        return f'[{relation.card_min},{relation.card_max}]{{{names}}}'

    def serialize_attributes(self) -> str:
        result = '%Attributes\n'
        for feature in self.model.get_features():
            used: set[str] = set()
            for attribute in feature.get_attributes():
                spec = self.read_attribute(attribute)
                if spec is None:
                    continue
                name = attribute_name(attribute.name, used)
                used.add(name)
                self.mapping_attribute_names[(feature.name, attribute.name)] = name
                result += f'{self._name(feature)}.{name}: {spec};\n'
        return result + '\n'

    @classmethod
    def read_attribute(cls, attribute: Attribute) -> Optional[str]:
        """The domain, default value and null value of the attribute, or None when AFM cannot
        represent the attribute (no domain and a value that is not a number or a string)."""
        domain = attribute.domain
        default = attribute.default_value
        null = attribute.null_value
        if domain is not None and domain.get_range_list():
            spec = 'Integer ' + ''.join(f'[{r.min_value} to {r.max_value}]'
                                        for r in domain.get_range_list())
            fallback: Any = domain.get_range_list()[0].min_value
        elif domain is not None and domain.get_element_list():
            spec = '[' + ','.join(value_text(v) for v in domain.get_element_list()) + ']'
            fallback = domain.get_element_list()[0]
        elif is_scalar(default):
            spec = f'[{value_text(default)}]'
            fallback = default
        else:
            return None
        default = default if is_scalar(default) else fallback
        null = null if is_scalar(null) else default
        return f'{spec},{value_text(default)},{value_text(null)}'

    def serialize_constraints(self) -> str:
        result = '%Constraints\n'
        for constraint in self.model.get_logical_constraints():
            ast = xor_as_negated_equivalence(constraint.ast)
            result += self.recursive_constraint_read(ast.root) + ';\n'
        return result

    def recursive_constraint_read(self, node: Node) -> str:
        """The expression of the AST node, fully parenthesised (the grammar does not take a
        bare NOT as an operand). XOR must have been rewritten before (see `transform`)."""
        if not node.is_op():
            return self.mapping_names.get(node.data, str(node.data))
        if node.data == ASTOperation.NOT:
            operand = node.left if node.left is not None else node.right
            return f'(NOT {self.recursive_constraint_read(operand)})'
        left = self.recursive_constraint_read(node.left)
        right = self.recursive_constraint_read(node.right)
        operator = BINARY_OPERATORS.get(node.data)
        if operator is None:
            raise FlamaException(f'AFM cannot represent the operator {node.data} '
                                 f'of a logical constraint.')
        return f'({left} {operator} {right})'


def feature_names(feature_model: FeatureModel) -> dict[str, str]:
    """Original feature name -> AFM name: letters and digits only, a capital first letter,
    never a keyword, unique (a number is appended to repeated ones)."""
    mapping: dict[str, str] = {}
    used: set[str] = set()
    for feature in feature_model.get_features():
        if feature.name in mapping:
            raise FlamaException(f'The feature model has repeated feature names ({feature.name}), '
                                 f'which cannot be mapped unambiguously.')
        name = unique_name(afm_word(feature.name, capital=True), used)
        used.add(name)
        mapping[feature.name] = name
    return mapping


def attribute_name(name: str, used: set[str]) -> str:
    """The AFM name of an attribute: lowercase letters and digits."""
    return unique_name(afm_word(name, capital=False), used)


def afm_word(name: str, capital: bool) -> str:
    """Letters and digits only; a feature name starts with a capital, an attribute name is
    all lowercase (the lexer's WORD and LOWERCASE tokens)."""
    word = re.sub(r'[^A-Za-z0-9]', '', name)
    if not word or not word[0].isalpha():
        word = LETTER_PREFIX + word
    return word[0].upper() + word[1:] if capital else word.lower()


def unique_name(word: str, used: set[str]) -> str:
    candidate, suffix = word, 1
    while candidate in used or candidate in AFM_KEYWORDS:
        candidate = f'{word}{suffix}'
        suffix += 1
    return candidate


def is_scalar(value: Any) -> bool:
    return isinstance(value, (bool, int, float, str))


def value_text(value: Any) -> str:
    """A value as AFM writes it: booleans as words, the numbers the lexer reads (non-negative
    integers and decimals with a non-zero integer part) as they are, everything else quoted
    (a plain word is left bare)."""
    if isinstance(value, bool):
        return str(value).lower()
    text = repr(value) if isinstance(value, float) else str(value)
    if isinstance(value, (int, float)) and re.fullmatch(r'0|[1-9][0-9]*(\.[0-9]+)?', text):
        return text
    if re.fullmatch(r'[A-Za-z][A-Za-z0-9]*', text) and text not in AFM_KEYWORDS:
        return text
    return '"' + text.replace('"', '') + '"'
