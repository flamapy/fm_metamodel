from enum import Enum
from typing import Any

from flamapy.core.transformations import ModelToText
from flamapy.core.models.ast import ASTOperation
from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature, Constraint, Relation
from flamapy.metamodels.fm_metamodel.transformations.group_support import (
    decompose_groups,
    mixed_decomposition,
    prepare_model,
)
from flamapy.metamodels.fm_metamodel.transformations.fm_secure_features_names import (
    FMSecureFeaturesNames,
    prefix_if_not_letter,
    secure_name,
)


ATTRIBUTED_FEATURE = 'AttributedFeature'
INSTANCE = 'CP'

# Reserved words of the Clafer grammar (gsdlab/clafer, src/clafer.cf) and identifiers the compiler
# refuses as clafer names (keywordIdents in src/Language/Clafer/Common.hs).
CLAFER_KEYWORDS = frozenset({
    'clafer', 'this', 'parent', 'dref', 'root', 'children',
    'string', 'int', 'integer', 'decimal', 'double', 'real', 'boolean',
    'abstract', 'after', 'all', 'always', 'and', 'assert', 'before', 'between', 'disj', 'else',
    'enum', 'eventually', 'F', 'final', 'finally', 'finalref', 'finaltarget', 'follow', 'G',
    'globally', 'if', 'in', 'initial', 'initially', 'let', 'lonce', 'lone', 'max', 'maximize',
    'min', 'minimize', 'must', 'mux', 'never', 'next', 'no', 'not', 'one', 'opt', 'or', 'precede',
    'product', 'some', 'sometime', 'sum', 'then', 'U', 'until', 'W', 'weakuntil', 'X', 'xor',
})
# A Clafer identifier must start with a letter.
LETTER_PREFIX = 'f'


class ClaferAttributeType(Enum):
    BOOL = 'boolean'  # written as an optional child clafer (a flag), not as a reference
    STRING = 'string'
    INT = 'integer'
    FLOAT = 'real'    # valid Clafer, but Clafer's Alloy and Choco backends cannot analyse reals


class ClaferWriter(ModelToText):
    """Transform a feature model to a Clafer format.

    Clafer identifiers are letters, digits, '_' and quotes, starting with a letter, and cannot be
    quoted. Feature names are therefore written through FMSecureFeaturesNames (with the Clafer
    keywords reserved and a leading letter enforced); the mapping from original to written
    names is available in `mapping_names` after `transform()`.

    A Clafer group cardinality (xor, or, mux, m..n) applies to all the children of a clafer, so
    features with several groups, or a group and other relations, are rejected; with
    `apply_refactorings=True` a copy of the model is refactored first
    (MultipleGroupDecompositionRefactoring), which adds abstract features but keeps the
    configurations of the original features.

    Only the logical constraints of the model are written; arithmetic and other non-Boolean
    constraints are omitted (as in PLWriter and FeatureIDEWriter).
    """

    @staticmethod
    def get_destination_extension() -> str:
        return 'txt'

    def __init__(self, path: str, source_model: FeatureModel,
                 apply_refactorings: bool = False) -> None:
        self.path = path
        self.source_model = source_model
        self.apply_refactorings = apply_refactorings
        self.mapping_names: dict[str, str] = {}

    def transform(self) -> str:
        model = prepare_model(self.source_model, 'Clafer', [mixed_decomposition],
                              [decompose_groups], self.apply_refactorings)
        secure_names = FMSecureFeaturesNames(model)
        secure_names.reserved_names |= CLAFER_KEYWORDS | {ATTRIBUTED_FEATURE, INSTANCE}
        secure_names.letter_prefix = LETTER_PREFIX
        feature_model = secure_names.transform()
        self.mapping_names = secure_names.mapping_names
        clafer_str = fm_to_clafer(feature_model)
        if self.path is not None:
            with open(self.path, 'w', encoding='utf8') as file:
                file.write(clafer_str)
        return clafer_str


def fm_to_clafer(feature_model: FeatureModel) -> str:
    """Write the model in Clafer. Feature names must already be Clafer identifiers
    (ClaferWriter.transform ensures it)."""
    used_names = {feature.name for feature in feature_model.get_features()}
    used_names |= CLAFER_KEYWORDS | {ATTRIBUTED_FEATURE, INSTANCE}
    attributes = clafer_attributes(feature_model, used_names)
    result = read_features(feature_model.root, 0, attributes)
    # The root feature is always abstract in Clafer:
    result = f'abstract {result}'
    # Definition of attributes at the top of the model:
    result = attributes_definition(attributes) + '\n' + result
    for ctc in feature_model.get_logical_constraints():
        result += read_constraints(ctc)
    # Create an instance
    result += f'\n\n{INSTANCE} : {feature_model.root.name}\n'
    return result


ClaferAttributes = dict[str, tuple[str, ClaferAttributeType]]


def clafer_attributes(feature_model: FeatureModel, used_names: set[str]) -> ClaferAttributes:
    """Clafer name and type of every attribute that has a value somewhere in the model.

    Names follow the same rules as feature names. An attribute whose values mix integers and
    decimals is a real; one that mixes text, Booleans and numbers is a string.
    """
    kinds: dict[str, set[ClaferAttributeType]] = {}
    for feature in feature_model.get_features():
        for attribute in feature.get_attributes():
            if attribute.get_default_value() is not None:
                kinds.setdefault(attribute.get_name(), set()).add(
                    parse_type_value(attribute.get_default_value()))
    attributes: ClaferAttributes = {}
    for name, value_kinds in kinds.items():
        if value_kinds == {ClaferAttributeType.BOOL}:
            attr_type = ClaferAttributeType.BOOL
        elif ClaferAttributeType.STRING in value_kinds or ClaferAttributeType.BOOL in value_kinds:
            attr_type = ClaferAttributeType.STRING
        elif ClaferAttributeType.FLOAT in value_kinds:
            attr_type = ClaferAttributeType.FLOAT
        else:
            attr_type = ClaferAttributeType.INT
        new_name = secure_name(prefix_if_not_letter(name, LETTER_PREFIX),
                               FMSecureFeaturesNames.SECURE_CHARS,
                               FMSecureFeaturesNames.REPLACEMENT_CHAR, False, used_names)
        used_names.add(new_name)
        attributes[name] = (new_name, attr_type)
    return attributes


def read_features(feature: Feature, tab_count: int, attributes: ClaferAttributes,
                  in_group: bool = False) -> str:
    """Write a feature and its subtree. The feature has a single group (written as the group
    keyword) or only mandatory/optional children; children of a group are marked `?`."""
    tabs = '\t' * tab_count  # Indentation
    relations = feature.get_relations()
    single_group = len(relations) == 1 and relations[0].is_group()
    result = tabs

    # Group type
    if single_group:
        result += f'{group_keyword(relations[0])} '

    # Feature
    result += feature.name
    if any(attribute.get_name() in attributes for attribute in feature.get_attributes()):
        result += f' : {ATTRIBUTED_FEATURE}'
    if feature.is_optional() or in_group:
        result += ' ?'

    # Feature's attributes
    tab_count += 1
    result += read_feature_attributes(feature, tab_count, attributes)

    # Children
    result += '\n'
    for relation in relations:
        for child in relation.children:
            result += read_features(child, tab_count, attributes, in_group=single_group)
    return result


def read_feature_attributes(feature: Feature, tab_count: int,
                            attributes: ClaferAttributes) -> str:
    result = ''
    tabs = '\t' * tab_count
    for attribute in feature.get_attributes():
        value = attribute.get_default_value()
        if value is None or attribute.get_name() not in attributes:
            continue
        name, attr_type = attributes[attribute.get_name()]
        if attr_type == ClaferAttributeType.BOOL:
            constraint = name if value else f'no {name}'
        elif attr_type == ClaferAttributeType.STRING:
            text = str(value).lower() if isinstance(value, bool) else str(value)
            constraint = f'{name} = "{escape_string(text)}"'
        elif attr_type == ClaferAttributeType.FLOAT:
            constraint = f'{name} = {real_literal(value)}'
        else:
            constraint = f'{name} = {value}'
        result += f'\n{tabs}[{constraint}]'
    return result


def escape_string(text: str) -> str:
    """Clafer string literals allow the escapes \\" \\\\ \\n and \\t only."""
    text = text.replace('\\', '\\\\').replace('"', '\\"')
    text = text.replace('\r\n', '\n').replace('\r', '\n').replace('\n', '\\n').replace('\t', '\\t')
    return ''.join(char if char >= ' ' else ' ' for char in text)


def real_literal(value: Any) -> str:
    """A Clafer real literal (digits '.' digits), without exponent notation."""
    literal = f'{float(value):.15f}'.rstrip('0')
    return literal + '0' if literal.endswith('.') else literal


def group_keyword(relation: Relation) -> str:
    """Clafer group cardinality of a group relation."""
    if relation.is_alternative():
        return 'xor'
    if relation.is_or():
        return 'or'
    if relation.is_mutex():
        return 'mux'
    card_max = '*' if relation.card_max == -1 else str(relation.card_max)
    return f'{relation.card_min}..{card_max}'


def read_constraints(const: Constraint) -> str:
    result = ""
    constraint_text = serialize_constraint(const)
    result = "\n" + constraint_text
    return result


CLAFER_OPERATORS = {
    ASTOperation.NOT: '!',
    ASTOperation.AND: '&&',
    ASTOperation.OR: '||',
    ASTOperation.XOR: 'xor',
    ASTOperation.IMPLIES: '=>',
    ASTOperation.EQUIVALENCE: '<=>',
    ASTOperation.REQUIRES: '=>',
    ASTOperation.EXCLUDES: '=> !',
}


def serialize_constraint(ctc: Constraint) -> str:
    """Write a constraint in Clafer syntax (`!` is negation; `not` is a quantifier in Clafer)."""
    ctc_str = ctc.ast.pretty_str(CLAFER_OPERATORS)
    return f'[{ctc_str}]'


def attributes_definition(attributes: ClaferAttributes) -> str:
    result = ''
    if attributes:
        result = f'abstract {ATTRIBUTED_FEATURE}\n'
        for name, attr_type in attributes.values():
            if attr_type == ClaferAttributeType.BOOL:
                result += f'\t{name} ?\n'
            else:
                result += f'\t{name} -> {attr_type.value}\n'
    return result


def parse_type_value(value: Any) -> ClaferAttributeType:
    if isinstance(value, bool):
        return ClaferAttributeType.BOOL
    if isinstance(value, int):
        return ClaferAttributeType.INT
    if isinstance(value, float):
        return ClaferAttributeType.FLOAT
    return ClaferAttributeType.STRING
