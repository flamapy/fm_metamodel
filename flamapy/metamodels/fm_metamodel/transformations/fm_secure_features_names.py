import re
import copy
from collections import Counter
from typing import cast

from flamapy.core.exceptions import FlamaException
from flamapy.core.models import VariabilityModel, AST
from flamapy.core.transformations import ModelToModel

from flamapy.metamodels.fm_metamodel.models import FeatureModel


class FMSecureFeaturesNames(ModelToModel):
    """Given a feature model, it returns a new feature model with secure feature names.

    That is, it replaces the feature names with a secure version of the name.
    It replaces the feature names in the feature tree and in the constraints.

    By default, it uses only alphanumeric characters and underscores
    (no spaces, no special characters).
    It replaces each character that is not alphanumeric or underscore with an underscore.
    If the name starts with a number, it adds an underscore at the beginning.
    It ensures that the new names are unique.
    Names that solver front-ends read as constants or keywords (RESERVED_NAMES, e.g. a feature
    called `True`, which the BDD expression parser takes for the constant) get a suffix too;
    writers add the keywords of their own format to `reserved_names`.
    With `letter_prefix` set, a name that does not start with a letter is prefixed with it
    (for formats whose identifiers must start with a letter, such as Clafer).

    The class exposes a mapping between the original feature names and the new secure names.
    Feature names must be unique (as UVL requires): a model with repeated names cannot be
    mapped unambiguously, so it is rejected with a FlamaException.
    """

    SECURE_CHARS = r'A-Za-z0-9_'
    REPLACEMENT_CHAR = '_'
    RESERVED_NAMES = frozenset({'True', 'False', 'TRUE', 'FALSE', 'true', 'false', 'ite'})
    MAX_NAMES_IN_ERROR = 10

    @staticmethod
    def get_source_extension() -> str:
        return 'fm'

    @staticmethod
    def get_destination_extension() -> str:
        return 'fm'

    def __init__(self, source_model: VariabilityModel) -> None:
        self.feature_model = cast(FeatureModel, source_model)
        self.secure_chars: str = FMSecureFeaturesNames.SECURE_CHARS
        self.replacement_char: str = FMSecureFeaturesNames.REPLACEMENT_CHAR
        self.allow_starting_digit: bool = False
        self.reserved_names: set[str] = set(FMSecureFeaturesNames.RESERVED_NAMES)
        self.letter_prefix: str = ''
        self.mapping_names: dict[str, str] = {}

    def transform(self) -> FeatureModel:
        self.mapping_names = {}
        repeated = sorted(name for name, count in
                          Counter(f.name for f in self.feature_model.get_features()).items()
                          if count > 1)
        if repeated:
            raise FlamaException(
                f'The feature model has {len(repeated)} repeated feature name(s), '
                f'which cannot be mapped unambiguously: '
                f'{", ".join(repeated[:self.MAX_NAMES_IN_ERROR])}'
                f'{", ..." if len(repeated) > self.MAX_NAMES_IN_ERROR else ""}')
        new_feature_model = copy.deepcopy(self.feature_model)
        # Uniqueness is checked against the secure names already assigned, so two names that
        # sanitize to the same string (e.g. 'A B' and 'A_B') get different secure names.
        # Reserved names start as used, so a feature can never be given one of them.
        used_names: set[str] = set(self.reserved_names)
        for feature in new_feature_model.get_features():
            new_feature_name = secure_name(name=prefix_if_not_letter(feature.name,
                                                                     self.letter_prefix),
                                           secure_chars=self.secure_chars,
                                           replacement_char=self.replacement_char,
                                           allow_starting_digit=self.allow_starting_digit,
                                           existing_names=used_names)
            used_names.add(new_feature_name)
            self.mapping_names[feature.name] = new_feature_name
            feature.name = new_feature_name
        if set(self.mapping_names.keys()) != set(self.mapping_names.values()):
            for constraint in new_feature_model.get_constraints():
                constraint.ast = secure_ast(constraint.ast, self.mapping_names)
        return new_feature_model


def secure_name(name: str,
                secure_chars: str,
                replacement_char: str,
                allow_starting_digit: bool,
                existing_names: set[str]) -> str:
    """Return a secure version of the name, using only the secure characters."""
    # Replace all characters that are not alphanumeric or underscore with an underscore
    regex = f'[^{secure_chars}]'
    secure = re.sub(regex, replacement_char, name)

    # If the name starts with a digit, add an underscore at the beginning
    if not allow_starting_digit:
        if re.match(r'^\d', secure):
            secure = replacement_char + secure

    # Check if the name is already in the set of existing names
    original_secure = secure
    suffix = 1
    while secure in existing_names:
        secure = f"{original_secure}_{suffix}"
        suffix += 1
    return secure


def prefix_if_not_letter(name: str, prefix: str) -> str:
    """Prefix the name when it does not start with an ASCII letter (no-op if prefix is empty)."""
    if prefix and not re.match(r'^[A-Za-z]', name):
        return prefix + name
    return name


def secure_ast(ast: AST,
               mapping_names: dict[str, str]) -> AST:
    stack = [ast.root]
    while stack:
        node = stack.pop()
        if node.is_unique_term():
            node.data = mapping_names.get(node.data, node.data)
        elif node.is_unary_op():
            stack.append(node.left)
        elif node.is_binary_op():
            stack.append(node.right)
            stack.append(node.left)
    return ast
