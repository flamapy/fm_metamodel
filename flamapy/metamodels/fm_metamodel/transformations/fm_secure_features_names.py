import re
import copy
from typing import cast

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

    The class exposes a mapping between the original feature names and the new secure names.
    """

    SECURE_CHARS = r'A-Za-z0-9_'
    REPLACEMENT_CHAR = '_'

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
        self.mapping_names: dict[str, str] = {}

    def transform(self) -> FeatureModel:
        self.mapping_names = {}
        new_feature_model = copy.deepcopy(self.feature_model)
        for feature in new_feature_model.get_features():
            new_feature_name = secure_name(name=feature.name,
                                           secure_chars=self.secure_chars,
                                           replacement_char=self.replacement_char,
                                           allow_starting_digit=self.allow_starting_digit,
                                           existing_names=set(self.mapping_names.keys()))
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
