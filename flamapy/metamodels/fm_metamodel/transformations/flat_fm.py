import copy
from typing import cast

from flamapy.core.transformations import ModelToModel
from flamapy.core.models import VariabilityModel, AST

from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature


class FlatFM(ModelToModel):
    """Given a feature model with imports information, it returns a flat feature model with
    all references resolved.

    That is, it copies all imported feature models in only one feature model ready to be analyzed.
    """

    @staticmethod
    def get_source_extension() -> str:
        return 'fm'

    @staticmethod
    def get_destination_extension() -> str:
        return 'fm'

    def __init__(self, source_model: VariabilityModel) -> None:
        self.feature_model = cast(FeatureModel, source_model)
        self._maintain_namespaces: bool = True

    def set_maintain_namespaces(self, maintain_namespaces: bool) -> None:
        """Set whether to maintain the namespaces of the imported features."""
        self._maintain_namespaces = maintain_namespaces

    def transform(self) -> FeatureModel:
        new_feature_model = copy.deepcopy(self.feature_model)
        features = [new_feature_model.root]
        while features:
            feature = features.pop()
            if feature.reference is not None:
                # Copy the feature's attributes and relations from the referenced feature
                feature.relations = feature.reference.relations
                feature.attributes.extend(feature.reference.attributes)
                if not self._maintain_namespaces:
                    feature.name = feature.reference.name
                else:
                    namespace = '.'.join(feature.name.split('.')[:-1])
                    put_namespace_to_features(feature.reference, namespace)
                feature.reference = None  # Clear reference after copying
            features.extend(feature.get_children())
        if not self._maintain_namespaces:
            for ctcs in new_feature_model.ctcs:
                process_namespace_constraint(ctcs.ast, new_feature_model.alias_namespace)
        new_feature_model.imports = {}  # Clear imports as they are not needed in the flat model
        new_feature_model.alias_namespace = {}  # Clear alias namespace as it is not needed in
        return new_feature_model


def put_namespace_to_features(root: Feature, namespace: str) -> None:
    """Put the namespace to the features in the feature model."""
    features = [root]
    while features:
        feature = features.pop()
        feature.name = f'{namespace}.{feature.name}'
        features.extend(feature.get_children())


def process_namespace_constraint(ast: AST, alias_namespace: dict[str, str]) -> None:
    """Replace the namespace of the features in the constraints."""
    stack = [ast.root]
    while stack:
        node = stack.pop()
        if node is None:
            continue
        if node.is_unique_term():
            if (isinstance(node.data, (int, float)) or node.data.startswith("'")):
                continue
            feature_reference = node.data.split('.')
            if len(feature_reference) > 1:
                namespace = feature_reference[0]
                feature_name = feature_reference[-1]
                if namespace in alias_namespace:
                    node.data = feature_name
        elif node.is_unary_op():
            stack.append(node.left)
        elif node.is_binary_op():
            stack.append(node.right)
            stack.append(node.left)
