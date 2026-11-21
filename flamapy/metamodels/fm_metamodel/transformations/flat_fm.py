import copy
from typing import cast

from flamapy.core.transformations import ModelToModel
from flamapy.core.models import VariabilityModel, AST
from flamapy.core.models.ast import ASTOperation, Node

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
        imported_models = get_imported_models_by_root(new_feature_model)
        if not self._maintain_namespaces:
            for ctc in new_feature_model.ctcs:
                remove_namespace_constraint(ctc.ast, new_feature_model)
        features = [new_feature_model.root]
        while features:
            feature = features.pop()
            if feature.reference is not None:
                imported_fm = imported_models[id(feature.reference)]
                # Copy the feature's attributes and relations from the referenced feature
                feature.relations = feature.reference.relations
                for relation in feature.relations:
                    relation.parent = feature
                    for child in relation.children:
                        child.parent = feature
                feature.attributes.extend(feature.reference.attributes)
                # Process attribute constraints
                # (they are also in the imported model's ctcs, which are processed below)
                feature.constraints_attributes.extend(feature.reference.constraints_attributes)
                if not self._maintain_namespaces:
                    feature.name = feature.reference.name
                    for ctc in imported_fm.ctcs:
                        remove_namespace_constraint(ctc.ast, imported_fm)
                else:
                    namespace = '.'.join(feature.name.split('.')[:-1])
                    put_namespace_to_features(feature.reference, namespace)
                    for ctc in imported_fm.ctcs:
                        put_namespace_to_constraint(ctc.ast, namespace)
                # Add the constraints of the imported model (nested imports are reached when
                # their referencing features are visited)
                new_feature_model.ctcs.extend(imported_fm.ctcs)
                feature.reference = None  # Clear reference after copying
            features.extend(feature.get_children())
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


def get_imported_models_by_root(feature_model: FeatureModel) -> dict[int, FeatureModel]:
    """Return all the (nested) imported models of the feature model, indexed by the id of their
    root feature, which is the object referenced by the importing feature."""
    imported_models = {}
    models = list(feature_model.imports.values())
    while models:
        imported_fm = models.pop()
        imported_models[id(imported_fm.root)] = imported_fm
        models.extend(imported_fm.imports.values())
    return imported_models


def get_feature_terms(ast: AST) -> list[Node]:
    """Return the terms of the constraint that reference a feature (or a Feature.Attribute).

    Literals are excluded, and so is the attribute name of the sum/avg aggregates
    (e.g., in sum(Price, B) only B is a feature).
    """
    terms = []
    stack = [ast.root]
    while stack:
        node = stack.pop()
        if node is None:
            continue
        if node.data in (ASTOperation.SUM, ASTOperation.AVG):
            stack.append(node.right)
        elif node.is_op():
            stack.extend([node.left, node.right])
        elif (isinstance(node.data, str) and not node.is_literal()
              and not node.data.startswith("'")):
            terms.append(node)
    return terms


def put_namespace_to_constraint(ast: AST, namespace: str) -> None:
    """Put the namespace to the features referenced in the constraint."""
    for node in get_feature_terms(ast):
        node.data = f'{namespace}.{node.data}'


def remove_namespace_constraint(ast: AST, feature_model: FeatureModel) -> None:
    """Remove the import aliases of the features referenced in a constraint of the model."""
    for node in get_feature_terms(ast):
        node.data = remove_namespace(node.data, feature_model)


def remove_namespace(reference: str, feature_model: FeatureModel) -> str:
    """Remove the import alias prefixing the reference, following nested imports
    (e.g., 's.l.Y' -> 'Y', and 's.B.Price' -> 'B.Price')."""
    for alias, namespace in feature_model.alias_namespace.items():
        if reference.startswith(f'{alias}.') and namespace in feature_model.imports:
            return remove_namespace(reference[len(alias) + 1:], feature_model.imports[namespace])
    return reference
