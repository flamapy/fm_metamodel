from famapy.core.operations import CoreFeatures

from famapy.metamodels.fm_metamodel.models import FeatureModel, Feature


class FMCoreFeatures(CoreFeatures):
    """Core features are features that are present in all configurations of the feature model.

    This implementation assumes that:
        (1) there are not any dead-features in the model.
        (2) there are not any false-optional features in the model.

    These assumptions imply that there are not any cross-tree constraints
    involving the core features, otherwise the model would contain:
        dead-features (e.g., for 'excludes' constraints) and
        false-optional features (e.g., for 'requires' constraints).
    """

    def __init__(self) -> None:
        self.result: list[Feature] = []

    def get_result(self) -> list[Feature]:
        return self.result

    def execute(self, model: FeatureModel) -> 'FMCoreFeatures':
        self.result = get_core_features(model)
        return self

    def get_core_features(self) -> list[Feature]:
        return self.get_result()


def get_core_features(feature_model: FeatureModel) -> list[Feature]:
    if feature_model.root is None:
        return []

    core_features = [feature_model.root]
    features = [feature_model.root]
    while features:
        feature = features.pop()
        for relation in feature.get_relations():
            if relation.is_mandatory():
                core_features.extend(relation.children)
                features.extend(relation.children)
    return core_features
