from flamapy.core.operations import Operation

from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature


class FMFeatureAncestors(Operation):
    """
    This operation returns the list of ancestors of a given feature
    (i.e., all parents recursively up to the root feature).
    """

    def __init__(self, feature: Feature):
        self.result = []  # type: list[Feature]
        self.feature = feature

    def get_result(self) -> list[Feature]:
        return self.result

    def execute(self, model: FeatureModel) -> 'FMFeatureAncestors':
        self.result = get_feature_ancestors(self.feature)
        return self


def get_feature_ancestors(feature: Feature) -> list[Feature]:
    features = []
    parent = feature.get_parent()
    while parent is not None:
        features.append(parent)
        parent = parent.get_parent()
    return features
