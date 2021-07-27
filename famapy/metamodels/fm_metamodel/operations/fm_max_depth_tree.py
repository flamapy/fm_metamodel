from famapy.core.operations import Operation

from famapy.metamodels.fm_metamodel.models import FeatureModel
from famapy.metamodels.fm_metamodel.operations import get_feature_ancestors, get_leaf_features


class FMMaxDepthTree(Operation):
    """This operation returns the maximum depth of the feature model tree."""

    def __init__(self) -> None:
        self.result = 0

    def get_result(self) -> int:
        return self.result

    def execute(self, model: FeatureModel) -> 'FMMaxDepthTree':
        self.result = max_depth_tree(model)
        return self


def max_depth_tree(feature_model: FeatureModel) -> int:
    return max(len(get_feature_ancestors(f)) for f in get_leaf_features(feature_model))
