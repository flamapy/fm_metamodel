from typing import cast

from flamapy.core.models import VariabilityModel
from flamapy.core.operations import Operation
from flamapy.core.operations.descriptor import OperationDescriptor
from flamapy.metamodels.fm_metamodel.models import FeatureModel
from flamapy.metamodels.fm_metamodel.operations.fm_feature_ancestors import get_feature_ancestors
from flamapy.metamodels.fm_metamodel.operations.fm_leaf_features import get_leaf_features


class FMMaxDepthTree(Operation):
    """This operation returns the maximum depth of the feature model tree."""

    facade = OperationDescriptor(
        doc=(
            'This operation is used to find the max depth of the tree in a model: It\n'
            'returns the max depth of the tree. If the model does not follow the UVL\n'
            'specification, an exception is raised and the operation returns False.'
        ),
        returns='Union[None, int]',
        name='max_depth', operation='FMMaxDepthTree'
    )

    def __init__(self) -> None:
        self.result = 0

    def get_result(self) -> int:
        return self.result

    def execute(self, model: VariabilityModel) -> 'FMMaxDepthTree':
        fm_model = cast(FeatureModel, model)
        self.result = max_depth_tree(fm_model)
        return self


def max_depth_tree(feature_model: FeatureModel) -> int:
    return max(len(get_feature_ancestors(f)) for f in get_leaf_features(feature_model))
