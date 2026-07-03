from typing import Any, cast

from flamapy.core.models import VariabilityModel
from flamapy.core.operations import Operation
from flamapy.core.operations.descriptor import OperationDescriptor
from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature


def _leaf_features_result(result: Any) -> Any:
    return [feature.name for feature in result]


class FMLeafFeatures(Operation):
    """
    This operation returns the list of features that are leaves of the feature model tree
    (i.e., they have not children).
    """

    facade = OperationDescriptor(
        name='leaf_features', operation='FMLeafFeatures', result_adapter=_leaf_features_result
    )

    def __init__(self) -> None:
        self.result: list[Feature] = []

    def get_result(self) -> list[Feature]:
        return self.result

    def execute(self, model: VariabilityModel) -> 'FMLeafFeatures':
        fm_model = cast(FeatureModel, model)
        self.result = get_leaf_features(fm_model)
        return self


def get_leaf_features(feature_model: FeatureModel) -> list[Feature]:
    return [f for f in feature_model.get_features() if len(f.get_relations()) == 0]
