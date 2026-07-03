from typing import Any, Optional

from flamapy.core.models import VariabilityModel
from flamapy.core.exceptions import FlamaException
from flamapy.core.operations import Operation
from flamapy.core.operations.descriptor import OperationDescriptor, Input
from flamapy.metamodels.fm_metamodel.models import Feature


def _feature_ancestors_inputs(operation: Any, facade: Any, kwargs: dict[str, Any]) -> None:
    operation.set_feature(facade.fm_model.get_feature_by_name(kwargs['feature_name']))


def _feature_ancestors_result(result: Any) -> Any:
    return [feature.name for feature in result]


class FMFeatureAncestors(Operation):
    """
    This operation returns the list of ancestors of a given feature
    (i.e., all parents recursively up to the root feature).
    """

    facade = OperationDescriptor(
        name='feature_ancestors', operation='FMFeatureAncestors',
        inputs=(Input('feature_name', str, required=True),),
        input_adapter=_feature_ancestors_inputs,
        result_adapter=_feature_ancestors_result,
    )

    def __init__(self) -> None:
        self.result: list[Feature] = []
        self.feature: Optional[Feature] = None

    def set_feature(self, feature: Feature) -> None:
        self.feature = feature

    def get_result(self) -> list[Feature]:
        return self.result

    def execute(self, model: VariabilityModel) -> 'FMFeatureAncestors':
        if self.feature is None:
            raise FlamaException("The feature is not defined")
        self.result = get_feature_ancestors(self.feature)
        return self


def get_feature_ancestors(feature: Feature) -> list[Feature]:
    features = []
    parent = feature.get_parent()
    while parent is not None:
        features.append(parent)
        parent = parent.get_parent()
    return features
