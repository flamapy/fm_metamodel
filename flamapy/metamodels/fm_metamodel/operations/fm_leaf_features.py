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
        doc=(
            'This operation is used to find leaf features in a model: It returns the leaf\n'
            'features if they are found in the model. If the model does not follow the UVL\n'
            'specification, an exception is raised and the operation returns False.\n'
            '\n'
            'Traditionally you would use the flama tool by features =\n'
            "discover_metamodel.use_operation_from_file('OperationString', model) however,\n"
            'in this tool we know that this operation is from the fm metamodel, so we avoid\n'
            'to execute the transformation if possible'
        ),
        returns='Union[None, List[str]]',
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
