from famapy.core.operations import Operation

from famapy.metamodels.fm_metamodel.models import FeatureModel, Feature


class FMLeafFeatures(Operation):
    """
    This operation returns the list of features that are leaves of the feature model tree
    (i.e., they have not children).
    """

    def __init__(self) -> None:
        self.result: list[Feature] = []

    def get_result(self) -> list[Feature]:
        return self.result

    def execute(self, model: FeatureModel) -> 'FMLeafFeatures':
        self.result = get_leaf_features(model)
        return self


def get_leaf_features(feature_model: FeatureModel) -> list[Feature]:
    return [f for f in feature_model.get_features() if len(f.get_relations()) == 0]
