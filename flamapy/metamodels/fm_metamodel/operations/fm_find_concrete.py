from typing import Any
from flamapy.core.operations import FindConcreteFeatures

from flamapy.metamodels.fm_metamodel.models import FeatureModel


class FMFindConcreteFeatures(FindConcreteFeatures):

    def __init__(self) -> None:
        self.result = 0

    def get_result(self) -> list[Any]:
        return self.result

    def execute(self, model: FeatureModel) -> 'FMFindConcreteFeatures':
        self.result = find_concrete_features(model)
        return self

    def get_concrete_features(self) -> list[Any]:
        return self.get_result()


def find_concrete_features(feature_model: FeatureModel) -> list[Any]:
    concrete = []
    for feature in feature_model.get_features():
        if not feature.is_abstract:
            concrete.append(feature.name)
    return concrete