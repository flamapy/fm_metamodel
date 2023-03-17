from typing import Any
from flamapy.core.operations import FindAbstractFeatures

from flamapy.metamodels.fm_metamodel.models import FeatureModel


class FMFindAbstractFeatures(FindAbstractFeatures):

    def __init__(self) -> None:
        self.result = 0

    def get_result(self) -> list[Any]:
        return self.result

    def execute(self, model: FeatureModel) -> 'FMFindAbstractFeatures':
        self.result = find_abstract_features(model)
        return self

    def get_abstract_features(self) -> list[Any]:
        return self.get_result()


def find_abstract_features(feature_model: FeatureModel) -> list[Any]:
    abstract = []
    for feature in feature_model.get_features():
        if feature.is_abstract:
            abstract.append(feature.name)
    return abstract