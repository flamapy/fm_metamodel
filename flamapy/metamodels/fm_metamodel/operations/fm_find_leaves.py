from flamapy.core.operations import FindLeaves

from flamapy.metamodels.fm_metamodel.models import FeatureModel
from typing import Any

class FMFindLeaves(FindLeaves):

    def __init__(self) -> None:
        self.result = 0

    def get_result(self) -> list[Any]:
        return self.result

    def execute(self, model: FeatureModel) -> 'FMFindLeaves':
        self.result = find_leaf_features(model)
        return self

    def get_leaves(self) -> list[Any]:
        return self.get_result()


def find_leaf_features(feature_model: FeatureModel) -> int:
    leaves = []
    for f in feature_model.get_features():
        if f.is_leaf():
            leaves.append(f.name)
    return leaves