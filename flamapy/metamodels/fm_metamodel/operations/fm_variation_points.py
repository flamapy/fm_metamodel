from typing import cast

from flamapy.core.models import VariabilityModel
from flamapy.metamodels.fm_metamodel.operations.interfaces import VariationPoints
from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature


class FMVariationPoints(VariationPoints):

    def __init__(self) -> None:
        self.result: dict[Feature, list[Feature]] = {}

    def get_result(self) -> dict[Feature, list[Feature]]:
        return self.result

    def execute(self, model: VariabilityModel) -> 'FMVariationPoints':
        fm_model = cast(FeatureModel, model)
        self.result = variation_points(fm_model)
        return self

    def variation_points(self) -> dict[Feature, list[Feature]]:
        return self.get_result()


def variation_points(feature_model: FeatureModel) -> dict[Feature, list[Feature]]:
    vps: dict[Feature, list[Feature]] = {}
    features = [feature_model.root]
    while features:
        feature = features.pop()
        variants = []
        for relation in feature.get_relations():
            if not relation.is_mandatory():
                variants.extend(relation.children)
        if variants:
            vps[feature] = variants
        features.extend(feature.get_children())
    return vps
