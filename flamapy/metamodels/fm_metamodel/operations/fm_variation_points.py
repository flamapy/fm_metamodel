from typing import Any, cast

from flamapy.core.models import VariabilityModel
from flamapy.core.operations.descriptor import OperationDescriptor
from flamapy.metamodels.fm_metamodel.operations.interfaces import VariationPoints
from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature


def _variation_points_result(result: Any) -> Any:
    return {vp.name: [variant.name for variant in variants] for vp, variants in result.items()}


class FMVariationPoints(VariationPoints):
    facade = OperationDescriptor(
        doc=(
            'Returns the variation points of the feature model. A variation point is a\n'
            'feature that has at least one non-mandatory child, representing a decision\n'
            'point in the model. The result maps each variation point name to the list of\n'
            'its variant feature names.'
        ),
        returns='Union[None, Dict[str, List[str]]]',
        name='variation_points', operation='FMVariationPoints',
        result_adapter=_variation_points_result,
    )

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
