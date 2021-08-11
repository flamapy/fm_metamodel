from typing import Optional
import math

from famapy.core.operations import NumberOfConfigurations

from famapy.metamodels.fm_metamodel.models import FeatureModel, Feature, FMConfiguration


class FMNumberOfConfigurations(NumberOfConfigurations):
    """It computes the number of configurations of the feature model.

    It only uses the structure of the feature model, 
    without taking into account the cross-tree constraints,
    and thus, the number is an upper limit of the real number of configurations.
    """
    
    def __init__(self, partial_configuration: Optional[FMConfiguration] = None) -> None:
        self.result = 0
        self.feature_model = None
        self.partial_configuration = partial_configuration
    
    def execute(self, feature_model: FeatureModel) -> 'FMNumberOfConfigurations':
        self.feature_model = feature_model
        self.result = self.get_number_of_configurations(self.partial_configuration)
        return self

    def get_result(self) -> int:
        return self.result

    def get_number_of_configurations(self, partial_configuration: Optional[FMConfiguration] = None) -> int:
        return count_configurations(self.feature_model, partial_configuration)

def count_configurations(feature_model: FeatureModel, partial_configuration: Optional[FMConfiguration] = None) -> int:
    return count_configurations_rec(feature_model.root)

def count_configurations_rec(feature: Feature) -> int:
    if feature.is_leaf():
        return 1
    counts = []
    for relation in feature.get_relations():
        if relation.is_mandatory():
            counts.append(count_configurations_rec(relation.children[0]))
        elif relation.is_optional():
            counts.append(count_configurations_rec(relation.children[0]) + 1)
        elif relation.is_alternative():
            counts.append(sum((count_configurations_rec(f) for f in relation.children)))
        elif relation.is_or():
            counts.append(math.prod([count_configurations_rec(f) + 1 for f in relation.children]) - 1)
    return math.prod(counts)