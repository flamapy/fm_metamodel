from typing import cast

from flamapy.core.models import VariabilityModel
from flamapy.core.operations import AverageBranchingFactor
from flamapy.core.operations.descriptor import OperationDescriptor
from flamapy.metamodels.fm_metamodel.models.feature_model import FeatureModel


class FMAverageBranchingFactor(AverageBranchingFactor):
    """The average branching factor is the average of branches in the feature model.

    The number of branches is the number of features that have at least one child.
    """

    facade = OperationDescriptor(
        doc=(
            'This refers to the average number of child features that a parent feature has\n'
            "in a feature model. It's calculated by dividing the total number of child\n"
            'features by the total number of parent features. A high average branching\n'
            'factor indicates a complex feature model with many options, while a low\n'
            'average branching factor indicates a simpler model.'
        ),
        returns='Union[None, float]',
        name='average_branching_factor', operation='FMAverageBranchingFactor'
    )

    def __init__(self) -> None:
        self.result: float = 0

    def get_result(self) -> float:
        return self.result

    def execute(self, model: VariabilityModel) -> 'FMAverageBranchingFactor':
        fm_model = cast(FeatureModel, model)
        self.result = average_branching_factor(fm_model)
        return self

    def get_average_branching_factor(self) -> float:
        return self.get_result()


def average_branching_factor(feature_model: FeatureModel, precision: int = 2) -> float:
    nof_branches = 0
    nof_children = 0
    for feature in feature_model.get_features():
        if feature.get_relations():
            nof_branches += 1
            nof_children += sum(len(r.children) for r in feature.get_relations())
    return round(nof_children / nof_branches, precision)
