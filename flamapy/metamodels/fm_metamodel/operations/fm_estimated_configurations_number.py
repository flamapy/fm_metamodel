import math
from typing import Optional, cast

from flamapy.core.models import VariabilityModel
from flamapy.core.exceptions import FlamaException
from flamapy.core.operations import EstimatedConfigurationsNumber
from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature, Relation


class FMEstimatedConfigurationsNumber(EstimatedConfigurationsNumber):
    """It computes an estimation of the number of products of the feature model.

    It only uses the structure of the feature model,
    without taking into account the cross-tree constraints,
    and thus, the number is an upper limit of the real number of products.
    """

    def __init__(self) -> None:
        self.result = 0
        self.feature_model: Optional[FeatureModel] = None

    def execute(self, model: VariabilityModel) -> 'FMEstimatedConfigurationsNumber':
        self.feature_model = cast(FeatureModel, model)
        self.result = self.get_configurations_number()
        return self

    def get_result(self) -> int:
        return self.result

    def get_configurations_number(self) -> int:
        if self.feature_model is None:
            raise FlamaException("The feature model is not defined")

        return count_configurations(self.feature_model)


def count_configurations(feature_model: FeatureModel) -> int:
    return count_configurations_rec(feature_model.root)


def count_configurations_rec(feature: 'Feature') -> int:
    if feature.is_leaf():
        return 1
    total_feature_configurations = 1
    for relation in feature.get_relations():
        relation_configurations = 0
        if relation.is_mandatory():
            relation_configurations = count_configurations_rec(relation.children[0])
        elif relation.is_optional():
            relation_configurations = count_configurations_rec(relation.children[0]) + 1
        elif relation.is_alternative():
            relation_configurations = sum(count_configurations_rec(f) for f in relation.children)
        elif relation.is_or():
            children_counts = [count_configurations_rec(f) + 1 for f in relation.children]
            relation_configurations = math.prod(children_counts) - 1
        elif relation.is_cardinal() or relation.is_mutex():
            relation_configurations = calculate_group_cardinality_configurations(relation)
        else:
            relation_configurations = 0
        total_feature_configurations *= relation_configurations
    return total_feature_configurations


def calculate_group_cardinality_configurations(relation: Relation) -> int:
    relation_configurations = 0
    # For a generic cardinality group [a..b]:
    # This is the most complex case, where between 'card_min' and 'card_max' children
    # must be selected.
    # Each selected child contributes its own number of configurations.

    # Pre-calculate configurations of each child to avoid redundant calls
    children_configs = [count_configurations_rec(f) for f in relation.children]
    num_children = len(children_configs)

    # Use a dynamic programming approach to calculate this efficiently.
    # dp[i][j] will represent the number of ways to choose 'j' elements from the
    # first 'i' children, considering their internal configurations.

    # The size of dp will be (num_children + 1) x (num_children + 1)
    dp = [[0] * (num_children + 1) for _ in range(num_children + 1)]

    # Initialization: There's 1 way to choose 0 elements from 0 children (the empty selection).
    dp[0][0] = 1

    # Fill the dp table
    for i in range(1, num_children + 1):
        # The i-th child (at index i-1 in children_configs) has 'current_child_config'
        # configurations.
        current_child_config = children_configs[i-1]

        for j in range(num_children + 1):
            # Option 1: Do not choose the current (i-th) child
            # Inherit the ways to choose 'j' elements from the first 'i-1' children.
            dp[i][j] = dp[i-1][j]

            # Option 2: Choose the current (i-th) child
            # If we can choose 'j-1' elements from the first 'i-1' children,
            # then we can choose 'j' elements by including the current child.
            # Multiply by its internal configurations.
            if j > 0:
                dp[i][j] += dp[i-1][j-1] * current_child_config

    # Sum the configurations for the allowed cardinalities [card_min..card_max]
    for k in range(relation.card_min, relation.card_max + 1):
        relation_configurations += dp[num_children][k]
    return relation_configurations
