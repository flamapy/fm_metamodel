import math

from famapy.core.operations import ProductsNumber

from famapy.metamodels.fm_metamodel.models import FeatureModel, Feature


class FMEstimatedProductsNumber(ProductsNumber):
    """It computes an estimation of the number of products of the feature model.

    It only uses the structure of the feature model,
    without taking into account the cross-tree constraints,
    and thus, the number is an upper limit of the real number of products.
    """

    def __init__(self) -> None:
        self.result = 0
        self.feature_model = None

    def execute(self, model: FeatureModel) -> 'FMEstimatedProductsNumber':
        self.feature_model = model
        self.result = self.get_products_number()
        return self

    def get_result(self) -> int:
        return self.result

    def get_products_number(self) -> int:
        return count_configurations(self.feature_model)


def count_configurations(feature_model: FeatureModel) -> int:
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
            children_counts = [count_configurations_rec(f) + 1 for f in relation.children]
            counts.append(math.prod(children_counts) - 1)
    return math.prod(counts)
