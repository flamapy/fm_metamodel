from flamapy.core.operations import CountLeafs

from flamapy.metamodels.fm_metamodel.models import FeatureModel


class FMCountLeafs(CountLeafs):

    def __init__(self) -> None:
        self.result = 0

    def get_result(self) -> int:
        return self.result

    def execute(self, model: FeatureModel) -> 'FMCountLeafs':
        self.result = count_leaf_features(model)
        return self

    def get_number_of_leafs(self) -> int:
        return self.get_result()


def count_leaf_features(feature_model: FeatureModel) -> int:
    return sum(f.is_leaf() for f in feature_model.get_features())
