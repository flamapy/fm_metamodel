from famapy.core.operations import CountLeafs
from famapy.metamodels.fm_metamodel.models.feature_model import FeatureModel


class FMCountLeafs(CountLeafs):

    def __init__(self):
        self.number_of_leafs = 0

    def get_number_of_leafs(self):
        return self.number_of_leafs

    def get_result(self):
        return self.get_number_of_leafs()

    def execute(self, model: FeatureModel) -> 'FMCountLeafs':
        number = 0
        for feat in model.get_features():
            if len(feat.get_relations()) == 0:
                number += 1
        self.number_of_leafs = number
        return self
