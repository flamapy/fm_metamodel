from famapy.core.operations import AverageBranchingFactor
from famapy.metamodels.fm_metamodel.models.feature_model import FeatureModel


class AverageBranchingFactor(AverageBranchingFactor):

    def __init__(self):
        self.average_branching_factor = 0

    def get_average_branching_factor(self):
        return self.average_branching_factor

    def execute(self, model: FeatureModel) -> 'AverageBranchingFactor':
        features = model.get_features()
        childrens = 0
        for feat in features:
            for relation in feat.get_relations():
                childrens+=len(relation.children)
        self.average_branching_factor = round(childrens/len(features))
        return self
