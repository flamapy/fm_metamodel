import json

from famapy.core.transformations import ModelToText
from famapy.metamodels.fm_metamodel.models.feature_model import Feature, FeatureModel, Relation


class JsonWriter(ModelToText):

    @staticmethod
    def get_destiny_extension() -> str:
        return 'json'

    def __init__(self, path: str, model: FeatureModel):
        self.path = path
        self.model = model

    def transform(self):
        data = {}
        root = self.model.root

        data['hierachy'] = self.process_feature(root)
        data['ctc'] = self.process_constraints()

        with open(self.path, 'w') as outfile:
            json.dump(data, outfile)
        return self.path

    def process_feature(self, f: Feature):
        _dict = {}
        _dict["featureName"] = f.name
        relationships = []
        for relation in f.get_relations():
            relationships.append(self.process_relation(relation))
        _dict["relationships"] = relationships
        return _dict

    def process_relation(self, relation: Relation):
        _dict = {}
        _dict["card_min"] = relation.card_min
        _dict["card_max"] = relation.card_max

        for child in relation.children:
            _dict[child.name] = self.process_feature(child)

        return _dict

    def process_constraints(self):
        print("This is not yet supported")
        return {}
