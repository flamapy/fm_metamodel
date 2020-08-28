import json
import sys

from famapy.core.transformations.ModelToText import ModelToText
from famapy.metamodels.fm_metamodel.models.FeatureModel import Feature, FeatureModel, Relation


class JsonWriter(ModelToText):
    EXT_DST = 'json'

    def __init__(self, path: str, model: FeatureModel):
        self.file = path
        self.model = model

    def transform(self):
        data={}
        root = self.model.root

        data['hierachy']=self.processFeature(root)
        data['ctc']=self.processConstraints()

        with open('data.json', 'w') as outfile:
            json.dump(data, outfile)
        return outfile

    def processFeature(self,f:Feature):
        dict={}
        dict["featureName"]=f.name
        relationships=[]
        for i,relation in enumerate(f.get_relations()):

            relationships.append(self.processRelation(relation))
        dict["relationships"]=relationships
        return(dict)

    def processRelation(self,relation:Relation):
        dict={}
        dict["card_min"]=relation.card_min
        dict["card_max"]=relation.card_max

        for child in relation.children:
            dict[child.name]=self.processFeature(child)

        return dict

    def processConstraints(self):
        print("This is not yet supported")
        return {}
