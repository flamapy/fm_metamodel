from ast import AST
from typing import Sequence

from core.models.VariabilityModel import VariabilityModel


class Relation(object):

    def __init__(self, parent: 'Feature', children: Sequence['Feature'], card_min: int, card_max: int):
        self.parent = parent
        self.children = children
        self.card_min = card_min
        self.card_max = card_max

    def add_child(self,feature:'Feature'):
        self.children.append(feature)

    def is_mandatory(self) -> bool:
        return (len(self.children)==1 and self.card_max==1 and self.card_min==1)

    def is_optional(self)-> bool:
        return (len(self.children)==1 and self.card_max==1 and self.card_min==0)

    def is_or(self)-> bool:
        return (len(self.children)>1 and self.card_max==len(self.children) and self.card_min==1)

    def is_alternative(self)-> bool:
        return (len(self.children)>1 and self.card_max==1 and self.card_min==1)

    def __str__(self):
        res= self.parent.name + '[' + str(self.card_min) + ',' + str(self.card_max) + ']'
        for _child in self.children:
            res=res+_child.name+' '
        return res

class Feature():

    def __init__(self, name: str, relations: Sequence['Relation']):
        self.name = name
        self.relations = relations

    def add_relation(self, relation: 'Relation'):
        self.relations.append(relation)

    def get_relations(self):
        return self.relations

    def __str__(self):
        return self.name


class FeatureModel(VariabilityModel):

    def __init__(self, root: Feature, constraint: Sequence[AST]):
        self.root = root
        self.ctc = constraint #implementar CTC con AST

    def get_relations(self, feature=None):
        relations = []
        if not feature:
            feature = self.root
        for relation in feature.relations:
            relations.append(relation)
            for _feature in relation.children:
                relations.extend(self.get_relations(_feature))
        return relations

    def get_features(self, feature=None):
        features = []
        features.append(self.root)
        for relation in self.get_relations():
            features.extend(relation.children)
        return features

    def __str__(self) -> str:
        res= 'root: '+ self.root.name + '\r\n'
        for i,relation in enumerate(self.get_relations()):
            res=res+'relation '+ str(i)+': '+str(relation) + '\r\n'
        return(res)