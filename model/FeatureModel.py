from ast import AST
from typing import Sequence

from core.models.VariabilityModel import VariabilityModel


class Relation(object):

    def __init__(self, parent: 'Feature', children: Sequence['Feature'], card_min: int, card_max: int):
        self.parent = parent
        self.children = children
        self.card_min = card_min
        self.card_max = card_max

    def is_mandatory(self) -> bool:
        return (len(self.children)==1 and self.card_max==1 and self.card_min==1)

    def is_optional(self)-> bool:
        return (len(self.children)==1 and self.card_max==1 and self.card_min==0)

    def is_or(self)-> bool:
        return (len(self.children)>1 and self.card_max==len(self.children) and self.card_min==1)

    def is_alternative(self)-> bool:
        return (len(self.children)>1 and self.card_max==1 and self.card_min==1)


class Feature():

    def __init__(self, name: str, relations: Sequence['Relation']):
        self.name = name
        self.relations = relations

    def add_relation(self, relation: 'Relation'):
        self.relations.append(relation)


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
                self.get_relations(_feature)
        return relations

    def get_features(self, feature=None):
        features = []
        if not feature:
            feature = self.root
            features.append(feature)
        for relation in feature.relations:
            for _feature in relation.children:
                features.append(_feature)
                self.get_features(_feature)
        return features

    def __str__(self) -> str:
        return self.root.name
