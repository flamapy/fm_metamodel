from ast import AST
from typing import Sequence

from famapy.core.models import VariabilityModel
from famapy.core.exceptions import ElementNotFound

class Relation:

    def __init__(self, parent: 'Feature', children: Sequence['Feature'], card_min: int, card_max: int):
        self.parent = parent
        self.children = children
        self.card_min = card_min
        self.card_max = card_max

    def add_child(self, feature: 'Feature'):
        self.children.append(feature)

    def is_mandatory(self) -> bool:
        return self.card_min == 1 and self.card_max == 1 and len(self.children) == 1

    def is_optional(self) -> bool:
        return self.card_min == 0 and self.card_max == 1 and len(self.children) == 1

    def is_or(self) -> bool:
        return self.card_min == 1 and self.card_max == len(self.children) and len(self.children) > 1

    def is_alternative(self) -> bool:
        return self.card_min == 1 and self.card_max == 1 and len(self.children) > 1

    def __str__(self):
        res = (self.parent.name if self.parent else '') + '[' + str(self.card_min) + ',' + str(self.card_max) + ']'
        for _child in self.children:
            res += _child.name + ' '
        return res


class Feature:

    def __init__(self, name: str, relations: Sequence['Relation']):
        self.name = name
        self.relations = relations

    def add_relation(self, relation: 'Relation'):
        self.relations.append(relation)

    def get_relations(self):
        return self.relations

    def get_parent(self):
        return next((r.parent for r in self.relations if not r.children), None)

    def __str__(self):
        return self.name

    def __repr__(self):
        return self.name

class Constraint:
    #This is heavily limited. Currently this will only support requires and excludes
    def __init__(self, name: str, origin:'Feature', destination:'Feature', ctc_type:str):
        self.name = name
        self.origin = origin
        self.destination = destination
        self.ctc_type = ctc_type

class FeatureModel(VariabilityModel):

    @staticmethod
    def get_extension() -> str:
        return 'fm'

    def __init__(self, root: Feature, constraint: Sequence[Constraint]=[], features: Sequence[Feature]=[], relations: Sequence[Relation]=[]):
        self.root = root
        self.ctcs = constraint  # implementar CTC con AST
        self.features = features
        self.relations = relations
        if not features:
            self.features = self.get_features()
        if not relations:
            self.relations = self.get_relations()

    def get_relations(self, feature=None):
        if not self.relations:
            relations = []
            if not feature:
                feature = self.root
            for relation in feature.relations:
                relations.append(relation)
                for _feature in relation.children:
                    relations.extend(self.get_relations(_feature))
            self.relations
        return self.relations

    def get_features(self):
        if not self.features:
            features = []
            features.append(self.root)
            for relation in self.get_relations():
                features.extend(relation.children)
            self.features = features
        return self.features

    #This method is for consistency with the getters
    def get_constraints(self):
        return self.ctcs

    def get_feature_by_name(self, str) -> Feature:
        features = self.get_features()
        for feat in features:
            if feat.name == str:
                return feat
        raise ElementNotFoundException

    def __str__(self) -> str:
        res = 'root: ' + self.root.name + '\r\n'
        for i, relation in enumerate(self.get_relations()):
            res += f'relation {i}: {relation}\r\n'
        for i, ctc in enumerate(self.ctcs):
            res += ctc.origin.name +" "+ctc.ctc_type + " " + ctc.destination.name
        return(res)
