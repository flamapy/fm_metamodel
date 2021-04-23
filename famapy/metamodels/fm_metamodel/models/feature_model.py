from famapy.core.models import AST
from famapy.core.models import VariabilityModel


class Relation:

    def __init__(
        self,
        parent: 'Feature',
        children: list['Feature'],
        card_min: int,
        card_max: int
    ):

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
        return (
            self.card_min == 1 and
            self.card_max == len(self.children) and
            len(self.children) > 1
        )

    def is_alternative(self) -> bool:
        return self.card_min == 1 and self.card_max == 1 and len(self.children) > 1

    def __str__(self):
        parent_name = self.parent.name if self.parent else ''
        res = f'{parent_name}[{self.card_min},{self.card_max}]'
        for _child in self.children:
            res += _child.name + ' '
        return res


class Feature:

    def __init__(self, name: str, relations: list['Relation']):
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
    '''
    This is heavily limited. Currently this will only support requires and excludes
    '''
    def __init__(self, name: str, ast: AST):
        self.name = name
        self.ast = ast
        #self.origin = origin
        #self.destination = destination
        #self.ctc_type = ctc_type


class FeatureModel(VariabilityModel):

    @staticmethod
    def get_extension() -> str:
        return 'fm'

    def __init__(
        self,
        root: Feature,
        constraint: list['Constraint'] = list,
        features: list['Feature'] = list,
        relations: list['Relation'] = list
    ):
        self.root = root
        self.ctcs = constraint  # implementar CTC con AST
        self.features = features
        self.relations = relations

    def get_relations(self, feature=None):
        if not self.relations:
            relations = []
            if not feature:
                feature = self.root
            for relation in feature.relations:
                relations.append(relation)
                for _feature in relation.children:
                    relations.extend(self.get_relations(_feature))
            self.relations = relations
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

    def get_feature_by_name(self, feature_name: str) -> Feature:
        if feature_name not in self.features_by_name.keys():
            raise Exception
        return self.features_by_name[feature_name]

    def __str__(self) -> str:
        res = 'root: ' + self.root.name + '\r\n'
        for i, relation in enumerate(self.get_relations()):
            res += f'relation {i}: {relation}\r\n'
        for i, ctc in enumerate(self.ctcs):
            root = ctc.ast.get_root()
            first_child_name = ctc.ast.get_first_child(root).get_name()
            second_child_name = ctc.ast.get_second_child(root).get_name()
            res += f'{first_child_name} {root.get_name()} {second_child_name}'
        return res
