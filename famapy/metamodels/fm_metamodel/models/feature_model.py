from typing import Optional

from famapy.core.models import AST
from famapy.core.models import VariabilityModel


class Relation:

    def __init__(
        self,
        parent: 'Feature',
        children: list['Feature'],
        card_min: int,
        card_max: int
    ) -> None:

        self.parent = parent
        self.children = children
        self.card_min = card_min
        self.card_max = card_max

    def add_child(self, feature: 'Feature') -> None:
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

    def __str__(self) -> str:
        parent_name = self.parent.name if self.parent else ''
        res = f'{parent_name}[{self.card_min},{self.card_max}]'
        for _child in self.children:
            res += _child.name + ' '
        return res

    def __hash__(self) -> int:
        return hash((self.parent, frozenset(self.children), self.card_min, self.card_max))

    def __eq__(self, other: object) -> bool:
        return (isinstance(other, Relation)
                and self.parent == other.parent
                and self.children == other.children
                and self.card_min == other.card_min
                and self.card_max == other.card_max)


class Feature:

    def __init__(
        self,
        name: str,
        relations: Optional[list['Relation']] = None,
        parent: Optional['Feature'] = None,
        is_abstract: bool = False
    ):

        self.name = name
        self.relations = [] if relations is None else relations
        self.parent = self._get_parent() if parent is None else parent
        self.is_abstract = is_abstract

    def is_empty(self) -> bool:
        return self.parent is None and self.relations == []

    def add_relation(self, relation: 'Relation') -> None:
        self.relations.append(relation)

    def get_relations(self) -> list['Relation']:
        return self.relations

    def get_parent(self) -> Optional['Feature']:
        return self.parent

    def _get_parent(self) -> Optional['Feature']:
        return next((r.parent for r in self.get_relations() if not r.children), None)

    def is_root(self) -> bool:
        return self.parent is None

    def is_mandatory(self) -> bool:
        return (self.parent is None
                or any(r.is_mandatory() and self in r.children
                       for r in self.parent.get_relations()))

    def is_optional(self) -> bool:
        return (self.parent is not None
                and any(r.is_optional() and self in r.children
                        for r in self.parent.get_relations()))

    def is_or_group(self) -> bool:
        return any(r.is_or() for r in self.get_relations())

    def is_alternative_group(self) -> bool:
        return any(r.is_alternative() for r in self.get_relations())

    def is_group(self) -> bool:
        return self.is_or_group() or self.is_alternative_group()

    def __str__(self) -> str:
        return self.name

    def __hash__(self) -> int:
        return hash(self.name)

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Feature) and self.name == other.name


class Constraint:
    def __init__(self, name: str, ast: AST):
        self.name = name
        self.ast = ast

    def __hash__(self) -> int:
        return hash(self.name)

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Constraint) and self.name == other.name


class FeatureModel(VariabilityModel):

    @staticmethod
    def get_extension() -> str:
        return 'fm'

    def __init__(
        self,
        root: 'Feature',
        constraints: Optional[list['Constraint']] = None
    ) -> None:
        self.root = root
        self.ctcs = [] if constraints is None else constraints

    def get_relations(self, feature: Optional['Feature'] = None) -> list['Relation']:
        if self.root.is_empty():
            return []
        if feature is None:
            feature = self.root
        relations = []
        for relation in feature.relations:
            relations.append(relation)
            for _feature in relation.children:
                relations.extend(self.get_relations(_feature))
        return relations

    def get_features(self) -> list['Feature']:
        features: list['Feature'] = []
        features.append(self.root)
        for relation in self.get_relations():
            features.extend(relation.children)
        return features

    def get_constraints(self) -> list['Constraint']:
        return self.ctcs

    def get_feature_by_name(self, feature_name: str) -> 'Feature':
        if feature_name not in self.features_by_name.keys():
            raise Exception(f'Not feature with name: {feature_name}')
        return self.features_by_name[feature_name]

    def __str__(self) -> str:
        if self.root.is_empty():
            return '(empty feature model)'
        res = 'root: ' + self.root.name + '\r\n'
        for i, relation in enumerate(self.get_relations()):
            res += f'relation {i}: {relation}\r\n'
        for i, ctc in enumerate(self.ctcs):
            res += f'{ctc.ast}' + '\r\n'
        return res

    def __hash__(self) -> int:
        return hash((
            self.root,
            frozenset(self.get_features()),
            frozenset(self.get_relations()),
            frozenset(self.ctcs)
        ))

    def __eq__(self, other: object) -> bool:
        return (
            isinstance(other, FeatureModel) and
            self.root == other.root and
            self.get_features() == other.get_features() and
            self.get_relations() == other.get_relations() and
            self.ctcs == other.ctcs
        )
