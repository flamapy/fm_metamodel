import os
from typing import Any, Optional

from uvlparser import get_tree, UVLParser
from famapy.core.transformations import TextToModel
from famapy.core.models.ast import AST, ASTOperation
from famapy.metamodels.fm_metamodel.models.feature_model import (
    Constraint,
    Feature,
    FeatureModel,
    Relation,
    Attribute,
)


class UVLReader(TextToModel):

    @staticmethod
    def get_source_extension() -> str:
        return 'uvl'

    def __init__(self, path: str) -> None:
        self.path: str = path
        self.parse_tree: Any = None
        self.model: Optional[FeatureModel] = None

    def set_parse_tree(self) -> None:
        absolute_path = os.path.abspath(self.path)
        self.parse_tree = get_tree(absolute_path)

    def transform(self) -> FeatureModel:
        self.set_parse_tree()
        # Find ParseTree node of root feature
        parse_tree_root_feature = self.find_root_feature()
        root_feature_text = self.get_feature_text(parse_tree_root_feature)
        root_feature = Feature(root_feature_text, [])
        self.add_attributes(parse_tree_root_feature, root_feature)
        # Feature model created with root feature
        self.model = FeatureModel(root_feature, [])
        # Recursively read the ParseTree root feature subnode to find all features and relations
        self.read_children(parse_tree_root_feature, root_feature)
        self.read_constraints()
        return self.model

    def find_root_feature(self) -> Feature:
        return self.parse_tree.features().child()

    @classmethod
    def get_feature_text(cls, node: Feature) -> str:
        return node.feature_spec().ref().WORD()[0].getText()

    @classmethod
    def get_relation_text(cls, node: Feature) -> str:
        return node.relation_spec().RELATION_WORD().getText()

    def read_children(self, parse_tree_node: Feature, node_feature: Feature) -> None:
        relations = parse_tree_node.relation()
        for relation_node in relations:
            relation_text = self.get_relation_text(relation_node)
            features = relation_node.child()
            children = []
            for feature_node in features:
                feature_text = self.get_feature_text(feature_node)
                feature = Feature(feature_text, [])
                self.add_attributes(feature_node, feature)
                # self.model.features.append(feature)
                children.append(feature)
                self.read_children(feature_node, feature)
            self.add_relation(node_feature, children, relation_text)

    @classmethod
    def add_relation(cls, parent: Feature, children: Feature, relation_text: str) -> None:
        if relation_text == 'mandatory':
            for child in children:
                relation = Relation(parent, [child], 1, 1)
                parent.add_relation(relation)
        elif relation_text == 'optional':
            for child in children:
                relation = Relation(parent, [child], 0, 1)
                parent.add_relation(relation)
        elif relation_text == 'or':
            relation = Relation(parent, children, 1, len(children))
            parent.add_relation(relation)
        elif relation_text == 'alternative':
            relation = Relation(parent, children, 1, 1)
            parent.add_relation(relation)
        else:
            cls.__add_relation_min_max(parent, children, relation_text)

    @classmethod
    def add_attributes(cls, feature_node: UVLParser.FeaturesContext, feature: Feature) -> None:

        attributes_node = feature_node.feature_spec().attributes()

        attribute_node = []
        if attributes_node is not None:
            attribute_node = attributes_node.attribute()

        for att_node in attribute_node:
            name = att_node.key().getText()
            value = None
            if att_node.value() is not None:
                value = att_node.value().getText()
            attribute = Attribute(name, None, value, None)
            attribute.set_parent(feature)
            feature.add_attribute(attribute)

    @classmethod
    def __add_relation_min_max(
        cls,
        parent: Feature,
        children: Feature,
        relation_text: str
    ) -> None:
        relation_text = relation_text.replace("[", "").replace("]", "")
        words = relation_text.split("..")
        if len(words) == 1:
            _min = int(words[0])
            _max = int(words[0])
        else:
            _min = int(words[0])
            _max = int(words[1])
        assert _min <= _max, 'minimum cardinality must be lower or equal than maximum'
        assert _max <= len(children), (
            'maximum cardinality must be lower or equal than the amount of children'
        )

        if _min == _max == len(children):
            for child in children:
                relation = Relation(parent, [child], 1, 1)
                parent.add_relation(relation)
        elif _min == 0 and _max == len(children):
            for child in children:
                relation = Relation(parent, [child], 0, 1)
                parent.add_relation(relation)
        else:
            relation = Relation(parent, children, _min, _max)
            parent.add_relation(relation)

    def read_constraints(self) -> None:
        assert self.model is not None
        constraints_node = self.parse_tree.constraints().constraint()
        constraints = self.parse_constraints(constraints_node)
        self.model.ctcs = constraints

    @classmethod
    def parse_constraints(cls, constraints_node: list[Any]) -> list[Constraint]:
        constraints: list[Constraint] = []
        for constraint_node in constraints_node:
            constraint_text = constraint_node.getText()
            features = [
                list(constraint_node.getChildren())[0].WORD()[0].getText(),
                list(constraint_node.getChildren())[0].WORD()[1].getText()
            ]
            operator = constraint_text.replace(
                features[0], "").replace(features[1], "")
            operator_dict = {
                '!': ASTOperation.NOT,
                '&': ASTOperation.AND,
                '|': ASTOperation.OR,
                '=>': ASTOperation.IMPLIES,
                '<=>': ASTOperation.EQUIVALENCE,
                'requires': ASTOperation.REQUIRES,
                'excludes': ASTOperation.EXCLUDES,
            }
            operator_type = operator_dict.get(operator)
            assert operator_type is not None, operator
            constraint = Constraint(
                operator_type.name,
                AST.create_simple_binary_operation(operator_type, features[0], features[1])
            )
            constraints.append(constraint)
        return constraints
