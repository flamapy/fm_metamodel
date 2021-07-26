import os
from .uvl_parser.get_tree import get_tree as get_tree

from famapy.core.transformations import TextToModel
from famapy.core.models.ast import AST
from famapy.core.exceptions import DuplicatedFeature
from famapy.metamodels.fm_metamodel.models.feature_model import (
    Constraint,
    Feature,
    FeatureModel,
    Relation
)

class UVLTransformation(TextToModel):

    @staticmethod
    def get_source_extension() -> str:
        return 'uvl'

    def __init__(self, path):
        self.path = path
        self.parse_tree = None
        self.model = None

    def set_parse_tree(self):
        absolute_path = os.path.abspath(self.path)
        self.parse_tree = get_tree(absolute_path)

    def transform(self):
        self.set_parse_tree()
        # Find ParseTree node of root feature
        parse_tree_root_feature = self.find_root_feature()
        root_feature_text = self.get_feature_text(parse_tree_root_feature)
        root_feature = Feature(root_feature_text, [])
        # Feature model created with root feature
        self.model = FeatureModel(root_feature, [] )
        # Recursively read the ParseTree root feature subnode to find all features and relations
        self.read_children(parse_tree_root_feature, root_feature)
        self.read_constraints()
        return self.model

    def find_root_feature(self):
        return self.parse_tree.features().child()

    def get_feature_text(self, node):
        return node.feature_spec().ref().WORD()[
            0].getText()

    def get_relation_text(self, node):
        return node.relation_spec().RELATION_WORD().getText()

    def read_children(self, parse_tree_node, node_feature):
        relations = parse_tree_node.relation()
        for relation_node in relations:
            relation_text = self.get_relation_text(relation_node)
            features = relation_node.child()
            children = []
            for feature_node in features:
                feature_text = self.get_feature_text(feature_node)
                feature = Feature(feature_text, [])
                #self.model.features.append(feature)
                children.append(feature)
            self.add_relation(node_feature, children, relation_text)
            for feature_node in features:
                self.read_children(feature_node, feature)

    def add_relation(self, parent, children, relation_text):
        if relation_text == "mandatory":
            for child in children:
                relation = Relation(parent, [child], 1, 1)
                parent.add_relation(relation)
        elif relation_text == "optional":
            for child in children:
                relation = Relation(parent, [child], 0, 1)
                parent.add_relation(relation)
        elif relation_text == "or":
            relation = Relation(parent, children, 1, 1)
            parent.add_relation(relation)
        elif relation_text == "alternative":
            relation = Relation(parent, children, 1, len(children))
            parent.add_relation(relation)
        else:
            relation_text = relation_text.replace("[", "").replace("]", "")
            words = relation_text.split("..")
            if len(words) == 1:
                min, max = words[0]
            else:
                min = int(words[0])
                max = int(words[1])
            assert(
                min <= max, "minimum cardinality must be lower or equal than maximum")
            assert(max <= len(
                children), "maximum cardinality must be lower or equal than the amount of children")

            if(min == max == len(children)):
                for child in children:
                    relation = Relation(parent, [child], 1, 1)
                    parent.add_relation(relation)
            elif(min == 0 and max == len(children)):
                for child in children:
                    relation = Relation(parent, [child], 0, 1)
                    parent.add_relation(relation)
            else:
                relation = Relation(parent, children, min, max)
                parent.add_relation(relation)

    def read_constraints(self):
        constraints_node = self.parse_tree.constraints().constraint()
        constraints = self.parse_constraints(constraints_node)
        self.model.ctcs = constraints

    def parse_constraints(self, constraints_node):
        constraints = []
        for constraint_node in constraints_node:
            constraint_text = constraint_node.getText()
            features = [list(constraint_node.getChildren())[0].WORD()[0].getText(), list(
                constraint_node.getChildren())[0].WORD()[1].getText()]
            operator = constraint_text.replace(
                features[0], "").replace(features[1], "")
            operator_dict = {'!': "not", '&': "and",
                             '|': "or", '=>': "implies", '<=>': "equivalence"}
            operator_name = operator_dict.get(operator)
            constraint = Constraint(
                operator_name, AST.create_simple_binary_operation(operator_name,features[0],features[1]))
            constraints.append(constraint)
        return constraints



