import functools
import json
from typing import Any

from famapy.core.models.ast import AST, Node, ASTOperation
from famapy.core.transformations import TextToModel

from famapy.metamodels.fm_metamodel.models import FeatureModel, Feature, Relation, Constraint


class GlencoeReader(TextToModel):

    @staticmethod
    def get_source_extension() -> str:
        return 'gfm.json'

    def __init__(self, path: str) -> None:
        self.path = path

    def transform(self) -> FeatureModel:
        with open(self.path, 'r') as file:
            data = json.load(file)
            features_info = data['features']
            root_node = data['tree']
            constraints_info = data['constraints']
            root_feature = self._parse_tree(None, root_node, features_info)
            constraints = self._parse_constraints(constraints_info, features_info)
            return FeatureModel(root_feature, constraints)

    def _parse_tree(self, parent: Feature, feature_node: dict[Any], features_info: dict[Any]) -> Feature:
        """Parse the tree structure and returns the root feature."""
        feature_id = feature_node['id']
        feature_name = features_info[feature_id]['name']
        feature = Feature(name=feature_name, parent=parent)
        
        if 'children' in feature_node:
            children = []
            for child in feature_node['children']:
                child_feature = self._parse_tree(feature, child, features_info)
                if features_info[feature_id]['type'] == 'FEATURE':
                    optional = features_info[child['id']]['optional']
                    min = 0 if optional else 1
                    relation = Relation(parent=feature, children=[child_feature], card_min=min, card_max=1)
                    feature.add_relation(relation)
                else:
                    children.append(child_feature)
                    relation = None
            if relation is None:
                if features_info[feature_id]['type'] == 'XOR':
                    relation = Relation(parent=feature, children=children, card_min=1, card_max=1)
                elif features_info[feature_id]['type'] == 'OR':
                    relation = Relation(parent=feature, children=children, card_min=0, card_max=len(children))
                elif features_info[feature_id]['type'] == 'GENOR':  # Group Cardinality
                    min = features_info[feature_id]['min']
                    max = features_info[feature_id]['max']
                    relation = Relation(parent=feature, children=children, card_min=min, card_max=max)
                feature.add_relation(relation)
        # Additional relation because Glencoe supports mandatory features in groups
        if parent is not None and parent.is_group():
            optional = features_info[feature_id]['optional']
            if not optional:
                parent.add_relation(Relation(parent, [feature], 1, 1))
        # Create an attribute for the 'note' parameter
        note = features_info[feature_id]['note']
        if note:
            pass

        return feature

    def _parse_constraints(self, constraints_info: dict[Any], features_info: dict[Any]) -> list[Constraint]:
        constraints = []
        print(constraints_info)
        for ctc_info in constraints_info.values():
            ctc = self._parse_ast_constraint(ctc_info, features_info)
            constraints.append(ctc)
        return constraints

    def _parse_ast_constraint(self, constraint_info: dict[Any], features_info: dict[Any]) -> Node:
        if constraint_info['type'] == 'FeatureTerm':
            feature_id = constraint_info['operands'][0]
            feature_name = features_info[feature_id]['name']
            return Node(feature_name)
        operands = constraint_info['operands']
        if constraint_info['type'] == 'NotTerm':
            left = self._parse_ast_constraint(operands[0], features_info)
            return Node(ASTOperation.NOT, left)
        if constraint_info['type'] == 'ImpliesTerm':
            left = self._parse_ast_constraint(operands[0], features_info)
            right = self._parse_ast_constraint(operands[1], features_info)
            return Node(ASTOperation.IMPLIES, left, right)
        if constraint_info['type'] == 'ExcludesTerm':
            left = self._parse_ast_constraint(operands[0],features_info)
            right = self._parse_ast_constraint(operands[1], features_info)
            return Node(ASTOperation.EXCLUDES, left, right)
        if constraint_info['type'] == 'EquivalentTerm':
            left = self._parse_ast_constraint(operands[0], features_info)
            right = self._parse_ast_constraint(operands[1], features_info)
            return Node(ASTOperation.EQUIVALENCE, left, right)
        if constraint_info['type'] == 'AndTerm':
            op_list = [self._parse_ast_constraint(op, features_info) for op in operands]
            return functools.reduce(lambda l, r: Node(ASTOperation.AND, l, r), op_list)
        if constraint_info['type'] == 'OrTerm':
            op_list = [self._parse_ast_constraint(op, features_info) for op in operands]
            return functools.reduce(lambda l, r: Node(ASTOperation.OR, l, r), op_list)
        if constraint_info['type'] == 'XorTerm':
            op_list = [self._parse_ast_constraint(op, features_info) for op in operands]
            return functools.reduce(lambda l, r: Node(ASTOperation.XOR, l, r), op_list)
        return None