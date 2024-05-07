import json
from typing import Any

from flamapy.core.models.ast import Node
from flamapy.core.transformations import ModelToText
from flamapy.metamodels.fm_metamodel.models import (
    Constraint,
    Feature,
    FeatureModel,
    Relation,
)


class JsonWriter(ModelToText):

    @staticmethod
    def get_destination_extension() -> str:
        return 'json'

    def __init__(self, source_model: FeatureModel, path: str):
        self.path = path
        self.model = source_model

    def transform(self) -> FeatureModel:
        data: dict[str, Any] = {}
        root = self.model.root

        data['hierachy'] = self.process_feature(root)
        data['ctc'] = self.process_constraints(self.model.get_constraints())

        if self.path is not None:
            with open(self.path, 'w', encoding='utf8') as outfile:
                json.dump(data, outfile)
        return data

    def process_feature(self, feature: Feature) -> dict[str, Any]:
        _dict: dict[str, Any] = {}
        _dict["featureName"] = feature.name
        relationships = []
        for relation in feature.get_relations():
            relationships.append(self.process_relation(relation))
        _dict["relationships"] = relationships
        return _dict

    def process_relation(self, relation: Relation) -> dict[str, Any]:
        _dict: dict[str, Any] = {}
        _dict["card_min"] = relation.card_min
        _dict["card_max"] = relation.card_max

        for child in relation.children:
            _dict[child.name] = self.process_feature(child)

        return _dict

    def process_constraints(self, constraints: list[Constraint]) -> dict[str, Any]:
        constraints_info = {}
        for ctc in constraints:
            constraints_info[ctc.name] = self._get_ctc_info(ctc.ast.root)
        return constraints_info

    def _get_ctc_info(self, ast_node: Node) -> dict[str, Any]:
        ctc_info: dict[str, Any] = {}
        if ast_node.is_term():
            ctc_info['type'] = 'FeatureTerm'
            ctc_info['operands'] = [ast_node.data]
        else:
            ctc_info['type'] = ast_node.data
            operands = []
            left = self._get_ctc_info(ast_node.left)
            operands.append(left)
            if ast_node.right is not None:
                right = self._get_ctc_info(ast_node.right)
                operands.append(right)
            ctc_info['operands'] = operands
        return ctc_info
