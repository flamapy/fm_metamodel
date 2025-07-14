import json
import string
from typing import Any

from flamapy.core.models.ast import Node, ASTOperation
from flamapy.core.transformations import ModelToText
from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature, Constraint


class GlencoeWriter(ModelToText):
    CTC_TYPES = {
        ASTOperation.NOT: "NotTerm",
        ASTOperation.AND: "AndTerm",
        ASTOperation.OR: "OrTerm",
        ASTOperation.XOR: "XorTerm",
        ASTOperation.IMPLIES: "ImpliesTerm",
        ASTOperation.REQUIRES: "ImpliesTerm",
        ASTOperation.EXCLUDES: "ExcludesTerm",
        ASTOperation.EQUIVALENCE: "EquivalentTerm",
    }

    @staticmethod
    def get_destination_extension() -> str:
        return "gfm.json"

    def __init__(self, path: str, source_model: FeatureModel) -> None:
        self.path = path
        self.source_model = source_model

    def transform(self) -> str:
        json_object = _to_json(self.source_model)
        json_str = json.dumps(json_object, ensure_ascii=False, indent=4)
        if self.path is not None:
            with open(self.path, 'w', encoding="utf8") as file:
                file.write(json_str)
        return json_str


def _to_json(feature_model: FeatureModel) -> dict[str, Any]:
    result: dict[str, Any] = {}
    print(f'-{feature_model.root.name}-')
    print(f'-{safename(feature_model.root.name)}-')
    result["id"] = f"FM_{safename(feature_model.root.name)}"
    result["name"] = f"FM_{safename(feature_model.root.name)}"
    result["features"] = _get_features_info(feature_model.get_features())
    result["tree"] = _get_tree_info(feature_model.root)
    result["constraints"] = _get_constraints_info(feature_model.get_constraints())
    return result


def _get_features_info(features: list[Feature]) -> dict[str, Any]:
    features_info = {}
    for feature in sorted(features, key=lambda f: f.name):
        feature_type = "FEATURE"
        if feature.is_alternative_group():
            feature_type = "XOR"
        elif feature.is_or_group():
            feature_type = "OR"
        elif feature.is_cardinality_group():
            feature_type = "GENOR"

        features_info[safename(feature.name)] = {
            "name": safename(feature.name),
            "optional": not feature.is_mandatory(),
            "type": feature_type,
            "note": "",  # ToDo: add 'note' attribute information
        }

        if feature_type == "GENOR":
            relation = next(r for r in feature.get_relations() if r.is_cardinal())
            features_info[safename(feature.name)]["min"] = relation.card_min
            features_info[safename(feature.name)]["max"] = relation.card_max
    return features_info


def _get_tree_info(feature: Feature) -> dict[str, Any]:
    feature_info: dict[str, Any] = {}
    feature_info["id"] = safename(feature.name)
    children = [
        _get_tree_info(child)
        for child in sorted(feature.get_children(), key=lambda f: safename(f.name))
    ]
    if children:
        feature_info["children"] = children
    return feature_info


def _get_constraints_info(constraints: list[Constraint]) -> dict[str, Any]:
    constraints_info = {}
    for ctc in constraints:
        constraints_info[ctc.name] = _get_ctc_info(ctc.ast.root)
    return constraints_info


def _get_ctc_info(ast_node: Node) -> dict[str, Any]:
    ctc_info: dict[str, Any] = {}
    if ast_node.is_term():
        ctc_info["type"] = "FeatureTerm"
        ctc_info["operands"] = [safename(str(ast_node.data))]
    else:
        if ast_node.data not in GlencoeWriter.CTC_TYPES:
            raise ValueError(f"Unsupported constraint type: {ast_node.data}")
        ctc_info["type"] = GlencoeWriter.CTC_TYPES[ast_node.data]
        operands = []
        left = _get_ctc_info(ast_node.left)
        operands.append(left)
        if ast_node.right is not None:
            right = _get_ctc_info(ast_node.right)
            operands.append(right)
        ctc_info["operands"] = operands
    return ctc_info


def safename(name: str) -> str:
    if '.' in name:
        return '.'.join([safe_simple_name(simple_name) for simple_name in name.split('.')])
    return safe_simple_name(name)


def safe_simple_name(name: str) -> str:
    if name.startswith("'") and name.endswith("'"):
        return name
    allowed = set(safecharacters())
    return ''.join(c if c in allowed else '_' for c in name)


def safecharacters() -> str:
    return string.ascii_letters + string.digits + '_'


