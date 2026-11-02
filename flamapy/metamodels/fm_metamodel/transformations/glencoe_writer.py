import json
import logging
from typing import Any

from flamapy.core.models.ast import Node, ASTOperation
from flamapy.core.transformations import ModelToText
from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature, Constraint, Relation
from flamapy.metamodels.fm_metamodel.transformations.group_support import (
    decompose_groups,
    mixed_decomposition,
    prepare_model,
)

logger = logging.getLogger(__name__)


class GlencoeWriter(ModelToText):
    """Write a feature model in Glencoe's JSON format.

    Features with several groups, or a group and other relations, are rejected; with
    `apply_refactorings=True` a copy of the model is refactored first
    (MultipleGroupDecompositionRefactoring), which adds abstract features but keeps the
    configurations of the original features.
    """
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

    def __init__(self, path: str, source_model: FeatureModel,
                 apply_refactorings: bool = False) -> None:
        self.path = path
        self.source_model = source_model
        self.apply_refactorings = apply_refactorings

    def transform(self) -> str:
        model = prepare_model(self.source_model, 'Glencoe', [mixed_decomposition],
                              [decompose_groups], self.apply_refactorings)
        json_object = _to_json(model)
        json_str = json.dumps(json_object, ensure_ascii=False, indent=4)
        if self.path is not None:
            with open(self.path, 'w', encoding="utf8") as file:
                file.write(json_str)
        return json_str


def _to_json(feature_model: FeatureModel) -> dict[str, Any]:
    """Glencoe gives each feature one decomposition type: FEATURE (mandatory/optional children),
    XOR, OR or GENOR [min..max] (also used for mutex groups, [0..1]). Every feature of the model
    has a single group or only mandatory/optional children (see GlencoeWriter.transform).
    Names are written as they are: Glencoe is JSON and needs no escaping. Only logical
    constraints are written; arithmetic constraints are omitted (as in PLWriter)."""
    result: dict[str, Any] = {}
    result["id"] = f"FM_{feature_model.root.name}"
    result["name"] = f"FM_{feature_model.root.name}"
    features_info: dict[str, Any] = {}
    result["tree"] = _feature_node(feature_model.root, not feature_model.root.is_mandatory(),
                                   features_info)
    result["features"] = dict(sorted(features_info.items()))
    result["constraints"] = _get_constraints_info(feature_model.get_logical_constraints())
    return result


def _group_info(relation: Relation) -> dict[str, Any]:
    if relation.is_alternative():
        return {"type": "XOR"}
    if relation.is_or():
        return {"type": "OR"}
    # mutex [0..1] and any other group cardinality [m..n]
    return {"type": "GENOR", "min": relation.card_min, "max": relation.card_max}


def _add_info(features_info: dict[str, Any], name: str, optional: bool,
              group: dict[str, Any]) -> None:
    features_info[name] = {"name": name, "optional": optional, "type": group["type"], "note": ""}
    if group["type"] == "GENOR":
        features_info[name]["min"] = group["min"]
        features_info[name]["max"] = group["max"]


def _feature_node(feature: Feature, optional: bool,
                  features_info: dict[str, Any]) -> dict[str, Any]:
    relations = feature.get_relations()
    children: list[dict[str, Any]] = []
    if len(relations) == 1 and relations[0].is_group():
        _add_info(features_info, feature.name, optional, _group_info(relations[0]))
        children = [_feature_node(child, True, features_info) for child in relations[0].children]
    else:
        _add_info(features_info, feature.name, optional, {"type": "FEATURE"})
        children = [_feature_node(relation.children[0], not relation.is_mandatory(), features_info)
                    for relation in relations]
    node: dict[str, Any] = {"id": feature.name}
    if children:
        node["children"] = children
    return node


def _get_constraints_info(constraints: list[Constraint]) -> dict[str, Any]:
    constraints_info = {}
    for ctc in constraints:
        constraints_info[ctc.name] = _get_ctc_info(ctc.ast.root)
    return constraints_info


def _get_ctc_info(ast_node: Node) -> dict[str, Any]:
    ctc_info: dict[str, Any] = {}
    if ast_node.is_term():
        ctc_info["type"] = "FeatureTerm"
        ctc_info["operands"] = [str(ast_node.data)]
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
