import re
import string
import functools
from typing import Union

from flamapy.core.models.ast import ASTOperation
from flamapy.core.transformations import ModelToText
from flamapy.metamodels.fm_metamodel.models import (
    Constraint,
    Feature,
    FeatureModel,
    Relation
)


UVL_OPERATORS: dict[ASTOperation, str] = {ASTOperation.AND: "&",
                                          ASTOperation.OR: "|",
                                          ASTOperation.NOT: "!",
                                          ASTOperation.IMPLIES: "=>",
                                          ASTOperation.EQUIVALENCE: "<=>",
                                          ASTOperation.REQUIRES: "=>",
                                          ASTOperation.EXCLUDES: "=> !",
                                          ASTOperation.EQUALS: '==',
                                          ASTOperation.LOWER: '<',
                                          ASTOperation.GREATER: '>',
                                          ASTOperation.LOWER_EQUALS: '<=',
                                          ASTOperation.GREATER_EQUALS: '>=',
                                          ASTOperation.NOT_EQUALS: '!=',
                                          ASTOperation.ADD: '+',
                                          ASTOperation.SUB: '-',
                                          ASTOperation.MUL: '*',
                                          ASTOperation.DIV: '/',
                                          ASTOperation.SUM: 'sum',
                                          ASTOperation.AVG: 'avg',
                                          ASTOperation.LEN: 'len',
                                          ASTOperation.FLOOR: 'floor',
                                          ASTOperation.CEIL: 'ceil',
                                          ASTOperation.XOR: ASTOperation.XOR.value  # Not soported
                                          }


class UVLWriter(ModelToText):
    @staticmethod
    def get_destination_extension() -> str:
        return "uvl"

    def __init__(self, path: str, source_model: FeatureModel):
        self.path = path
        self.model = source_model

    def transform(self) -> str:
        model = self.model
        root = model.root

        serialized_model = (
            self.read_features(root, "features", 0) + "\n" + self.read_constraints()
        )

        if self.path is not None:
            with open(self.path, "w", encoding="utf8") as file:
                file.write(serialized_model)
        return serialized_model

    def read_features(self, feature: Feature, result: str, tab_count: int) -> str:
        tab_count = tab_count + 1
        feature_type = f'{feature.feature_type.value} ' if not feature.is_boolean() else ''
        fmincard = feature.feature_cardinality.min
        fmaxcard: Union[int, str] = feature.feature_cardinality.max
        fmaxcard = '*' if fmaxcard == -1 else fmaxcard
        feature_cardinality = f'cardinality [{fmincard}..{fmaxcard}] '
        feature_cardinality = feature_cardinality if feature.is_multifeature() else ''
        result = (
            result
            + "\n"
            + tab_count * "\t"
            + feature_type
            + safename(feature.name)
            + " "
            + feature_cardinality
            + self.read_attributes(feature)
        )
        tab_count = tab_count + 1
        for relation in feature.relations:
            relation_name = self.serialize_relation(relation)
            result = result + "\n" + tab_count * "\t" + relation_name
            for feature_node in relation.children:
                result = self.read_features(feature_node, result, tab_count)
        return result

    @classmethod
    def read_attributes(cls, feature: Feature) -> str:
        attributes = []
        if feature.is_abstract:
            attributes.append("abstract")
        for attribute in feature.get_attributes():
            attribute_str = safename(attribute.name)
            if attribute.default_value is not None:
                if isinstance(attribute.default_value, str):
                    attribute_str += f" '{attribute.default_value}'"
                elif isinstance(attribute.default_value, bool):
                    attribute_str += f" {str(attribute.default_value).lower()}"
                else:
                    attribute_str += f" {attribute.default_value}"
            attributes.append(attribute_str)
        return f'{{{", ".join(attributes)}}}' if attributes else ""

    @staticmethod
    def serialize_relation(rel: Relation) -> str:
        result = ""

        if rel.is_alternative():
            result = "alternative"
        elif rel.is_mandatory():
            result = "mandatory"
        elif rel.is_optional():
            result = "optional"
        elif rel.is_or():
            result = "or"
        else:
            min_value = rel.card_min
            max_value: Union[int, str] = rel.card_max

            if min_value == max_value:
                result = "[" + str(min_value) + "]"
            else:
                max_value = '*' if max_value == -1 else max_value
                result = "[" + str(min_value) + ".." + str(max_value) + "]"

        return result

    def read_constraints(self) -> str:
        result = ""
        constraints = self.model.ctcs
        if constraints:
            result = "constraints"
            for constraint in constraints:
                constraint_text = self.serialize_constraint(constraint)
                result = result + "\n\t" + constraint_text
        return result

    @staticmethod
    def _substitute_operator(str_constraint: str,
                             operator: ASTOperation,
                             new_operator: str) -> str:
        return re.sub(rf"\b{operator.value}\b", new_operator, str_constraint)

    @staticmethod
    def serialize_constraint(ctc: Constraint) -> str:
        str_constraint = ctc.ast.pretty_str()
        return functools.reduce(lambda acc, op: UVLWriter._substitute_operator(acc,
                                                                               op,
                                                                               UVL_OPERATORS[op]),
                                ASTOperation, str_constraint)


def safename(name: str) -> str:
    if '.' in name:
        return '.'.join([safe_simple_name(simple_name) for simple_name in name.split('.')])
    return safe_simple_name(name)


def safe_simple_name(name: str) -> str:
    if name.startswith("'") and name.endswith("'"):
        return name
    return f'"{name}"' if any(char not in safecharacters() for char in name) else name


def safecharacters() -> str:
    return string.ascii_letters + string.digits + '_'
