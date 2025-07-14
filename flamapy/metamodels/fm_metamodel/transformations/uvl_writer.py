import re
import string
import functools
import pathlib
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
        self._compact_relations = False

    def set_compact_relations(self, compact_relations: bool) -> None:
        """Set whether to compact relations in the UVL output."""
        self._compact_relations = compact_relations

    def transform(self) -> str:
        model = self.model
        root = model.root
        self._constraints_attributes: list[Constraint] = []

        result = ''
        if self.model.imports:
            result += "imports\n"
            for alias, namespace in self.model.alias_namespace.items():
                result += f'\t{namespace}'
                if alias != namespace:
                    result += f' as {alias}'
                result += '\n'
        result += 'features'
        serialized_model = (
            self.read_features(root, result, 0) + '\n' + self.read_constraints()
        )

        if self.path is not None:
            with open(self.path, "w", encoding="utf8") as file:
                file.write(serialized_model)

        for namespace, submodel in self.model.imports.items():
            path = f'{"/".join(namespace.split("."))}.uvl'
            pathlib.Path(path).parent.mkdir(parents=True, exist_ok=True)
            submodel_str = UVLWriter(path, submodel).transform()
            serialized_model += f'\n\n{submodel_str}'
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
        if not self._compact_relations:
            result = self.serialize_relations_with_children(feature.relations, result, tab_count)
        else:
            mandatory_relations = [rel for rel in feature.relations if rel.is_mandatory()]
            optional_relations = [rel for rel in feature.relations if rel.is_optional()]
            group_relations = [rel for rel in feature.relations if rel.is_group()]
            result = self.serialize_compact_relations(mandatory_relations, result, tab_count)
            result = self.serialize_compact_relations(optional_relations, result, tab_count)
            result = self.serialize_relations_with_children(group_relations, result, tab_count)
        return result

    def serialize_relations_with_children(self,
                                          relations: list[Relation],
                                          result: str,
                                          tab_count: int) -> str:
        for relation in relations:
            relation_name = self.serialize_relation(relation)
            result += "\n" + tab_count * "\t" + relation_name
            for feature_node in relation.children:
                result = self.read_features(feature_node, result, tab_count)
        return result

    def serialize_compact_relations(self,
                                    relations: list[Relation],
                                    result: str,
                                    tab_count: int) -> str:
        if not relations:
            return result
        relation_name = self.serialize_relation(relations[0])
        result += "\n" + tab_count * "\t" + relation_name
        children = [child for relation in relations for child in relation.children]
        for child in children:
            result = self.read_features(child, result, tab_count)
        return result

    def read_attributes(self, feature: Feature) -> str:
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
        feature_constraints_str = self.read_feature_constraints(feature)
        if feature_constraints_str:
            attributes.append(feature_constraints_str)
        return f'{{{", ".join(attributes)}}}' if attributes else ""

    def read_feature_constraints(self, feature: Feature) -> str:
        result = ''
        feature_constraints = []
        if feature.constraints_attributes:
            for constraint in feature.constraints_attributes:
                if isinstance(constraint, Constraint):
                    feature_constraints.append(self.serialize_constraint(constraint))
                    self._constraints_attributes.append(constraint)
                else:
                    raise TypeError(f"Unsupported type for constraint: {type(constraint)}")
            if len(feature_constraints) == 1:
                result += f'constraint {feature_constraints[0]}'
            elif len(feature_constraints) > 1:
                result += f'constraints [{", ".join(feature_constraints)}]'
        return result

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
        constraints = self.model.get_constraints()
        if len(constraints) > len(self._constraints_attributes):
            result = "constraints"
            for constraint in constraints:
                if constraint in self._constraints_attributes:
                    self._constraints_attributes.remove(constraint)
                else:
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
