import re

from famapy.core.models.ast import ASTOperation
from famapy.core.transformations import ModelToText
from famapy.metamodels.fm_metamodel.models import (
    Constraint,
    Feature,
    FeatureModel,
    Relation,
)


class UVLWriter(ModelToText):

    @staticmethod
    def get_destination_extension() -> str:
        return 'uvl'

    def __init__(self, source_model: FeatureModel, path: str):
        self.path = path
        self.model = source_model

    def transform(self) -> FeatureModel:
        model = self.model
        root = model.root

        serialized_model = self.read_features(
            root, "features", 0) + "\n" + self.read_constraints()

        with open(self.path, 'w', encoding='utf8') as file:
            file.write(serialized_model)

    def read_features(self, feature: Feature, result: str, tab_count: int) -> str:
        tab_count = tab_count + 1
        result = result + "\n" + tab_count * "\t" + \
            feature.name + self.read_attributes(feature)
        tab_count = tab_count + 1
        for relation in feature.relations:
            relation_name = self.serialize_relation(relation)
            result = result + "\n" + tab_count * "\t" + relation_name
            for feature_node in relation.children:
                result = self.read_features(feature_node, result, tab_count)
        return result

    @classmethod
    def read_attributes(cls, feature: Feature) -> str:
        attributes = feature.get_attributes()
        result = ""

        if len(attributes) > 0:
            result = "{"
            first = True
            for attribute in attributes:
                if not first:
                    result += ", "
                result += attribute.name
                if attribute.default_value is not None:
                    result += ' "' + attribute.default_value + '"'
                first = False
            result += "}"

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
            max_value = rel.card_max
            if min_value == max_value:
                result = "[" + str(min) + "]"
            else:
                result = "[" + str(min) + ".." + str(max) + "]"

        return result

    def read_constraints(self) -> str:
        result = "constraints"
        constraints = self.model.ctcs
        for constraint in constraints:
            constraint_text = self.serialize_constraint(constraint)
            result = result + "\n\t" + constraint_text

        return result

    @staticmethod
    def serialize_constraint(ctc: Constraint) -> str:
        ctc = ctc.ast.pretty_str()
        ctc = re.sub(fr'\b{ASTOperation.NOT.value}\ \b', '!', ctc)
        ctc = re.sub(fr'\b{ASTOperation.AND.value}\b', '&', ctc)
        ctc = re.sub(fr'\b{ASTOperation.OR.value}\b', '|', ctc)
        ctc = re.sub(fr'\b{ASTOperation.IMPLIES.value}\b', '=>', ctc)
        ctc = re.sub(fr'\b{ASTOperation.EQUIVALENCE.value}\b', '<=>', ctc)
        ctc = re.sub(fr'\b{ASTOperation.REQUIRES.value}\b', 'requires', ctc)
        ctc = re.sub(fr'\b{ASTOperation.EXCLUDES.value}\b', 'excludes', ctc)
        return ctc
