from famapy.core.transformations import ModelToText
from famapy.metamodels.fm_metamodel.models.feature_model import (
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
        f = open(self.path, "w")
        f.write(serialized_model)

    def read_features(self, feature: Feature, result: str, tab_count: int):
        tab_count = tab_count + 1
        result = result + "\n" + tab_count*"\t" + feature.name
        tab_count = tab_count + 1
        for relation in feature.relations:
            relation_name = self.serialize_relation(relation)
            result = result + "\n" + tab_count*"\t" + relation_name
            for feature in relation.children:
                result = self.read_features(feature, result, tab_count)
        return result

    def serialize_relation(self, relation: Relation):
        result = ""

        if relation.is_alternative():
            result = "alternative"
        elif relation.is_mandatory():
            result = "mandatory"
        elif relation.is_optional():
            result = "optional"
        elif relation.is_or():
            result = "or"
        else:
            min = relation.card_min
            max = relation.card_max
            if min == max:
                result = "[" + str(min) + "]"
            else:
                result = "[" + str(min) + ".." + str(max) + "]"

        return result

    def read_constraints(self):
        result = "constraints"
        constraints = self.model.ctcs
        for constraint in constraints:
            constraint_text = self.serialize_constraint(constraint)
            result = result + "\n\t" + constraint_text

        return result

    def serialize_constraint(self, constraint: Constraint):
        left = constraint.ast.root.left
        right = constraint.ast.root.right
        data = constraint.ast.root.data

        symbol_dict = {'not': '!', 'and': '&', 'or': '|',
                       'implies': '=>', 'equivalence': '<=>'}
        symbol = symbol_dict.get(data)

        result = str(left) + " " + symbol + " " + str(right)

        return result
