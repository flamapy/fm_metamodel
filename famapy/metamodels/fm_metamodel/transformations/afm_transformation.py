import sys
from typing import Any

from famapy.core.exceptions import DuplicatedFeature
from famapy.core.models.ast import AST
from famapy.core.transformations import TextToModel
from famapy.metamodels.fm_metamodel.models.feature_model import (
    Constraint,
    Feature,
    FeatureModel,
    Relation,
)


class AFMTransformation(TextToModel):

    @staticmethod
    def get_source_extension() -> str:
        return 'afm'

    def __init__(self, path: str) -> None:
        self.path = path
        self.name_feature: dict[Any, Any] = {}
        self.parents: list[Any] = []
        self.ctc_counter = [0, 0, 0]

    def transform(self) -> FeatureModel:
        with open(self.path, 'r') as _file:
            lines = [line.strip() for line in _file.readlines() if line.strip() != ""]

        for line in lines:
            if "%Relationships" in line:
                index_r = lines.index(line)
            if "%Constraints" in line:
                index_c = lines.index(line)
        relations = lines[index_r + 1:index_c]
        constraints = lines[index_c + 1:]

        feature_model = FeatureModel(Feature("", []))
        for relation in relations:
            words = relation.split(" ")
            self.parse_features(words, feature_model)

        for constraint in constraints:
            constraint = constraint.replace(";", "")
            ctc = self.parse_ctc(constraint)
            feature_model.ctcs.append(ctc)

        return feature_model

    def parse_ctc(self, ctc: str) -> Constraint:
        # TODO: review
        return Constraint('Ctc-' + str(self.ctc_counter), AST(ctc))

    def parse_features(self, words: list[str], model: FeatureModel) -> Feature:
        name = words[0].replace(":", "")
        words.pop(0)

        if name in self.parents:
            print("This AFM contains duplicated feature names", file=sys.stderr)
            raise DuplicatedFeature
        self.parents.append(name)

        feature_parent = Feature(name, [])
        if name in self.name_feature:
            feature_parent = self.name_feature[name]
        else:
            model.features.append(feature_parent)
            model.root = feature_parent
            self.name_feature[name] = feature_parent

        alternative_rel = "[1,1]"
        or_rel = "[1," + str(len(words) - 1) + "]"
        if words.__contains__(alternative_rel) or words.__contains__(or_rel):
            if words.__contains__(alternative_rel):
                words.remove(alternative_rel)
                relation = self.parse_relation("Alternative", feature_parent)
            elif words.__contains__(or_rel):
                words.remove(or_rel)
                relation = self.parse_relation("Or", feature_parent, len(words))
            model.relations.append(relation)
            for word in words:
                word = word.replace("{", "").replace("}", "").replace(";", "")
                self.add_feature(relation, word, model)
        else:
            for word in words:
                if word.__contains__("[") and word.__contains__("]"):
                    relation = self.parse_relation("Optional", feature_parent)
                    word = word.replace("[", "").replace("]", "").replace(";", "")
                    self.add_feature(relation, word, model)
                else:
                    relation = self.parse_relation("Mandatory", feature_parent)
                    word = word.replace(";", "")
                    self.add_feature(relation, word, model)
                model.relations.append(relation)
        return feature_parent

    def add_feature(self, relation: Relation, word: str, model: FeatureModel) -> None:
        if word in self.name_feature:
            print("This AFM contains duplicated feature names", file=sys.stderr)
            raise DuplicatedFeature
        feature = Feature(word, [])
        model.features.append(feature)
        self.name_feature[word] = feature
        relation.children.append(feature)

    @classmethod
    def parse_relation(
        cls,
        relation_type: str,
        feature_parent: Feature,
        c_max: int = 1
    ) -> Relation:
        if relation_type in ("Mandatory", "Alternative"):
            relation = Relation(parent=feature_parent, children=[], card_min=1, card_max=1)
        elif relation_type == "Optional":
            relation = Relation(parent=feature_parent, children=[], card_min=0, card_max=1)
        elif relation_type == "Or":
            relation = Relation(parent=feature_parent, children=[], card_min=1, card_max=c_max)
        feature_parent.relations.append(relation)
        return relation
