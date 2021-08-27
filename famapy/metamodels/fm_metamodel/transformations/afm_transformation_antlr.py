from afmparser import *
from afmparser import get_tree

import os
from typing import Any, Optional

from famapy.core.transformations import TextToModel
from famapy.core.models.ast import AST, Node
from famapy.metamodels.fm_metamodel.models.feature_model import (
    Constraint,
    Domain,
    Feature,
    FeatureModel,
    Range,
    Relation,
    Attribute
)


class AFM_ANTLR_Transformation(TextToModel):

    @staticmethod
    def get_source_extension() -> str:
        return 'afm'

    def __init__(self, path: str) -> None:
        self.path: str = path
        self.parse_tree: AFMParser.Feature_modelContext = None
        self.model: Optional[FeatureModel] = None

    def set_parse_tree(self) -> None:
        absolute_path = os.path.abspath(self.path)
        self.parse_tree = get_tree(absolute_path)

    def transform(self) -> None:
        self.set_parse_tree()
        self.set_relations()
        self.set_attributes()
        self.set_constraints()

    def set_relations(self) -> None:
        root_feature_node = self.parse_tree
        relationships_block = root_feature_node.relationships_block()
        self.set_root_feature(relationships_block)
        self.set_child_features(relationships_block)

    def set_root_feature(self, relationships_block: AFMParser.Relationships_blockContext) -> None:
        root_feature_spec = relationships_block.relationship_spec()[0]
        root_feature_name = root_feature_spec.init_spec().WORD().getText()
        root_feature = Feature(root_feature_name, [])

        self.model = FeatureModel(root_feature, None)

        self.read_children(root_feature_spec)

    def set_child_features(self, relationships_block: AFMParser.Relationships_blockContext) -> None:
        relationship_spec_list = relationships_block.relationship_spec()
        if len(relationship_spec_list) == 1:
            pass
        else:
            for relationship_spec in relationship_spec_list[1:]:
                self.read_children(relationship_spec)

    def read_children(self, feature_spec: AFMParser.Relationship_specContext) -> None:
        parent_feature = self.model.get_feature_by_name(
            feature_spec.init_spec().WORD().getText())

        cardinal_spec = feature_spec.cardinal_spec()
        if cardinal_spec is not None:

            children = []
            cardinality = cardinal_spec.cardinality()
            card_min = cardinality.INT()[0]
            card_max = cardinality.INT()[1]

            for feature_node in cardinal_spec.obligatory_spec():
                child = Feature(feature_node.WORD().getText(), [])
                children.append(child)
            relation = Relation(parent_feature, children, card_min, card_max)
            parent_feature.add_relation(relation)

        non_cardinal_spec = feature_spec.non_cardinal_spec()
        if non_cardinal_spec is not None:

            for obligatory in non_cardinal_spec.obligatory_spec():
                child = Feature(obligatory.WORD().getText())
                relation = Relation(parent_feature, [child], 1, 1)
                parent_feature.add_relation(relation)

            for optional in non_cardinal_spec.optional_spec():
                child = Feature(optional.WORD().getText())
                relation = Relation(parent_feature, [child], 0, 1)
                parent_feature.add_relation(relation)

    def set_attributes(self) -> None:
        attributes_block = self.parse_tree.attributes_block()
        for attribute_spec in attributes_block.attribute_spec():
            self.read_attribute(attribute_spec)

    def read_attribute(self, attribute_spec: AFMParser.Attribute_specContext) -> None:
        attribute_name_node = attribute_spec.attribute_name()
        feature_name = attribute_name_node.WORD().getText()
        attribute_name = attribute_name_node.LOWERCASE().getText()
        attribute_feature = self.model.get_feature_by_name(feature_name)

        domain_node = attribute_spec.attribute_domain()
        domain = None

        discrete_domain_node = domain_node.discrete_domain_spec()
        if discrete_domain_node is not None:
            values = []
            for value in discrete_domain_node.value_spec():
                values.append(value.getText())
            domain = Domain(None, values)

        range_domain_node = domain_node.range_domain_spec()
        if range_domain_node is not None:
            range_list = []
            for domain_range in range_domain_node.domain_range():
                min_value = domain_range.INT()[0]
                max_value = domain_range.INT()[1]
                value_range = Range(min_value, max_value)
                range_list.append(value_range)
            domain = Domain(range_list, None)

        default_value = attribute_spec.attribute_default_value().value_spec().getText()
        null_value = attribute_spec.attribute_null_value().value_spec().getText()

        attribute = Attribute(attribute_name, attribute_feature, domain,
                              default_value, null_value)
        attribute_feature.add_attribute(attribute)

    def set_constraints(self) -> None:
        constraints_block = self.parse_tree.constraints_block()

        for constraint_spec in constraints_block.constraint_spec():

            simple_spec = constraint_spec.simple_spec()
            if simple_spec is not None:
                prefix = ""
                self.read_expression(simple_spec.expression(), prefix)

            brackets_spec = constraint_spec.brackets_spec()
            if brackets_spec is not None:
                prefix = brackets_spec.WORD().getText() + "."
                for spec in brackets_spec.simple_spec():
                    self.read_expression(spec.expression(), prefix)

    def read_expression(self, expression: AFMParser.ExpressionContext, prefix: str) -> None:

        root_node = self.build_ast_node(expression, prefix)
        ast = AST(root_node)
        cst = Constraint(expression.getText(), ast)
        self.model.ctcs.append(cst)

    def build_ast_node(self, expression: AFMParser.ExpressionContext, prefix: str) -> Node:

        if isinstance(expression, AFMParser.AtomContext):
            if expression.variable() is not None:
                var_name = prefix + expression.variable().getText()
            if expression.number() is not None:
                var_name = expression.number().getText()

            return Node(var_name)

        if isinstance(expression, AFMParser.LogicalExpContext):
            result = Node(expression.logical_operator().getText())
            result.left = self.build_ast_node(
                expression.expression()[0], prefix)
            result.right = self.build_ast_node(
                expression.expression()[1], prefix)
            return result

        if isinstance(expression, AFMParser.OrExpContext):
            result = Node("OR")
            result.left = self.build_ast_node(
                expression.expression()[0], prefix)
            result.right = self.build_ast_node(
                expression.expression()[1], prefix)
            return result

        if isinstance(expression, AFMParser.AndExpContext):
            result = Node("AND")
            result.left = self.build_ast_node(
                expression.expression()[0], prefix)
            result.right = self.build_ast_node(
                expression.expression()[1], prefix)
            return result

        if isinstance(expression, AFMParser.RelationalExpContext):
            result = Node(expression.relational_operator().getText())
            result.left = self.build_ast_node(
                expression.expression()[0], prefix)
            result.right = self.build_ast_node(
                expression.expression()[1], prefix)
            return result

        if isinstance(expression, AFMParser.ArithmeticExpContext):
            result = Node(expression.arithmetic_operator().getText())
            result.left = self.build_ast_node(
                expression.expression()[0], prefix)
            result.right = self.build_ast_node(
                expression.expression()[1], prefix)
            return result

        if isinstance(expression, AFMParser.NotExpContext):
            result = Node("NOT")
            result.right = self.build_ast_node(
                expression.expression(), prefix)
            return result

        if isinstance(expression, AFMParser.ParenthesisExpContext):
            result = self.build_ast_node(expression.expression(), prefix)
            return result


afmtransf = AFM_ANTLR_Transformation("test.afm")
afmtransf.transform()
print(afmtransf.model)
