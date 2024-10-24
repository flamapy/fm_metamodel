import os
import logging
import functools
from typing import Any, Optional

from antlr4 import CommonTokenStream, FileStream
from antlr4.error.ErrorListener import ErrorListener
from uvl.UVLCustomLexer import UVLCustomLexer
from uvl.UVLPythonParser import UVLPythonParser

from flamapy.core.exceptions import FlamaException
from flamapy.core.transformations import TextToModel
from flamapy.core.models.ast import AST, ASTOperation, Node
from flamapy.metamodels.fm_metamodel.models import (
    Constraint,
    Feature,
    FeatureModel,
    Relation,
    Attribute,
    Cardinality,
    FeatureType
)


class CustomErrorListener(ErrorListener):
    def __init__(self) -> None:
        super().__init__()
        self.errors: list[str] = []

    def syntaxError(  # noqa: N802
        self,
        recognizer: Any,
        offendingSymbol: Any,  # noqa: N803
        line: Any,
        column: Any,
        msg: Any,
        e: Any,
    ) -> None:
        error_msg = f"Syntax error at line {line}, column {column}: {msg}"
        self.errors.append(error_msg)


class UVLReader(TextToModel):
    @staticmethod
    def get_source_extension() -> str:
        return "uvl"

    def __init__(self, path: str) -> None:
        self.path: str = os.sep.join(path.split(os.sep)[:-1])
        self.file: str = path.split(os.sep)[-1]
        self.namespace: str = ""
        self.parse_tree: Any = None
        self.model: Optional[FeatureModel] = None
        self.imports: dict[str, FeatureModel] = {}
        self.import_root: dict[str, str] = {}

    def set_parse_tree(self) -> None:
        absolute_path = os.path.abspath(os.path.join(self.path, self.file))
        input_stream = FileStream(absolute_path)
        lexer = UVLCustomLexer(input_stream)

        stream = CommonTokenStream(lexer)
        parser = UVLPythonParser(stream)

        # Attach custom error listener
        error_listener = CustomErrorListener()
        parser.removeErrorListeners()
        parser.addErrorListener(error_listener)

        self.parse_tree = parser.featureModel()

        if error_listener.errors:
            for error in error_listener.errors:
                logging.error(error)
            raise FlamaException("Parsing failed due to syntax errors.")

    def process_attributes(
        self, attributes_node: UVLPythonParser.AttributeContext
    ) -> dict[str, Any]:
        attributes_list = attributes_node.attribute()
        attributes_dict = {}

        for attribute_context in attributes_list:
            # First, check which kind of attribute we're dealing with
            value_attribute = attribute_context.valueAttribute()
            constraint_attribute = attribute_context.constraintAttribute()

            if value_attribute:
                key = value_attribute.key().getText().replace('"', '')
                if value_attribute.value():
                    value = self.process_value(value_attribute.value())
                else:
                    value = None  # or some default value

            elif constraint_attribute:
                # Here you can handle constraint attributes if you need them
                # If they should be processed differently, provide methods similar to process_value
                logging.warning("This attributes are not yet supported in flama.")
            else:
                # Handle unexpected case
                cleaned_text = attribute_context.getText().replace('"', '')

                # Raise the ValueError with the cleaned text
                raise ValueError(f"Unknown attribute type for: {cleaned_text}")

            attributes_dict[key] = value
        return attributes_dict

    def process_value(self, value_context: UVLPythonParser.ValueContext) -> Any:
        value = None
        if value_context.BOOLEAN():
            value = value_context.BOOLEAN().getText() == "true"
        elif value_context.FLOAT():
            value = float(value_context.FLOAT().getText())
        elif value_context.INTEGER():
            value = int(value_context.INTEGER().getText())
        elif value_context.STRING():
            value = value_context.STRING().getText()[1:-1]  # Removing quotes
        elif value_context.attributes():
            value = self.process_attributes(value_context.attributes())
        elif value_context.vector():
            value = [self.process_value(val) for val in value_context.vector().value()]
        return value

    def _check_feature_cardinality(
        self, feature: Feature, feature_node: UVLPythonParser.FeatureContext
    ) -> None:
        # See if there is a feature cardinality and attributes (TODO)
        if feature_node.featureCardinality():
            cardinality_text = feature_node.featureCardinality().CARDINALITY().getText()
            min_val, max_val = self.parse_cardinality(cardinality_text)
            feature.feature_cardinality = Cardinality(min=min_val, max=max_val)

    def _check_feature_type(
        self, feature: Feature, feature_node: UVLPythonParser.FeatureContext
    ) -> None:
        if feature_node.featureType():
            typed_text = feature_node.featureType().getText()
            if typed_text == 'Boolean':
                feature_type = FeatureType.BOOLEAN
            elif typed_text == 'String':
                feature_type = FeatureType.STRING
            elif typed_text == 'Integer':
                feature_type = FeatureType.INTEGER
            elif typed_text == 'Real':
                feature_type = FeatureType.REAL
            else:
                raise FlamaException('Error: unknow feature type for ' \
                                     f'{typed_text} of feature {feature.name}.')
            feature.feature_type = feature_type

    def _check_attributes(
        self, feature: Feature, feature_node: UVLPythonParser.FeatureContext
    ) -> None:
        if feature_node.attributes():
            attributes = self.process_attributes(feature_node.attributes())

            for key, value in attributes.items():
                if key == "abstract" and (value is None or value):
                    feature.is_abstract = True
                else:
                    feature.add_attribute(Attribute(name=key, default_value=value))

    def process_feature(
        self, feature: Feature, feature_node: UVLPythonParser.FeatureContext
    ) -> Feature:
        self._check_feature_cardinality(feature, feature_node)
        self._check_feature_type(feature, feature_node)
        self._check_attributes(feature, feature_node)

        # Get the relationship type
        for relationship in feature_node.group():
            childs = self.process_group(relationship.groupSpec())
            if isinstance(relationship, UVLPythonParser.AlternativeGroupContext):
                feature.add_relation(Relation(feature, childs, 1, 1))
            elif isinstance(relationship, UVLPythonParser.OptionalGroupContext):
                for child in childs:
                    feature.add_relation(Relation(feature, [child], 0, 1))
            elif isinstance(relationship, UVLPythonParser.OrGroupContext):
                feature.add_relation(Relation(feature, childs, 1, len(childs)))
            elif isinstance(relationship, UVLPythonParser.MandatoryGroupContext):
                for child in childs:
                    feature.add_relation(Relation(feature, [child], 1, 1))
            elif isinstance(relationship, UVLPythonParser.CardinalityGroupContext):
                # Access the CARDINALITY token text.
                cardinality_text = relationship.CARDINALITY().getText()

                min_value, max_value = self.parse_cardinality(cardinality_text)
                feature.add_relation(Relation(feature, childs, min_value, max_value))

                if max_value > len(childs):
                    logging.warning(
                        "Cardinality error: max value is greater than the number of childs"
                    )

        return feature

    def parse_cardinality(self, cardinality_text: str) -> tuple[int, int]:
        # Extract the minimum and maximum values.
        # This assumes a format like "[min..max]" or "[min]" or "[min..*]"
        min_value: str = ""
        max_value: str = ""

        # Remove brackets.
        cardinality_text = cardinality_text[1:-1]

        if ".." in cardinality_text:
            parts = cardinality_text.split("..")
            min_value = parts[0]
            max_value = parts[1]
        else:
            min_value = cardinality_text
            max_value = min_value  # Assuming max is the same as min if not specified.

        try:
            return int(min_value), int(max_value)
        except Exception as exc:
            raise exc

    def process_group(
        self, group_spec_node: UVLPythonParser.GroupSpecContext
    ) -> list[Feature]:
        list_features = []
        for feature_context in group_spec_node.feature():
            feature_name = feature_context.reference().getText().replace('"', '')
            feature = Feature(feature_name, [])
            self.process_feature(feature, feature_context)
            list_features.append(feature)
        return list_features

    def process_constraints(
        self, constraint_node: UVLPythonParser.ConstraintContext
    ) -> Node:
        process = None
        if isinstance(constraint_node, UVLPythonParser.EquationConstraintContext):
            process = self.process_equation_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.LiteralConstraintContext):
            process = self.process_literal_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.ParenthesisConstraintContext):
            process = self.process_parenthesis_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.NotConstraintContext):
            process = self.process_not_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.AndConstraintContext):
            process = self.process_and_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.OrConstraintContext):
            process = self.process_or_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.ImplicationConstraintContext):
            process = self.process_implication_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.EquivalenceConstraintContext):
            process = self.process_equivalence_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.LiteralExpressionContext):
            process = self.process_literal_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.IntegerLiteralExpressionContext):
            process = self.process_integer_literal_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.FloatLiteralExpressionContext):
            process = self.process_float_literal_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.StringLiteralExpressionContext):
            process = self.process_string_literal_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.BracketExpressionContext):
            process = self.process_bracket_expression_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.AddExpressionContext):
            process = self.process_add_expression_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.SubExpressionContext):
            process = self.process_sub_expression_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.DivExpressionContext):
            process = self.process_div_expression_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.MulExpressionContext):
            process = self.process_mul_expression_constraint(constraint_node)
        elif isinstance(constraint_node, UVLPythonParser.AggregateFunctionExpressionContext):
            process = self.process_aggregate_function_constraint(constraint_node)
        else:
            # Handle unexpected constraint types
            raise NotImplementedError(
                f"Constraint of type {type(constraint_node)} not handled"
            )

        return process

    def process_aggregate_function_constraint(
        self, aggregate_function_context: UVLPythonParser.AggregateFunctionExpressionContext
    ) -> Node:
        """Process an aggregate function constraint."""
        aggregate_function = aggregate_function_context.aggregateFunction()
        if isinstance(aggregate_function, UVLPythonParser.StringAggregateFunctionExpressionContext):
            string_function = aggregate_function.stringAggregateFunction()
            if isinstance(string_function, UVLPythonParser.LengthAggregateFunctionContext):
                literal = string_function.reference()
                return Node(ASTOperation.LEN, Node(literal.getText().replace('"', '')))
            else:
                raise NotImplementedError(f"String function {type(string_function)} not handled.")
        elif isinstance(aggregate_function, UVLPythonParser.NumericAggregateFunctionExpressionContext):
            numeric_function = aggregate_function.numericAggregateFunction()
            if isinstance(numeric_function, UVLPythonParser.FloorAggregateFunctionContext):
                literal = numeric_function.reference()
                return Node(ASTOperation.FLOOR, Node(literal.getText().replace('"', '')))
            elif isinstance(numeric_function, UVLPythonParser.CeilAggregateFunctionContext):
                literal = numeric_function.reference()
                return Node(ASTOperation.CEIL, Node(literal.getText().replace('"', '')))
            else:
                raise NotImplementedError(f"Numeric function {type(numeric_function)} not handled.")
        elif isinstance(aggregate_function, UVLPythonParser.AvgAggregateFunctionContext):
            literals = aggregate_function.reference()
            elements = [Node(literal.getText().replace('"', '')) for literal in literals]
            return functools.reduce(lambda node, left: AST.create_binary_operation(ASTOperation.AVG, 
                                                                     Node(left), node).root, elements)
        elif isinstance(aggregate_function, UVLPythonParser.SumAggregateFunctionContext):
            literals = aggregate_function.reference()
            elements = [Node(literal.getText().replace('"', '')) for literal in literals]
            return functools.reduce(lambda node, left: AST.create_binary_operation(ASTOperation.SUM, 
                                                                     Node(left), node).root, elements)
        else:
            raise NotImplementedError(f"Aggregate function {type(aggregate_function)} not handled.")
        return None

    def process_equation_constraint(
        self, equation_context: UVLPythonParser.EquationConstraintContext
    ) -> Node:
        """Process an equation constraint."""
        equation = equation_context.equation()
        operator = None
        if isinstance(equation, UVLPythonParser.EqualEquationContext):
            operator = ASTOperation.EQUALS
        elif isinstance(equation, UVLPythonParser.LowerEquationContext):
            operator = ASTOperation.LOWER
        elif isinstance(equation, UVLPythonParser.LowerEqualsEquationContext):
            operator = ASTOperation.LOWER_EQUALS
        elif isinstance(equation, UVLPythonParser.GreaterEquationContext):
            operator = ASTOperation.GREATER
        elif isinstance(equation, UVLPythonParser.GreaterEqualsEquationContext):
            operator = ASTOperation.GREATER_EQUALS
        elif isinstance(equation, UVLPythonParser.NotEqualsEquationContext):
            operator = ASTOperation.NOT_EQUALS
        else:
            raise NotImplementedError(f"Expression of type {type(equation)} not handled.")
        left_constraint = equation.expression(0)
        right_constraint = equation.expression(1)
        return Node(
            operator,
            self.process_constraints(left_constraint),
            self.process_constraints(right_constraint),
        )

    def process_integer_literal_constraint(
        self, literal_context: UVLPythonParser.IntegerLiteralExpressionContext
    ) -> Node:
        """Process an integer literal expression."""
        return Node(int(literal_context.getText()))

    def process_float_literal_constraint(
        self, literal_context: UVLPythonParser.IntegerLiteralExpressionContext
    ) -> Node:
        """Process a float literal expression."""
        return Node(float(literal_context.getText()))

    def process_string_literal_constraint(
        self, literal_context: UVLPythonParser.IntegerLiteralExpressionContext
    ) -> Node:
        """Process a string literal expression."""
        return Node(literal_context.getText())

    def process_add_expression_constraint(
        self, context: UVLPythonParser.AddExpressionContext
    ) -> Node:
        """Process an add expression."""
        left_constraint = context.expression(0)
        right_constraint = context.expression(1)
        return Node(
            ASTOperation.ADD,
            self.process_constraints(left_constraint),
            self.process_constraints(right_constraint),
        )
    
    def process_sub_expression_constraint(
        self, context: UVLPythonParser.AddExpressionContext
    ) -> Node:
        """Process a sub expression."""
        left_constraint = context.expression(0)
        right_constraint = context.expression(1)
        return Node(
            ASTOperation.SUB,
            self.process_constraints(left_constraint),
            self.process_constraints(right_constraint),
        )
    
    def process_div_expression_constraint(
        self, context: UVLPythonParser.AddExpressionContext
    ) -> Node:
        """Process a div expression."""
        left_constraint = context.expression(0)
        right_constraint = context.expression(1)
        return Node(
            ASTOperation.DIV,
            self.process_constraints(left_constraint),
            self.process_constraints(right_constraint),
        )
    
    def process_mul_expression_constraint(
        self, context: UVLPythonParser.AddExpressionContext
    ) -> Node:
        """Process a mul expression."""
        left_constraint = context.expression(0)
        right_constraint = context.expression(1)
        return Node(
            ASTOperation.MUL,
            self.process_constraints(left_constraint),
            self.process_constraints(right_constraint),
        )

    def process_literal_constraint(
        self, literal_context: UVLPythonParser.LiteralConstraintContext
    ) -> Node:
        """Process a literal constraint."""
        literal = literal_context.reference()
        return Node(literal.getText().replace('"', ''))

    def process_parenthesis_constraint(
        self, parenthesis_context: UVLPythonParser.ParenthesisConstraintContext
    ) -> Node:
        """Process a parenthesis constraint."""
        inner_constraint = parenthesis_context.constraint()
        return self.process_constraints(inner_constraint)
    
    def process_bracket_expression_constraint(
        self, bracket_context: UVLPythonParser.BracketExpressionContext
    ) -> Node:
        """Process a bracket (parenthesis) expression constraint."""
        inner_constraint = bracket_context.expression()
        return self.process_constraints(inner_constraint)

    def process_not_constraint(
        self, not_context: UVLPythonParser.NotConstraintContext
    ) -> Node:
        """Process a not constraint."""
        inner_constraint = not_context.constraint()
        return Node(ASTOperation.NOT, self.process_constraints(inner_constraint))

    def process_and_constraint(
        self, and_context: UVLPythonParser.AndConstraintContext
    ) -> Node:
        """Process an and constraint."""
        left_constraint = and_context.constraint(0)
        right_constraint = and_context.constraint(1)
        return Node(
            ASTOperation.AND,
            self.process_constraints(left_constraint),
            self.process_constraints(right_constraint),
        )

    def process_or_constraint(
        self, or_context: UVLPythonParser.OrConstraintContext
    ) -> Node:
        """Process an or constraint."""
        left_constraint = or_context.constraint(0)
        right_constraint = or_context.constraint(1)
        return Node(
            ASTOperation.OR,
            self.process_constraints(left_constraint),
            self.process_constraints(right_constraint),
        )

    def process_implication_constraint(
        self, implication_context: UVLPythonParser.ImplicationConstraintContext
    ) -> Node:
        """Process an implication constraint."""
        left_constraint = implication_context.constraint(0)
        right_constraint = implication_context.constraint(1)
        return Node(
            ASTOperation.IMPLIES,
            self.process_constraints(left_constraint),
            self.process_constraints(right_constraint),
        )

    def process_equivalence_constraint(
        self, equivalence_context: UVLPythonParser.EquivalenceConstraintContext
    ) -> Node:
        """Process an equivalence constraint."""
        left_constraint = equivalence_context.constraint(0)
        right_constraint = equivalence_context.constraint(1)
        return Node(
            ASTOperation.EQUIVALENCE,
            self.process_constraints(left_constraint),
            self.process_constraints(right_constraint),
        )

    def process_includes(
        self, includes_node: UVLPythonParser.IncludesContext
    ) -> list[str]:
        include_lines = includes_node.includeLine()

        # This will hold the processed includes
        includes_list = []

        for include_line in include_lines:
            language_level_node = include_line.languageLevel()
            includes_list.append(self.process_language_level(language_level_node))

        return includes_list

    def process_language_level(
        self, language_level_node: UVLPythonParser.LanguageLevelContext
    ) -> str:
        major_level = language_level_node.majorLevel().getText()

        # Check if there's a minor level or a wildcard
        if language_level_node.minorLevel():
            minor_level = language_level_node.minorLevel().getText()
            return f"{major_level}.{minor_level}"

        if (
            language_level_node.getChildCount() > 1
            and language_level_node.getChild(1).getText() == "*"
        ):
            return f"{major_level}.*"

        return major_level

    def process_namespace(
        self, namespace_node: UVLPythonParser.NamespaceContext
    ) -> str:
        return namespace_node.reference().getText()

    def process_imports(
        self, imports_node: UVLPythonParser.ImportsContext
    ) -> list[tuple[str, Optional[str]]]:
        import_lines = imports_node.importLine()

        # This will hold the processed imports
        imports_list = []

        for import_line in import_lines:
            namespace = import_line.ns.getText()

            # Check if there's an alias
            alias = import_line.alias.getText() if import_line.alias else None

            imports_list.append((namespace, alias))

        return imports_list

    def transform(self) -> FeatureModel:
        self.set_parse_tree()

        # Processing the namespace
        namespace_node = self.parse_tree.namespace()
        if namespace_node:
            namespace_value = self.process_namespace(namespace_node)
            logging.warning(
                "Namespaces are not meningful for Flama."
                "This model has the following namespaces: %s ",
                namespace_value,
            )

        # Processing the imports
        imports_node = self.parse_tree.imports()
        if imports_node:
            imports_list = self.process_imports(imports_node)
            logging.warning(
                "Imports are not yet supported in flama."
                "This model has the following imports: %s",
                imports_list,
            )

        includes_node = self.parse_tree.includes()
        if includes_node:
            includes_list = self.process_includes(includes_node)
            logging.warning(
                "Includes are not yet supported in flama."
                "This model has the following imports: %s",
                includes_list,
            )

        # Find ParseTree node of root feature
        root_feature_ast = self.parse_tree.features().feature()
        # Get the root and process it
        feature_text = root_feature_ast.reference().getText().replace('"', '')
        feature = Feature(feature_text, [])
        root = self.process_feature(feature, root_feature_ast)

        feature_model = FeatureModel(root, [])

        if self.parse_tree.constraints():  # Check if constraints exist
            contraint_counter = 0
            for constraint_line in self.parse_tree.constraints().constraintLine():
                node = self.process_constraints(constraint_line.constraint())
                feature_model.ctcs.append(
                    Constraint(
                        name="Constraint " + str(contraint_counter), ast=AST(node)
                    )
                )
                contraint_counter = contraint_counter + 1
        self.model = feature_model
        return self.model
