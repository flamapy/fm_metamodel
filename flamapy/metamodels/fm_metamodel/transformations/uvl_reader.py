import os
import logging
from typing import Any, Optional, Union

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

    def syntaxError(  # noqa: PLR0913
        self,
        recognizer: Any,
        offendingSymbol: Any,
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
        self.imports: dict[str, FeatureModel] = {}  # namespace -> FeatureModel
        self.import_root: dict[str, str] = {}  # alias -> namespace
        self.constraints_attributes: dict[Feature, list[Constraint]] = {}
        self.constraint_counter: int = 0

    def set_parse_tree(self) -> None:
        absolute_path = os.path.abspath(os.path.join(self.path, self.file))
        input_stream = FileStream(absolute_path, encoding="utf-8")
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
        self, feature: Feature, attributes_node: UVLPythonParser.AttributeContext
    ) -> dict[Optional[Any], Optional[Any]]:
        attributes_list = attributes_node.attribute()
        attributes_dict = {}

        for attribute_context in attributes_list:
            # First, check which kind of attribute we're dealing with
            value_attribute = attribute_context.valueAttribute()
            constraint_attribute = attribute_context.constraintAttribute()

            key = None  # Ensure key is initialized
            value = None  # Ensure value is initialized

            if value_attribute:
                key = value_attribute.key().getText().replace('"', '')
                if value_attribute.value():
                    value = self.process_value(feature, value_attribute.value())
                else:
                    value = None  # or some default value
                attributes_dict[key] = value

            elif constraint_attribute:
                self.process_constraints_attributes(feature, constraint_attribute)
            else:
                # Handle unexpected case
                cleaned_text = attribute_context.getText().replace('"', '')

                # Raise the ValueError with the cleaned text
                raise ValueError(f"Unknown attribute type for: {cleaned_text}")
        return attributes_dict

    def process_value(self, feature: Feature, value_context: UVLPythonParser.ValueContext) -> Any:
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
            value = self.process_attributes(feature, value_context.attributes())
        elif value_context.vector():
            value = [self.process_value(feature, val) for val in value_context.vector().value()]
        return value

    def process_constraints_attributes(self,
                                       feature: Feature,
                                       cac: UVLPythonParser.ConstraintAttributeContext) -> None:
        """Process a constraint attribute."""
        if isinstance(cac, UVLPythonParser.SingleConstraintAttributeContext):
            node = self.process_constraints(cac.constraint())
            ctc = Constraint(name=f'Constraint {self.constraint_counter}',
                             ast=AST(node))
            self.constraints_attributes[feature].append(ctc)
            self.constraint_counter += 1
        elif isinstance(cac, UVLPythonParser.ListConstraintAttributeContext):
            for constraint_attribute in cac.constraintList().constraint():
                node = self.process_constraints(constraint_attribute)
                ctc = Constraint(name=f'Constraint {self.constraint_counter}',
                                 ast=AST(node))
                self.constraints_attributes[feature].append(ctc)
                self.constraint_counter += 1
        else:
            raise NotImplementedError(
                f"Constraint attribute of type {type(cac)} not handled."
            )

    def _check_feature_cardinality(
        self, feature: Feature, feature_node: UVLPythonParser.FeatureContext
    ) -> None:
        # See if there is a feature cardinality and attributes (TODO)
        if feature_node.featureCardinality():
            cardinality_text = feature_node.featureCardinality().CARDINALITY().getText()
            min_val, max_val = self.parse_cardinality(cardinality_text)
            feature.feature_cardinality = Cardinality(card_min=min_val, card_max=max_val)

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
                raise FlamaException('Error: unknow feature type for '
                                     f'{typed_text} of feature {feature.name}.')
            feature.feature_type = feature_type

    def _check_attributes(
        self, feature: Feature, feature_node: UVLPythonParser.FeatureContext
    ) -> None:
        self.constraints_attributes[feature] = []
        if feature_node.attributes():
            attributes = self.process_attributes(feature, feature_node.attributes())
            for key, value in attributes.items():
                if key == "abstract" and (value is None or value):
                    feature.is_abstract = True
                else:
                    feature.add_attribute(Attribute(name=str(key), default_value=value))
        feature.constraints_attributes = self.constraints_attributes[feature]


    def _process_imported_feature(self, feature: Feature) -> None:
        feature_reference = feature.name.split('.')
        alias_namespace = '.'.join(feature_reference[:-1])
        feature_reference_name = feature_reference[-1]
        if alias_namespace in self.import_root:
            namespace = self.import_root[alias_namespace]
            imported_fm = self.imports.get(namespace)
            if imported_fm is None:
                raise FlamaException(f'Imported model {namespace} not found.')
            if feature_reference_name == imported_fm.root.name:
                feature.reference = imported_fm.root
            else:
                raise FlamaException(f'Feature {feature_reference_name} not found in '
                                     f'imported model {namespace}.')

    def process_relationship_type(self,
                                  feature: Feature,
                                  feature_node: UVLPythonParser.FeatureContext) -> None:
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

    def process_feature(
        self, feature: Feature, feature_node: UVLPythonParser.FeatureContext
    ) -> Feature:
        self._process_imported_feature(feature)

        self._check_feature_cardinality(feature, feature_node)
        self._check_feature_type(feature, feature_node)
        self._check_attributes(feature, feature_node)

        # Get the relationship type
        self.process_relationship_type(feature, feature_node)
        return feature

    def parse_cardinality(self, cardinality_text: str) -> tuple[int, int]:
        # Extract the minimum and maximum values.
        # This assumes a format like "[min..max]" or "[min]" or "[min..*]"
        min_value: Union[int,str] = ""
        max_value: Union[int,str] = ""
        # Remove brackets.
        cardinality_text = cardinality_text[1:-1]

        if ".." in cardinality_text:
            parts = cardinality_text.split("..")
            min_value = parts[0]
            max_value = parts[1]
        else:
            min_value = cardinality_text
            max_value = min_value  # Assuming max is the same as min if not specified.
        if max_value == "*":
            max_value = -1
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
        process = self.process_literal_constraint(constraint_node)
        if process is None:
            process = self.process_logical_constraint(constraint_node)
        if process is None:
            process = self.process_arithmetic_constraint(constraint_node)
        if process is None:
            if isinstance(constraint_node, UVLPythonParser.EquationConstraintContext):
                process = self.process_equation_constraint(constraint_node)
            elif isinstance(constraint_node, UVLPythonParser.ParenthesisConstraintContext):
                process = self.process_parenthesis_constraint(constraint_node)
            elif isinstance(constraint_node, UVLPythonParser.BracketExpressionContext):
                process = self.process_bracket_expression_constraint(constraint_node)
            elif isinstance(constraint_node, UVLPythonParser.AggregateFunctionExpressionContext):
                process = self.process_aggregate_function_constraint(constraint_node)
        if process is None:  # Handle unexpected constraint types
            raise NotImplementedError(
                f"Constraint of type {type(constraint_node)} not handled"
            )
        return process

    def process_aggregate_function_constraint(
        self, aggregate_function_context: UVLPythonParser.AggregateFunctionExpressionContext
    ) -> Node:
        """Process an aggregate function constraint."""
        aggregation = aggregate_function_context.aggregateFunction()
        if isinstance(aggregation, UVLPythonParser.StringAggregateFunctionExpressionContext):
            string_function = aggregation.stringAggregateFunction()
            if isinstance(string_function, UVLPythonParser.LengthAggregateFunctionContext):
                literal = string_function.reference()
                return Node(ASTOperation.LEN, Node(literal.getText().replace('"', '')))
            raise NotImplementedError(f"String function {type(string_function)} not handled.")
        if isinstance(aggregation, UVLPythonParser.NumericAggregateFunctionExpressionContext):
            numeric_function = aggregation.numericAggregateFunction()
            if isinstance(numeric_function, UVLPythonParser.FloorAggregateFunctionContext):
                literal = numeric_function.reference()
                return Node(ASTOperation.FLOOR, Node(literal.getText().replace('"', '')))
            if isinstance(numeric_function, UVLPythonParser.CeilAggregateFunctionContext):
                literal = numeric_function.reference()
                return Node(ASTOperation.CEIL, Node(literal.getText().replace('"', '')))
            raise NotImplementedError(f"Numeric function {type(numeric_function)} not handled.")
        if isinstance(aggregation, UVLPythonParser.AvgAggregateFunctionContext):
            literals = aggregation.reference()
            attribute_literal = literals[0].getText().replace('"', '')
            if len(literals) > 1:
                feature_literal = literals[1].getText().replace('"', '')
                node = Node(ASTOperation.AVG, Node(attribute_literal), Node(feature_literal))
            else:
                node = Node(ASTOperation.AVG, Node(attribute_literal))
            return node
        if isinstance(aggregation, UVLPythonParser.SumAggregateFunctionContext):
            literals = aggregation.reference()
            attribute_literal = literals[0].getText().replace('"', '')
            if len(literals) > 1:
                feature_literal = literals[1].getText().replace('"', '')
                node = Node(ASTOperation.SUM, Node(attribute_literal), Node(feature_literal))
            else:
                node = Node(ASTOperation.SUM, Node(attribute_literal))
            return node
        raise NotImplementedError(f"Aggregate function {type(aggregation)} not handled.")

    def process_literal_constraint(self, ctc_node: UVLPythonParser.ConstraintContext) -> Node:
        """Process a literal constraint."""
        process = None
        if isinstance(ctc_node,
                      (UVLPythonParser.LiteralConstraintContext,
                       UVLPythonParser.LiteralExpressionContext)):
            process = self.process_literal(ctc_node)
        elif isinstance(ctc_node, UVLPythonParser.IntegerLiteralExpressionContext):
            process = self.process_integer_literal_constraint(ctc_node)
        elif isinstance(ctc_node, UVLPythonParser.FloatLiteralExpressionContext):
            process = self.process_float_literal_constraint(ctc_node)
        elif isinstance(ctc_node, UVLPythonParser.StringLiteralExpressionContext):
            process = self.process_string_literal_constraint(ctc_node)
        return process

    def process_logical_constraint(self, ctc_node: UVLPythonParser.ConstraintContext) -> Node:
        """Process a logical constraint."""
        process = None
        operator = None
        if isinstance(ctc_node, UVLPythonParser.NotConstraintContext):
            operator = ASTOperation.NOT
            process = self.process_unary_constraint(ctc_node, operator)
        elif isinstance(ctc_node, UVLPythonParser.AndConstraintContext):
            operator = ASTOperation.AND
        elif isinstance(ctc_node, UVLPythonParser.OrConstraintContext):
            operator = ASTOperation.OR
        elif isinstance(ctc_node, UVLPythonParser.ImplicationConstraintContext):
            operator = ASTOperation.IMPLIES
        elif isinstance(ctc_node, UVLPythonParser.EquivalenceConstraintContext):
            operator = ASTOperation.EQUIVALENCE
        if operator is None:  # It is other type of constraint
            return None
        if process is None:  # It is a binary constraint
            return self.process_binary_constraints(ctc_node, operator)
        return process

    def process_arithmetic_constraint(self, ctc_node: UVLPythonParser.ConstraintContext) -> Node:
        """Process an arithmetic constraint."""
        operator = None
        if isinstance(ctc_node, UVLPythonParser.AddExpressionContext):
            operator = ASTOperation.ADD
        elif isinstance(ctc_node, UVLPythonParser.SubExpressionContext):
            operator = ASTOperation.SUB
        elif isinstance(ctc_node, UVLPythonParser.DivExpressionContext):
            operator = ASTOperation.DIV
        elif isinstance(ctc_node, UVLPythonParser.MulExpressionContext):
            operator = ASTOperation.MUL
        if operator is None:  # It is other type of constraint
            return None
        return self.process_binary_expression(ctc_node, operator)

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
        return self.process_binary_expression(equation, operator)

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

    def process_binary_constraints(
        self, context: UVLPythonParser.ConstraintContext, operation: ASTOperation
    ) -> Node:
        """Process a binary constraint."""
        left_constraint = context.constraint(0)
        right_constraint = context.constraint(1)
        return Node(operation,
                    self.process_constraints(left_constraint),
                    self.process_constraints(right_constraint)
                    )

    def process_unary_constraint(
        self, context: UVLPythonParser.ConstraintContext, operation: ASTOperation
    ) -> Node:
        """Process a unary constraint."""
        inner_constraint = context.constraint()
        return Node(operation, self.process_constraints(inner_constraint))

    def process_binary_expression(self, context: Any, operation: ASTOperation) -> Node:
        """Process a binary expression."""
        left_constraint = context.expression(0)
        right_constraint = context.expression(1)
        return Node(operation,
                    self.process_constraints(left_constraint),
                    self.process_constraints(right_constraint)
                    )

    def process_literal(
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

    def read_submodels(self, imports_list: list[tuple[str, Optional[str]]]) -> None:
        for import_model in imports_list:
            namespace, alias = import_model
            if namespace not in self.imports:
                # If the import is not already processed, we can process it
                relative_path = namespace.replace('.', '/')
                import_path = os.path.join(self.path, f'{relative_path}.uvl')
                if os.path.exists(import_path):
                    imported_model = UVLReader(import_path).transform()
                    self.imports[namespace] = imported_model
                    if alias:
                        self.import_root[alias] = namespace
                    else:
                        self.import_root[namespace] = namespace
                else:
                    logging.warning(
                        "Import %s not found in path %s", namespace, self.path
                    )

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
            self.read_submodels(imports_list)
            # logging.warning(
            #     "Imports are not yet supported in flama."
            #     "This model has the following imports: %s",
            #     imports_list,
            # )
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
        feature_model.imports = self.imports
        feature_model.alias_namespace = self.import_root
        for ctcs in self.constraints_attributes.values():
            for ctc in ctcs:
                feature_model.ctcs.append(ctc)

        if self.parse_tree.constraints():  # Check if constraints exist
            contraint_counter = len(feature_model.ctcs)
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
