import copy
import functools
from dataclasses import dataclass
from typing import Any

from flamapy.core.models.ast import AST, ASTOperation, Node
from flamapy.metamodels.fm_metamodel.models import (
    FeatureModel,
    Feature,
    Relation,
    Constraint,
    Cardinality
)
from flamapy.metamodels.fm_metamodel.transformations.refactorings import (
    FMRefactoring,
    RefactoringException
)


@dataclass
class ContextualConstraintContext:
    feature_model: FeatureModel
    feature_clone_i: Feature
    features_names_map: dict[str, str]
    ctcs_to_be_removed: set[Constraint]
    ctcs_to_be_added: list[Constraint]
    ctcs_for_all_clones: set[Constraint]


class FeatureCardinalityRefactoring(FMRefactoring):
    """It changes the feature cardinality [a..b] by cloning the subtree within a group cardinality
     that ensure that [a..b] of those subtrees must be selected.

    It also create contextual clone constraints according to the semantics of UVL specified
    in [Benavides et al. 2025 - UVL: Feature modelling with the Universal Variability Language]
    (https://doi.org/10.1016/j.jss.2024.112326).
    """

    def get_name(self) -> str:
        return 'Feature cardinality refactoring'

    def get_instances(self) -> list[Feature]:
        if self.feature_model is None:
            return []
        return [feat for feat in self.feature_model.get_features() if feat.is_multifeature()]

    def is_applicable(self) -> bool:
        if self.feature_model is None:
            return False
        return any(feat.is_multifeature() for feat in self.feature_model.get_features())

    def apply(self, instance: Any) -> FeatureModel | None:
        if self.feature_model is None:
            raise RefactoringException('Feature model is None.')
        if instance is None:
            raise RefactoringException(f'Invalid instance for {self.get_name()}.')
        if not isinstance(instance, Feature):
            raise RefactoringException(f'Invalid instance for {self.get_name()}.'
                                       f'Expected Feature, got {type(instance)} for {instance}.')
        if not instance.is_multifeature():
            raise RefactoringException(f'Feature {instance.name} is not a feature cardinality.')

        # Get feature names (to check for duplicates when creating clones)
        #feature_names = {feature.name for feature in self.feature_model.get_features()}
        # Get cardinalities
        card_min = instance.feature_cardinality.min
        card_max = instance.feature_cardinality.max
        # The feature cardinality is not any more a multifeature
        instance.feature_cardinality = Cardinality(1, 1)

        # Number of clones
        n_clones = get_number_of_clones(card_min, card_max)

        # Create the root clones
        clones = []
        clones_features_names_map = {}
        constraints_to_be_removed: set[Constraint] = set()
        constraints_to_be_added: list[Constraint] = []
        constraints_for_all_clones: set[Constraint] = set()
        for clone_i in range(1, n_clones + 1):
            name = FMRefactoring.get_new_feature_name(self.feature_model,
                                                      f'{instance.name}_{clone_i}')
            feature_clone_i = copy.deepcopy(instance)
            feature_clone_i.name = name
            feature_clone_i.parent = instance.parent
            feature_clone_i.feature_cardinality = Cardinality(1, 1)
            clones.append(feature_clone_i)

            # Change names of subfeatures
            features_names_map = rename_features(self.feature_model, feature_clone_i, clone_i)
            clones_features_names_map[feature_clone_i.name] = features_names_map
            # Create contextual clone constraints
            context = ContextualConstraintContext(feature_model=self.feature_model,
                                                  feature_clone_i=feature_clone_i,
                                                  features_names_map=features_names_map,
                                                  ctcs_to_be_removed=constraints_to_be_removed,
                                                  ctcs_to_be_added=constraints_to_be_added,
                                                  ctcs_for_all_clones=constraints_for_all_clones
                                                  )
            create_contextual_constraint(context)
        # Remove the original constraints
        remove_constraints(self.feature_model, constraints_to_be_removed)
        # Add the new constraints
        add_constraints(self.feature_model, constraints_to_be_added)
        # Contextualize constraints for all clones
        for ctc in constraints_for_all_clones:
            for feature_name in ctc.get_features():
                if any(feature_name in names_map
                       for names_map in clones_features_names_map.values()):
                    # Contextualize constraint for the clone
                    or_ctc = create_or_constraint_for_clones(feature_name,
                                                             clones_features_names_map)
                    new_ast = replace_feature_by_ctc(ctc.ast, feature_name, or_ctc)
                    new_ctc = Constraint(FMRefactoring.get_new_constraint_name(self.feature_model,
                                                                               ctc.name),
                                         new_ast)
            self.feature_model.ctcs.append(new_ctc)
        # Create the cardinality group relationship
        cg_relation = Relation(instance, clones, card_min, card_max)
        instance.relations = [cg_relation]

        return self.feature_model


def get_number_of_clones(card_min: int, card_max: int) -> int:
    """Get the number of clones to be created based on the cardinality.
    If the cardinality is unbounded, we set it to the minimum cardinality."""
    return card_min if card_max == -1 else card_max


def remove_constraints(feature_model: FeatureModel,
                       constraints_to_be_removed: set[Constraint]) -> None:
    for constraint in constraints_to_be_removed:
        feature_model.ctcs.remove(constraint)


def add_constraints(feature_model: FeatureModel,
                    constraints_to_be_added: list[Constraint]) -> None:
    for constraint in constraints_to_be_added:
        feature_model.ctcs.append(constraint)


def create_contextual_constraint(context: ContextualConstraintContext) -> None:
    """Create contextual constraints for the given feature clone."""
    for constraint in context.feature_model.get_constraints():
        features_in_constraint = constraint.get_features()
        if all(feat in context.features_names_map for feat in features_in_constraint):
            # Contextualize constraint for the clone
            context.ctcs_to_be_removed.add(constraint)
            new_constraint = contextualize_constraint(context.feature_model,
                                                      constraint,
                                                      context.feature_clone_i,
                                                      context.features_names_map)
            context.ctcs_to_be_added.append(new_constraint)
        elif any(feat in context.features_names_map for feat in features_in_constraint):
            # Contextualize constraint for the clone and mark it for all clones
            context.ctcs_to_be_removed.add(constraint)
            context.ctcs_for_all_clones.add(constraint)
            new_constraint = contextualize_constraint(context.feature_model,
                                                      constraint,
                                                      context.feature_clone_i,
                                                      context.features_names_map)
            context.ctcs_to_be_added.append(new_constraint)

def contextualize_constraint(feature_model: FeatureModel,
                             constraint: Constraint,
                             feature_clone_i: Feature,
                             features_names_map: dict[str, str]) -> Constraint:
    """Create a contextualized constraint for the given constraints according to the provided
    feature clone."""
    # Create a copy of the constraint
    new_constraint = copy.deepcopy(constraint)
    # Rename the constraint's name
    name_ctc = FMRefactoring.get_new_constraint_name(feature_model, new_constraint.name)
    new_constraint.name = name_ctc
    # Update the AST with the new names of features clones
    new_constraint.ast = rename_ast(new_constraint.ast, features_names_map)
    # Add context to the constraint
    new_constraint.ast = AST.create_binary_operation(ASTOperation.IMPLIES,
                                                     Node(feature_clone_i.name),
                                                     new_constraint.ast.root)
    return new_constraint


def rename_features(feature_model: FeatureModel,
                    root_feature: Feature,
                    clone_i: int) -> dict[str, str]:
    """Rename the features of the subtree of the given feature."""
    features_map = {}
    features = root_feature.get_children()
    while features:
        child = features.pop()
        old_name = child.name
        new_name = f'{root_feature.name}_{old_name}'
        child.name = FMRefactoring.get_new_feature_name(feature_model, new_name)
        features_map[old_name] = child.name
        features.extend(child.get_children())
    return features_map


def create_or_constraint_for_clones(feature_name: str,
                                    clones_features_names_map: dict[str, dict[str, str]]
                                    ) -> Node:
    """Create an OR constraint for the clones of the given feature."""
    elements = [Node(names_map[feature_name]) for names_map in clones_features_names_map.values()]
    result = functools.reduce(lambda left, right:
                              AST.create_binary_operation(ASTOperation.OR, left, right).root,
                              elements)
    return result


def replace_feature_by_ctc(ast: AST, feature_name: str, or_ctc: Node) -> AST:
    """Replace the feature by the contextualized constraint."""
    stack = [ast.root]
    while stack:
        node = stack.pop()
        if node.is_unique_term() and node.data == feature_name:
            node.data = or_ctc.data
            node.left = or_ctc.left
            node.right = or_ctc.right
        elif node.is_unary_op():
            stack.append(node.left)
        elif node.is_binary_op():
            stack.append(node.right)
            stack.append(node.left)
    return ast


def rename_ast(ast: AST,
               mapping_names: dict[str, str]) -> AST:
    stack = [ast.root]
    while stack:
        node = stack.pop()
        if node.is_unique_term():
            node.data = mapping_names.get(node.data, node.data)
        elif node.is_unary_op():
            stack.append(node.left)
        elif node.is_binary_op():
            stack.append(node.right)
            stack.append(node.left)
    return ast
