import copy
from dataclasses import dataclass
from typing import Any, cast

from flamapy.core.models.variability_model import VariabilityModel
from flamapy.core.models.ast import AST
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
class CloneContext:
    feature_model: FeatureModel
    original_feature: Feature
    clone_feature: Feature
    clone_index: int
    clones_features_names_map: dict[str, dict[str, str]]
    constraints_to_be_removed: set[Constraint]
    constraints_to_be_added: list[Constraint]


class FeatureCardinalityRefactoring(FMRefactoring):
    """It changes the feature cardinality [a..b] by cloning the subtree within an alternative
    group for each possible value of the cardinality and that number of subtrees for each child.

    This refactoring preserves the semantics of the feature model, i.e., it does not change
    the set of products that can be derived from the feature model.

    This is an alternative refactoring to the one proposed in
    [Benavides et al. 2025 - UVL: Feature modelling with the Universal Variability Language]
    (https://doi.org/10.1016/j.jss.2024.112326) which does not preserve the semantics.
    """

    def __init__(self, source_model: VariabilityModel) -> None:
        self._feature_model: FeatureModel = cast(FeatureModel, source_model)
        # A mapping of feature names to their clones' names
        # Original features -> clones' names -> subtree features' names
        self.mapping_names: dict[str, dict[str, dict[str, str]]] = {}

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

        # Get cardinalities
        card_min = instance.feature_cardinality.min
        card_max = instance.feature_cardinality.max
        # The feature cardinality is not any more a multifeature
        instance.feature_cardinality = Cardinality(1, 1)

        # Number of clones
        n_clones = get_number_of_clones(card_min, card_max)

        clones_features_names_map : dict[str, dict[str, str]] = {}
        constraints_to_be_removed: set[Constraint] = set()
        constraints_to_be_added: list[Constraint] = []
        possible_instances: list[Feature] = []

        for clone_i in range(card_min, n_clones + 1):
            # Create an abstract feature for each number of instances
            name = FMRefactoring.get_new_feature_name(self.feature_model,
                                                      f'{instance.name}_n{clone_i}')
            new_feature = Feature(name,
                                  parent=instance,
                                  relations=[],
                                  feature_cardinality=Cardinality(1, 1),
                                  is_abstract=True)
            possible_instances.append(new_feature)

            context = CloneContext(feature_model=self.feature_model,
                                   original_feature=instance,
                                   clone_feature=new_feature,
                                   clone_index=clone_i,
                                   clones_features_names_map=clones_features_names_map,
                                   constraints_to_be_removed=constraints_to_be_removed,
                                   constraints_to_be_added=constraints_to_be_added
                                   )
            create_clone_subtree(context)
        # Store the mapping names
        self.mapping_names[instance.name] = clones_features_names_map
        # The original feature cardinality becomes abstract
        instance.is_abstract = True
        # Create the alternative group relationship
        xor_relation = Relation(instance, possible_instances, 1, 1)
        instance.relations = [xor_relation]
        # Remove the original constraints
        remove_constraints(self.feature_model, constraints_to_be_removed)
        # Add the new constraints
        add_constraints(self.feature_model, constraints_to_be_added)
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


def create_clone_subtree(context: CloneContext) -> None:
    """Create the clone of the subtree."""
    for child_clone in range(1, context.clone_index + 1):
        # Create the clone of the subtree
        clone = copy.deepcopy(context.original_feature)
        clone_name = f'{context.clone_feature.name}_{context.original_feature.name}_{child_clone}'
        clone.name = FMRefactoring.get_new_feature_name(context.feature_model, clone_name)
        clone.parent = context.clone_feature
        # Add mandatory relation to the clone
        mandatory_relation = Relation(context.clone_feature, [clone], 1, 1)
        context.clone_feature.relations.append(mandatory_relation)
        # Rename the features in the subtree
        features_names_map = rename_features(context.feature_model, clone)
        context.clones_features_names_map[clone.name] = features_names_map

        # Contextualize constraints for this clone
        for constraint in context.feature_model.get_constraints():
            features_in_constraint = constraint.get_features()
            if any(feat in features_names_map for feat in features_in_constraint):
                new_ctc = contextualize_constraint(context.feature_model,
                                                   constraint,
                                                   features_names_map)
                context.constraints_to_be_removed.add(constraint)
                context.constraints_to_be_added.append(new_ctc)


def contextualize_constraint(feature_model: FeatureModel,
                             constraint: Constraint,
                             features_names_map: dict[str, str]) -> Constraint:
    """Create a contextualized constraint for the given constraints according to the provided
    feature clone."""
    print(f'Contextualizing constraint {constraint.name} for features {features_names_map}')
    # Create a copy of the constraint
    new_constraint = copy.deepcopy(constraint)
    # Rename the constraint's name
    name_ctc = FMRefactoring.get_new_constraint_name(feature_model, new_constraint.name)
    new_constraint.name = name_ctc
    # Update the AST with the new names of features clones
    new_constraint.ast = rename_ast(new_constraint.ast, features_names_map)
    return new_constraint


def rename_features(feature_model: FeatureModel,
                    root_feature: Feature) -> dict[str, str]:
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
