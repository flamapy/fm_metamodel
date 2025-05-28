import copy
import functools
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


class FeatureCardinalityRefactoring(FMRefactoring):
    """It changes the feature cardinality [a..b] by cloning the subtree within an alternative
    group for each possible value of the cardinality and that number of subtrees for each child.

    This refactoring preserves the semantics of the feature model, i.e., it does not change
    the set of products that can be derived from the feature model.

    This is an alternative refactoring to the one proposed in 
    [Benavides et al. 2025 - UVL: Feature modelling with the Universal Variability Language]
    (https://doi.org/10.1016/j.jss.2024.112326) which does not preserve the semantics.
    """

    def get_name(self) -> str:
        return 'Feature cardinality refactoring'

    def get_instances(self) -> list[Feature]:
        return [feat for feat in self.feature_model.get_features() if feat.is_multifeature()]

    def is_applicable(self) -> bool:
        return any(feat.is_multifeature() for feat in self.feature_model.get_features())

    def apply(self, instance: Any) -> FeatureModel:
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
        if card_max == -1:
            n_clones = card_min  # NOTE: Unbounded cardinality, we set it to the minimum
        else:
            n_clones = card_max
        
        clones_features_names_map = {}
        constraints_to_be_removed = set()
        constraints_to_be_added = []
        possible_instances = []
        
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

            for child_clone in range(1, clone_i + 1):
                # Create the clone of the subtree
                clone = copy.deepcopy(instance)
                clone_name = f'{name}_{instance.name}_{child_clone}'
                clone.name = FMRefactoring.get_new_feature_name(self.feature_model, clone_name)
                clone.parent = new_feature
                # Add mandatory relation to the clone
                mandatory_relation = Relation(new_feature, [clone], 1, 1)
                new_feature.relations.append(mandatory_relation)
                # Rename the features in the subtree
                features_names_map = rename_features(self.feature_model, clone)
                clones_features_names_map[clone.name] = features_names_map

                # Contextualize constraints for this clone
                for constraint in self.feature_model.get_constraints():
                    features_in_constraint = constraint.get_features()
                    if any(feat in features_names_map for feat in features_in_constraint):
                        new_ctc = contextualize_constraint(self.feature_model, 
                                                           constraint,
                                                           features_names_map)
                        constraints_to_be_removed.add(constraint)
                        constraints_to_be_added.append(new_ctc)
        # The original feature cardinality becomes abstract
        instance.is_abstract = True
        # Create the alternative group relationship
        xor_relation = Relation(instance, possible_instances, 1, 1)
        instance.relations = [xor_relation]
        # Remove the original constraints
        for constraint in constraints_to_be_removed:
            self.feature_model.ctcs.remove(constraint)
        # Add the new constraints
        for constraint in constraints_to_be_added:
            self.feature_model.ctcs.append(constraint)
        return self.feature_model
    

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