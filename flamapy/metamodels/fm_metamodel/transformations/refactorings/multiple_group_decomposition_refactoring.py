from typing import Any

from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature, Relation
from flamapy.metamodels.fm_metamodel.transformations.refactorings import (
    FMRefactoring,
    RefactoringException
)


class MultipleGroupDecompositionRefactoring(FMRefactoring):
    """"It substitutes each group in the feature by a mandatory abstract feature which becomes a
    new group below the original feature."""

    def get_name(self) -> str:
        return 'Multiple group decomposition refactoring'

    def get_instances(self) -> list[Feature]:
        if self.feature_model is None:
            return []
        return [feat for feat in self.feature_model.get_features()
                if feat.is_multiple_group_decomposition()]

    def is_applicable(self) -> bool:
        if self.feature_model is None:
            return False
        return any(feat.is_multiple_group_decomposition()
                   for feat in self.feature_model.get_features())

    def apply(self, instance: Any) -> FeatureModel | None:
        if self.feature_model is None:
            raise RefactoringException('Feature model is None.')
        if instance is None:
            raise RefactoringException(f'Invalid instance for {self.get_name()}.')
        if not isinstance(instance, Feature):
            raise RefactoringException(f'Invalid instance for {self.get_name()}.'
                                       f'Expected Feature, got {type(instance)} for {instance}.')
        if not instance.is_multiple_group_decomposition():
            raise RefactoringException(f'Feature {instance.name} does not have multiple groups.')

        for relation in instance.get_relations():
            if relation.is_group():
                self.feature_model = new_decomposition(self.feature_model, instance, relation)
        return self.feature_model


def new_decomposition(fm_model: FeatureModel, feature: Feature, r_group: Relation) -> FeatureModel:
    new_name = FMRefactoring.get_new_feature_name(fm_model, feature.name)
    parent = Feature(name=new_name, parent=feature, is_abstract=True)
    r_mand = Relation(feature, [parent], 1, 1)  # mandatory
    feature.add_relation(r_mand)
    r_group.parent = parent
    for child in r_group.children:
        child.parent = parent
    # Add relations to features
    parent.add_relation(r_group)
    feature.get_relations().remove(r_group)
    return fm_model
