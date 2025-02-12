from typing import Any

from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature, Relation
from flamapy.metamodels.fm_metamodel.transformations.refactorings import (
    FMRefactoring,
    RefactoringException
)


class MutexGroupRefactoring(FMRefactoring):
    """It changes the mutex group to an and-group with one optional abstract sub-feature f which
    becomes an alternative-group with the original sub-features."""

    def get_name(self) -> str:
        return 'Mutex group refactoring'

    def get_instances(self) -> list[Feature]:
        return [feat for feat in self.feature_model.get_features() if feat.is_mutex_group()]

    def is_applicable(self) -> bool:
        return any(feat.is_mutex_group() for feat in self.feature_model.get_features())

    def apply(self, instance: Any) -> FeatureModel:
        if instance is None:
            raise RefactoringException(f'Invalid instance for {self.get_name()}.')
        if not isinstance(instance, Feature):
            raise RefactoringException(f'Invalid instance for {self.get_name()}.'
                                       f'Expected Feature, got {type(instance)} for {instance}.')
        if not instance.is_mutex_group():
            raise RefactoringException(f'Feature {instance.name} is not a mutex group.')

        new_name = FMRefactoring.get_new_feature_name(self.feature_model, instance.name)
        parent = Feature(name=new_name, parent=instance, is_abstract=True)
        r_opt = Relation(instance, [parent], 0, 1)  # optional
        r_mutex = next((relation for relation in instance.get_relations() if relation.is_mutex()),
                       None)
        r_mutex.parent = parent
        r_mutex.card_min = 1  # xor

        for child in r_mutex.children:
            child.parent = parent
        instance.get_relations().remove(r_mutex)

        # Add relations to features
        instance.add_relation(r_opt)
        parent.add_relation(r_mutex)
        return self.feature_model
