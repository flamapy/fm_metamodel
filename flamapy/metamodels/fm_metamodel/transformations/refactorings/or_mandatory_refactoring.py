from typing import Any

from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature, Relation
from flamapy.metamodels.fm_metamodel.transformations.refactorings import (
    FMRefactoring,
    RefactoringException
)


class OrMandatoryRefactoring(FMRefactoring):
    """It transforms the or-group with mandatory subfeatures into an and-group with mandatory
    and optional subfeatures."""

    def get_name(self) -> str:
        return 'Or mandatory refactoring'

    def get_instances(self) -> list[Feature]:
        if self.feature_model is None:
            return []
        return [feat for feat in self.feature_model.get_features()
                if is_or_group_with_mandatory(feat)]

    def is_applicable(self) -> bool:
        if self.feature_model is None:
            return False
        return any(is_or_group_with_mandatory(feat) for feat in self.feature_model.get_features())

    def apply(self, instance: Any) -> FeatureModel | None:
        if self.feature_model is None:
            raise RefactoringException('Feature model is None.')
        if instance is None:
            raise RefactoringException(f'Invalid instance for {self.get_name()}.')
        if not isinstance(instance, Feature):
            raise RefactoringException(f'Invalid instance for {self.get_name()}.'
                                       f'Expected Feature, got {type(instance)} for {instance}.')
        if not is_or_group_with_mandatory(instance):
            raise RefactoringException(f'Feature {instance.name} is not a or-group '
                                       f'with mandatory features.')

        or_group = next((relation for relation in instance.get_relations() if relation.is_or()),
                        None)
        if or_group is not None:
            for child in or_group.children:
                if not child.is_mandatory():
                    r_opt = Relation(instance, [child], 0, 1)  # optional
                    instance.add_relation(r_opt)
            instance.get_relations().remove(or_group)
        return self.feature_model


def is_or_group_with_mandatory(feature: Feature) -> bool:
    or_group = next((relation for relation in feature.get_relations() if relation.is_or()), None)
    return (or_group is not None and
            any(r.children[0] in or_group.children for r in feature.get_relations()
                if r.is_mandatory()))
