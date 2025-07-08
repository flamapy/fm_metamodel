from typing import Any

from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature
from flamapy.metamodels.fm_metamodel.transformations.refactorings import (
    FMRefactoring,
    RefactoringException
)


class XorMandatoryRefactoring(FMRefactoring):
    """It transforms the xor-group with a mandatory subfeature into an or-group with cardinality
    <0..0> (converting all non-mandatory subfeatures in dead features) and mantaining the mandatory
    subfeature."""

    def get_name(self) -> str:
        return 'Xor mandatory refactoring'

    def get_instances(self) -> list[Feature]:
        if self.feature_model is None:
            return []
        return [feat for feat in self.feature_model.get_features()
                if is_xor_group_with_mandatory(feat)]

    def is_applicable(self) -> bool:
        if self.feature_model is None:
            return False
        return any(is_xor_group_with_mandatory(feat) for feat in self.feature_model.get_features())

    def apply(self, instance: Any) -> FeatureModel | None:
        if instance is None:
            raise RefactoringException(f'Invalid instance for {self.get_name()}.')
        if not isinstance(instance, Feature):
            raise RefactoringException(f'Invalid instance for {self.get_name()}.'
                                       f'Expected Feature, got {type(instance)} for {instance}.')
        if not is_xor_group_with_mandatory(instance):
            raise RefactoringException(f'Feature {instance.name} is not a xor-group '
                                       f'with mandatory features.')

        xor_group = next((r for r in instance.get_relations() if r.is_alternative()), None)
        if xor_group is not None:
            mandatory_features = [c for c in xor_group.children if c.is_mandatory()]
            if mandatory_features:
                xor_group.card_min = 0
                xor_group.card_max = 0
                xor_group.children = [c for c in xor_group.children if c not in mandatory_features]
        return self.feature_model


def is_xor_group_with_mandatory(feature: Feature) -> bool:
    xor_group = next((relation for relation in feature.get_relations()
                      if relation.is_alternative()), None)
    return (xor_group is not None and
            any(r.children[0] in xor_group.children for r in feature.get_relations()
                if r.is_mandatory()))
