from typing import Any

from flamapy.metamodels.fm_metamodel.models import FeatureModel, Constraint
from flamapy.metamodels.fm_metamodel.models.feature_model import split_constraint
from flamapy.metamodels.fm_metamodel.transformations.refactorings import (
    FMRefactoring,
    RefactoringException
)


class PseudoComplexConstraintRefactoring(FMRefactoring):
    """It splits a pseudo-complex constraint in multiple constraints dividing it by the AND
    operator when possible."""

    def get_name(self) -> str:
        return 'Pseudo-complex constraint refactoring'

    def get_instances(self) -> list[Constraint]:
        if self.feature_model is None:
            return []
        return self.feature_model.get_pseudocomplex_constraints()

    def is_applicable(self) -> bool:
        if self.feature_model is None:
            return False
        return any(ctc.is_pseudocomplex_constraint()
                   for ctc in self.feature_model.get_constraints())

    def apply(self, instance: Any) -> FeatureModel | None:
        if self.feature_model is None:
            raise RefactoringException('Feature model is None.')
        if instance is None:
            raise RefactoringException(f'Invalid instance for {self.get_name()}.')
        if not isinstance(instance, Constraint):
            raise RefactoringException(f'Invalid instance for {self.get_name()}.'
                                       f'Expected Constraint, '
                                       f'got {type(instance)} for {instance}.')
        if not instance.is_pseudocomplex_constraint():
            raise RefactoringException(f'Constraint {instance.name} is not pseudo-complex.')

        self.feature_model.ctcs.remove(instance)
        for ctc in split_constraint(instance):
            self.feature_model.ctcs.append(ctc)
        return self.feature_model
