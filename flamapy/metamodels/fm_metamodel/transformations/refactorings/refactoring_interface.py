from typing import Any, cast
from abc import abstractmethod

from flamapy.core.models import VariabilityModel
from flamapy.core.transformations import ModelToModel
from flamapy.metamodels.fm_metamodel.models import FeatureModel


class FMRefactoring(ModelToModel):
    """Interface for refactoring operations on feature models.

    A refactoring is an operation that transforms a feature model into another feature model
    without modifying its semantics (i.e., maintaining the same products).
    Refactoring normally involves restructuring the feature model (e.g., by moving features around,
    adding or removing features and/or constraints, etc.).
    Thus, configurations of the feature model may change, but the set of products remains the same.
    This means that refactorings take into account the difference between abstract and concrete
    features.

    Note that refactorings are applied in-place, so that the given feature model is modified.
    You should create a copy of the feature model before applying the refactoring if you want to
    keep the original model .
    """

    @staticmethod
    def get_source_extension() -> str:
        return 'fm'

    @staticmethod
    def get_destination_extension() -> str:
        return 'fm'

    def __init__(self, source_model: VariabilityModel) -> None:
        self._feature_model: FeatureModel | None = cast(FeatureModel, source_model)

    @property
    def feature_model(self) -> FeatureModel | None:
        return self._feature_model

    @feature_model.setter
    def feature_model(self, new_feature_model: FeatureModel | None) -> None:
        self._feature_model = new_feature_model

    def transform(self) -> FeatureModel | None:
        """Apply the refactoring to all instance of the feature model."""
        instances = self.get_instances()
        while instances:
            instance = instances.pop(0)
            self.feature_model = self.apply(instance)
            instances = self.get_instances()
        return self.feature_model

    @abstractmethod
    def get_name(self) -> str:
        """Name of the refactoring."""

    @abstractmethod
    def is_applicable(self) -> bool:
        """Return whether the refactoring is applicable to the given model."""

    @abstractmethod
    def get_instances(self) -> list[Any]:
        """Return the instances of the refactoring that can be applied to the model."""

    @abstractmethod
    def apply(self, instance: Any) -> FeatureModel | None:
        """Apply the refactoring to the given instance."""

    @staticmethod
    def get_new_feature_name(feature_model: FeatureModel, name: str) -> str:
        count = 1
        new_name = f'{name}'
        while feature_model.get_feature_by_name(new_name) is not None:
            new_name = f'{name}_{count}'
            count += 1
        return new_name

    @staticmethod
    def get_new_constraint_name(feature_model: FeatureModel, name: str) -> str:
        ctcs_names = [ctc.name for ctc in feature_model.get_constraints()]
        count = 1
        new_name = f'{name}'
        while new_name in ctcs_names:
            new_name = f'{name}_{count}'
            count += 1
        return new_name
