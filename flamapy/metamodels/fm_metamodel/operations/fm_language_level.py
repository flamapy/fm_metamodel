from enum import Enum, auto
from dataclasses import dataclass, field
from typing import cast

from flamapy.core.models import VariabilityModel, ASTOperation
from flamapy.core.operations import Operation

from flamapy.metamodels.fm_metamodel.models import FeatureModel, Constraint


class MajorLevel(Enum):
    BOOLEAN = 1
    ARITHMETIC = 2
    TYPE = 3


class MinorLevel(Enum):
    STRING_CONSTRAINTS = auto()
    FEATURE_CARDINALITY = auto()
    AGGREGATE_FUNCTION = auto()
    GROUP_CARDINALITY = auto()


@dataclass(order=True, frozen=True)
class LanguageLevel:
    major: MajorLevel
    minors: set[MinorLevel] = field(default_factory=set)

    def __str__(self) -> str:
        minors_str = ', '.join(ext.name for ext in sorted(self.minors, key=lambda x: x.name))
        return f'Major: {self.major.name}, Minor: {{{minors_str}}}'


class FMLanguageLevel(Operation):
    """Operation to calculate the maximum language level of a feature model."""

    def __init__(self) -> None:
        self.result: LanguageLevel = LanguageLevel(MajorLevel.BOOLEAN, set())

    def get_result(self) -> LanguageLevel:
        return self.result

    def execute(self, model: VariabilityModel) -> 'FMLanguageLevel':
        fm_model = cast(FeatureModel, model)
        self.result = calculate_language_level(fm_model)
        return self

    def get_language_level(self) -> LanguageLevel:
        return self.get_result()


def calculate_language_level(feature_model: FeatureModel) -> LanguageLevel:
    major_level = get_major_level(feature_model)
    minor_levels = get_minor_levels(feature_model)
    return LanguageLevel(major_level, minor_levels)


def get_major_level(feature_model: FeatureModel) -> MajorLevel:
    major_level = MajorLevel.BOOLEAN
    if any(not feature.is_boolean() for feature in feature_model.get_features()):
        major_level = MajorLevel.TYPE
    elif any(ctc.is_arithmetic_constraint() for ctc in feature_model.get_constraints()):
        major_level = MajorLevel.ARITHMETIC
    return major_level


def get_minor_levels(feature_model: FeatureModel) -> set[MinorLevel]:
    minor_levels = set()
    if any(is_string_constraint(ctc, feature_model) for ctc in feature_model.get_constraints()):
        minor_levels.add(MinorLevel.STRING_CONSTRAINTS)
    if any(feature.is_multifeature() for feature in feature_model.get_features()):
        minor_levels.add(MinorLevel.FEATURE_CARDINALITY)
    if any(ctc.is_aggregation_constraint() for ctc in feature_model.get_constraints()):
        minor_levels.add(MinorLevel.AGGREGATE_FUNCTION)
    if any(feature.is_cardinality_group() for feature in feature_model.get_features()):
        minor_levels.add(MinorLevel.GROUP_CARDINALITY)
    return minor_levels


def is_string_constraint(constraint: Constraint, feature_model: FeatureModel) -> bool:
    """Return true if the constraint contains at least one string operator."""
    if any(op in [ASTOperation.LEN] for op in constraint.ast.get_operators()):
        return True
    if not constraint.is_arithmetic_constraint():
        return False
    features_in_ctc = [feature for feat in constraint.get_features() if
                       (feature := feature_model.get_feature_by_name(feat)) is not None]
    if any(feature.is_string() for feature in features_in_ctc):
        return True
    if any(isinstance(term, str) and term.startswith("'")
           for term in constraint.ast.get_operands()):
        return True
    return False
