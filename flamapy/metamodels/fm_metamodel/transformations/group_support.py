"""Group decompositions that some output formats cannot represent.

FeatureIDE, Glencoe and Clafer give each feature a single decomposition: either one group or only
mandatory/optional children (FeatureIDE has, besides, no mutex or [m..n] group cardinalities).
Their writers do not restructure a model on their own, because that changes the results of
operations on the written model (new abstract features, core features, metrics, ...). By default
they reject such models; with `apply_refactorings=True` they apply the corresponding refactorings
from `transformations.refactorings` to a copy of the model and write that copy.
"""
import copy
from typing import Callable, Optional

from flamapy.core.exceptions import FlamaException
from flamapy.metamodels.fm_metamodel.models import Feature, FeatureModel
from flamapy.metamodels.fm_metamodel.transformations.refactorings import (
    CardinalityGroupRefactoring,
    MultipleGroupDecompositionRefactoring,
    MutexGroupRefactoring,
)
from flamapy.metamodels.fm_metamodel.transformations.refactorings.\
    multiple_group_decomposition_refactoring import needs_group_decomposition

# A check returns the reason why the format cannot represent the feature, or None.
FeatureCheck = Callable[[Feature], Optional[str]]
Refactoring = Callable[[FeatureModel], FeatureModel]
MAX_SHOWN = 5  # features listed in the error message


def mixed_decomposition(feature: Feature) -> Optional[str]:
    if needs_group_decomposition(feature):
        return ('several groups or a group with other relations: '
                'MultipleGroupDecompositionRefactoring')
    return None


def mutex_group(feature: Feature) -> Optional[str]:
    return 'mutex group: MutexGroupRefactoring' if feature.is_mutex_group() else None


def cardinality_group(feature: Feature) -> Optional[str]:
    return 'group cardinality: CardinalityGroupRefactoring' if feature.is_cardinality_group() \
        else None


def decompose_groups(model: FeatureModel) -> FeatureModel:
    return MultipleGroupDecompositionRefactoring(model).transform() or model


def refactor_mutex_groups(model: FeatureModel) -> FeatureModel:
    return MutexGroupRefactoring(model).transform() or model


def refactor_cardinality_groups(model: FeatureModel) -> FeatureModel:
    return CardinalityGroupRefactoring(model, clauses=True).transform() or model


def prepare_model(feature_model: FeatureModel, format_name: str, checks: list[FeatureCheck],
                  refactorings: list[Refactoring], apply_refactorings: bool) -> FeatureModel:
    """The model to write: the original one, or a refactored copy when `apply_refactorings`.

    Raises FlamaException naming the features the format cannot represent, and the refactoring
    that would make them representable.
    """
    model = feature_model
    if apply_refactorings:
        model = copy.deepcopy(feature_model)
        for refactoring in refactorings:
            model = refactoring(model)
    problems = []
    for feature in model.get_features():
        for check in checks:
            reason = check(feature)
            if reason is not None:
                problems.append(f'{feature.name} ({reason})')
    if problems:
        shown = '; '.join(problems[:MAX_SHOWN]) + (f'; ... ({len(problems)} in total)'
                                                    if len(problems) > MAX_SHOWN else '')
        raise FlamaException(
            f'{format_name} cannot represent these features without restructuring the model: '
            f'{shown}. Apply the refactorings named, or write with apply_refactorings=True to '
            f'write a refactored copy (this adds abstract features and may change the results '
            f'of operations on the written model).')
    return model
