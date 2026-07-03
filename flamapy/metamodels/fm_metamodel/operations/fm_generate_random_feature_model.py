import random
from typing import Optional

from flamapy.core.models import VariabilityModel
from flamapy.core.models.ast import AST, ASTOperation
from flamapy.core.operations import Operation
from flamapy.core.operations.descriptor import OperationDescriptor, Input
from flamapy.metamodels.fm_metamodel.models import (
    FeatureModel,
    Feature,
    Relation,
    Constraint,
)

# Minimum children an or/alternative group needs (and minimum features for a cross-tree constraint).
_MIN_GROUP_MEMBERS = 2


class GenerateRandomFeatureModel(Operation):
    """Generate a random synthetic feature model as a :class:`FeatureModel` object.

    The model is built directly from feature-model objects (features, relations and cross-tree
    constraints) — no serialization is involved. Serialize it to UVL (or any format) afterwards
    with the corresponding writer transformation if needed. Useful to build corpora for
    benchmarking, testing, or training learning-based operations. When ``void`` is set, a
    contradictory constraint is added so the resulting model is unsatisfiable.

    The operation produces a model, so ``execute`` ignores its (optional) input model.
    """

    facade = OperationDescriptor(
        doc=(
            'Generates a random synthetic feature model and returns it as a FeatureModel.\n'
            '``num_features`` (>= 2) sizes the tree, ``max_constraints`` bounds the number of\n'
            'cross-tree constraints, ``seed`` makes it reproducible, and ``void=True`` forces an\n'
            'unsatisfiable model. Useful for building corpora, testing, or training.'
        ),
        returns='FeatureModel',
        name='generate_random_feature_model', operation='GenerateRandomFeatureModel',
        kind='producer',
        inputs=(
            Input('num_features', int, default=10, setter='set_num_features'),
            Input('max_constraints', int, default=3, setter='set_max_constraints'),
            Input('seed', int, default=0, setter='set_seed'),
            Input('void', bool, default=False, setter='set_void'),
        ),
    )

    def __init__(self) -> None:
        self.result: FeatureModel
        self._num_features: int = 10
        self._max_constraints: int = 3
        self._seed: int = 0
        self._void: bool = False

    def get_result(self) -> FeatureModel:
        return self.result

    def set_num_features(self, num_features: int) -> None:
        self._num_features = max(2, num_features)

    def set_max_constraints(self, max_constraints: int) -> None:
        self._max_constraints = max(0, max_constraints)

    def set_seed(self, seed: int) -> None:
        self._seed = seed

    def set_void(self, void: bool) -> None:
        self._void = void

    def _build_tree(self, rng: random.Random) -> Feature:
        names = [f'F{i}' for i in range(self._num_features)]
        features = {name: Feature(name, is_abstract=(index == 0))
                    for index, name in enumerate(names)}

        assignments: dict[str, list[str]] = {name: [] for name in names}
        for name in names[1:]:
            parent = rng.choice(names[: names.index(name)])
            assignments[parent].append(name)

        for parent_name, kids in assignments.items():
            if not kids:
                continue
            parent = features[parent_name]
            kid_features = [features[kid] for kid in kids]
            for kid in kid_features:
                kid.parent = parent
            group = rng.choice(['mandatory', 'optional', 'or', 'alternative'])
            if group in ('or', 'alternative') and len(kids) < _MIN_GROUP_MEMBERS:
                group = 'optional'
            if group == 'mandatory':
                parent.relations.extend(Relation(parent, [kid], 1, 1) for kid in kid_features)
            elif group == 'optional':
                parent.relations.extend(Relation(parent, [kid], 0, 1) for kid in kid_features)
            elif group == 'or':
                parent.relations.append(Relation(parent, kid_features, 1, len(kid_features)))
            else:  # alternative
                parent.relations.append(Relation(parent, kid_features, 1, 1))
        return features[names[0]]

    def _build_constraints(self, root: Feature, rng: random.Random) -> list[Constraint]:
        non_root = [feature.name for feature in _descendants(root) if feature is not root]
        constraints: list[Constraint] = []
        if len(non_root) >= _MIN_GROUP_MEMBERS and self._max_constraints > 0:
            for index in range(rng.randint(1, self._max_constraints)):
                left, right = rng.sample(non_root, 2)
                operation = rng.choice([ASTOperation.REQUIRES, ASTOperation.EXCLUDES])
                constraints.append(
                    Constraint(f'ctc{index}',
                               AST.create_simple_binary_operation(operation, left, right))
                )
        if self._void and non_root:
            # Force a feature to be present (required by the always-selected root) and absent.
            forced = rng.choice(non_root)
            constraints.append(Constraint(
                'void_requires',
                AST.create_simple_binary_operation(ASTOperation.REQUIRES, root.name, forced)))
            constraints.append(Constraint(
                'void_excludes', AST.create_simple_unary_operation(ASTOperation.NOT, forced)))
        return constraints

    def execute(self, model: Optional[VariabilityModel] = None) -> 'GenerateRandomFeatureModel':
        rng = random.Random(self._seed)
        root = self._build_tree(rng)
        constraints = self._build_constraints(root, rng)
        self.result = FeatureModel(root, constraints)
        return self


def _descendants(feature: Feature) -> list[Feature]:
    collected = [feature]
    for relation in feature.relations:
        for child in relation.children:
            collected.extend(_descendants(child))
    return collected
