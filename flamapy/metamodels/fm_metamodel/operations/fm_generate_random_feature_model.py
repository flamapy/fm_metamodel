import os
import random
import tempfile
from typing import Optional

from flamapy.core.models import VariabilityModel
from flamapy.core.operations import Operation
from flamapy.metamodels.fm_metamodel.models import FeatureModel
from flamapy.metamodels.fm_metamodel.transformations import UVLReader

# Minimum children an or/alternative group needs (and minimum features for a cross-tree constraint).
_MIN_GROUP_MEMBERS = 2


class GenerateRandomFeatureModel(Operation):
    """Generate a random synthetic feature model.

    A random feature tree (mandatory/optional/or/alternative groups) plus random cross-tree
    constraints are emitted as UVL and parsed back into a :class:`FeatureModel`. Useful to build
    corpora for benchmarking, testing, or training learning-based operations. When ``void`` is
    set, a contradictory constraint is added so the resulting model is unsatisfiable.

    The operation produces a model, so ``execute`` ignores its (optional) input model.
    """

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

    def _emit_uvl(self, rng: random.Random) -> str:
        names = [f'F{i}' for i in range(self._num_features)]
        root = names[0]
        lines = ['features', f'    {root} {{abstract}}']

        children: dict[str, list[str]] = {name: [] for name in names}
        for name in names[1:]:
            parent = rng.choice(names[: names.index(name)])
            children[parent].append(name)

        def emit(parent: str, depth: int) -> None:
            kids = children[parent]
            if not kids:
                return
            indent = '    ' * (depth + 1)
            group = rng.choice(['mandatory', 'optional', 'or', 'alternative'])
            if group in ('or', 'alternative') and len(kids) < _MIN_GROUP_MEMBERS:
                group = 'optional'
            lines.append(f'{indent}{group}')
            lines.extend(f'{indent}    {kid}' for kid in kids)
            for kid in kids:
                emit(kid, depth + 2)

        emit(root, 1)

        constraints = []
        non_root = names[1:]
        if len(non_root) >= _MIN_GROUP_MEMBERS and self._max_constraints > 0:
            for _ in range(rng.randint(1, self._max_constraints)):
                left, right = rng.sample(non_root, 2)
                constraints.append(
                    rng.choice([f'{left} => {right}', f'!{left} | {right}', f'!{left} | !{right}'])
                )
        if self._void and non_root:
            forced = rng.choice(non_root)
            constraints.append(f'{root} => {forced}')
            constraints.append(f'!{forced}')
        if constraints:
            lines.append('constraints')
            lines.extend(f'    {constraint}' for constraint in constraints)
        return '\n'.join(lines) + '\n'

    def execute(self, model: Optional[VariabilityModel] = None) -> 'GenerateRandomFeatureModel':
        rng = random.Random(self._seed)
        uvl = self._emit_uvl(rng)
        handle, path = tempfile.mkstemp(suffix='.uvl')
        try:
            with os.fdopen(handle, 'w') as file:
                file.write(uvl)
            self.result = UVLReader(path).transform()
        finally:
            os.remove(path)
        return self
