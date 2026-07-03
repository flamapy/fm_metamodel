# flamapy-fm

The feature model metamodel plugin for [flamapy](https://flamapy.org): the
feature model classes plus readers and writers for many formats (UVL,
FeatureIDE, Glencoe, AFM, SPLOT, JSON, …).

**Documentation:** https://docs.flamapy.org/framework/plugins/feature_model_plugin

## Installation

```bash
pip install flamapy-fm
```

## Operations

Beyond the metrics/analysis operations (core/dead features, atomic sets, branching factor, …),
this plugin includes a **random feature-model generator**:

- `GenerateRandomFeatureModel` — builds a random synthetic `FeatureModel` object directly
  (random tree of mandatory/optional/or/alternative groups plus random cross-tree constraints;
  an optional `void` flag makes it unsatisfiable). Useful for benchmarking, testing, and building
  corpora for learning-based operations. Serialize it to UVL (or any format) with the writer
  transformations if you need text.

```python
from flamapy.metamodels.fm_metamodel.operations import GenerateRandomFeatureModel

op = GenerateRandomFeatureModel()
op.set_num_features(20)
op.set_max_constraints(5)
op.set_seed(0)
fm = op.execute().get_result()   # -> FeatureModel
```
