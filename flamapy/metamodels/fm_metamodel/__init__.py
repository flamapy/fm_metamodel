from flamapy.core.manifest import OperationCapability, PluginManifest
from flamapy.core.models.language_level import ALL_MINOR_LEVELS, LanguageLevel, MajorLevel

# The metamodel itself understands the full UVL language; its own (tree-level)
# operations run on any model. Costs are estimated milliseconds measured on 2026-11-08 on the
# betty calibration models (1% and 30% density, 1,000-10,000 features): both operations walk
# the tree once, core features in under 1 ms and atomic sets in 7 ms at 10,000 features.
MANIFEST = PluginManifest(
    name='fm',
    calibration='2026-11-08',
    extension='fm',
    supported_level=LanguageLevel(MajorLevel.TYPE, set(ALL_MINOR_LEVELS)),
    operations={
        # FMCoreFeatures reads the tree only (mandatory chains from the root) and ignores
        # the cross-tree constraints: an estimate, never chosen by the planner unless the
        # caller allows approximate results.
        'core_features': OperationCapability(exact=False, cost=1),
        # FMAtomicSets groups mandatory features with their parents and ignores the
        # cross-tree constraints, which can merge sets: an estimate as well.
        'atomic_sets': OperationCapability(
            exact=False, cost_curve=((1000, 1), (2000, 2), (5000, 4), (10000, 7))),
    },
)
