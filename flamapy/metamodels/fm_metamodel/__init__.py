from flamapy.core.manifest import OperationCapability, PluginManifest
from flamapy.core.models.language_level import ALL_MINOR_LEVELS, LanguageLevel, MajorLevel

# The metamodel itself understands the full UVL language; its own (tree-level)
# operations run on any model.
MANIFEST = PluginManifest(
    name='fm',
    extension='fm',
    supported_level=LanguageLevel(MajorLevel.TYPE, set(ALL_MINOR_LEVELS)),
    operations={
        # FMCoreFeatures reads the tree only (mandatory chains from the root) and ignores
        # the cross-tree constraints: an estimate, never chosen by the planner unless the
        # caller allows approximate results.
        'core_features': OperationCapability(exact=False, cost=1),
    },
)
