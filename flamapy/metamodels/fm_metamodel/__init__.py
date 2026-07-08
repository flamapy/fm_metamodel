from flamapy.core.manifest import PluginManifest
from flamapy.core.models.language_level import ALL_MINOR_LEVELS, LanguageLevel, MajorLevel

# The metamodel itself understands the full UVL language; its own (tree-level)
# operations run on any model.
MANIFEST = PluginManifest(
    name='fm',
    extension='fm',
    supported_level=LanguageLevel(MajorLevel.TYPE, set(ALL_MINOR_LEVELS)),
)
