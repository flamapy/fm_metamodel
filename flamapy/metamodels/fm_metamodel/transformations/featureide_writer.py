from collections import Counter
from xml.dom import minidom
from xml.etree import ElementTree
from xml.etree.ElementTree import Element

from flamapy.core.exceptions import FlamaException
from flamapy.core.models.ast import ASTOperation, Node
from flamapy.core.transformations import ModelToText

from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature
from flamapy.metamodels.fm_metamodel.transformations import FeatureIDEReader
from flamapy.metamodels.fm_metamodel.transformations.uvl_writer import xor_as_negated_equivalence
from flamapy.metamodels.fm_metamodel.transformations.group_support import (
    cardinality_group,
    decompose_groups,
    mixed_decomposition,
    mutex_group,
    prepare_model,
    refactor_cardinality_groups,
    refactor_mutex_groups,
)



class FeatureIDEWriter(ModelToText):
    """Write a feature model in FeatureIDE's XML format.

    FeatureIDE gives each feature a single decomposition (`and` with mandatory/optional
    children, `or` or `alt`) and has no mutex or [m..n] group cardinalities. Models using them
    are rejected; with `apply_refactorings=True` a copy of the model is refactored first
    (MultipleGroupDecomposition, MutexGroup and CardinalityGroup refactorings), which adds
    abstract features and constraints but keeps the configurations of the original features.

    FeatureIDE's XML constraints are propositional: only the logical constraints of the model are
    written; arithmetic and other non-Boolean constraints are omitted (as in PLWriter).
    """

    CTC_TYPES = {ASTOperation.NOT: FeatureIDEReader.TAG_NOT,
                 ASTOperation.AND: FeatureIDEReader.TAG_CONJ,
                 ASTOperation.OR: FeatureIDEReader.TAG_DISJ,
                 ASTOperation.IMPLIES: FeatureIDEReader.TAG_IMP,
                 ASTOperation.REQUIRES: FeatureIDEReader.TAG_IMP,
                 ASTOperation.EQUIVALENCE: FeatureIDEReader.TAG_EQ}

    @staticmethod
    def get_destination_extension() -> str:
        return '.xml'

    def __init__(self, path: str, source_model: FeatureModel,
                 apply_refactorings: bool = False) -> None:
        self._path = path
        self._source_model = source_model
        self.apply_refactorings = apply_refactorings

    def transform(self) -> str:
        model = prepare_model(self._source_model, 'FeatureIDE XML',
                              [mixed_decomposition, mutex_group, cardinality_group],
                              [decompose_groups, refactor_mutex_groups,
                               refactor_cardinality_groups],
                              self.apply_refactorings)
        fm_tree = _to_featureidexml(model).getroot()
        if fm_tree is None:
            raise ValueError("The FeatureIDE XML tree has no root element.")
        xml_str = ElementTree.tostring(fm_tree,
                                       encoding='UTF-8',
                                       method='xml',
                                       xml_declaration=True)
        xml_bytes = prettify(xml_str)
        if self._path is not None:
            with open(self._path, 'wb') as file:
                file.write(xml_bytes)
        return xml_bytes.decode('utf-8')


def _to_featureidexml(feature_model: FeatureModel) -> ElementTree.ElementTree:
    names = [feature.name for feature in feature_model.get_features()]
    repeated = sorted(name for name, count in Counter(names).items() if count > 1)
    if repeated:
        raise FlamaException(
            f'The feature model has {len(repeated)} repeated feature name(s), which FeatureIDE '
            f'cannot represent: {", ".join(repeated[:10])}')
    fm_element = ElementTree.Element(FeatureIDEReader.TAG_FEATUREMODEL)
    tree = ElementTree.ElementTree(fm_element)
    struct = ElementTree.SubElement(fm_element, FeatureIDEReader.TAG_STRUCT)
    constraints = ElementTree.SubElement(fm_element, FeatureIDEReader.TAG_CONSTRAINTS)
    _write_feature(struct, feature_model.root, mandatory=False)
    for ctc in feature_model.get_logical_constraints():
        rule = ElementTree.SubElement(constraints, FeatureIDEReader.TAG_RULE)
        _write_constraint_node(rule, xor_as_negated_equivalence(ctc.ast).root)
    return tree


def _write_feature(parent_element: Element, feature: Feature, mandatory: bool) -> None:
    """Write a feature whose decomposition is a single group or mandatory/optional children."""
    relations = feature.get_relations()
    if not relations:
        tag = FeatureIDEReader.TAG_FEATURE
    elif len(relations) == 1 and relations[0].is_or():
        tag = FeatureIDEReader.TAG_OR
    elif len(relations) == 1 and relations[0].is_alternative():
        tag = FeatureIDEReader.TAG_ALT
    else:
        tag = FeatureIDEReader.TAG_AND
    element = ElementTree.SubElement(parent_element, tag,
                                     _feature_attributes(feature.name, mandatory,
                                                         feature.is_abstract))
    for relation in relations:
        for child in relation.children:
            _write_feature(element, child,
                           mandatory=tag == FeatureIDEReader.TAG_AND and relation.is_mandatory())


def _feature_attributes(name: str, mandatory: bool, abstract: bool) -> dict[str, str]:
    attributes = {}
    if mandatory:
        attributes['mandatory'] = 'true'
    if abstract:
        attributes['abstract'] = 'true'
    attributes['name'] = name
    return attributes


def _var(name: str) -> Element:
    element = Element(FeatureIDEReader.TAG_VAR)
    element.text = name
    return element


def _write_constraint_node(parent_element: Element, node: Node) -> None:
    """Write an AST node with the constraint elements FeatureIDE reads."""
    if node.is_term():
        parent_element.append(_var(str(node.data)))
        return
    if node.data == ASTOperation.EXCLUDES:
        imp = ElementTree.SubElement(parent_element, FeatureIDEReader.TAG_IMP)
        _write_constraint_node(imp, node.left)
        neg = ElementTree.SubElement(imp, FeatureIDEReader.TAG_NOT)
        _write_constraint_node(neg, node.right)
        return
    if node.data not in FeatureIDEWriter.CTC_TYPES:
        raise ValueError(f'Operator {node.data} cannot be written in FeatureIDE XML.')
    element = ElementTree.SubElement(parent_element, FeatureIDEWriter.CTC_TYPES[node.data])
    _write_constraint_node(element, node.left)
    if node.right is not None:
        _write_constraint_node(element, node.right)


def prettify(xml: bytes) -> bytes:
    """Return a pretty-printed XML string for the Element."""
    reparsed = minidom.parseString(xml)
    return reparsed.toprettyxml(indent="\t", encoding='UTF-8')
