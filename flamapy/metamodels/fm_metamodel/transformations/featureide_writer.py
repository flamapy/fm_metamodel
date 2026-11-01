import itertools
import math
from collections import Counter
from xml.dom import minidom
from xml.etree import ElementTree
from xml.etree.ElementTree import Element

from flamapy.core.exceptions import FlamaException
from flamapy.core.models.ast import ASTOperation, Node
from flamapy.core.transformations import ModelToText

from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature, Relation
from flamapy.metamodels.fm_metamodel.transformations import FeatureIDEReader
from flamapy.metamodels.fm_metamodel.transformations.uvl_writer import xor_as_negated_equivalence



# Cardinality groups [m..k] are written as clauses (as in PLWriter); groups that would need more
# clauses than this are rejected.
MAX_CARDINALITY_CLAUSES = 100_000


class FeatureIDEWriter(ModelToText):
    """Write a feature model in FeatureIDE's XML format.

    FeatureIDE gives each feature a single decomposition: `and` (mandatory/optional children),
    `or` or `alt`. A feature that has a group and other relations, or several groups, gets an
    abstract helper feature `<feature>_group<n>` per group:
      * or / alternative group: a mandatory helper with that group;
      * mutex group [0..1]: an optional helper with an alternative group (none or exactly one);
      * cardinality group [m..k]: a mandatory helper whose children are optional, plus clauses
        stating that the helper implies at least m children (one child of every set of n-m+1)
        and that at most k children are selected (no k+1 of them together).
    The configurations of the original features are preserved.

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

    def __init__(self, path: str, source_model: FeatureModel) -> None:
        self._path = path
        self._source_model = source_model

    def transform(self) -> str:
        fm_tree = _to_featureidexml(self._source_model).getroot()
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
    writer = _TreeWriter(set(names), constraints)
    writer.write_feature(struct, feature_model.root, mandatory=False)
    for ctc in feature_model.get_logical_constraints():
        rule = ElementTree.SubElement(constraints, FeatureIDEReader.TAG_RULE)
        _write_constraint_node(rule, xor_as_negated_equivalence(ctc.ast).root)
    return tree


class _TreeWriter:
    """Writes the feature tree, adding helper features and constraints where needed."""

    def __init__(self, used_names: set[str], constraints: Element) -> None:
        self._used_names = used_names
        self._constraints = constraints

    def write_feature(self, parent_element: Element, feature: Feature, mandatory: bool) -> None:
        relations = feature.get_relations()
        if not relations:
            tag = FeatureIDEReader.TAG_FEATURE
        elif len(relations) == 1 and (relations[0].is_or() or relations[0].is_alternative()):
            tag = FeatureIDEReader.TAG_OR if relations[0].is_or() else FeatureIDEReader.TAG_ALT
        else:
            tag = FeatureIDEReader.TAG_AND
        element = ElementTree.SubElement(parent_element, tag,
                                         _feature_attributes(feature.name, mandatory,
                                                             feature.is_abstract))
        if tag in (FeatureIDEReader.TAG_OR, FeatureIDEReader.TAG_ALT):
            for child in relations[0].children:
                self.write_feature(element, child, mandatory=False)
            return
        group_index = 0
        for relation in relations:
            if relation.is_mandatory() or relation.is_optional():
                self.write_feature(element, relation.children[0], relation.is_mandatory())
                continue
            group_index += 1
            helper = self._helper_name(f'{feature.name}_group{group_index}')
            self._write_group(element, relation, helper)

    def _write_group(self, parent_element: Element, relation: Relation, helper: str) -> None:
        children = relation.children
        card_max = len(children) if relation.card_max < 0 else min(relation.card_max,
                                                                     len(children))
        if relation.is_or() or relation.is_alternative():
            tag = FeatureIDEReader.TAG_OR if relation.is_or() else FeatureIDEReader.TAG_ALT
            group = ElementTree.SubElement(parent_element, tag,
                                           _feature_attributes(helper, True, True))
            for child in children:
                self.write_feature(group, child, mandatory=False)
        elif relation.is_mutex():
            # Optional helper with an alternative group: the helper is deselected when no child
            # is selected and selected with exactly one child, so configurations correspond.
            group = ElementTree.SubElement(parent_element, FeatureIDEReader.TAG_ALT,
                                           _feature_attributes(helper, False, True))
            for child in children:
                self.write_feature(group, child, mandatory=False)
        else:
            group = ElementTree.SubElement(parent_element, FeatureIDEReader.TAG_AND,
                                           _feature_attributes(helper, True, True))
            all_mandatory = relation.card_min == len(children)
            for child in children:
                self.write_feature(group, child, mandatory=all_mandatory)
            if relation.card_min > 0 or card_max < len(children):
                if not all_mandatory:
                    self._cardinality_constraint(helper, [c.name for c in children],
                                                 relation.card_min, card_max)

    def _cardinality_constraint(self, helper: str, children: list[str],
                                card_min: int, card_max: int) -> None:
        """Clauses for a [card_min..card_max] group under a mandatory helper feature."""
        n_children = len(children)
        at_least = math.comb(n_children, n_children - card_min + 1) \
            if 0 < card_min <= n_children else 0
        at_most = math.comb(n_children, card_max + 1) if card_max < n_children else 0
        if at_least + at_most > MAX_CARDINALITY_CLAUSES:
            raise ValueError(
                f'Group cardinality [{card_min}..{card_max}] over {n_children} children needs '
                f'{at_least + at_most} clauses; at most {MAX_CARDINALITY_CLAUSES} are supported '
                f'in FeatureIDE XML.')
        if card_min > n_children:
            rule = ElementTree.SubElement(self._constraints, FeatureIDEReader.TAG_RULE)
            ElementTree.SubElement(rule, FeatureIDEReader.TAG_NOT).append(_var(helper))
        elif card_min > 0:
            for subset in itertools.combinations(children, n_children - card_min + 1):
                rule = ElementTree.SubElement(self._constraints, FeatureIDEReader.TAG_RULE)
                imp = ElementTree.SubElement(rule, FeatureIDEReader.TAG_IMP)
                imp.append(_var(helper))
                imp.append(_any_of(list(subset)))
        if card_max < n_children:
            for subset in itertools.combinations(children, card_max + 1):
                rule = ElementTree.SubElement(self._constraints, FeatureIDEReader.TAG_RULE)
                neg = ElementTree.SubElement(rule, FeatureIDEReader.TAG_NOT)
                neg.append(_all_of(list(subset)))

    def _helper_name(self, base: str) -> str:
        name, suffix = base, 1
        while name in self._used_names:
            suffix += 1
            name = f'{base}_{suffix}'
        self._used_names.add(name)
        return name


def _feature_attributes(name: str, mandatory: bool, abstract: bool) -> dict[str, str]:
    attributes = {}
    if mandatory:
        attributes['mandatory'] = 'true'
    if abstract:
        attributes['abstract'] = 'true'
    attributes['name'] = name
    return attributes


def _any_of(names: list[str]) -> Element:
    if len(names) == 1:
        return _var(names[0])
    disj = Element(FeatureIDEReader.TAG_DISJ)
    for name in names:
        disj.append(_var(name))
    return disj


def _all_of(names: list[str]) -> Element:
    if len(names) == 1:
        return _var(names[0])
    conj = Element(FeatureIDEReader.TAG_CONJ)
    for name in names:
        conj.append(_var(name))
    return conj


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
