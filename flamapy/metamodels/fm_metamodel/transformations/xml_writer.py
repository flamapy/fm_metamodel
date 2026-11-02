import logging
from xml.dom import minidom
import xml.etree.ElementTree as ET

from flamapy.core.models.ast import ASTOperation, Node
from flamapy.core.transformations import ModelToText

from flamapy.metamodels.fm_metamodel.models import Feature, FeatureModel, Relation

logger = logging.getLogger(__name__)


class XMLWriter(ModelToText):
    """Serialize a FeatureModel to FAMA XML format.

    This is the counterpart of XMLReader. The output is readable by XMLReader
    and compatible with the FaMA analysis tool.

    Only requires/excludes cross-tree constraints are supported by this format.
    Complex constraints are skipped; inspect ``skipped_constraints`` after
    calling ``transform()`` to retrieve the ones that were dropped.
    """

    @staticmethod
    def get_destination_extension() -> str:
        return 'xml'

    def __init__(self, path: str, source_model: FeatureModel) -> None:
        self._path = path
        self._source_model = source_model
        self.skipped_constraints: list[str] = []

    def transform(self) -> str:
        self.skipped_constraints = []

        fm_el = ET.Element('feature-model')
        fm_el.set('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance')
        fm_el.set('xsi:noNamespaceSchemaLocation', 'feature-model-schema.xsd')

        root_feature = self._source_model.root
        feat_el = ET.SubElement(fm_el, 'feature')
        feat_el.set('name', root_feature.name)

        counters = [0, 0]  # [br_count, sr_count]
        _write_feature(root_feature, feat_el, counters)

        requires_list, excludes_list = self._extract_binary_constraints()

        re_count = 0
        for feat_a, feat_b in requires_list:
            re_count += 1
            req_el = ET.SubElement(fm_el, 'requires')
            req_el.set('name', f'Re-{re_count}')
            req_el.set('feature', feat_a)
            req_el.set('requires', feat_b)

        ex_count = 0
        for feat_a, feat_b in excludes_list:
            ex_count += 1
            exc_el = ET.SubElement(fm_el, 'excludes')
            exc_el.set('name', f'Ex-{ex_count}')
            exc_el.set('feature', feat_a)
            exc_el.set('excludes', feat_b)

        rough_str = ET.tostring(fm_el, encoding='unicode')
        dom = minidom.parseString(rough_str)
        lines = dom.toprettyxml(indent='\t').splitlines()
        if lines and lines[0].startswith('<?xml'):
            lines[0] = '<?xml version="1.0" encoding="UTF-8"?>'
        xml_str = '\n'.join(lines)

        if self._path is not None:
            with open(self._path, 'w', encoding='utf-8') as fh:
                fh.write(xml_str)

        return xml_str

    def _extract_binary_constraints(
        self,
    ) -> tuple[list[tuple[str, str]], list[tuple[str, str]]]:
        """requires/excludes pairs for every constraint FaMa XML can express.

        Besides `A requires B` / `A => B` and `A excludes B`, the equivalent forms
        `A => !B` and `!(A & B)` are written as excludes and `A <=> B` as two requires.
        """
        requires_list: list[tuple[str, str]] = []
        excludes_list: list[tuple[str, str]] = []
        for constraint in self._source_model.get_constraints():
            pairs = _binary_pairs(constraint.ast.root)
            if pairs is None:
                raw = str(constraint)
                logger.warning('Constraint skipped (unsupported in XML format): %s', raw)
                self.skipped_constraints.append(raw)
                continue
            kind, found = pairs
            (requires_list if kind == 'requires' else excludes_list).extend(found)
        return requires_list, excludes_list


def _binary_pairs(node: Node) -> tuple[str, list[tuple[str, str]]] | None:
    """('requires' | 'excludes', pairs) when the constraint is a binary relation between two
    features that FaMa XML can express, otherwise None."""
    def term(n: Node | None) -> str | None:
        return str(n.data) if n is not None and n.is_term() else None

    def negated_term(n: Node | None) -> str | None:
        return term(n.left) if n is not None and n.data == ASTOperation.NOT else None

    op, left, right = node.data, node.left, node.right
    if op in (ASTOperation.REQUIRES, ASTOperation.IMPLIES) and term(left):
        if term(right):
            return 'requires', [(term(left), term(right))]  # type: ignore[list-item]
        if negated_term(right):
            return 'excludes', [(term(left), negated_term(right))]  # type: ignore[list-item]
    if op == ASTOperation.EXCLUDES and term(left) and term(right):
        return 'excludes', [(term(left), term(right))]  # type: ignore[list-item]
    if op == ASTOperation.EQUIVALENCE and term(left) and term(right):
        return 'requires', [(term(left), term(right)), (term(right), term(left))]  # type: ignore[list-item]
    if op == ASTOperation.NOT and left is not None and left.data == ASTOperation.AND \
            and term(left.left) and term(left.right):
        return 'excludes', [(term(left.left), term(left.right))]  # type: ignore[list-item]
    return None

def _write_feature(feature: Feature, parent_el: ET.Element, counters: list[int]) -> None:
    """Recursively append FAMA XML child elements to *parent_el*."""
    for relation in feature.get_relations():
        _write_relation(relation, parent_el, counters)


def _write_relation(relation: Relation, parent_el: ET.Element, counters: list[int]) -> None:
    children = list(relation.children)
    if len(children) == 1:
        counters[0] += 1
        br_el = ET.SubElement(parent_el, 'binaryRelation')
        br_el.set('name', f'BR-{counters[0]}')

        card_el = ET.SubElement(br_el, 'cardinality')
        card_el.set('min', str(relation.card_min))
        card_el.set('max', str(relation.card_max))

        child = children[0]
        sol_el = ET.SubElement(br_el, 'solitaryFeature')
        sol_el.set('name', child.name)
        _write_feature(child, sol_el, counters)

    elif len(children) > 1:
        counters[1] += 1
        sr_el = ET.SubElement(parent_el, 'setRelation')
        sr_el.set('name', f'SR-{counters[1]}')

        card_el = ET.SubElement(sr_el, 'cardinality')
        card_el.set('min', str(relation.card_min))
        card_el.set('max', str(relation.card_max))

        for child in children:
            gf_el = ET.SubElement(sr_el, 'groupedFeature')
            gf_el.set('name', child.name)
            _write_feature(child, gf_el, counters)
