import logging
from xml.dom import minidom
import xml.etree.ElementTree as ET

from flamapy.core.models.ast import ASTOperation
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
        requires_list: list[tuple[str, str]] = []
        excludes_list: list[tuple[str, str]] = []
        for constraint in self._source_model.get_constraints():
            raw = str(constraint)
            ast_root = constraint.ast.root
            op = getattr(ast_root.data, 'value', str(ast_root.data)).upper()
            left = ast_root.left
            right = ast_root.right
            if (
                left is not None
                and right is not None
                and left.is_term()
                and right.is_term()
            ):
                if op in ('REQUIRES', 'IMPLIES', '=>') or ast_root.data is ASTOperation.REQUIRES:
                    requires_list.append((str(left.data), str(right.data)))
                    continue
                if op == 'EXCLUDES' or ast_root.data is ASTOperation.EXCLUDES:
                    excludes_list.append((str(left.data), str(right.data)))
                    continue
            logger.warning('Constraint skipped (unsupported in XML format): %s', raw)
            self.skipped_constraints.append(raw)
        return requires_list, excludes_list


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
