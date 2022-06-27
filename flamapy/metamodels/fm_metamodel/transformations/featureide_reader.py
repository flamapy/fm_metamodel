from xml.etree import ElementTree
from xml.etree.ElementTree import Element

from flamapy.core.models.ast import AST, Node, ASTOperation
from flamapy.core.transformations import TextToModel
from flamapy.metamodels.fm_metamodel.models import (
    Constraint,
    Feature,
    FeatureModel,
    Relation,
)


class FeatureIDEReader(TextToModel):
    """Reader for FeatureIDE models (.xml)."""

    # Main tags
    TAG_STRUCT = 'struct'
    TAG_CONSTRAINTS = 'constraints'
    TAG_GRAPHICS = 'graphics'

    # Feature tags
    TAG_AND = 'and'
    TAG_OR = 'or'
    TAG_ALT = 'alt'

    # Constraints tags
    TAG_VAR = 'var'
    TAG_NOT = 'not'
    TAG_IMP = 'imp'
    TAG_DISJ = 'disj'
    TAG_CONJ = 'conj'
    TAG_EQ = 'eq'

    # Feature attributes
    ATTRIB_NAME = 'name'
    ATTRIB_ABSTRACT = 'abstract'
    ATTRIB_MANDATORY = 'mandatory'

    @staticmethod
    def get_source_extension() -> str:
        return 'fide'

    def __init__(self, path: str) -> None:
        self._path = path

    def transform(self) -> FeatureModel:
        return self._read_feature_model(self._path)

    def _read_feature_model(self, filepath: str) -> FeatureModel:
        tree = ElementTree.parse(filepath)
        root = tree.getroot()
        model = FeatureModel(root=None, constraints=[])
        for child in root:
            if child.tag == FeatureIDEReader.TAG_STRUCT:
                (root_feature, _) = self._read_features(child, None)
                model.root = root_feature
            elif child.tag == FeatureIDEReader.TAG_CONSTRAINTS:
                constraints = self._read_constraints(child)
                model.ctcs.extend(constraints)
        return model

    def _read_features(
        self,
        root_tree: Element,
        parent: Feature
    ) -> tuple[Feature, list[Feature]]:
        children = []
        feature = None
        for child in root_tree:
            if not child.tag == FeatureIDEReader.TAG_GRAPHICS:
                is_abstract = (
                    FeatureIDEReader.ATTRIB_ABSTRACT in child.attrib and
                    child.attrib[FeatureIDEReader.ATTRIB_ABSTRACT] == "true"
                )

                feature = Feature(
                    name=child.attrib[FeatureIDEReader.ATTRIB_NAME],
                    relations=[],
                    parent=parent,
                    is_abstract=is_abstract
                )

                children.append(feature)
                if root_tree.tag == FeatureIDEReader.TAG_AND:
                    if FeatureIDEReader.ATTRIB_MANDATORY in child.attrib:  # Mandatory feature
                        rel = Relation(parent=parent, children=[feature], card_min=1, card_max=1)
                        parent.add_relation(rel)
                    else:  # Optional feature
                        rel = Relation(parent=parent, children=[feature], card_min=0, card_max=1)
                        parent.add_relation(rel)

                if child.tag == FeatureIDEReader.TAG_ALT:
                    (_, direct_children) = self._read_features(child, feature)
                    rel = Relation(parent=feature, children=direct_children,
                                   card_min=1, card_max=1)
                    feature.add_relation(rel)
                elif child.tag == FeatureIDEReader.TAG_OR:
                    (_, direct_children) = self._read_features(child, feature)
                    rel = Relation(parent=feature, children=direct_children, card_min=1,
                                   card_max=len(direct_children))
                    feature.add_relation(rel)
                elif child.tag == FeatureIDEReader.TAG_AND:
                    (_, direct_children) = self._read_features(child, feature)
        return (feature, children)

    def _read_constraints(self, ctcs_root: Element) -> list[Constraint]:
        number = 1
        constraints = []
        for ctc in ctcs_root:
            index = 0
            if ctc[index].tag == FeatureIDEReader.TAG_GRAPHICS:
                index += 1
            rule = ctc[index]
            ast = self._parse_rule(rule)
            if ast:
                ctc = Constraint(str(number), ast)
                constraints.append(ctc)
            else:
                raise Exception()
            number += 1
        return constraints

    def _parse_rule(self, rule: Element) -> AST:
        """Return the representation of the constraint (rule) in the AST syntax."""
        if rule.tag == FeatureIDEReader.TAG_VAR:
            node = Node(rule.text)
        elif rule.tag == FeatureIDEReader.TAG_NOT:
            node = Node(ASTOperation.NOT)
            node.left = self._parse_rule(rule[0]).root
        elif rule.tag == FeatureIDEReader.TAG_IMP:
            node = Node(ASTOperation.IMPLIES)
            node.left = self._parse_rule(rule[0]).root
            node.right = self._parse_rule(rule[1]).root
        elif rule.tag == FeatureIDEReader.TAG_EQ:
            node = Node(ASTOperation.AND)
            node.left = Node(ASTOperation.IMPLIES)
            node.left.left = self._parse_rule(rule[0]).root
            node.left.right = self._parse_rule(rule[1]).root
            node.right = Node(ASTOperation.IMPLIES)
            node.right.left = self._parse_rule(rule[1]).root
            node.right.right = self._parse_rule(rule[0]).root

        elif rule.tag == FeatureIDEReader.TAG_DISJ:
            if len(rule) > 1:
                node = Node(ASTOperation.OR)
                node.left = self._parse_rule(rule[0]).root
                node.right = self._parse_rule(rule[1]).root

            else:
                node = self._parse_rule(rule[0]).root

        elif rule.tag == FeatureIDEReader.TAG_CONJ:
            if len(rule) > 1:
                node = Node(ASTOperation.AND)
                node.left = self._parse_rule(rule[0]).root
                node.right = self._parse_rule(rule[1]).root
            else:
                node = self._parse_rule(rule[0]).root
        return AST(node)
