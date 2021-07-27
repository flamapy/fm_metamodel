from xml.etree import ElementTree
from xml.etree.ElementTree import Element

from famapy.core.models.ast import AST, Node
from famapy.core.transformations import TextToModel
from famapy.metamodels.fm_metamodel.models.feature_model import (
    Constraint,
    Feature,
    FeatureModel,
    Relation,
)


class FeatureIDEParser(TextToModel):
    """Parser for FeatureIDE models (.xml)."""

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
        for child in root:
            if child.tag == FeatureIDEParser.TAG_STRUCT:
                root = child[0]
                is_abstract = False
                if (FeatureIDEParser.ATTRIB_ABSTRACT in root.attrib
                        and root.attrib[FeatureIDEParser.ATTRIB_ABSTRACT]):
                    is_abstract = True
                root_feature = Feature(name=root.attrib[FeatureIDEParser.ATTRIB_NAME],
                                       relations=[], parent=None, is_abstract=is_abstract)
                self._read_features(root, root_feature)
                model = FeatureModel(root_feature, [])
            elif child.tag == FeatureIDEParser.TAG_CONSTRAINTS:
                constraints = self._read_constraints(child)
                model.ctcs.extend(constraints)
        return model

    def _read_features(
        self,
        root_tree: Element,
        parent: Feature
    ) -> tuple[list[Feature], list[Feature], list[Relation]]:
        features = []
        children = []
        relations = []
        for child in root_tree:
            if not child.tag == FeatureIDEParser.TAG_GRAPHICS:
                is_abstract = (
                    FeatureIDEParser.ATTRIB_ABSTRACT in child.attrib and
                    child.attrib[FeatureIDEParser.ATTRIB_ABSTRACT]
                )

                feature = Feature(
                    name=child.attrib[FeatureIDEParser.ATTRIB_NAME],
                    relations=[],
                    parent=parent,
                    is_abstract=is_abstract
                )

                features.append(feature)
                children.append(feature)
                if root_tree.tag == FeatureIDEParser.TAG_AND:
                    if FeatureIDEParser.ATTRIB_MANDATORY in child.attrib:  # Mandatory feature
                        rel = Relation(parent=parent, children=[feature], card_min=1, card_max=1)
                        parent.add_relation(rel)
                        relations.append(rel)
                    else:  # Optional feature
                        rel = Relation(parent=parent, children=[feature], card_min=0, card_max=1)
                        parent.add_relation(rel)
                        relations.append(rel)

                if child.tag == FeatureIDEParser.TAG_ALT:
                    (alt_children, direct_children,
                     children_relations) = self._read_features(child, feature)
                    rel = Relation(parent=feature, children=direct_children,
                                   card_min=1, card_max=1)
                    feature.add_relation(rel)
                    features.extend(alt_children)
                    relations.append(rel)
                    relations.extend(children_relations)
                elif child.tag == FeatureIDEParser.TAG_OR:
                    (or_children, direct_children,
                     children_relations) = self._read_features(child, feature)
                    rel = Relation(parent=feature, children=direct_children, card_min=1,
                                   card_max=len(direct_children))
                    feature.add_relation(rel)
                    features.extend(or_children)
                    relations.append(rel)
                    relations.extend(children_relations)
                elif child.tag == FeatureIDEParser.TAG_AND:
                    (and_children, direct_children,
                     children_relations) = self._read_features(child, feature)
                    features.extend(and_children)
                    relations.extend(children_relations)
        return (features, children, relations)

    def _read_constraints(self, ctcs_root: Element) -> list[Constraint]:
        number = 1
        constraints = []
        for ctc in ctcs_root:
            index = 0
            if ctc[index].tag == FeatureIDEParser.TAG_GRAPHICS:
                index += 1
            rule = ctc[index]
            ast = AST(self._parse_rule(rule))
            if ast:
                ctc = Constraint(str(number), ast)
                constraints.append(ctc)
            else:
                raise Exception()
            number += 1
        return constraints

    def _parse_rule(self, rule: Element) -> AST:
        """Return the representation of the constraint (rule) in the AST syntax."""
        if rule.tag == FeatureIDEParser.TAG_VAR:
            node = Node(rule.text)
        elif rule.tag == FeatureIDEParser.TAG_NOT:
            node = Node("NOT")
            node.left = self._parse_rule(rule[0])
        elif rule.tag == FeatureIDEParser.TAG_IMP:
            node = Node("IMPLIES")
            node.left = self._parse_rule(rule[0])
            node.right = self._parse_rule(rule[1])
        elif rule.tag == FeatureIDEParser.TAG_EQ:
            node = Node("AND")
            node.left = Node("IMPLIES")
            node.left.left = self._parse_rule(rule[0])
            node.left.right = self._parse_rule(rule[1])
            node.right = Node("IMPLIES")
            node.right.left = self._parse_rule(rule[1])
            node.right.right = self._parse_rule(rule[0])

        elif rule.tag == FeatureIDEParser.TAG_DISJ:
            if len(rule) > 1:
                node = Node("OR")
                node.left = self._parse_rule(rule[0])
                node.left = self._parse_rule(rule[1])

            else:
                node = self._parse_rule(rule[0])

        elif rule.tag == FeatureIDEParser.TAG_CONJ:
            if len(rule) > 1:
                node = Node("AND")
                node.left = self._parse_rule(rule[0])
                node.left = self._parse_rule(rule[1])
            else:
                node = self._parse_rule(rule[0])
        return node
