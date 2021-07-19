from xml.etree import ElementTree
from xml.etree.ElementTree import Element

from famapy.core.models.ast import AST
from famapy.core.transformations import TextToModel

from famapy.metamodels.fm_metamodel.models import FeatureModel, Feature, Relation, Constraint


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
        return 'xml'

    def __init__(self, path: str):
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
                (features, _, relations) = self._read_features(root, root_feature)
                features = [root_feature] + features
                model = FeatureModel(root_feature, [], features, relations)
            elif child.tag == FeatureIDEParser.TAG_CONSTRAINTS:
                constraints = self._read_constraints(child)
                model.ctcs.extend(constraints)
        return model

    def _read_features(self, root_tree: Element, 
                       parent: Feature) -> tuple[list[Feature], list[Feature], list[Relation]]:
        features = []
        children = []
        relations = []
        for child in root_tree:
            if not child.tag == FeatureIDEParser.TAG_GRAPHICS:
                is_abstract = False
                if (FeatureIDEParser.ATTRIB_ABSTRACT in child.attrib 
                        and child.attrib[FeatureIDEParser.ATTRIB_ABSTRACT]):
                    is_abstract = True
                feature = Feature(name=child.attrib[FeatureIDEParser.ATTRIB_NAME], relations=[], 
                                  parent=parent, is_abstract=is_abstract)
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

    def _read_constraints(self, ctcs_root) -> list[Constraint]:
        number = 1
        constraints = []
        for ctc in ctcs_root:
            index = 0
            if ctc[index].tag == FeatureIDEParser.TAG_GRAPHICS:
                index += 1
            rule = ctc[index]
            str_ast = self._parse_rule(rule)
            if str_ast:
                ctc = Constraint(str(number), AST(str_ast))
                constraints.append(ctc)
            else:
                raise Exception()
            number += 1
        return constraints

    def _parse_rule(self, rule) -> str:
        """Return the representation of the constraint (rule) in the AST syntax."""
        str_ast = ''
        #print(f'Rule tag: {rule.tag}')
        #print(f'Rule values: {[r for r in rule]}')
        #print(f'Rule text: {rule.text}')
        if rule.tag == FeatureIDEParser.TAG_VAR:
            str_ast = rule.text
        elif rule.tag == FeatureIDEParser.TAG_NOT:
            str_ast = 'not ' + self._parse_rule(rule[0])
        elif rule.tag == FeatureIDEParser.TAG_IMP:
            str_ast = '(' + self._parse_rule(rule[0]) + ' implies ' \
                          + self._parse_rule(rule[1]) + ')'
        elif rule.tag == FeatureIDEParser.TAG_EQ:
            str_ast = '(' + self._parse_rule(rule[0]) + ' implies ' \
                          + self._parse_rule(rule[1]) + ') and (' \
                          + self._parse_rule(rule[1]) + ' implies ' \
                          + self._parse_rule(rule[0]) + ')'
        elif rule.tag == FeatureIDEParser.TAG_DISJ:
            if len(rule) > 1:
                str_ast = '(' + self._parse_rule(rule[0]) + ' or ' \
                              + self._parse_rule(rule[1]) + ')'
            else:
                str_ast = self._parse_rule(rule[0])
        elif rule.tag == FeatureIDEParser.TAG_CONJ:
            if len(rule) > 1:
                str_ast = '(' + self._parse_rule(rule[0]) + ' and ' \
                              + self._parse_rule(rule[1]) + ')'
            else:
                str_ast = self._parse_rule(rule[0])

        return str_ast
