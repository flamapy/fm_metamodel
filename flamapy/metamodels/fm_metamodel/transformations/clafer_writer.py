import re
from flamapy.core.transformations import ModelToText
from flamapy.core.models.ast import ASTOperation
from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature, Constraint


class ClaferWriter(ModelToText):
    """Transform a feature model to a Clafer format."""

    @staticmethod
    def get_destination_extension() -> str:
        return 'txt'

    def __init__(self, path: str, source_model: FeatureModel) -> None:
        self.path = path
        self.source_model = source_model

    def transform(self) -> str:
        clafer_str = fm_to_clafer(self.source_model)
        if self.path is not None:
            with open(self.path, 'w', encoding='utf-8') as file:
                file.write(clafer_str)
        return clafer_str


def fm_to_clafer(feature_model: FeatureModel) -> str:
    result = read_features(feature_model.root, 0)
    for ctc in feature_model.get_constraints():
       result += read_constraints(ctc)
    return result


def read_features(feature: Feature, tab_count: int) -> str:
    result = ''
    group_type = ''

    # Parse group types
    if feature.is_alternative_group():
        group_type = 'xor'
    elif feature.is_or_group():
        group_type = 'or'
    elif feature.is_cardinality_group():
        rel = next((r for r in feature.get_relations() if r.is_cardinal()), None)
        group_type = str(rel.card_min) + ".." + str(rel.card_max)
    elif feature.is_mutex_group():
        group_type = 'mux'

    # Indentation
    tabs = '\t' * tab_count
    result += tabs

    if group_type:
        result += f'{group_type} '

    result += feature.name

    if feature.is_optional():
        result += ' ?'

    # Feature's attributes
    tab_count += 1
    for attribute in feature.get_attributes():
        tabs = '\t' * tab_count
        attribute_value = ''
        if attribute.default_value is not None:
            if isinstance(attribute.default_value, str):
                attribute_value = f"'{attribute.default_value}'"
            elif isinstance(attribute.default_value, bool):
                attribute_value = f"{str(attribute.default_value).lower()}"
            else:
                attribute_value = f"{attribute.default_value}"
        result += f'\n{tabs}[{attribute.get_name()} {attribute_value}]'

    result += '\n'
    for child in feature.get_children():
        result += read_features(child, tab_count)
    return result


def read_constraints(const: Constraint) -> str:
    result = ""
    constraint_text = serialize_constraint(const)
    result = "\n" + constraint_text
    return result


def serialize_constraint(ctc: Constraint) -> str:
    ctc = ctc.ast.pretty_str()
    ctc = re.sub(fr'\b{ASTOperation.NOT.value}\b', 'not', ctc)
    ctc = re.sub(fr'\b{ASTOperation.AND.value}\b', '&&', ctc)
    ctc = re.sub(fr'\b{ASTOperation.OR.value}\b', '||', ctc)
    ctc = re.sub(fr'\b{ASTOperation.IMPLIES.value}\b', '=>', ctc)
    ctc = re.sub(fr'\b{ASTOperation.EQUIVALENCE.value}\b', '<=>', ctc)
    ctc = re.sub(fr'\b{ASTOperation.REQUIRES.value}\b', '=>', ctc)
    ctc = re.sub(fr'\b{ASTOperation.EXCLUDES.value}\b', '=> not', ctc)
    return f'[{ctc}]'