from pathlib import Path
from famapy.core.models.ast import AST, Node
from famapy.metamodels.fm_metamodel.models.feature_model import Constraint, Feature, FeatureModel, Relation
from famapy.metamodels.fm_metamodel.transformations.xml_transformation import XMLTransformation
from famapy.metamodels.fm_metamodel.transformations.afm_transformation import AFMTransformation
from famapy.metamodels.fm_metamodel.transformations.afm_writer import AFMWriter
from famapy.metamodels.fm_metamodel.transformations.uvl_transformation import UVLTransformation
from famapy.metamodels.fm_metamodel.transformations.uvl_writter import UVLWriter


def xml_transform(path: str, model: FeatureModel) -> None:
    original_model = model
    xml_transformation = XMLTransformation(path + ".xml")
    transformed_model = xml_transformation.transform()

    assert(original_model == transformed_model)


def afm_write_to_transformation(path: str, model: FeatureModel) -> None:
    original_model = model
    afm_writer = AFMWriter(original_model, path + ".afm")
    afm_writer.transform()
    afm_transformation = AFMTransformation(path + ".afm")
    afm_transformation.transform()
    transformed_model = afm_transformation.model

    original_model_relations = original_model.get_relations()
    transformed_model_relations = transformed_model.get_relations()

    assert(original_model == transformed_model)


def uvl_write_to_transformation(path: str, model: FeatureModel) -> None:
    original_model = model
    uvl_writer = UVLWriter(original_model, path + ".uvl")
    uvl_writer.transform()
    uvl_transformation = UVLTransformation(path + ".uvl")
    uvl_transformation.transform()
    transformed_model = uvl_transformation.model

    print(original_model)
    print(transformed_model)

    assert(original_model == transformed_model)


def run(path: str, model: FeatureModel) -> None:
    xml_transform(path, model)
    afm_write_to_transformation(path, model)
    uvl_write_to_transformation(path, model)


def normalize_path(path: str) -> str:
    return str(Path(__file__).parent.resolve()) + "/" + path


def test_error_guessing_core_features_case_1() -> None:

    feature_1 = Feature("A", None, None, False)
    feature_2 = Feature("B", None, None, False)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2, feature_3], 1, 2)
    feature_1.add_relation(relation_1)

    node_1 = Node("requires")
    node_1.left = Node("C")
    node_1.right = Node("B")
    ast_1 = AST(node_1)

    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path("error-guessing/core-features/case1/cf-case1")
    run(path, model)


def test_error_guessing_core_features_case_2() -> None:
    feature_1 = Feature("A", None, None, False)
    feature_2 = Feature("B", None, None, False)

    relation_1 = Relation(feature_1, [feature_2], 0, 1)

    feature_1.add_relation(relation_1)

    node_1 = Node("requires")
    node_1.left = Node("A")
    node_1.right = Node("B")
    ast_1 = AST(node_1)

    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path("error-guessing/core-features/case2/cf-case2")
    run(path, model)


def test_error_guessing_core_features_case_3() -> None:
    feature_1 = Feature("A", None, None, False)
    feature_2 = Feature("B", None, None, False)
    feature_3 = Feature("C", None, None, False)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 0, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    node_1 = Node("requires")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)

    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path("error-guessing/core-features/case3/cf-case3")
    run(path, model)


def test_error_guessing_core_features_case_4() -> None:
    feature_1 = Feature("A", None, None, False)
    feature_2 = Feature("B", None, None, False)
    feature_3 = Feature("C", None, None, False)
    feature_4 = Feature("D", None, None, False)
    feature_5 = Feature("E", None, None, False)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_3, [feature_4, feature_5], 1, 1)

    feature_3.add_relation(relation_3)

    node_1 = Node("excludes")
    node_1.left = Node("B")
    node_1.right = Node("D")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path("error-guessing/core-features/case4/cf-case4")
    run(path, model)


def test_error_guessing_core_features_case_5() -> None:
    feature_1 = Feature("A", None, None, False)
    feature_2 = Feature("B", None, None, False)
    feature_3 = Feature("C", None, None, False)
    feature_4 = Feature("D", None, None, False)
    feature_5 = Feature("E", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_3, [feature_4, feature_5], 1, 1)

    feature_3.add_relation(relation_3)

    node_1 = Node("requires")
    node_1.left = Node("B")
    node_1.right = Node("D")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path("error-guessing/core-features/case5/cf-case5")
    run(path, model)


def test_error_guessing_core_features_case_6() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    node_1 = Node("excludes")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path("error-guessing/core-features/case6/cf-case6")
    run(path, model)


def test_error_guessing_dead_features_case_1() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, False)
    feature_5 = Feature("E", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_3, [feature_4, feature_5], 1, 1)

    feature_3.add_relation(relation_3)

    node_1 = Node("excludes")
    node_1.left = Node("D")
    node_1.right = Node("B")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path("error-guessing/dead-features/case1/df-case1")
    run(path, model)


def test_error_guessing_dead_features_case_2() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, True)
    feature_5 = Feature("E", None, None, False)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_3, [feature_4, feature_5], 1, 1)

    feature_3.add_relation(relation_3)

    node_1 = Node("requires")
    node_1.left = Node("B")
    node_1.right = Node("D")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path("error-guessing/dead-features/case2/df-case2")
    run(path, model)


def test_error_guessing_dead_features_case_3() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, False)
    feature_5 = Feature("E", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_3, [feature_4, feature_5], 1, 2)

    feature_3.add_relation(relation_3)

    node_1 = Node("excludes")
    node_1.left = Node("D")
    node_1.right = Node("B")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path("error-guessing/dead-features/case3/df-case3")
    run(path, model)


def test_error_guessing_dead_features_case_4() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 0, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    node_1 = Node("excludes")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path("error-guessing/dead-features/case4/df-case4")
    run(path, model)


def test_error_guessing_dead_features_case_4() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, False)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 0, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    node_1 = Node("excludes")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path("error-guessing/dead-features/case4/df-case4")
    run(path, model)


def test_error_guessing_dead_features_case_5() -> None:
    feature_1 = Feature("A", None, None, False)
    feature_2 = Feature("B", None, None, False)
    feature_3 = Feature("C", None, None, False)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    node_1 = Node("excludes")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path("error-guessing/dead-features/case5/df-case5")
    run(path, model)


def test_error_guessing_dead_features_case_6() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, False)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2, feature_3], 1, 1)

    feature_1.add_relation(relation_1)

    node_1 = Node("requires")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path("error-guessing/dead-features/case6/df-case6")
    run(path, model)


def test_error_guessing_dead_features_case_7() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, False)
    feature_3 = Feature("C", None, None, False)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)

    feature_1.add_relation(relation_1)

    relation_2 = Relation(feature_2, [feature_3], 1, 1)

    feature_2.add_relation(relation_2)

    node_1 = Node("excludes")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path("error-guessing/dead-features/case7/df-case7")
    run(path, model)


def test_error_guessing_dead_features_case_8() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, False)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 0, 1)
    relation_2 = Relation(feature_1, [feature_3], 0, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    node_1 = Node("excludes")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    node_2 = Node("requires")
    node_2.left = Node("B")
    node_2.right = Node("C")
    ast_2 = AST(node_2)
    ctc_2 = Constraint("ctc_1", ast_2)

    model = FeatureModel(feature_1, [ctc_1, ctc_2])
    path = normalize_path("error-guessing/dead-features/case8/df-case8")
    run(path, model)


def test_error_guessing_false_optional_features_case_1() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, False)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 0, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    node_1 = Node("requires")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path(
        "error-guessing/false-optional-features/case1/fof-case1")
    run(path, model)


def test_error_guessing_false_optional_features_case_2() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, False)
    feature_4 = Feature("D", None, None, False)
    feature_5 = Feature("E", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 0, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_3, [feature_4, feature_5], 1, 1)

    feature_3.add_relation(relation_3)

    node_1 = Node("requires")
    node_1.left = Node("B")
    node_1.right = Node("D")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path(
        "error-guessing/false-optional-features/case2/fof-case2")
    run(path, model)


def test_error_guessing_false_optional_features_case_3() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, False)
    feature_4 = Feature("D", None, None, False)
    feature_5 = Feature("E", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 0, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_3, [feature_4, feature_5], 1, 2)

    feature_3.add_relation(relation_3)

    node_1 = Node("requires")
    node_1.left = Node("B")
    node_1.right = Node("D")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path(
        "error-guessing/false-optional-features/case3/fof-case3")
    run(path, model)


def test_error_guessing_false_optional_features_case_4() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, False)

    relation_1 = Relation(feature_1, [feature_2, feature_3], 1, 1)

    feature_1.add_relation(relation_1)

    node_1 = Node("requires")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path(
        "error-guessing/false-optional-features/case4/fof-case4")
    run(path, model)


def test_error_guessing_false_optional_features_case_5() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, False)

    relation_1 = Relation(feature_1, [feature_2, feature_3], 1, 2)

    feature_1.add_relation(relation_1)

    node_1 = Node("requires")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path(
        "error-guessing/false-optional-features/case5/fof-case5")
    run(path, model)


def test_error_guessing_false_optional_features_case_6() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, False)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, True)
    feature_5 = Feature("E", None, None, False)
    feature_6 = Feature("F", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 0, 1)
    relation_2 = Relation(feature_1, [feature_3], 1, 1)
    relation_3 = Relation(feature_1, [feature_4], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)
    feature_1.add_relation(relation_3)

    relation_4 = Relation(feature_3, [feature_5, feature_6], 1, 1)

    feature_3.add_relation(relation_4)

    node_1 = Node("requires")
    node_1.left = Node("E")
    node_1.right = Node("B")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    node_2 = Node("excludes")
    node_2.left = Node("D")
    node_2.right = Node("F")
    ast_2 = AST(node_2)
    ctc_2 = Constraint("ctc_2", ast_2)

    model = FeatureModel(feature_1, [ctc_1, ctc_2])
    path = normalize_path(
        "error-guessing/false-optional-features/case6/fof-case6")
    run(path, model)


def test_error_guessing_redundancies_case_1() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 0, 1)
    relation_2 = Relation(feature_1, [feature_3], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    node_1 = Node("requires")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path(
        "error-guessing/redundancies/case1/r-case1")
    run(path, model)


def test_error_guessing_redundancies_case_2() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 0, 1)
    relation_2 = Relation(feature_1, [feature_3], 1, 1)
    relation_3 = Relation(feature_1, [feature_4], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)
    feature_1.add_relation(relation_3)

    node_1 = Node("requires")
    node_1.left = Node("C")
    node_1.right = Node("B")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    node_2 = Node("requires")
    node_2.left = Node("D")
    node_2.right = Node("B")
    ast_2 = AST(node_2)
    ctc_2 = Constraint("ctc_2", ast_2)

    model = FeatureModel(feature_1, [ctc_1, ctc_2])
    path = normalize_path(
        "error-guessing/redundancies/case2/r-case2")
    run(path, model)


def test_refinement_alternative_no_or() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, True)
    feature_5 = Feature("E", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_5], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_2, [feature_3, feature_4], 1, 1)

    feature_2.add_relation(relation_3)

    node_1 = Node("requires")
    node_1.left = Node("E")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    node_2 = Node("requires")
    node_2.left = Node("E")
    node_2.right = Node("D")
    ast_2 = AST(node_2)
    ctc_2 = Constraint("ctc_2", ast_2)

    model = FeatureModel(feature_1, [ctc_1, ctc_2])
    path = normalize_path(
        "refinement/alternative-noOr/alternative-noOr")
    run(path, model)


def test_refinement_alternative_no_parent_last_child() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, True)
    feature_5 = Feature("E", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 0, 1)
    relation_2 = Relation(feature_1, [feature_5], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_2, [feature_3, feature_4], 1, 1)

    feature_2.add_relation(relation_3)

    node_1 = Node("excludes")
    node_1.left = Node("E")
    node_1.right = Node("B")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    node_2 = Node("requires")
    node_2.left = Node("E")
    node_2.right = Node("D")
    ast_2 = AST(node_2)
    ctc_2 = Constraint("ctc_2", ast_2)

    model = FeatureModel(feature_1, [ctc_1, ctc_2])
    path = normalize_path(
        "refinement/alternative-noParentLastChild/alternative-noParentLastChild")
    run(path, model)


def test_refinement_alternative_odd_children() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, True)
    feature_5 = Feature("E", None, None, True)
    feature_6 = Feature("F", None, None, True)
    feature_7 = Feature("G", None, None, True)
    feature_8 = Feature("H", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_8], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(
        feature_2, [feature_3, feature_4, feature_5, feature_6, feature_7], 1, 1)

    feature_2.add_relation(relation_3)

    node_1 = Node("requires")
    node_1.left = Node("H")
    node_1.right = Node("G")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    node_2 = Node("requires")
    node_2.left = Node("H")
    node_2.right = Node("E")
    ast_2 = AST(node_2)
    ctc_2 = Constraint("ctc_2", ast_2)

    model = FeatureModel(feature_1, [ctc_1, ctc_2])
    path = normalize_path(
        "refinement/alternative-oddChildren/alternative-oddChildren")
    run(path, model)


def test_refinement_df_alternative_excludes() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, True)
    feature_5 = Feature("E", None, None, False)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 0, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_3, [feature_4, feature_5], 1, 1)

    feature_3.add_relation(relation_3)

    node_1 = Node("requires")
    node_1.left = Node("B")
    node_1.right = Node("D")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path(
        "refinement/df-alternative-excludes/df-alternative-excludes")
    run(path, model)


def test_refinement_optional_alternative_valid_p() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 0, 1)

    feature_1.add_relation(relation_1)

    relation_2 = Relation(feature_2, [feature_3, feature_4], 1, 1)

    feature_2.add_relation(relation_2)

    model = FeatureModel(feature_1)
    path = normalize_path(
        "refinement/optional-alternativeValidP/optional-alternativeVP")
    run(path, model)


def test_refinement_or_no_alternative() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, True)
    feature_5 = Feature("E", None, None, False)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_5], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_2, [feature_3, feature_4], 1, 2)

    feature_2.add_relation(relation_3)

    node_1 = Node("requires")
    node_1.left = Node("E")
    node_1.right = Node("D")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    node_2 = Node("requires")
    node_2.left = Node("E")
    node_2.right = Node("C")
    ast_2 = AST(node_2)
    ctc_2 = Constraint("ctc_2", ast_2)

    model = FeatureModel(feature_1, [ctc_1, ctc_2])
    path = normalize_path(
        "refinement/or-noAlternative/or-noAlternative")
    run(path, model)


def test_relationships_allrelationships() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, True)
    feature_5 = Feature("E", None, None, True)
    feature_6 = Feature("F", None, None, True)
    feature_7 = Feature("G", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 0, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_2, [feature_4, feature_5], 1, 1)

    feature_2.add_relation(relation_3)

    relation_4 = Relation(feature_3, [feature_6, feature_7], 1, 2)

    feature_3.add_relation(relation_4)

    node_1 = Node("requires")
    node_1.left = Node("E")
    node_1.right = Node("F")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    node_2 = Node("excludes")
    node_2.left = Node("D")
    node_2.right = Node("G")
    ast_2 = AST(node_2)
    ctc_2 = Constraint("ctc_2", ast_2)

    model = FeatureModel(feature_1, [ctc_1, ctc_2])
    path = normalize_path(
        "relationships/allrelationships/allrelationships")
    run(path, model)


def test_relationships_alternative() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2, feature_3], 1, 1)

    feature_1.add_relation(relation_1)

    model = FeatureModel(feature_1)
    path = normalize_path(
        "relationships/alternative/alternative")
    run(path, model)


def test_relationships_alternative_excludes() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2, feature_3], 1, 1)

    feature_1.add_relation(relation_1)

    node_1 = Node("excludes")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path(
        "relationships/alternative-excludes/alternative-excludes")
    run(path, model)


def test_relationships_alternative_requires() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2, feature_3], 1, 1)

    feature_1.add_relation(relation_1)

    node_1 = Node("requires")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path(
        "relationships/alternative-requires/alternative-requires")
    run(path, model)


def test_relationships_excludes() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 0, 1)
    relation_2 = Relation(feature_1, [feature_3], 0, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    node_1 = Node("excludes")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path(
        "relationships/excludes/excludes")
    run(path, model)


def test_relationships_mandatory() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)

    feature_1.add_relation(relation_1)

    model = FeatureModel(feature_1)
    path = normalize_path(
        "relationships/mandatory/mandatory")
    run(path, model)


def test_relationships_mandatory_alternative() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, True)
    feature_5 = Feature("E", None, None, True)
    feature_6 = Feature("F", None, None, True)
    feature_7 = Feature("G", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3, feature_4], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_2, [feature_5, feature_6], 1, 1)

    feature_2.add_relation(relation_3)

    relation_4 = Relation(feature_3, [feature_7], 1, 1)

    feature_3.add_relation(relation_4)

    model = FeatureModel(feature_1)
    path = normalize_path(
        "relationships/mandatory-alternative/mandatory-alternative")
    run(path, model)


def test_relationships_mandatory_excludes() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    node_1 = Node("excludes")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path(
        "relationships/mandatory-excludes/mandatory-excludes")
    run(path, model)


def test_relationships_mandatory_optional() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, True)
    feature_5 = Feature("E", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 0, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_2, [feature_4], 0, 1)

    feature_2.add_relation(relation_3)

    relation_4 = Relation(feature_3, [feature_5], 1, 1)

    feature_3.add_relation(relation_4)

    model = FeatureModel(feature_1)
    path = normalize_path(
        "relationships/mandatory-optional/mandatory-optional")
    run(path, model)


def test_relationships_mandatory_or() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, True)
    feature_5 = Feature("E", None, None, True)
    feature_6 = Feature("F", None, None, True)
    feature_7 = Feature("G", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3, feature_4], 1, 2)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_2, [feature_5, feature_6], 1, 2)

    feature_2.add_relation(relation_3)

    relation_4 = Relation(feature_4, [feature_7], 1, 1)

    feature_4.add_relation(relation_4)

    model = FeatureModel(feature_1)
    path = normalize_path(
        "relationships/mandatory-or/mandatory-or")
    run(path, model)


def test_relationships_mandatory_requires() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 1, 1)
    relation_2 = Relation(feature_1, [feature_3], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    node_1 = Node("requires")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path(
        "relationships/mandatory-requires/mandatory-requires")
    run(path, model)


def test_relationships_optional() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 0, 1)

    feature_1.add_relation(relation_1)

    model = FeatureModel(feature_1)
    path = normalize_path(
        "relationships/optional/optional")
    run(path, model)


def test_relationships_optional_alternative() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, True)
    feature_5 = Feature("E", None, None, True)
    feature_6 = Feature("F", None, None, True)
    feature_7 = Feature("G", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 0, 1)
    relation_2 = Relation(feature_1, [feature_3, feature_4], 1, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_2, [feature_5, feature_6], 1, 1)

    feature_2.add_relation(relation_3)

    relation_4 = Relation(feature_4, [feature_7], 0, 1)

    feature_4.add_relation(relation_4)

    model = FeatureModel(feature_1)
    path = normalize_path(
        "relationships/optional-alternative/optional-alternative")
    run(path, model)


def test_relationships_optional_or() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, True)
    feature_5 = Feature("E", None, None, True)
    feature_6 = Feature("F", None, None, True)
    feature_7 = Feature("G", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 0, 1)
    relation_2 = Relation(feature_1, [feature_3, feature_4], 1, 2)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_2, [feature_5, feature_6], 1, 2)

    feature_2.add_relation(relation_3)

    relation_4 = Relation(feature_3, [feature_7], 0, 1)

    feature_3.add_relation(relation_4)

    model = FeatureModel(feature_1)
    path = normalize_path(
        "relationships/optional-or/optional-or")
    run(path, model)


def test_relationships_or() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2, feature_3], 1, 2)

    feature_1.add_relation(relation_1)

    model = FeatureModel(feature_1)
    path = normalize_path(
        "relationships/or/or")
    run(path, model)


def test_relationships_or_alternative() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)
    feature_4 = Feature("D", None, None, True)
    feature_5 = Feature("E", None, None, True)
    feature_6 = Feature("F", None, None, True)
    feature_7 = Feature("G", None, None, True)
    feature_8 = Feature("H", None, None, True)
    feature_9 = Feature("I", None, None, True)

    relation_1 = Relation(feature_1, [feature_2, feature_3], 1, 1)
    relation_2 = Relation(feature_1, [feature_4, feature_5], 1, 2)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    relation_3 = Relation(feature_2, [feature_6, feature_7], 1, 2)

    feature_2.add_relation(relation_3)

    relation_4 = Relation(feature_5, [feature_8, feature_9], 1, 1)

    feature_5.add_relation(relation_4)

    model = FeatureModel(feature_1)
    path = normalize_path(
        "relationships/or-alternative/or-alternative")
    run(path, model)


def test_relationships_or_excludes() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2, feature_3], 1, 2)

    feature_1.add_relation(relation_1)

    node_1 = Node("excludes")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path(
        "relationships/or-excludes/or-excludes")
    run(path, model)


def test_relationships_or_requires() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2, feature_3], 1, 2)

    feature_1.add_relation(relation_1)

    node_1 = Node("requires")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path(
        "relationships/or-requires/or-requires")
    run(path, model)


def test_relationships_requires() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 0, 1)
    relation_2 = Relation(feature_1, [feature_3], 0, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    node_1 = Node("requires")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    model = FeatureModel(feature_1, [ctc_1])
    path = normalize_path(
        "relationships/requires/requires")
    run(path, model)


def test_relationships_requires_excludes() -> None:
    feature_1 = Feature("A", None, None, True)
    feature_2 = Feature("B", None, None, True)
    feature_3 = Feature("C", None, None, True)

    relation_1 = Relation(feature_1, [feature_2], 0, 1)
    relation_2 = Relation(feature_1, [feature_3], 0, 1)

    feature_1.add_relation(relation_1)
    feature_1.add_relation(relation_2)

    node_1 = Node("requires")
    node_1.left = Node("B")
    node_1.right = Node("C")
    ast_1 = AST(node_1)
    ctc_1 = Constraint("ctc_1", ast_1)

    node_2 = Node("excludes")
    node_2.left = Node("B")
    node_2.right = Node("C")
    ast_2 = AST(node_2)
    ctc_2 = Constraint("ctc_1", ast_2)

    model = FeatureModel(feature_1, [ctc_1, ctc_2])
    path = normalize_path(
        "relationships/requires-excludes/requires-excludes")
    run(path, model)
