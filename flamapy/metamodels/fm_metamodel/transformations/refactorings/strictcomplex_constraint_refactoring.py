from typing import Any

from flamapy.core.models.ast import AST, ASTOperation, Node
from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature, Relation, Constraint
from flamapy.metamodels.fm_metamodel.transformations.refactorings import (
    FMRefactoring,
    RefactoringException,
    DeletionFeature,
    CommitmentFeature
)


class StrictComplexConstraintRefactoring(FMRefactoring):
    """It transforms a strict-complex constraint to an additional abstract feature tree and a new
    set of simple constraints."""

    ABSTRACT_AUX_OR_FEATURE_NAME = 'OR_FEATURE'

    def get_name(self) -> str:
        return 'Strict-complex constraint refactoring'

    def get_instances(self) -> list[Constraint]:
        if self.feature_model is None:
            return []
        return self.feature_model.get_strictcomplex_constraints()

    def is_applicable(self) -> bool:
        if self.feature_model is None:
            return False
        return any(ctc.is_strictcomplex_constraint()
                   for ctc in self.feature_model.get_constraints())

    def apply(self, instance: Any) -> FeatureModel | None:
        if self.feature_model is None:
            raise RefactoringException('Feature model is None.')
        if instance is None:
            raise RefactoringException(f'Invalid instance for {self.get_name()}.')
        if not isinstance(instance, Constraint):
            raise RefactoringException(f'Invalid instance for {self.get_name()}.'
                                       f'Expected Constraint, '
                                       f'got {type(instance)} for {instance}.')
        if not instance.is_strictcomplex_constraint():
            raise RefactoringException(f'Constraint {instance.name} is not strict-complex.')

        self.feature_model.ctcs.remove(instance)
        ctcs_names = [ctc.name for ctc in self.feature_model.get_constraints()]
        features_dict = get_features_clauses(instance)  # NOT before negatives (dict)
        if len(features_dict) == 1:
            feature = self.feature_model.get_feature_by_name(next(iter(features_dict.keys())))
            if feature is None:
                raise RefactoringException(f'Feature {next(iter(features_dict.keys()))} \
                                           not found in the feature model.')
            # this is an old method to be avoided
            # feature = self.feature_model.get_feature_by_name(list(features_dict.keys())[0])
            if features_dict[feature.name]:
                commit_feature_op = CommitmentFeature(self.feature_model)
                commit_feature_op.set_feature(feature)
                self.feature_model = commit_feature_op.transform()
            else:
                deletion_feature_op = DeletionFeature(self.feature_model)
                deletion_feature_op.set_feature(feature)
                self.feature_model = deletion_feature_op.transform()
        else:
            new_or = Feature(FMRefactoring.get_new_feature_name(self.feature_model,
                             StrictComplexConstraintRefactoring.ABSTRACT_AUX_OR_FEATURE_NAME),
                             is_abstract=True)
            features = []
            for feat, positive in features_dict.items():
                new_feature = Feature(StrictComplexConstraintRefactoring.get_new_feature_name(
                    self.feature_model, feat), parent=new_or, is_abstract=True)
                features.append(new_feature)
                ast_op = ASTOperation.REQUIRES if positive else ASTOperation.EXCLUDES
                ctc = Constraint(FMRefactoring.get_new_ctc_name(self.feature_model, 'CTC'),
                                 AST.create_binary_operation(ast_op,
                                 Node(new_feature.name), Node(feat)))
                ctcs_names.append(ctc.name)
                self.feature_model.ctcs.append(ctc)

            # New branch with OR as root
            new_or.add_relation(Relation(new_or, features, 1, len(features)))

            # New root (only needed if the root feature is a group)
            if self.feature_model.root.is_group():
                new_root = Feature(FMRefactoring.get_new_feature_name(
                    self.feature_model, 'root'), is_abstract=True)
                # Mandatory relation
                new_root.add_relation(Relation(new_root, [self.feature_model.root], 1, 1))
                self.feature_model.root.parent = new_root
            else:
                new_root = self.feature_model.root
            # Mandatory relation
            new_root.add_relation(Relation(new_root, [new_or], 1, 1))
            new_or.parent = new_root
            self.feature_model.root = new_root
        return self.feature_model


def get_features_clauses(instance: Constraint) -> dict[str, bool]:
    """Returns a dictionary of 'feature name -> bool',
    that sets 'bool' to 'false' if the feature has a negation."""
    features = {}
    clauses = instance.ast.to_cnf()
    stack = [clauses.root]
    while stack:
        node = stack.pop()
        if node.is_unique_term():
            features[node.data] = True
        elif node.is_unary_op():
            features[node.left.data] = False
        elif node.is_binary_op():
            stack.append(node.right)
            stack.append(node.left)
    return features
