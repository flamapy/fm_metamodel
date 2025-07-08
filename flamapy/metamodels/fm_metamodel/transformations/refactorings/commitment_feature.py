from typing import cast

from flamapy.core.models import VariabilityModel
from flamapy.core.transformations import ModelToModel

from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature, Relation


class CommitmentFeature(ModelToModel):
    """Given a feature diagram T and a feature F, this algorithm computes T(+F)
    whose products are precisely those products of T with contain F.

    The algorithm is an adaptation from:
        [Broek2008 @ SPLC: Elimination of constraints from feature trees].
    """

    @staticmethod
    def get_source_extension() -> str:
        return 'fm'

    @staticmethod
    def get_destination_extension() -> str:
        return 'fm'

    def __init__(self, source_model: VariabilityModel) -> None:
        self.feature_model = cast(FeatureModel, source_model)
        self.feature: Feature | None = None

    def set_feature(self, feature: Feature) -> None:
        self.feature = feature

    def transform(self) -> FeatureModel | None:
        feature = self.feature
        # Step 1. If T does not contain F, the result is NIL.
        if feature is None:
            return None
        feature_to_commit = feature
        # Step 2. If F is the root of T, the result is T.
        while feature_to_commit != self.feature_model.root:
            # Step 3. Let the parent feature of F be P.
            parent = feature_to_commit.get_parent()
            # If P is a MandOpt feature and F is an optional subfeature,
            # make F a mandatory subfeature of P.
            if parent is not None and not parent.is_group() and feature_to_commit.is_optional():
                rel = next((r for r in parent.get_relations()
                            if feature_to_commit in r.children), None)
                if rel is not None:
                    rel.card_min = 1
            # If P is an Xor feature,
            # make P a MandOpt feature which has F as single mandatory subfeature
            # and has no optional subfeatures. All other subfeatures of P are removed from
            # the tree.
            elif parent is not None and parent.is_alternative_group():
                # Delete feature branch
                parent.get_relations()[0].children = [feature_to_commit]
            # If P is an Or feature,
            # make P a MandOpt feature which has F as single mandatory subfeature,
            # and has all other subfeatures of P as optional subfeatures.
            elif parent is not None and parent.is_or_group():
                parent_relations = parent.get_relations()
                or_relation = parent_relations[0]
                or_relation.children.remove(feature_to_commit)
                parent_relations.remove(or_relation)
                new_mandatory_rel = Relation(parent, [feature_to_commit], 1, 1)
                parent_relations.append(new_mandatory_rel)
                for child in or_relation.children:
                    new_optional_rel = Relation(parent, [child], 0, 1)
                    parent_relations.append(new_optional_rel)
            # Step 4. GOTO step 2 with P instead of F.
            if parent is not None:
                feature_to_commit = parent
        return self.feature_model
