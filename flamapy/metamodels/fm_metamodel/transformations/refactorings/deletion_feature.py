from typing import cast

from flamapy.core.models import VariabilityModel
from flamapy.core.transformations import ModelToModel

from flamapy.metamodels.fm_metamodel.models import FeatureModel, Feature


class DeletionFeature(ModelToModel):
    """Given a feature diagram T and a feature F, this algorithm computes T(-F)
    whose products are precisely those products of T with do not contain F.

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
        if self.feature is None:
            raise ValueError("Feature to delete must be set before transformation.")
        feature = self.feature
        if feature is not None:  # Step 1. If T does not contain F, the result is T.
            feature_to_delete = feature
            parent = feature_to_delete.get_parent()  # Step 2. Let the parent feature of F be P.
            # Step 3. If P is a MandOpt feature and F is a mandatory subfeature of P,
            # GOTO step 4 with P instead of F.
            while (feature_to_delete != self.feature_model.root and parent is not None and
                    not parent.is_group() and
                    feature_to_delete.is_mandatory()):
                feature_to_delete = parent
                parent = feature_to_delete.get_parent()
            # If F is the root of T, the result is NIL.
            if feature_to_delete == self.feature_model.root:
                return None
            # If P is a MandOpt feature and F is an optional subfeature of P, delete F.
            if parent is not None and not parent.is_group() and feature_to_delete.is_optional():
                rel = next((r for r in parent.get_relations() if feature_to_delete in r.children),
                           None)
                if rel is not None:
                    parent.get_relations().remove(rel)  # Delete feature branch
            # If P is an Xor feature or an Or feature, delete F;
            # if P has only one remaining subfeature,
            # make P a MandOpt feature and its subfeature a mandatory subfeature.
            elif parent is not None and (parent.is_alternative_group() or parent.is_or_group()):
                rel = parent.get_relations()[0]
                rel.children.remove(feature_to_delete)  # Delete feature branch
                if rel.card_max > 1:
                    rel.card_max -= 1
        return self.feature_model
