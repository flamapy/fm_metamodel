from abc import abstractmethod

from flamapy.core.operations import Operation

from flamapy.metamodels.fm_metamodel.models import Feature


class VariationPoints(Operation):
    """The variation points of a feature model are those features that require to make a choice
    (i.e., select a variant).

    This operation returns the variation points and the variants of each variation point.
    """

    @abstractmethod
    def __init__(self) -> None:
        pass

    @abstractmethod
    def variation_points(self) -> dict[Feature, list[Feature]]:
        pass
