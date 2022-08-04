import csv

from flamapy.core.transformations import TextToModel
from flamapy.metamodels.fm_metamodel.models import FeatureModel


class AttributesCSVReader(TextToModel):
    """Reader for feature attributes in .csv.

    The csv format is as follow:

    Feature, Attribute1, Attribute2, Attribute3,...
    featureAname, valueA1, valueA2, valueA3,...
    featureBname, valueB1, valueB2, valueB3,...
    ...
    """

    @staticmethod
    def get_source_extension() -> str:
        return 'csv'

    def __init__(self, path: str, model: FeatureModel) -> None:
        self.path = path
        self.model = model

    def transform(self) -> FeatureModel:
        with open(self.path, newline='', encoding='utf-8') as csvfile:
            reader = csv.DictReader(csvfile, delimiter=',', quotechar='"', skipinitialspace=True)
            for row in reader:
                print(row)
