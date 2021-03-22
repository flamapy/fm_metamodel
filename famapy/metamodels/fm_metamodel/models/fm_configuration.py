from famapy.core.models import Configuration


class FMConfiguration(Configuration):

    def __init__(self, elements: list):
        self.elements = elements
