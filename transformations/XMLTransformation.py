import sys
import xml.etree.ElementTree as ET

from core.transformations.TextToModel import TextToModel
from fm_metamodel.model.FeatureModel import Feature, FeatureModel, Relation

class XMLTransformation(TextToModel):

    def __init__(self,path): 
        self.file=path

    def register(self, extension, metamodel):
        print("This ain't working yet")

    def transform(self):
        
        rootcounter=1

        tree = ET.parse(self.file)
        xml_root = tree.getroot()
       
        #iterate over child of the xml root element
        for child in xml_root:
            if child.tag.casefold() == 'feature':
                rootcounter=rootcounter+1
                self.parseFeature(child)
                
            elif child.tag.casefold() == 'excludes' or child.tag.casefold() == 'requires' :
                print("Fatal error. Not yet supported", file=sys.stderr)

            elif child.tag.casefold() == 'constraint':
                print("Fatal error. Not yet supported", file=sys.stderr)

            else:
                print("This XML contains non supported elements", file=sys.stderr)

    def parseFeature(self,element) -> Feature:
        name= element.attrib.get('name')
        #TODO add a dict to prevent duplicated feature names
        
        feature = Feature(name, [])

        for child in element.getchildren:
            if child.tag.casefold() == 'setRelation' or child.tag.casefold() == 'binaryRelation':
                relation = self.parseRelation(child)
                feature.add_relation(relation)
        return feature

    def parseRelation(self,element) -> Relation:
                
        if element.tar.casefold == 'binaryRelation' :
            numSolitaryFeatures = 0
            r = Relation(parent=None,children=[],card_min=0,card_max=0)    

            for child in element.getchildren:
                if child.tag.casefold == 'solitaryFeature':
                    numSolitaryFeatures=numSolitaryFeatures+1
                    f = self.parseFeature(child)
                    r.children.append(f)
                elif child.tag.casefold == 'cardinality':
                    r.card_min=0
                    r.card_max=0
                else:
                    print("This XML contains non supported elements", file=sys.stderr)

        elif element.tar.casefold == 'setRelation' :
            r = Relation(parent=None,children=[],card_min=0,card_max=0)    
            for child in element.getchildren:
                if child.tag.casefold == 'groupedFeature':
                    f = self.parseFeature(child)
                    r.children.append(f)
                elif child.tag.casefold == 'cardinality':
                    r.card_min=0
                    r.card_max=0
                else:
                    print("This XML contains non supported elements", file=sys.stderr)

        return Relation
