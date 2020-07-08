import sys
import xml.etree.ElementTree as ET

from core.transformations.TextToModel import TextToModel
from fm_metamodel.model.FeatureModel import Feature, FeatureModel, Relation

class XMLTransformation(TextToModel):

    def __init__(self,path): 
        self.file=path
        self.features_names=[]
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
                root=self.parseFeature(child)
                fm = FeatureModel(root, [])

            elif child.tag.casefold() == 'excludes' or child.tag.casefold() == 'requires' :
                print("Fatal error. REQUIRES. Not yet supported", file=sys.stderr)

            else:
                print("This XML contains non supported elements", file=sys.stderr)
        return fm

    def parseFeature(self,element) -> Feature:
        name= element.attrib.get('name')
        
        if name in self.features_names:
            print("This XML contains duplicated feature names", file=sys.stderr)
        else:
            self.features_names.append(name)

        feature = Feature(name, [])

        for child in element:
            if child.tag.casefold() == 'setrelation' or child.tag.casefold() == 'binaryrelation':
                relation = self.parseRelation(child)
                relation.parent=feature
                feature.relations.append(relation)

        return feature

    def parseRelation(self,element) -> Relation:
        r = Relation(parent=None,children=[],card_min=0,card_max=0)    
                
        if element.tag.casefold() == 'binaryrelation' :
            numSolitaryFeatures = 0

            for child in element:
                if child.tag.casefold() == 'solitaryfeature':
                    numSolitaryFeatures=numSolitaryFeatures+1
                    f = self.parseFeature(child)
                    r.children.append(f)
                elif child.tag.casefold() == 'cardinality':
                    r.card_min=int(child.attrib.get('min'))
                    r.card_max=int(child.attrib.get('max'))
                else:
                    print("This XML contains non supported elements", file=sys.stderr)

        elif element.tag.casefold() == 'setrelation' :
            for child in element:
                if child.tag.casefold() == 'groupedfeature':
                    f = self.parseFeature(child)
                    r.children.append(f)
                elif child.tag.casefold() == 'cardinality':
                    r.card_min=int(child.attrib.get('min'))
                    r.card_max=int(child.attrib.get('max'))
                else:
                    print("This XML contains non supported elements", file=sys.stderr)

        return r
