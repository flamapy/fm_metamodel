# Generated from UVL.g4 by ANTLR 4.9
from antlr4 import *
if __name__ is not None and "." in __name__:
    from .UVLParser import UVLParser
else:
    from UVLParser import UVLParser

# This class defines a complete listener for a parse tree produced by UVLParser.


class UVLListener(ParseTreeListener):

    # Enter a parse tree produced by UVLParser#feature_model.
    def enterFeature_model(self, ctx: UVLParser.Feature_modelContext):
        pass

    # Exit a parse tree produced by UVLParser#feature_model.
    def exitFeature_model(self, ctx: UVLParser.Feature_modelContext):
        pass

    # Enter a parse tree produced by UVLParser#features.
    def enterFeatures(self, ctx: UVLParser.FeaturesContext):
        root_feature = ctx.child()

        relations = root_feature.relation()
        for relation in relations:
            print(relation.relation_spec().RELATION_WORD().getText())
    # Exit a parse tree produced by UVLParser#features.

    def exitFeatures(self, ctx: UVLParser.FeaturesContext):
        pass

    # Enter a parse tree produced by UVLParser#child.
    def enterChild(self, ctx: UVLParser.ChildContext):
        pass

    # Exit a parse tree produced by UVLParser#child.
    def exitChild(self, ctx: UVLParser.ChildContext):
        pass

    # Enter a parse tree produced by UVLParser#relation.
    def enterRelation(self, ctx: UVLParser.RelationContext):
        pass

    # Exit a parse tree produced by UVLParser#relation.
    def exitRelation(self, ctx: UVLParser.RelationContext):
        pass

    # Enter a parse tree produced by UVLParser#feature_spec.
    def enterFeature_spec(self, ctx: UVLParser.Feature_specContext):
        pass

    # Exit a parse tree produced by UVLParser#feature_spec.
    def exitFeature_spec(self, ctx: UVLParser.Feature_specContext):
        pass

    # Enter a parse tree produced by UVLParser#ref.
    def enterRef(self, ctx: UVLParser.RefContext):
        pass

    # Exit a parse tree produced by UVLParser#ref.
    def exitRef(self, ctx: UVLParser.RefContext):
        pass

    # Enter a parse tree produced by UVLParser#attributes.
    def enterAttributes(self, ctx: UVLParser.AttributesContext):
        pass

    # Exit a parse tree produced by UVLParser#attributes.
    def exitAttributes(self, ctx: UVLParser.AttributesContext):
        pass

    # Enter a parse tree produced by UVLParser#attribute.
    def enterAttribute(self, ctx: UVLParser.AttributeContext):
        pass

    # Exit a parse tree produced by UVLParser#attribute.
    def exitAttribute(self, ctx: UVLParser.AttributeContext):
        pass

    # Enter a parse tree produced by UVLParser#key.
    def enterKey(self, ctx: UVLParser.KeyContext):
        pass

    # Exit a parse tree produced by UVLParser#key.
    def exitKey(self, ctx: UVLParser.KeyContext):
        pass

    # Enter a parse tree produced by UVLParser#value.
    def enterValue(self, ctx: UVLParser.ValueContext):
        pass

    # Exit a parse tree produced by UVLParser#value.
    def exitValue(self, ctx: UVLParser.ValueContext):
        pass

    # Enter a parse tree produced by UVLParser#relation_spec.
    def enterRelation_spec(self, ctx: UVLParser.Relation_specContext):
        pass

    # Exit a parse tree produced by UVLParser#relation_spec.
    def exitRelation_spec(self, ctx: UVLParser.Relation_specContext):
        pass

    # Enter a parse tree produced by UVLParser#constraints.
    def enterConstraints(self, ctx: UVLParser.ConstraintsContext):
        pass

    # Exit a parse tree produced by UVLParser#constraints.
    def exitConstraints(self, ctx: UVLParser.ConstraintsContext):
        pass

    # Enter a parse tree produced by UVLParser#constraint.
    def enterConstraint(self, ctx: UVLParser.ConstraintContext):
        pass

    # Exit a parse tree produced by UVLParser#constraint.
    def exitConstraint(self, ctx: UVLParser.ConstraintContext):
        pass

    # Enter a parse tree produced by UVLParser#negation.
    def enterNegation(self, ctx: UVLParser.NegationContext):
        pass

    # Exit a parse tree produced by UVLParser#negation.
    def exitNegation(self, ctx: UVLParser.NegationContext):
        pass

    # Enter a parse tree produced by UVLParser#conjunction.
    def enterConjunction(self, ctx: UVLParser.ConjunctionContext):
        pass

    # Exit a parse tree produced by UVLParser#conjunction.
    def exitConjunction(self, ctx: UVLParser.ConjunctionContext):
        pass

    # Enter a parse tree produced by UVLParser#disjuction.
    def enterDisjuction(self, ctx: UVLParser.DisjuctionContext):
        pass

    # Exit a parse tree produced by UVLParser#disjuction.
    def exitDisjuction(self, ctx: UVLParser.DisjuctionContext):
        pass

    # Enter a parse tree produced by UVLParser#implication.
    def enterImplication(self, ctx: UVLParser.ImplicationContext):
        pass

    # Exit a parse tree produced by UVLParser#implication.
    def exitImplication(self, ctx: UVLParser.ImplicationContext):
        pass

    # Enter a parse tree produced by UVLParser#equivalence.
    def enterEquivalence(self, ctx: UVLParser.EquivalenceContext):
        pass

    # Exit a parse tree produced by UVLParser#equivalence.
    def exitEquivalence(self, ctx: UVLParser.EquivalenceContext):
        pass

    # Enter a parse tree produced by UVLParser#imports.
    def enterImports(self, ctx: UVLParser.ImportsContext):
        pass

    # Exit a parse tree produced by UVLParser#imports.
    def exitImports(self, ctx: UVLParser.ImportsContext):
        pass

    # Enter a parse tree produced by UVLParser#imp.
    def enterImp(self, ctx: UVLParser.ImpContext):
        pass

    # Exit a parse tree produced by UVLParser#imp.
    def exitImp(self, ctx: UVLParser.ImpContext):
        pass


del UVLParser
