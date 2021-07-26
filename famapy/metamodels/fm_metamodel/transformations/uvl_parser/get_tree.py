import sys
from antlr4 import *
from UVLLexer import UVLLexer
from UVLParser import UVLParser


def get_tree(argv):
    input_stream = FileStream(argv)
    lexer = UVLLexer(input_stream)
    stream = CommonTokenStream(lexer)
    parser = UVLParser(stream)
    tree = parser.feature_model()

    return tree
