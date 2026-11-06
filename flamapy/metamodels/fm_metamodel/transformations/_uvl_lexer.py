"""Work around a lexer slowdown in uvlparser <= 2.5.0.

The generated UVL lexer's NEWLINE rule starts with the semantic predicate
``{self.atStartOfInput()}?``. ANTLR never caches the lexer's DFA start state when a predicate
is reachable from it, so every token recomputes the full ATN closure (~6x slower parsing).

The predicate is ``line == 1 and column == 0``, so it can only hold on the very first token.
The simulator below bypasses the start-state cache on that token and caches it everywhere
else, where the predicate is false and the closure does not depend on the position. Lexing
results are identical, including the rejection of an indented first line.

Once uvlparser ships a lexer without the predicate this becomes a no-op and can be removed.
"""
from typing import Any

from antlr4 import InputStream
from antlr4.PredictionContext import PredictionContextCache
from antlr4.atn.LexerATNSimulator import LexerATNSimulator
from antlr4.dfa.DFA import DFA
from uvl.UVLCustomLexer import UVLCustomLexer
from uvl.UVLPythonLexer import UVLPythonLexer

NEEDS_WORKAROUND = hasattr(UVLPythonLexer, 'NEWLINE_sempred')

# A private DFA cache for the patched lexers. ANTLR shares UVLPythonLexer.decisionsToDFA across
# all lexer instances; filling its start state there would leak into unpatched UVL lexers,
# which would then use it on the first token and skip the predicate.
_DECISIONS_TO_DFA = [DFA(state, index)
                     for index, state in enumerate(UVLPythonLexer.atn.decisionToState)]


class _StartStateCachingSimulator(LexerATNSimulator):

    # Parameter names follow LexerATNSimulator (pylint: arguments-renamed otherwise).
    # pylint: disable=redefined-builtin
    def match(self, input: InputStream, mode: int) -> int:
        if self.line == 1 and self.column == 0:
            # The only position where atStartOfInput() holds: neither read nor fill the cache.
            self.mode = mode
            mark = input.mark()
            try:
                self.startIndex = input.index
                self.prevAccept.reset()
                return int(self.execATN(input, self._start_state(input)))
            finally:
                input.release(mark)
        return int(super().match(input, mode))

    def matchATN(self, input: InputStream) -> int:
        # Past the first token the predicate is false, so the start state is safe to cache.
        start = self._start_state(input)
        self.decisionToDFA[self.mode].s0 = start
        return int(self.execATN(input, start))

    def _start_state(self, input: InputStream) -> Any:
        closure = self.computeStartState(input, self.atn.modeToStartState[self.mode])
        closure.hasSemanticContext = False
        return self.addDFAState(closure)


def uvl_lexer(input_stream: InputStream) -> UVLCustomLexer:
    """A UVL lexer, with the start-state cache workaround when the installed parser needs it."""
    lexer = UVLCustomLexer(input_stream)
    if NEEDS_WORKAROUND:
        lexer._interp = _StartStateCachingSimulator(  # pylint: disable=protected-access
            lexer, lexer.atn, _DECISIONS_TO_DFA, PredictionContextCache())
    return lexer
