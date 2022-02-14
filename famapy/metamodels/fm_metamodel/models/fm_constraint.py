from typing import Any

from famapy.core.models import AST


class Constraint:

    def __init__(self, name: str, ast: AST):
        self.name = name
        self._ast = ast
        self._clauses = self.ast.get_clauses()
        if len(self._clauses) == 0:
            raise ValueError(f'Error: wrong definition of constraint in AST: {ast}')
        self._sorted_clauses = list(map(sorted, self._clauses))
        self._sorted_clauses.sort(key=len)

    @property
    def ast(self) -> AST:
        return self._ast

    @ast.setter
    def ast(self, ast: AST) -> None:
        self._ast = ast
        self._clauses = self.ast.get_clauses()
        if len(self._clauses) == 0:
            raise ValueError(f'Error: wrong definition of constraint in AST: {ast}')

    def is_simple_constraint(self) -> bool:
        return self.is_requires_constraint() or self.is_excludes_constraint()

    def is_complex_constraint(self) -> bool:
        return self.is_pseudocomplex_constraint() or self.is_strictcomplex_constraint()

    def is_requires_constraint(self) -> bool:
        if len(self._clauses) == 1 and len(self._clauses[0]) == 2:
            nof_negative_clauses = sum(var.startswith('-') for var in self._clauses[0])
            return nof_negative_clauses == 1
        return False

    def is_excludes_constraint(self) -> bool:
        if len(self._clauses) == 1 and len(self._clauses[0]) == 2:
            nof_negative_clauses = sum(var.startswith('-') for var in self._clauses[0])
            return nof_negative_clauses == 2
        return False

    def is_strictcomplex_constraint(self) -> bool:
        if len(self._clauses) == 1 and len(self._clauses[0]) == 2:
            nof_negative_clauses = sum(var.startswith('-') for var in self._clauses[0])
            return nof_negative_clauses == 0
        strictcomplex = False
        i = iter(self._clauses)
        while not strictcomplex and (cls := next(i, None)) is not None:
            if len(cls) != 2:
                strictcomplex = True
            else:
                nof_negative_clauses = sum(var.startswith('-') for var in cls)
                if nof_negative_clauses not in [1, 2]:
                    strictcomplex = True
        return strictcomplex

    def is_pseudocomplex_constraint(self) -> bool:
        if len(self._clauses) == 1:
            return False
        strictcomplex = False
        i = iter(self._clauses)
        while not strictcomplex and (cls := next(i, None)) is not None:
            if len(cls) != 2:
                strictcomplex = True
            else:
                nof_negative_clauses = sum(var.startswith('-') for var in cls)
                if nof_negative_clauses not in [1, 2]:
                    strictcomplex = True
        return not strictcomplex

    def __str__(self) -> str:
        return f'({self.name}) {str(self.ast)}'

    def __hash__(self) -> int:
        return hash(str(self.ast).lower())

    def __eq__(self, other: Any) -> bool:
        return isinstance(other, Constraint) and str(self.ast).lower() == str(other.ast).lower()

    def __lt__(self, other: Any) -> bool:
        return str(self.ast).lower() < str(other.ast).lower()
