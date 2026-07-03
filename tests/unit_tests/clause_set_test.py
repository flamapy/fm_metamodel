import os
import tempfile

from flamapy.metamodels.fm_metamodel.models import ClauseSet
from flamapy.metamodels.fm_metamodel.transformations import UVLReader

_UVL = """features
    Root {abstract}
        mandatory
            Base
        optional
            A
        alternative
            X
            Y
constraints
    A => X
    X => !Y
"""


def _fm():
    handle, path = tempfile.mkstemp(suffix='.uvl')
    try:
        with os.fdopen(handle, 'w') as file:
            file.write(_UVL)
        return UVLReader(path).transform()
    finally:
        os.remove(path)


def test_maps_are_consistent_and_root_is_a_unit_clause() -> None:
    cs = ClauseSet.from_feature_model(_fm())
    # name <-> id bijection over the feature variables
    assert {cs.features[i] for i in cs.variables.values()} == set(cs.variables)
    assert set(cs.feature_variables()) == set(cs.features)
    # the root is asserted as a unit clause
    root_id = cs.variables['Root']
    assert [root_id] in cs.clauses


def test_distributive_has_no_auxiliaries() -> None:
    cs = ClauseSet.from_feature_model(_fm(), cnf_method='distributive')
    assert cs.auxiliary_variables == set()


def test_tseytin_introduces_auxiliaries() -> None:
    cs = ClauseSet.from_feature_model(_fm(), cnf_method='tseytin')
    assert cs.auxiliary_variables  # non-empty
    # aux ids are outside the feature id range
    assert cs.auxiliary_variables.isdisjoint(cs.features)


def test_clause_groups_cover_all_clauses() -> None:
    cs = ClauseSet.from_feature_model(_fm())
    grouped = [clause for _, clauses in cs.clause_groups for clause in clauses]
    assert grouped == cs.clauses  # provenance groups reproduce the flat clause list, in order
