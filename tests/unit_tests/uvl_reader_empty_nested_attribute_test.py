"""An empty attribute block inside another attribute ({x {y {}}}) is read as an attribute with no value.

The reader set the value of a nested block inside the loop over its attributes, so an empty block left it
unset: an UnboundLocalError if it came first, the value of the attribute before it otherwise
(flamapy/fm_metamodel#116).
"""
import pytest

from flamapy.metamodels.fm_metamodel.transformations import UVLReader


@pytest.mark.parametrize(
    ('attributes', 'expected'),
    [
        ('{x {y {}}}', {'x.y': None, 'x': None}),
        ('{x {a 5, y {}}}', {'x.a': 5, 'x.y': None, 'x': None}),
        ('{x {y {}, a 5}}', {'x.y': None, 'x.a': 5, 'x': None}),
        ('{x {y {z {}}}}', {'x.y.z': None, 'x.y': None, 'x': None}),
        ('{x {}}', {'x': None}),
    ],
    ids=['first', 'after_a_value', 'before_a_value', 'deeper', 'top_level'],
)
def test_empty_nested_attribute_has_no_value(tmp_path, attributes: str, expected: dict) -> None:
    src = tmp_path / 'model.uvl'
    src.write_text(f'features\n    A {attributes}\n')
    model = UVLReader(str(src)).transform()
    assert {a.name: a.default_value for a in model.root.get_attributes()} == expected
