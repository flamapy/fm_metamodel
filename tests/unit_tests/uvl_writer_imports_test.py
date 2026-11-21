import os

from flamapy.metamodels.fm_metamodel.transformations import UVLReader, UVLWriter


MAIN = """imports
    lib.sub as s
features
    Root
        optional
            s.A
"""
SUB = """features
    A
        optional
            B
"""


def _write_models(directory):
    (directory / "lib").mkdir(parents=True)
    (directory / "main.uvl").write_text(MAIN)
    (directory / "lib" / "sub.uvl").write_text(SUB)
    return UVLReader(str(directory / "main.uvl")).transform()


def _files(directory):
    return sorted(os.path.relpath(os.path.join(root, name), directory)
                  for root, _, names in os.walk(directory) for name in names)


def test_serializing_without_a_path_writes_no_file(tmp_path, monkeypatch):
    model = _write_models(tmp_path / "models")
    cwd = tmp_path / "cwd"
    cwd.mkdir()
    monkeypatch.chdir(cwd)
    text = UVLWriter(path=None, source_model=model).transform()
    assert "lib.sub as s" in text and "\tA" in text  # the imported model is serialized too
    assert _files(cwd) == []


def test_hashing_a_model_with_imports_writes_no_file(tmp_path, monkeypatch):
    # canonical_hash runs on every facade operation: it must not touch the disk, least of
    # all overwrite the imported models when the working directory is the model's own.
    models = tmp_path / "models"
    model = _write_models(models)
    monkeypatch.chdir(models)
    assert model.canonical_hash()
    assert _files(models) == [os.path.join("lib", "sub.uvl"), "main.uvl"]
    assert (models / "lib" / "sub.uvl").read_text() == SUB


def test_imported_models_are_written_next_to_the_importing_file(tmp_path, monkeypatch):
    model = _write_models(tmp_path / "models")
    cwd = tmp_path / "cwd"
    cwd.mkdir()
    monkeypatch.chdir(cwd)
    out = tmp_path / "out"
    out.mkdir()
    UVLWriter(str(out / "copy.uvl"), model).transform()
    assert _files(cwd) == []
    assert _files(out) == ["copy.uvl", os.path.join("lib", "sub.uvl")]
    # ... which is where the reader resolves the import, so the copy reads back.
    copy = UVLReader(str(out / "copy.uvl")).transform()
    assert [f.name for f in copy.imports["lib.sub"].get_features()] == ["A", "B"]
