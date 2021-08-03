from uvl_transformation import UVLTransformation
from uvl_writter import UVLWriter

uvl_transf = UVLTransformation("test.uvl")
uvl_transf.transform()
model = uvl_transf.model

uvl_writer = UVLWriter(model, "writer_test.uvl")
uvl_writer.transform()
