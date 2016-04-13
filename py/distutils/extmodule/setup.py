from distutils.core import setup, Extension

setup(
  name="noddy",
  version="1.0",
  author="kasicass",
  author_email="kasicass@163.com",
  ext_modules=[Extension(
    "noddy",
    ["noddy.c"],
  )]
)
