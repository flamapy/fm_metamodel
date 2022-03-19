import setuptools


with open("README.md", "r") as fh:
    long_description = fh.read()


setuptools.setup(
    name="famapy-fm",
    version="0.7.1",
    author="Víctor Ramírez de la Corte",
    author_email="me@virako.es",
    description="famapy-fm is a plugin to FaMaPy module",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/FaMaPy/fm_metamodel",
    packages=setuptools.find_namespace_packages(include=['famapy.*']),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.9',
    install_requires=[
        'famapy>=0.7.0',
        'uvlparser>=0.8.5',
        'afmparser>=0.1.5',
        'antlr-denter>=1.3.1',
    ],
    extras_require={
        'dev': [
            'pytest',
            'pytest-mock',
            'prospector',
            'mypy',
            'coverage',
        ]
    },
    dependency_links=[
        'famapy>=0.7.0'
    ],
)
