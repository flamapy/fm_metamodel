import setuptools


with open("README.md", "r") as fh:
    long_description = fh.read()


setuptools.setup(
    name="flamapy-fm",
    version="2.0.0.dev0",
    author="Flamapy",
    author_email="flamapy@us.es",
    description="flamapy-fm is a plugin to Flamapy module",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/flamapy/fm_metamodel",
    packages=setuptools.find_namespace_packages(include=['flamapy.*']),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.9',
    install_requires=[
        'flamapy-fw~=2.0.0.dev0',
        'uvlparser~=2.0.1',
        'afmparser~=1.0.3',
    ],
    extras_require={
        'dev': [
            'pytest',
            'pytest-mock',
            'prospector',
            'mypy',
            'coverage',            
            'antlr4-tools',
        ]
    },
    dependency_links=[
        'flamapy~=1.6.0.dev0',
    ],
)
