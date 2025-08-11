"""
Setup script for Legacy2Modern CLI
"""

from setuptools import setup, find_packages
import os

# Read the README file
def read_readme():
    with open("README.md", "r", encoding="utf-8") as fh:
        return fh.read()

# Read requirements
def read_requirements():
    with open("requirements.txt", "r", encoding="utf-8") as fh:
        return [line.strip() for line in fh if line.strip() and not line.startswith("#")]

setup(
    name="legacy2modern",
    version="0.1.0",
    author="Naing Oo Lwin",
    author_email="naingoolwin.astrio@gmail.com",
    description="AI-Powered Legacy Code Transpilation Engine with Modern CLI",
    long_description=read_readme(),
    long_description_content_type="text/markdown",
    url="https://github.com/astrio-ai/legacy2modern",
    packages=find_packages(),
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: Apache Software License",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Topic :: Software Development :: Code Generators",
        "Topic :: Software Development :: Compilers",
        "Topic :: Software Development :: Libraries :: Python Modules",
    ],
    python_requires=">=3.10",
    install_requires=read_requirements(),
    entry_points={
        "console_scripts": [
            "legacy2modern=engine.cli.cli:main",
            "l2m=engine.cli.cli:main",
        ],
    },
    scripts=[
        "scripts/legacy2modern",
        "scripts/l2m",
    ],
    include_package_data=True,
    zip_safe=False,
    keywords="cobol, transpiler, legacy, modernization, python, cli",
    project_urls={
        "Bug Reports": "https://github.com/astrio-ai/legacy2modern/issues",
        "Source": "https://github.com/astrio-ai/legacy2modern",
        "Documentation": "https://github.com/astrio-ai/legacy2modern#readme",
    },
) 