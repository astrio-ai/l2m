import os
import sys
import pytest

# Add the project root to sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from packages.transpiler.engine.cobol_lst import parse_cobol_source, CobolSemanticAnalyzer

# Directory containing sample COBOL files
EXAMPLES_DIR = os.path.join(os.path.dirname(__file__), "..", "examples", "cobol")

# List of sample COBOL files to test (add more as needed)
SAMPLE_FILES = [
    "HELLO.cobol",
    # Add more sample files here, e.g. "PAYROL00.cobol"
]

def load_cobol_file(filename):
    with open(os.path.join(EXAMPLES_DIR, filename), "r") as f:
        return f.read()

@pytest.mark.parametrize("filename", SAMPLE_FILES)
def test_parse_cobol_file(filename):
    """Test that COBOL files can be parsed into an LST and token stream."""
    source = load_cobol_file(filename)
    lst, tokens = parse_cobol_source(source)
    assert lst is not None
    assert tokens is not None

@pytest.mark.parametrize("filename", SAMPLE_FILES)
def test_symbol_table_extraction(filename):
    """Test that the symbol table is extracted and contains at least one program symbol."""
    source = load_cobol_file(filename)
    lst, tokens = parse_cobol_source(source)
    analyzer = CobolSemanticAnalyzer(lst, tokens)
    analyzer.analyze()
    programs = analyzer.symbol_table_root.find_all_kind("program")
    assert len(programs) >= 1
    # Optionally, check for specific program names (case-insensitive)
    names = [p.name.upper() for p in programs]
    assert any("HELLO" in n for n in names) 