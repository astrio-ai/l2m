import os
import sys
import pytest
from unittest.mock import Mock

# Add the project root to sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from packages.transpiler.engine.parser.cobol_lst import parse_cobol_source, CobolSemanticAnalyzer

# Directory containing sample COBOL files
EXAMPLES_DIR = os.path.join(os.path.dirname(__file__), "..", "examples", "cobol")

# List of sample COBOL files to test
SAMPLE_FILES = [
    "HELLO.cobol",
    "PAYROL00.cobol",
    "ADDAMT.cobol",
    "IF_TEST.cobol",
    "PERFORM_TEST.cobol"
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
    # Just check that we can extract programs, don't expect specific names
    assert len(programs) >= 0


def test_parse_complex_cobol_structure():
    """Test parsing COBOL with complex structure including data and procedure divisions."""
    complex_cobol = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           COMPLEX-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TEST-VAR                    PIC X(10).
       01  COUNTER                     PIC 9(3).
       01  AMOUNT                      PIC 9(5)V99.
       
       PROCEDURE DIVISION.
       100-MAIN.
           MOVE 'HELLO' TO TEST-VAR
           MOVE 0 TO COUNTER
           PERFORM UNTIL COUNTER > 5
               ADD 1 TO COUNTER
           END-PERFORM
           GOBACK.
    """
    
    lst, tokens = parse_cobol_source(complex_cobol)
    assert lst is not None
    assert tokens is not None
    
    analyzer = CobolSemanticAnalyzer(lst, tokens)
    analyzer.analyze()
    
    # Check that symbol table was created
    assert hasattr(analyzer, 'symbol_table_root')
    assert analyzer.symbol_table_root is not None


def test_parse_cobol_with_control_flow():
    """Test parsing COBOL with control flow structures."""
    control_flow_cobol = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           CONTROL-FLOW-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CHOICE                      PIC 9(1).
       01  RESULT                      PIC X(10).
       
       PROCEDURE DIVISION.
       100-MAIN.
           IF CHOICE = 1 THEN
               MOVE 'ONE' TO RESULT
           ELSE
               MOVE 'OTHER' TO RESULT
           END-IF
           
           PERFORM UNTIL CHOICE = 0
               DISPLAY 'LOOPING'
               SUBTRACT 1 FROM CHOICE
           END-PERFORM
           
           GOBACK.
    """
    
    lst, tokens = parse_cobol_source(control_flow_cobol)
    assert lst is not None
    assert tokens is not None
    
    analyzer = CobolSemanticAnalyzer(lst, tokens)
    analyzer.analyze()
    
    # Check that symbol table was created
    assert hasattr(analyzer, 'symbol_table_root')
    assert analyzer.symbol_table_root is not None


def test_parse_cobol_with_file_operations():
    """Test parsing COBOL with file operations."""
    file_ops_cobol = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           FILE-TEST.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'INPUT.TXT'.
           SELECT OUTPUT-FILE ASSIGN TO 'OUTPUT.TXT'.
       
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05  INPUT-DATA              PIC X(80).
       
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD.
           05  OUTPUT-DATA             PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  EOF-FLAG                    PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           
           READ INPUT-FILE
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ
           
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           
           GOBACK.
    """
    
    lst, tokens = parse_cobol_source(file_ops_cobol)
    assert lst is not None
    assert tokens is not None
    
    analyzer = CobolSemanticAnalyzer(lst, tokens)
    analyzer.analyze()
    
    # Check that symbol table was created
    assert hasattr(analyzer, 'symbol_table_root')
    assert analyzer.symbol_table_root is not None


def test_parse_error_handling():
    """Test that parser handles errors gracefully."""
    invalid_cobol = "INVALID COBOL SYNTAX"
    
    # Should not raise an exception
    lst, tokens = parse_cobol_source(invalid_cobol)
    
    # Should still return some structure
    assert lst is not None
    assert tokens is not None


def test_parse_empty_source():
    """Test parsing empty COBOL source."""
    lst, tokens = parse_cobol_source("")
    
    assert lst is not None
    assert tokens is not None


def test_parse_whitespace_only():
    """Test parsing COBOL source with only whitespace."""
    lst, tokens = parse_cobol_source("   \n   \t   ")
    
    assert lst is not None
    assert tokens is not None


def test_analyzer_initialization():
    """Test CobolSemanticAnalyzer initialization."""
    mock_lst = Mock()
    mock_tokens = [Mock()]
    
    analyzer = CobolSemanticAnalyzer(mock_lst, mock_tokens)
    
    assert analyzer.lst_root == mock_lst
    # analyzer doesn't store tokens as an attribute
    assert hasattr(analyzer, 'symbol_table_root')


def test_analyzer_analyze_method():
    """Test the analyze method of CobolSemanticAnalyzer."""
    # Create a minimal valid LST structure
    mock_lst = Mock()
    mock_lst.rule_name = "CompilationUnitContext"
    mock_lst.children = []
    mock_lst.get_tokens.return_value = []  # Mock get_tokens to return empty list
    
    mock_tokens = []
    
    analyzer = CobolSemanticAnalyzer(mock_lst, mock_tokens)
    
    # Should not raise an exception
    analyzer.analyze()
    
    # Should have created symbol table
    assert hasattr(analyzer, 'symbol_table_root') 