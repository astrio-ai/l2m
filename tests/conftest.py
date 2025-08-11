"""
Pytest configuration and shared fixtures for the legacy2modern test suite.
"""

import os
import sys
import pytest
from unittest.mock import Mock

# Add the project root to sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))


@pytest.fixture
def sample_cobol_files():
    """Fixture providing list of sample COBOL files for testing."""
    examples_dir = os.path.join(os.path.dirname(__file__), "..", "examples", "cobol")
    return [
        os.path.join(examples_dir, "HELLO.cobol"),
        os.path.join(examples_dir, "PAYROL00.cobol"),
        os.path.join(examples_dir, "ADDAMT.cobol"),
        os.path.join(examples_dir, "IF_TEST.cobol"),
        os.path.join(examples_dir, "PERFORM_TEST.cobol")
    ]


@pytest.fixture
def mock_lossless_node():
    """Fixture providing a mock LosslessNode."""
    from engine.modernizers.cobol_system.parsers.cobol_lst import LosslessNode
    return Mock(spec=LosslessNode)


@pytest.fixture
def mock_token():
    """Fixture providing a mock Token."""
    from engine.modernizers.cobol_system.parsers.cobol_lst import Token
    return Mock(spec=Token)


@pytest.fixture
def simple_cobol_source():
    """Fixture providing a simple COBOL source for testing."""
    return """
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           TEST-PROG.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TEST-VAR                    PIC X(10).
       
       PROCEDURE DIVISION.
       100-MAIN.
           DISPLAY 'HELLO WORLD'
           GOBACK.
    """


@pytest.fixture
def if_statement_cobol():
    """Fixture providing COBOL source with IF statement."""
    return """
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           IF-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CHOICE                      PIC 9(1).
       
       PROCEDURE DIVISION.
       100-MAIN.
           IF CHOICE = 1 THEN
               DISPLAY 'ONE'
           ELSE
               DISPLAY 'OTHER'
           END-IF
           GOBACK.
    """


@pytest.fixture
def perform_loop_cobol():
    """Fixture providing COBOL source with PERFORM loop."""
    return """
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           PERFORM-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  COUNTER                     PIC 9(3).
       
       PROCEDURE DIVISION.
       100-MAIN.
           PERFORM UNTIL COUNTER > 5
               ADD 1 TO COUNTER
           END-PERFORM
           GOBACK.
    """


@pytest.fixture
def test_variables():
    """Fixture providing test variables dictionary."""
    return {
        "TEST-VAR": {"type": "string", "pic": "X(10)"},
        "COUNTER": {"type": "number", "pic": "9(3)"},
        "AMOUNT": {"type": "number", "pic": "9(5)V99"},
        "CHOICE": {"type": "number", "pic": "9(1)"},
        "MORE-DATA": {"type": "string", "pic": "X(3)"}
    }


@pytest.fixture
def mock_rule_engine():
    """Fixture providing a mock rule engine."""
    from engine.modernizers.cobol_system.rules.rule_engine import RuleEngine
    return Mock(spec=RuleEngine)


@pytest.fixture
def mock_base_rule():
    """Fixture providing a mock base rule."""
    from engine.modernizers.cobol_system.rules.base_rule import BaseRule
    return Mock(spec=BaseRule) 