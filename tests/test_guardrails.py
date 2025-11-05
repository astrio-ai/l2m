"""Tests for guardrail functionality."""

import pytest
import tempfile
from pathlib import Path
from src.guardrails.cobol_input_guard import validate_cobol_input
from src.guardrails.python_output_guard import validate_python_output


class TestCOBOLInputGuard:
    """Tests for COBOL input validation."""
    
    def test_validate_cobol_input_not_found(self):
        """Test validation of non-existent file."""
        is_valid, error = validate_cobol_input("nonexistent.cbl")
        assert is_valid is False
        assert error is not None
        assert "not found" in error
    
    def test_validate_cobol_input_valid(self):
        """Test validation of valid COBOL file."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.cbl', delete=False) as f:
            f.write("       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.")
            temp_file = f.name
        
        try:
            is_valid, error = validate_cobol_input(temp_file)
            assert is_valid is True
            assert error is None
        finally:
            Path(temp_file).unlink()
    
    def test_validate_cobol_input_empty(self):
        """Test validation of empty file."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.cbl', delete=False) as f:
            f.write("")
            temp_file = f.name
        
        try:
            is_valid, error = validate_cobol_input(temp_file)
            assert is_valid is False
            assert "empty" in error.lower()
        finally:
            Path(temp_file).unlink()
    
    def test_validate_cobol_input_wrong_extension(self):
        """Test validation of file with non-standard extension."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("       IDENTIFICATION DIVISION.")
            temp_file = f.name
        
        try:
            # Should still validate (just warns)
            is_valid, error = validate_cobol_input(temp_file)
            assert is_valid is True  # Extension is a warning, not an error
        finally:
            Path(temp_file).unlink()


class TestPythonOutputGuard:
    """Tests for Python output validation."""
    
    def test_validate_python_output_valid(self):
        """Test validation of valid Python code."""
        valid_code = "def hello():\n    print('Hello')\n    return\n"
        is_valid, error = validate_python_output(valid_code)
        assert is_valid is True
        assert error is None
    
    def test_validate_python_output_invalid_syntax(self):
        """Test validation of Python code with syntax error."""
        invalid_code = "def hello(\n    print('Hello')\n"
        is_valid, error = validate_python_output(invalid_code)
        assert is_valid is False
        assert error is not None
        assert "Syntax error" in error
    
    def test_validate_python_output_empty(self):
        """Test validation of empty Python code."""
        is_valid, error = validate_python_output("")
        assert is_valid is False
        assert error is not None
        assert "empty" in error.lower()
    
    def test_validate_python_output_whitespace_only(self):
        """Test validation of whitespace-only code."""
        is_valid, error = validate_python_output("   \n\t  \n")
        assert is_valid is False
        assert error is not None

