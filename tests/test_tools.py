"""Tests for tool functionality."""

import pytest
import tempfile
from pathlib import Path
from src.tools.cobol_parser_tool import parse_cobol_file, analyze_cobol_structure
from src.tools.python_synth_tool import generate_python_code, validate_python_syntax
from src.tools.code_quality_tool import check_code_quality, get_quality_score
from src.tools.test_runner_tool import generate_test_code, run_tests


class TestCOBOLParserTool:
    """Tests for COBOL parser tool."""
    
    def test_parse_cobol_file_not_found(self):
        """Test parsing non-existent file."""
        result = parse_cobol_file("nonexistent.cbl")
        assert "Error" in result
        assert "not found" in result
    
    def test_parse_cobol_file_simple(self):
        """Test parsing simple COBOL file."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.cbl', delete=False) as f:
            f.write("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD!'.
           GOBACK.""")
            temp_file = f.name
        
        try:
            result = parse_cobol_file(temp_file)
            assert "PROGRAM-ID: HELLO" in result
            assert "PROCEDURES:" in result
        finally:
            Path(temp_file).unlink()
    
    def test_analyze_cobol_structure_not_found(self):
        """Test analyzing non-existent file."""
        result = analyze_cobol_structure("nonexistent.cbl")
        assert "error" in result
        assert "not found" in result["error"]
    
    def test_analyze_cobol_structure_simple(self):
        """Test analyzing simple COBOL file."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.cbl', delete=False) as f:
            f.write("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
           DISPLAY 'TEST'.
           GOBACK.""")
            temp_file = f.name
        
        try:
            result = analyze_cobol_structure(temp_file)
            assert result["program_id"] == "TEST"
            assert isinstance(result["display_statements"], list)
            assert len(result["display_statements"]) > 0
        finally:
            Path(temp_file).unlink()


class TestPythonSynthTool:
    """Tests for Python synthesis tool."""
    
    def test_generate_python_code(self):
        """Test Python code generation."""
        analysis = "COBOL analysis result"
        structure = "Target structure"
        
        code = generate_python_code(analysis, structure)
        assert isinstance(code, str)
        assert "Placeholder" in code
        assert "LLM agent" in code
        assert analysis[:100] in code  # Check that analysis is included (truncated)
    
    def test_validate_python_syntax_valid(self):
        """Test validation of valid Python code."""
        valid_code = "def hello():\n    print('Hello')\n"
        assert validate_python_syntax(valid_code) is True
    
    def test_validate_python_syntax_invalid(self):
        """Test validation of invalid Python code."""
        invalid_code = "def hello(\n    print('Hello')\n"  # Missing closing paren
        assert validate_python_syntax(invalid_code) is False
    
    def test_validate_python_syntax_empty(self):
        """Test validation of empty code."""
        assert validate_python_syntax("") is False


class TestCodeQualityTool:
    """Tests for code quality tool."""
    
    def test_check_code_quality_valid(self):
        """Test checking quality of valid code."""
        valid_code = "def test():\n    return True\n"
        issues = check_code_quality(valid_code)
        assert isinstance(issues, list)
    
    def test_check_code_quality_invalid(self):
        """Test checking quality of invalid code."""
        invalid_code = "def test(\n    return\n"  # Syntax error
        issues = check_code_quality(invalid_code)
        assert len(issues) > 0
        assert any("syntax_error" in str(issue) for issue in issues)
    
    def test_get_quality_score_valid(self):
        """Test quality score for valid code."""
        valid_code = "def test():\n    return True\n"
        score = get_quality_score(valid_code)
        assert isinstance(score, int)
        assert 0 <= score <= 100
    
    def test_get_quality_score_invalid(self):
        """Test quality score for invalid code."""
        invalid_code = "def test(\n    return\n"
        score = get_quality_score(invalid_code)
        assert isinstance(score, int)
        assert score < 100  # Should have lower score due to errors


class TestTestRunnerTool:
    """Tests for test runner tool."""
    
    def test_generate_test_code(self):
        """Test test code generation."""
        python_code = "def hello():\n    print('Hello')\n"
        requirements = "Test all functions"
        
        test_code = generate_test_code(python_code, requirements)
        assert isinstance(test_code, str)
        assert "test" in test_code.lower()
    
    def test_run_tests_empty(self):
        """Test running empty test code."""
        result = run_tests("")
        assert isinstance(result, str)

