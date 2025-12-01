"""Tests for Atlas helper functions."""

import pytest
from pathlib import Path
from unittest.mock import patch, MagicMock

from evals.codebleu.atlas_helpers import (
    find_generated_python,
    run_atlas_modernization,
)


def test_find_generated_python_exists(tmp_path):
    """Test finding generated Python file when it exists."""
    cobol_file = tmp_path / "HELLO.cbl"
    cobol_file.write_text("IDENTIFICATION DIVISION.")
    
    python_file = tmp_path / "HELLO.py"
    python_file.write_text("def hello(): pass")
    
    result = find_generated_python(cobol_file, search_dir=tmp_path)
    assert result == python_file


def test_find_generated_python_not_found(tmp_path):
    """Test finding generated Python file when it doesn't exist."""
    cobol_file = tmp_path / "HELLO.cbl"
    cobol_file.write_text("IDENTIFICATION DIVISION.")
    
    result = find_generated_python(cobol_file, search_dir=tmp_path)
    assert result is None


def test_find_generated_python_in_subdirectory(tmp_path):
    """Test finding generated Python file in subdirectory."""
    subdir = tmp_path / "subdir"
    subdir.mkdir()
    
    cobol_file = tmp_path / "HELLO.cbl"
    cobol_file.write_text("IDENTIFICATION DIVISION.")
    
    python_file = subdir / "HELLO.py"
    python_file.write_text("def hello(): pass")
    
    result = find_generated_python(cobol_file, search_dir=subdir)
    assert result == python_file


@patch("evals.codebleu.atlas_helpers.subprocess.run")
def test_run_atlas_modernization_success(mock_run, tmp_path):
    """Test running Atlas modernization successfully."""
    cobol_file = tmp_path / "HELLO.cbl"
    cobol_file.write_text("IDENTIFICATION DIVISION.")
    
    # Mock successful subprocess run
    mock_result = MagicMock()
    mock_result.returncode = 0
    mock_result.stdout = "Success"
    mock_result.stderr = ""
    mock_run.return_value = mock_result
    
    success, output = run_atlas_modernization(
        cobol_file,
        message="Convert to Python",
        output_dir=tmp_path,
        yes_always=True,
    )
    
    assert success is True
    assert "Success" in output
    mock_run.assert_called_once()


@patch("evals.codebleu.atlas_helpers.subprocess.run")
def test_run_atlas_modernization_failure(mock_run, tmp_path):
    """Test running Atlas modernization with failure."""
    cobol_file = tmp_path / "HELLO.cbl"
    cobol_file.write_text("IDENTIFICATION DIVISION.")
    
    # Mock failed subprocess run
    mock_result = MagicMock()
    mock_result.returncode = 1
    mock_result.stdout = ""
    mock_result.stderr = "Error occurred"
    mock_run.return_value = mock_result
    
    success, output = run_atlas_modernization(
        cobol_file,
        message="Convert to Python",
        output_dir=tmp_path,
        yes_always=True,
    )
    
    assert success is False
    assert "Error occurred" in output

