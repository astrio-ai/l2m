"""Tests for CodeBLEU benchmark."""

import pytest
from pathlib import Path
from evals.codebleu.benchmark import CodeBLEUBenchmark, run_benchmark

# Check if CodeBLEU is available and tree-sitter is compatible
def _check_codebleu_available():
    """Check if CodeBLEU is available and tree-sitter is compatible."""
    try:
        from codebleu import calc_codebleu
    except ImportError:
        return False, "codebleu package not installed"
    
    # Check tree-sitter compatibility by trying to create a Language object
    try:
        from tree_sitter import Language
        import tree_sitter_python
        # Try to create Language - this will fail with old tree-sitter versions
        _ = Language(tree_sitter_python.language())
        return True, None
    except (TypeError, AttributeError) as e:
        if "an integer is required" in str(e):
            return False, "tree-sitter version incompatible (need >= 0.24.0)"
        return False, f"tree-sitter error: {e}"
    except ImportError:
        return False, "tree-sitter-python not available"

CODEBLEU_AVAILABLE, CODEBLEU_ERROR = _check_codebleu_available()


def test_benchmark_initialization():
    """Test that CodeBLEUBenchmark can be initialized."""
    benchmark = CodeBLEUBenchmark(lang="python")
    assert benchmark.lang == "python"
    assert benchmark.evaluator is not None


def test_find_reference_files(tmp_path):
    """Test finding reference Python files."""
    # Create test files
    (tmp_path / "file1.py").write_text("def hello(): pass")
    (tmp_path / "file2.py").write_text("def goodbye(): pass")
    (tmp_path / "test_file.py").write_text("def test(): pass")  # Should be excluded
    
    benchmark = CodeBLEUBenchmark(lang="python")
    references = benchmark.find_reference_files(tmp_path)
    
    assert "file1" in references
    assert "file2" in references
    assert "test_file" not in references  # Test files excluded


def test_find_prediction_files(tmp_path):
    """Test finding prediction Python files."""
    # Create test files
    (tmp_path / "file1.py").write_text("def hello(): pass")
    (tmp_path / "file2.py").write_text("def goodbye(): pass")
    
    benchmark = CodeBLEUBenchmark(lang="python")
    predictions = benchmark.find_prediction_files(tmp_path)
    
    assert "file1" in predictions
    assert "file2" in predictions


@pytest.mark.skipif(not CODEBLEU_AVAILABLE, reason=f"CodeBLEU not available: {CODEBLEU_ERROR or ''}")
def test_evaluate_pair(tmp_path):
    """Test evaluating a single pair of files."""
    prediction_file = tmp_path / "pred.py"
    prediction_file.write_text("def hello():\n    print('Hello')")
    
    reference_file = tmp_path / "ref.py"
    reference_file.write_text("def hello():\n    print('Hello')")
    
    benchmark = CodeBLEUBenchmark(lang="python")
    result = benchmark.evaluate_pair(prediction_file, reference_file)
    
    # Handle error case (tree-sitter incompatibility)
    if "error" in result:
        if "tree-sitter" in result["error"].lower():
            pytest.skip(f"Tree-sitter compatibility issue: {result['error']}")
        raise AssertionError(f"Evaluation failed: {result['error']}")
    
    assert "codebleu" in result
    assert 0.0 <= result["codebleu"] <= 1.0

