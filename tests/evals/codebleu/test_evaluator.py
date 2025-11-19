"""Tests for CodeBLEU evaluator."""

import pytest
from pathlib import Path
from evals.codebleu.evaluator import CodeBLEUEvaluator, evaluate_codebleu

# Try to import codebleu and check tree-sitter compatibility
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


def test_evaluator_initialization():
    """Test that CodeBLEUEvaluator can be initialized."""
    evaluator = CodeBLEUEvaluator(lang="python")
    assert evaluator.lang == "python"
    assert evaluator.weights == (0.25, 0.25, 0.25, 0.25)


def test_evaluator_custom_weights():
    """Test CodeBLEUEvaluator with custom weights."""
    weights = (0.3, 0.3, 0.2, 0.2)
    evaluator = CodeBLEUEvaluator(lang="python", weights=weights)
    assert evaluator.weights == weights


@pytest.mark.skipif(not CODEBLEU_AVAILABLE, reason=f"CodeBLEU not available: {CODEBLEU_ERROR or ''}")
def test_evaluate_simple_code():
    """Test evaluating simple Python code."""
    prediction = "def hello():\n    print('Hello, World!')"
    reference = "def hello():\n    print('Hello, World!')"
    
    evaluator = CodeBLEUEvaluator(lang="python")
    try:
        result = evaluator.evaluate(prediction, reference)
    except ImportError as e:
        if "tree-sitter" in str(e).lower() or "Tree-sitter" in str(e):
            pytest.skip(f"Tree-sitter compatibility issue: {e}")
        raise
    
    assert "codebleu" in result
    assert 0.0 <= result["codebleu"] <= 1.0
    assert "ngram_match_score" in result
    assert "weighted_ngram_match_score" in result
    assert "syntax_match_score" in result
    assert "dataflow_match_score" in result
    
    # Identical code should have high score
    assert result["codebleu"] > 0.5  # Should be close to 1.0 for identical code


@pytest.mark.skipif(not CODEBLEU_AVAILABLE, reason=f"CodeBLEU not available: {CODEBLEU_ERROR or ''}")
def test_evaluate_different_code():
    """Test evaluating different Python code."""
    prediction = "def hello():\n    print('Hello')"
    reference = "def goodbye():\n    print('Goodbye')"
    
    evaluator = CodeBLEUEvaluator(lang="python")
    try:
        result = evaluator.evaluate(prediction, reference)
    except ImportError as e:
        if "tree-sitter" in str(e).lower() or "Tree-sitter" in str(e):
            pytest.skip(f"Tree-sitter compatibility issue: {e}")
        raise
    
    assert "codebleu" in result
    assert 0.0 <= result["codebleu"] <= 1.0
    # Different code should have lower score than identical code
    assert result["codebleu"] < 1.0


@pytest.mark.skipif(not CODEBLEU_AVAILABLE, reason=f"CodeBLEU not available: {CODEBLEU_ERROR or ''}")
def test_evaluate_codebleu_function():
    """Test the convenience function evaluate_codebleu."""
    prediction = "def hello():\n    return 'Hello'"
    reference = "def hello():\n    return 'Hello'"
    
    try:
        result = evaluate_codebleu(prediction, reference, lang="python")
    except ImportError as e:
        if "tree-sitter" in str(e).lower() or "Tree-sitter" in str(e):
            pytest.skip(f"Tree-sitter compatibility issue: {e}")
        raise
    
    assert "codebleu" in result
    assert 0.0 <= result["codebleu"] <= 1.0

