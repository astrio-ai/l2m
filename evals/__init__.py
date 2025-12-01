"""Evals package for Atlas benchmarking.

This package provides evaluation tools for benchmarking Atlas (Atlas):

- CodeBLEU: Code similarity evaluation using CodeBLEU metrics
- Harbor: Terminal/benchmarking integration for agent evaluation

See the individual module docstrings for more details.
"""

from evals.codebleu import CodeBLEUEvaluator, evaluate_codebleu

__all__ = ["CodeBLEUEvaluator", "evaluate_codebleu"]
