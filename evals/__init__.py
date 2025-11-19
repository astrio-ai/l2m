"""Evals package for L2M benchmarking.

This package provides evaluation tools for benchmarking L2M (Legacy2Modern):

- CodeBLEU: Code similarity evaluation using CodeBLEU metrics
- Harbor: Terminal/benchmarking integration for agent evaluation

See the individual module docstrings for more details.
"""

from evals.codebleu import CodeBLEUEvaluator, evaluate_codebleu

__all__ = ["CodeBLEUEvaluator", "evaluate_codebleu"]
