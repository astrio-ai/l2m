"""Evals package for Atlas benchmarking.

This package provides evaluation tools for benchmarking Atlas (Atlas):

- CodeBLEU: Code similarity evaluation using CodeBLEU metrics
- CA (Computational Accuracy): Functional equivalence evaluation by comparing outputs
- Harbor: Terminal/benchmarking integration for agent evaluation

See the individual module docstrings for more details.
"""

from evals.codebleu import CodeBLEUEvaluator, evaluate_codebleu
from evals.ca import CAEvaluator, evaluate_ca, CABenchmark, run_ca_benchmark

__all__ = [
    "CodeBLEUEvaluator",
    "evaluate_codebleu",
    "CAEvaluator",
    "evaluate_ca",
    "CABenchmark",
    "run_ca_benchmark",
]
