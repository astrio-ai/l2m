"""CodeBLEU evaluation module for L2M benchmarking."""

from evals.codebleu.benchmark import CodeBLEUBenchmark, run_benchmark
from evals.codebleu.evaluator import CodeBLEUEvaluator, evaluate_codebleu
from evals.codebleu.l2m_helpers import evaluate_l2m_output, run_l2m_modernization

__all__ = [
    "CodeBLEUEvaluator",
    "evaluate_codebleu",
    "CodeBLEUBenchmark",
    "run_benchmark",
    "evaluate_l2m_output",
    "run_l2m_modernization",
]

