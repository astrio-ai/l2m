"""Computational Accuracy (CA) evaluation module for Atlas benchmarking.

CA measures functional equivalence by comparing outputs of groundtruth and
modernized code when given the same inputs.
"""

from evals.ca.evaluator import CAEvaluator, evaluate_ca
from evals.ca.benchmark import CABenchmark, run_ca_benchmark

try:
    from evals.ca.visualize import (
        load_results,
        plot_ca_scores,
        plot_ca_distribution,
        plot_match_type_breakdown,
        plot_ca_vs_codebleu,
        create_all_visualizations,
    )
    __all__ = [
        "CAEvaluator",
        "evaluate_ca",
        "CABenchmark",
        "run_ca_benchmark",
        "load_results",
        "plot_ca_scores",
        "plot_ca_distribution",
        "plot_match_type_breakdown",
        "plot_ca_vs_codebleu",
        "create_all_visualizations",
    ]
except ImportError:
    __all__ = [
        "CAEvaluator",
        "evaluate_ca",
        "CABenchmark",
        "run_ca_benchmark",
    ]

