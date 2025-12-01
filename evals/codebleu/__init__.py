"""CodeBLEU evaluation module for Atlas benchmarking."""

from evals.codebleu.benchmark import CodeBLEUBenchmark, run_benchmark
from evals.codebleu.evaluator import CodeBLEUEvaluator, evaluate_codebleu
from evals.codebleu.atlas_helpers import evaluate_atlas_output, run_atlas_modernization

try:
    from evals.codebleu.visualize import (
        load_results,
        plot_codebleu_scores,
        plot_component_comparison,
        plot_score_distribution,
        create_all_visualizations,
    )
    __all__ = [
        "CodeBLEUEvaluator",
        "evaluate_codebleu",
        "CodeBLEUBenchmark",
        "run_benchmark",
        "evaluate_atlas_output",
        "run_atlas_modernization",
        "load_results",
        "plot_codebleu_scores",
        "plot_component_comparison",
        "plot_score_distribution",
        "create_all_visualizations",
    ]
except ImportError:
    # Visualization functions require matplotlib (optional dependency)
    __all__ = [
        "CodeBLEUEvaluator",
        "evaluate_codebleu",
        "CodeBLEUBenchmark",
        "run_benchmark",
        "evaluate_atlas_output",
        "run_atlas_modernization",
    ]

