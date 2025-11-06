"""
Run evaluation.
"""

from pathlib import Path
from evaluator import Evaluator

if __name__ == "__main__":
    evaluator = Evaluator(
        data_dir=Path("data"),
        output_dir=Path("data/output"),
        results_dir=Path("evals/results"),
        timeout_seconds=30,
        memory_limit_mb=512
    )
    
    metrics = evaluator.evaluate_all()
    evaluator.print_summary(metrics)
    
    # Save results
    filepath = evaluator.save_results(metrics)
    print(f"\nResults saved to: {filepath}")

