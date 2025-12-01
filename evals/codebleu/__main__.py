"""Command-line interface for CodeBLEU benchmark evaluation."""

import argparse
import sys
from pathlib import Path

from evals.codebleu.benchmark import CodeBLEUBenchmark, run_benchmark


def main():
    """CLI entry point for CodeBLEU benchmark."""
    parser = argparse.ArgumentParser(
        description="Run CodeBLEU evaluation on Atlas modernization results",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Evaluate predictions in output/ against references in data/
  python -m evals.codebleu data/ output/modernized-python/

  # Evaluate with custom output file
  python -m evals.codebleu data/ output/modernized-python/ -o results.json

  # Evaluate only references in data/ (compare files within data/)
  python -m evals.codebleu data/

  # Evaluate with custom weights
  python -m evals.codebleu data/ output/ --weights 0.3 0.3 0.2 0.2
        """,
    )

    parser.add_argument(
        "data_dir",
        type=str,
        help="Directory containing reference (groundtruth) files",
    )

    parser.add_argument(
        "prediction_dir",
        type=str,
        nargs="?",
        default=None,
        help="Directory containing prediction files (default: same as data_dir)",
    )

    parser.add_argument(
        "-o",
        "--output",
        type=str,
        default=None,
        help="Output JSON file for results (default: print only)",
    )

    parser.add_argument(
        "--lang",
        type=str,
        default="python",
        help="Programming language (default: python)",
    )

    parser.add_argument(
        "--weights",
        type=float,
        nargs=4,
        default=[0.25, 0.25, 0.25, 0.25],
        metavar=("NGRAM", "WEIGHTED_NGRAM", "SYNTAX", "DATAFLOW"),
        help="Weights for CodeBLEU components (default: 0.25 0.25 0.25 0.25)",
    )

    args = parser.parse_args()

    # Validate weights
    if abs(sum(args.weights) - 1.0) > 1e-6:
        print(f"Error: Weights must sum to 1.0, got {sum(args.weights)}", file=sys.stderr)
        sys.exit(1)

    # Validate directories
    data_path = Path(args.data_dir)
    if not data_path.exists():
        print(f"Error: Data directory does not exist: {data_path}", file=sys.stderr)
        sys.exit(1)

    if args.prediction_dir:
        pred_path = Path(args.prediction_dir)
        if not pred_path.exists():
            print(f"Error: Prediction directory does not exist: {pred_path}", file=sys.stderr)
            sys.exit(1)

    # Run benchmark
    try:
        results = run_benchmark(
            data_dir=str(data_path),
            prediction_dir=str(args.prediction_dir) if args.prediction_dir else None,
            output_file=str(args.output) if args.output else None,
            lang=args.lang,
            weights=tuple(args.weights),
        )
        sys.exit(0)
    except KeyboardInterrupt:
        print("\nInterrupted by user", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        import traceback

        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()

