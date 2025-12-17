"""Command-line interface for CA evaluation."""

import argparse
from pathlib import Path

from evals.ca.benchmark import run_ca_benchmark


def main():
    """Main entry point for CA evaluation CLI."""
    parser = argparse.ArgumentParser(
        description="Evaluate Computational Accuracy (CA) of modernized code"
    )
    parser.add_argument(
        "data_dir",
        type=str,
        help="Directory containing groundtruth/reference files",
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
        help="Output JSON file for results",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=30,
        help="Timeout in seconds for code execution (default: 30)",
    )
    parser.add_argument(
        "--no-normalize",
        action="store_true",
        help="Disable output normalization",
    )
    parser.add_argument(
        "--strict",
        action="store_true",
        help="Require exact match (strict mode)",
    )
    
    args = parser.parse_args()
    
    results = run_ca_benchmark(
        data_dir=args.data_dir,
        prediction_dir=args.prediction_dir,
        output_file=args.output,
        timeout=args.timeout,
        normalize=not args.no_normalize,
        strict_match=args.strict,
    )
    
    return 0 if results["summary"]["mean_ca_score"] > 0 else 1


if __name__ == "__main__":
    exit(main())

