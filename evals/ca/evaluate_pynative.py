#!/usr/bin/env python3
"""
Evaluate CA for pynative dataset.

Compares groundtruth Python files with ATLAS-generated Python files
from COBOL modernization.
"""

import sys
from pathlib import Path

from evals.ca.benchmark import run_ca_benchmark


def main():
    """Run CA evaluation on pynative dataset."""
    # Paths
    groundtruth_dir = Path("data/pynative-python/basic-exercises")
    prediction_dir = Path("data/pynative-python-generated/basic-exercises")
    output_file = Path("data/output/pynative_ca_results.json")
    
    # Check if directories exist
    if not groundtruth_dir.exists():
        print(f"Error: Groundtruth directory not found: {groundtruth_dir}")
        return 1
    
    if not prediction_dir.exists():
        print(f"Error: Prediction directory not found: {prediction_dir}")
        return 1
    
    print("=" * 70)
    print("PyNative CA Evaluation")
    print("=" * 70)
    print(f"Groundtruth: {groundtruth_dir}")
    print(f"Predictions: {prediction_dir}")
    print(f"Output: {output_file}")
    print()
    
    # Run CA evaluation
    print("Running CA evaluation...")
    results = run_ca_benchmark(
        data_dir=str(groundtruth_dir),
        prediction_dir=str(prediction_dir),
        output_file=str(output_file),
        timeout=30,
        normalize=True,
        strict_match=False,
    )
    
    # Print summary
    summary = results["summary"]
    print("\n" + "=" * 70)
    print("CA Evaluation Results")
    print("=" * 70)
    print(f"Files evaluated: {summary['num_files']}")
    print(f"Errors: {summary['num_errors']}")
    print()
    print(f"Mean CA Score:        {summary['mean_ca_score']:.4f}")
    print(f"Exact Match Rate:     {summary['exact_match_rate']:.4f}")
    print(f"Normalized Match Rate: {summary['normalized_match_rate']:.4f}")
    print(f"Return Code Match:    {summary['returncode_match_rate']:.4f}")
    print()
    print(f"Perfect Matches:      {summary['perfect_matches']}")
    print(f"Partial Matches:      {summary['partial_matches']}")
    print(f"Failures:             {summary['failures']}")
    print("=" * 70)
    
    # Show some examples
    if results["results"]:
        print("\nSample Results:")
        file_scores = [
            (key, r.get("ca_score", 0.0), r.get("exact_match", False))
            for key, r in results["results"].items()
            if "ca_score" in r
        ]
        file_scores.sort(key=lambda x: x[1], reverse=True)
        
        print("\nTop 5 by CA Score:")
        for key, score, exact in file_scores[:5]:
            match_type = "EXACT" if exact else "NORMALIZED"
            print(f"  {key:30s} {score:.4f} ({match_type})")
        
        if len(file_scores) > 5:
            print("\nBottom 5 by CA Score:")
            for key, score, exact in file_scores[-5:]:
                match_type = "EXACT" if exact else "NORMALIZED"
                print(f"  {key:30s} {score:.4f} ({match_type})")
    
    return 0


if __name__ == "__main__":
    sys.exit(main())

