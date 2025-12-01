"""Automated Atlas modernization and CodeBLEU evaluation workflow.

This script automates the entire process:
1. Finds all COBOL files in a directory
2. Runs Atlas modernization on each (non-interactive)
3. Compares generated Python with groundtruth using CodeBLEU
4. Generates evaluation report
"""

import argparse
import json
import shutil
import sys
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

from tqdm import tqdm

from evals.codebleu.benchmark import CodeBLEUBenchmark
from evals.codebleu.atlas_helpers import find_generated_python, run_atlas_modernization
from evals.codebleu.evaluator import CodeBLEUEvaluator


def find_cobol_files(data_dir: Path, pattern: str = "*.cbl") -> List[Path]:
    """Find all COBOL files in a directory recursively."""
    # Use rglob for recursive search
    cobol_files = list(data_dir.rglob(pattern))
    cobol_files.extend(data_dir.rglob(pattern.upper()))  # Also find .CBL
    # Filter out test files and files in output directories
    cobol_files = [
        f for f in cobol_files
        if "output" not in f.parts and not f.name.startswith("test_")
    ]
    return sorted(set(cobol_files))  # Remove duplicates and sort


def find_groundtruth_python(cobol_file: Path, data_dir: Path) -> Optional[Path]:
    """Find groundtruth Python file for a COBOL file."""
    # Python file should have same name/stem as COBOL file
    python_name = f"{cobol_file.stem}.py"
    
    # Check in same directory as COBOL file
    groundtruth = cobol_file.parent / python_name
    if groundtruth.exists():
        return groundtruth
    
    # Search recursively in data directory
    candidates = list(data_dir.rglob(python_name))
    # Filter out test files
    candidates = [c for c in candidates if not c.name.startswith("test_")]
    
    if candidates:
        return candidates[0]
    
    return None


def modernize_and_evaluate(
    cobol_dir: Path,
    output_dir: Path,
    groundtruth_dir: Optional[Path] = None,
    pattern: str = "*.cbl",
    lang: str = "python",
    weights: Tuple[float, float, float, float] = (0.25, 0.25, 0.25, 0.25),
    atlas_message: Optional[str] = None,
    yes_always: bool = True,
    skip_modernization: bool = False,
) -> Dict[str, Any]:
    """
    Automated workflow: modernize COBOL files and evaluate with CodeBLEU.

    Args:
        cobol_dir: Directory containing COBOL files (and groundtruth Python files).
        output_dir: Directory to save generated Python files.
        groundtruth_dir: Directory for groundtruth Python files. If None, uses cobol_dir.
        pattern: Glob pattern for COBOL files.
        lang: Programming language for CodeBLEU.
        weights: CodeBLEU component weights.
        atlas_message: Custom message for Atlas modernization.
        yes_always: Use --yes-always flag for non-interactive mode.
        skip_modernization: Skip Atlas modernization (assume Python files already exist).

    Returns:
        Dictionary with evaluation results.
    """
    if groundtruth_dir is None:
        groundtruth_dir = cobol_dir

    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)

    # Find all COBOL files
    cobol_files = find_cobol_files(cobol_dir, pattern=pattern)

    if not cobol_files:
        return {
            "error": f"No COBOL files found matching pattern '{pattern}' in {cobol_dir}",
            "results": {},
            "summary": {"total": 0, "successes": 0, "failures": 0},
        }

    print(f"Found {len(cobol_files)} COBOL files")
    print(f"Output directory: {output_dir}")
    print(f"Groundtruth directory: {groundtruth_dir}")

    # Initialize evaluator
    evaluator = CodeBLEUEvaluator(lang=lang, weights=weights)

    results = {}
    modernization_successes = 0
    modernization_failures = 0
    evaluation_successes = 0
    evaluation_failures = 0

    for cobol_file in tqdm(cobol_files, desc="Processing files"):
        file_key = cobol_file.relative_to(cobol_dir).as_posix()
        result = {
            "cobol_file": str(cobol_file.relative_to(cobol_dir)),
            "status": "pending",
        }

        # Step 1: Modernize (if not skipped)
        if not skip_modernization:
            # Maintain subdirectory structure in output directory
            rel_path = cobol_file.relative_to(cobol_dir)
            output_subdir = output_dir / rel_path.parent if str(rel_path.parent) != "." else output_dir
            output_subdir.mkdir(parents=True, exist_ok=True)
            
            # Copy COBOL file to output directory (maintaining structure)
            cobol_in_output = output_subdir / cobol_file.name
            if cobol_file != cobol_in_output:
                shutil.copy2(cobol_file, cobol_in_output)

            # Prepare message
            # Use "Convert" instead of "Modernize" to avoid triggering multi-agent pipeline
            file_message = atlas_message
            if file_message is None:
                file_message = f"Convert this COBOL file to Python: {cobol_in_output.name}"
            elif cobol_file.name not in file_message:
                file_message = f"{file_message} File: {cobol_in_output.name}"

            # Run Atlas modernization from the subdirectory
            success, output = run_atlas_modernization(
                cobol_in_output,
                message=file_message,
                output_dir=output_subdir,  # Use subdirectory as working directory
                yes_always=yes_always,
            )

            if success:
                modernization_successes += 1
            else:
                modernization_failures += 1
                result.update({
                    "status": "modernization_failed",
                    "error": output[:500] if output else "Atlas modernization failed",
                })
                results[file_key] = result
                continue

        # Step 2: Find generated Python file
        # Expected location: same subdirectory structure in output_dir as the COBOL file
        rel_path = cobol_file.relative_to(cobol_dir)
        expected_output_subdir = output_dir / rel_path.parent if str(rel_path.parent) != "." else output_dir
        python_name = f"{cobol_file.stem}.py"
        expected_python = expected_output_subdir / python_name
        
        generated_python = None
        
        # Try expected location first (maintained subdirectory structure)
        if expected_python.exists():
            generated_python = expected_python
        else:
            # Try the subdirectory where COBOL file was processed
            generated_python = find_generated_python(cobol_file, search_dir=expected_output_subdir)
        
        # Try output_dir root (flat structure - fallback)
        if generated_python is None:
            flat_output = output_dir / python_name
            if flat_output.exists():
                generated_python = flat_output
        
        # Try original location (Atlas might create it relative to git root)
        if generated_python is None:
            generated_python = find_generated_python(cobol_file, search_dir=cobol_file.parent)
            # If found in original location, copy to expected output location
            if generated_python and generated_python.exists():
                expected_output_subdir.mkdir(parents=True, exist_ok=True)
                target_python = expected_output_subdir / python_name
                if not target_python.exists() or generated_python != target_python:
                    shutil.copy2(generated_python, target_python)
                generated_python = target_python
        
        # Try searching recursively in output_dir (last resort)
        if generated_python is None or not generated_python.exists():
            candidates = list(output_dir.rglob(python_name))
            if candidates:
                generated_python = candidates[0]
                # Copy to expected location to maintain structure
                if generated_python != expected_python:
                    expected_output_subdir.mkdir(parents=True, exist_ok=True)
                    shutil.copy2(generated_python, expected_python)
                    generated_python = expected_python
        
        if generated_python is None:
            modernization_failures += 1
            result.update({
                "status": "no_generated_file",
                "error": f"Generated Python file not found for {cobol_file.stem}",
            })
            results[file_key] = result
            continue

        # Calculate relative path for generated_python
        try:
            result["generated_python"] = str(generated_python.relative_to(output_dir))
        except ValueError:
            # If file is outside output_dir, use absolute path
            result["generated_python"] = str(generated_python)

        # Step 3: Find groundtruth Python file
        groundtruth_python = find_groundtruth_python(cobol_file, groundtruth_dir)
        if groundtruth_python is None:
            evaluation_failures += 1
            result.update({
                "status": "no_groundtruth",
                "error": f"Groundtruth Python file not found for {cobol_file.stem}",
            })
            results[file_key] = result
            continue

        result["groundtruth_python"] = str(groundtruth_python.relative_to(groundtruth_dir))

        # Step 4: Evaluate with CodeBLEU
        try:
            eval_result = evaluator.evaluate_files(
                prediction_paths=[generated_python],
                reference_paths=[groundtruth_python],
            )

            result.update({
                "status": "success",
                "codebleu": eval_result.get("codebleu", 0.0),
                "ngram_match_score": eval_result.get("ngram_match_score", 0.0),
                "weighted_ngram_match_score": eval_result.get("weighted_ngram_match_score", 0.0),
                "syntax_match_score": eval_result.get("syntax_match_score", 0.0),
                "dataflow_match_score": eval_result.get("dataflow_match_score", 0.0),
            })
            evaluation_successes += 1

        except Exception as e:
            result.update({
                "status": "evaluation_failed",
                "error": str(e),
            })
            evaluation_failures += 1

        results[file_key] = result

    # Compute summary statistics
    codebleu_scores = [
        r["codebleu"] for r in results.values() if r.get("status") == "success" and "codebleu" in r
    ]

    summary = {
        "total_files": len(cobol_files),
        "modernization_successes": modernization_successes,
        "modernization_failures": modernization_failures,
        "evaluation_successes": evaluation_successes,
        "evaluation_failures": evaluation_failures,
        "mean_codebleu": sum(codebleu_scores) / len(codebleu_scores) if codebleu_scores else 0.0,
        "min_codebleu": min(codebleu_scores) if codebleu_scores else 0.0,
        "max_codebleu": max(codebleu_scores) if codebleu_scores else 0.0,
    }

    return {
        "summary": summary,
        "results": results,
        "config": {
            "cobol_dir": str(cobol_dir),
            "output_dir": str(output_dir),
            "groundtruth_dir": str(groundtruth_dir),
            "lang": lang,
            "weights": list(weights),
        },
    }


def print_summary(results: Dict[str, Any]):
    """Print a formatted summary of results."""
    if "error" in results:
        print("\n" + "=" * 70)
        print("Error")
        print("=" * 70)
        print(results["error"])
        print("=" * 70)
        return
    
    summary = results.get("summary", {})
    
    print("\n" + "=" * 70)
    print("Automated Atlas Modernization & CodeBLEU Evaluation Report")
    print("=" * 70)
    print(f"Total COBOL files:       {summary.get('total_files', 0)}")
    print()
    print("Modernization:")
    print(f"  Successful:            {summary.get('modernization_successes', 0)}")
    print(f"  Failed:                {summary.get('modernization_failures', 0)}")
    print()
    print("Evaluation:")
    print(f"  Successful:            {summary.get('evaluation_successes', 0)}")
    print(f"  Failed:                {summary.get('evaluation_failures', 0)}")
    print()
    
    if summary.get("evaluation_successes", 0) > 0:
        print("CodeBLEU Statistics:")
        print(f"  Mean CodeBLEU:         {summary.get('mean_codebleu', 0.0):.4f}")
        print(f"  Min CodeBLEU:          {summary.get('min_codebleu', 0.0):.4f}")
        print(f"  Max CodeBLEU:          {summary.get('max_codebleu', 0.0):.4f}")
        print()
        
        # Show top and bottom performers
        file_scores = [
            (key, r["codebleu"])
            for key, r in results["results"].items()
            if r.get("status") == "success" and "codebleu" in r
        ]
        file_scores.sort(key=lambda x: x[1], reverse=True)
        
        if file_scores:
            print("Top 5 files by CodeBLEU:")
            for key, score in file_scores[:5]:
                print(f"  {key:50s} {score:.4f}")
            
            if len(file_scores) > 5:
                print("\nBottom 5 files by CodeBLEU:")
                for key, score in file_scores[-5:]:
                    print(f"  {key:50s} {score:.4f}")
    
    # Show failures
    results_dict = results.get("results", {})
    failures = [
        (key, r) for key, r in results_dict.items() if r.get("status") != "success"
    ]
    if failures:
        print(f"\nFailures ({len(failures)}):")
        for key, r in failures[:10]:
            status = r.get("status", "unknown")
            error = r.get("error", "Unknown error")
            print(f"  {key}: {status}")
            if error:
                print(f"    {error[:100]}")
        if len(failures) > 10:
            print(f"  ... and {len(failures) - 10} more failures")
    
    print("=" * 70)


def main():
    """CLI entry point for automated evaluation."""
    parser = argparse.ArgumentParser(
        description="Automated Atlas modernization and CodeBLEU evaluation",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Modernize all COBOL files in data/ and evaluate against groundtruth
  python -m evals.codebleu.modernize_and_evaluate data/ data/output/

  # Evaluate existing generated files (skip modernization)
  python -m evals.codebleu.modernize_and_evaluate data/ data/output/ --skip-modernization

  # Save results to JSON file
  python -m evals.codebleu.modernize_and_evaluate data/ data/output/ -o results.json

  # Evaluate specific directory
  python -m evals.codebleu.modernize_and_evaluate data/aws-samples/ data/output/aws-samples/
        """,
    )

    parser.add_argument(
        "data_dir",
        type=str,
        help="Directory containing COBOL files and groundtruth Python files",
    )
    parser.add_argument(
        "output_dir",
        type=str,
        help="Directory to save generated Python files",
    )
    parser.add_argument(
        "--groundtruth-dir",
        type=str,
        default=None,
        help="Directory for groundtruth Python files (default: same as data_dir)",
    )
    parser.add_argument(
        "-o",
        "--output-json",
        type=str,
        default=None,
        help="Save results to JSON file",
    )
    parser.add_argument(
        "--pattern",
        type=str,
        default="*.cbl",
        help='Glob pattern for COBOL files (default: "*.cbl")',
    )
    parser.add_argument(
        "--lang",
        type=str,
        default="python",
        help="Programming language for CodeBLEU (default: python)",
    )
    parser.add_argument(
        "--weights",
        type=float,
        nargs=4,
        default=[0.25, 0.25, 0.25, 0.25],
        metavar=("NGRAM", "WEIGHTED_NGRAM", "SYNTAX", "DATAFLOW"),
        help="Weights for CodeBLEU components (default: 0.25 0.25 0.25 0.25)",
    )
    parser.add_argument(
        "--message",
        type=str,
        default=None,
        help="Custom message for Atlas modernization",
    )
    parser.add_argument(
        "--skip-modernization",
        action="store_true",
        help="Skip Atlas modernization (assume Python files already exist in output_dir)",
    )
    parser.add_argument(
        "--no-yes-always",
        action="store_true",
        help="Don't use --yes-always flag (interactive mode)",
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

    output_path = Path(args.output_dir)
    groundtruth_path = Path(args.groundtruth_dir) if args.groundtruth_dir else data_path

    # Run evaluation
    try:
        results = modernize_and_evaluate(
            cobol_dir=data_path,
            output_dir=output_path,
            groundtruth_dir=groundtruth_path,
            pattern=args.pattern,
            lang=args.lang,
            weights=tuple(args.weights),
            atlas_message=args.message,
            yes_always=not args.no_yes_always,
            skip_modernization=args.skip_modernization,
        )

        # Print summary
        print_summary(results)

        # Save to JSON if requested
        if args.output_json:
            output_json_path = Path(args.output_json)
            output_json_path.parent.mkdir(parents=True, exist_ok=True)
            with open(output_json_path, "w", encoding="utf-8") as f:
                json.dump(results, f, indent=2)
            print(f"\nResults saved to: {output_json_path}")

        evaluation_failures = results.get("summary", {}).get("evaluation_failures", 0)
        sys.exit(0 if evaluation_failures == 0 else 1)

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

