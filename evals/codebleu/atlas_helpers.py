"""Evaluate Atlas modernization results using CodeBLEU.

This script runs Atlas to modernize COBOL files and then evaluates the generated
Python code against groundtruth using CodeBLEU metrics.
"""

import subprocess
import sys
from pathlib import Path
from typing import Optional, Tuple

from evals.codebleu.evaluator import CodeBLEUEvaluator


def find_generated_python(cobol_file: Path, search_dir: Optional[Path] = None) -> Optional[Path]:
    """
    Find the generated Python file for a given COBOL file.

    Args:
        cobol_file: Path to the COBOL file that was modernized.
        search_dir: Directory to search for Python files. If None, searches in cobol_file's directory.

    Returns:
        Path to generated Python file, or None if not found.
    """
    if search_dir is None:
        search_dir = cobol_file.parent

    # Expected Python filename (same stem as COBOL file)
    expected_name = f"{cobol_file.stem}.py"
    python_file = search_dir / expected_name

    if python_file.exists():
        return python_file

    # Also try searching in the directory
    candidates = list(search_dir.glob(f"{cobol_file.stem}.py"))
    if candidates:
        return candidates[0]

    return None


def run_atlas_modernization(
    cobol_file: Path,
    message: Optional[str] = None,
    output_dir: Optional[Path] = None,
    yes_always: bool = True,
) -> Tuple[bool, str]:
    """
    Run Atlas modernization on a COBOL file.

    Args:
        cobol_file: Path to COBOL file to modernize.
        message: Custom message to send to Atlas. If None, uses default modernization message.
        output_dir: Optional working directory. If None, uses cobol_file's directory.
        yes_always: Whether to use --yes-always flag (non-interactive mode).

    Returns:
        Tuple of (success, output_text).
    """
    if message is None:
        # Use "Convert" instead of "Modernize" to avoid triggering multi-agent pipeline
        # Include file path in message so Atlas knows which file to convert
        try:
            cwd = Path.cwd()
            if str(cobol_file).startswith(str(cwd)):
                file_path_rel = cobol_file.relative_to(cwd)
            else:
                file_path_rel = cobol_file.name
        except (ValueError, AttributeError):
            file_path_rel = cobol_file.name
        message = f"Convert {file_path_rel} to Python"

    if output_dir is None:
        output_dir = cobol_file.parent

    cmd = ["atlas", "--message", message]
    if yes_always:
        cmd.append("--yes-always")
    # Optimize for speed: disable streaming, caching, and repo map to speed up
    cmd.extend(["--no-stream", "--no-cache-prompts", "--map-refresh", "manual"])
    # Pass the COBOL file as an argument - use absolute path to ensure it's found
    cmd.append(str(cobol_file.absolute()))

    try:
        # Use Popen with explicit stdin=DEVNULL to prevent any blocking on input
        import os
        result = subprocess.run(
            cmd,
            cwd=str(output_dir),
            capture_output=True,
            text=True,
            timeout=1800,  # 30 minute timeout (for large COBOL files)
            stdin=subprocess.DEVNULL,  # Prevent blocking on stdin
            env=dict(os.environ, ATLAS_YES_ALWAYS="1"),  # Ensure yes-always is respected
        )
        success = result.returncode == 0
        output = result.stdout + result.stderr
        return success, output
    except subprocess.TimeoutExpired:
        return False, "Atlas modernization timed out after 30 minutes"
    except FileNotFoundError:
        return False, "Atlas command not found. Make sure 'atlas' is installed and in PATH."
    except Exception as e:
        return False, f"Error running Atlas: {e}"


def evaluate_atlas_output(
    cobol_file: Path,
    groundtruth_dir: Path,
    prediction_dir: Optional[Path] = None,
    lang: str = "python",
    weights: Tuple[float, float, float, float] = (0.25, 0.25, 0.25, 0.25),
    run_modernization: bool = True,
    atlas_message: Optional[str] = None,
) -> dict:
    """
    Run Atlas modernization and evaluate with CodeBLEU.

    Args:
        cobol_file: Path to COBOL file to modernize.
        groundtruth_dir: Directory containing groundtruth Python files (e.g., data/).
        prediction_dir: Directory where predictions are/will be generated. If None, uses cobol_file's directory.
        lang: Programming language for CodeBLEU (default: "python").
        weights: CodeBLEU component weights (default: equal weights).
        run_modernization: Whether to run Atlas modernization first. If False, assumes Python file already exists.
        atlas_message: Custom message for Atlas. If None, uses default.

    Returns:
        Dictionary with evaluation results.
    """
    if prediction_dir is None:
        prediction_dir = cobol_file.parent

    # Step 1: Run Atlas modernization (if requested)
    if run_modernization:
        print(f"Running Atlas modernization on {cobol_file}...")
        success, output = run_atlas_modernization(cobol_file, message=atlas_message, output_dir=prediction_dir)
        if not success:
            return {
                "error": "Atlas modernization failed",
                "atlas_output": output,
                "cobol_file": str(cobol_file),
            }
        print("Modernization completed.")

    # Step 2: Find generated Python file
    generated_python = find_generated_python(cobol_file, search_dir=prediction_dir)
    if generated_python is None:
        return {
            "error": f"Generated Python file not found for {cobol_file.stem}",
            "cobol_file": str(cobol_file),
            "searched_dir": str(prediction_dir),
        }

    print(f"Found generated Python: {generated_python}")

    # Step 3: Find groundtruth Python file
    groundtruth_python = groundtruth_dir / generated_python.name
    if not groundtruth_python.exists():
        # Try to find it recursively in groundtruth_dir
        candidates = list(groundtruth_dir.rglob(generated_python.name))
        if candidates:
            groundtruth_python = candidates[0]
        else:
            return {
                "error": f"Groundtruth Python file not found: {generated_python.name}",
                "cobol_file": str(cobol_file),
                "groundtruth_dir": str(groundtruth_dir),
            }

    print(f"Found groundtruth: {groundtruth_python}")

    # Step 4: Evaluate with CodeBLEU
    print("Evaluating with CodeBLEU...")
    evaluator = CodeBLEUEvaluator(lang=lang, weights=weights)

    try:
        result = evaluator.evaluate_files(
            prediction_paths=[generated_python],
            reference_paths=[groundtruth_python],
        )

        return {
            "success": True,
            "cobol_file": str(cobol_file),
            "generated_python": str(generated_python),
            "groundtruth_python": str(groundtruth_python),
            "codebleu": result.get("codebleu", 0.0),
            "ngram_match_score": result.get("ngram_match_score", 0.0),
            "weighted_ngram_match_score": result.get("weighted_ngram_match_score", 0.0),
            "syntax_match_score": result.get("syntax_match_score", 0.0),
            "dataflow_match_score": result.get("dataflow_match_score", 0.0),
        }
    except Exception as e:
        return {
            "error": f"CodeBLEU evaluation failed: {e}",
            "cobol_file": str(cobol_file),
            "generated_python": str(generated_python),
            "groundtruth_python": str(groundtruth_python),
        }


def main():
    """CLI entry point for Atlas evaluation."""
    import argparse

    parser = argparse.ArgumentParser(
        description="Run Atlas modernization and evaluate with CodeBLEU",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Modernize HELLO.cbl and compare with groundtruth
  python -m evals.codebleu.atlas_helpers data/hello/HELLO.cbl data/

  # Evaluate existing generated file (skip modernization)
  python -m evals.codebleu.atlas_helpers data/hello/HELLO.cbl data/ --no-modernize

  # Use custom output directory
  python -m evals.codebleu.atlas_helpers data/hello/HELLO.cbl data/ -o output/
        """,
    )

    parser.add_argument("cobol_file", type=str, help="Path to COBOL file to modernize")
    parser.add_argument("groundtruth_dir", type=str, help="Directory containing groundtruth Python files")
    parser.add_argument(
        "-o",
        "--output-dir",
        type=str,
        default=None,
        help="Directory for generated Python files (default: same as COBOL file directory)",
    )
    parser.add_argument(
        "--no-modernize",
        action="store_true",
        help="Skip Atlas modernization (assume Python file already exists)",
    )
    parser.add_argument(
        "--message",
        type=str,
        default=None,
        help="Custom message for Atlas modernization",
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

    args = parser.parse_args()

    # Validate inputs
    cobol_file = Path(args.cobol_file)
    if not cobol_file.exists():
        print(f"Error: COBOL file not found: {cobol_file}", file=sys.stderr)
        sys.exit(1)

    groundtruth_dir = Path(args.groundtruth_dir)
    if not groundtruth_dir.exists():
        print(f"Error: Groundtruth directory not found: {groundtruth_dir}", file=sys.stderr)
        sys.exit(1)

    prediction_dir = Path(args.output_dir) if args.output_dir else None

    # Validate weights
    if abs(sum(args.weights) - 1.0) > 1e-6:
        print(f"Error: Weights must sum to 1.0, got {sum(args.weights)}", file=sys.stderr)
        sys.exit(1)

    # Run evaluation
    try:
        result = evaluate_atlas_output(
            cobol_file=cobol_file,
            groundtruth_dir=groundtruth_dir,
            prediction_dir=prediction_dir,
            lang=args.lang,
            weights=tuple(args.weights),
            run_modernization=not args.no_modernize,
            atlas_message=args.message,
        )

        # Print results
        print("\n" + "=" * 60)
        print("Atlas Modernization & CodeBLEU Evaluation Results")
        print("=" * 60)

        if "error" in result:
            print(f"Error: {result['error']}")
            if "atlas_output" in result:
                print(f"\nAtlas output:\n{result['atlas_output']}")
            sys.exit(1)

        print(f"COBOL file:     {result['cobol_file']}")
        print(f"Generated:      {result['generated_python']}")
        print(f"Groundtruth:    {result['groundtruth_python']}")
        print()
        print("CodeBLEU Scores:")
        print(f"  CodeBLEU:              {result['codebleu']:.4f}")
        print(f"  N-gram match:          {result['ngram_match_score']:.4f}")
        print(f"  Weighted n-gram match: {result['weighted_ngram_match_score']:.4f}")
        print(f"  Syntax match:          {result['syntax_match_score']:.4f}")
        print(f"  Data-flow match:       {result['dataflow_match_score']:.4f}")
        print("=" * 60)

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

