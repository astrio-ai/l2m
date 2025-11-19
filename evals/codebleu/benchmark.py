"""CodeBLEU benchmark runner for evaluating L2M modernization results."""

import json
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

from tqdm import tqdm

from evals.codebleu.evaluator import CodeBLEUEvaluator


class CodeBLEUBenchmark:
    """
    Benchmark runner for CodeBLEU evaluation on data directory.

    Matches prediction files with reference files and computes CodeBLEU scores.
    """

    def __init__(
        self,
        lang: str = "python",
        weights: Tuple[float, float, float, float] = (0.25, 0.25, 0.25, 0.25),
        tokenizer: Optional[callable] = None,
    ):
        """
        Initialize CodeBLEU benchmark.

        Args:
            lang: Programming language (default: "python").
            weights: CodeBLEU component weights (default: equal weights).
            tokenizer: Optional tokenizer function.
        """
        self.evaluator = CodeBLEUEvaluator(lang=lang, weights=weights, tokenizer=tokenizer)
        self.lang = lang

    def find_reference_files(self, data_dir: Path, exclude_patterns: Optional[List[str]] = None) -> Dict[str, Path]:
        """
        Find reference Python files in data directory.

        Args:
            data_dir: Root directory containing reference files.
            exclude_patterns: Optional list of filename patterns to exclude (e.g., ["test_*.py"]).

        Returns:
            Dictionary mapping file stem to Path (e.g., {"HELLO": Path("data/hello/HELLO.py")}).
        """
        if exclude_patterns is None:
            exclude_patterns = ["test_*.py"]

        references = {}
        for py_file in data_dir.rglob("*.py"):
            # Skip excluded patterns
            if any(py_file.match(pattern) for pattern in exclude_patterns):
                continue

            # Skip test files
            if py_file.name.startswith("test_"):
                continue

            # Use stem as key (filename without extension)
            stem = py_file.stem
            if stem not in references:
                references[stem] = py_file
            else:
                # Handle duplicates by preferring files in subdirectories
                # Keep the one with the shorter path (closer to root)
                if len(py_file.parts) < len(references[stem].parts):
                    references[stem] = py_file

        return references

    def find_prediction_files(
        self, prediction_dir: Path, reference_keys: Optional[List[str]] = None
    ) -> Dict[str, Path]:
        """
        Find prediction Python files in a directory.

        Args:
            prediction_dir: Directory containing prediction files.
            reference_keys: Optional list of expected file stems. If provided, only matches these.

        Returns:
            Dictionary mapping file stem to Path.
        """
        predictions = {}
        for py_file in prediction_dir.rglob("*.py"):
            stem = py_file.stem
            if reference_keys is None or stem in reference_keys:
                if stem not in predictions:
                    predictions[stem] = py_file

        return predictions

    def evaluate_pair(self, prediction_file: Path, reference_file: Path) -> Dict[str, float]:
        """
        Evaluate a single prediction-reference pair.

        Args:
            prediction_file: Path to prediction file.
            reference_file: Path to reference file.

        Returns:
            Dictionary with CodeBLEU scores.
        """
        try:
            prediction = prediction_file.read_text(encoding="utf-8")
            reference = reference_file.read_text(encoding="utf-8")

            result = self.evaluator.evaluate(prediction, reference)
            return result
        except Exception as e:
            return {"error": str(e)}

    def evaluate_directory(
        self,
        data_dir: Path,
        prediction_dir: Optional[Path] = None,
        output_file: Optional[Path] = None,
        exclude_patterns: Optional[List[str]] = None,
    ) -> Dict[str, Any]:
        """
        Evaluate predictions against references in data directory.

        Args:
            data_dir: Directory containing reference files (groundtruth).
            prediction_dir: Directory containing prediction files. If None, uses data_dir.
            output_file: Optional path to save results as JSON.
            exclude_patterns: Optional patterns to exclude from references.

        Returns:
            Dictionary with evaluation results including per-file scores and summary statistics.
        """
        if prediction_dir is None:
            prediction_dir = data_dir

        # Find reference and prediction files
        print(f"Scanning for reference files in {data_dir}...")
        references = self.find_reference_files(data_dir, exclude_patterns=exclude_patterns)
        print(f"Found {len(references)} reference files")

        print(f"Scanning for prediction files in {prediction_dir}...")
        predictions = self.find_prediction_files(prediction_dir, reference_keys=list(references.keys()))
        print(f"Found {len(predictions)} prediction files")

        # Find matching pairs
        common_keys = set(references.keys()) & set(predictions.keys())
        missing_predictions = set(references.keys()) - set(predictions.keys())
        extra_predictions = set(predictions.keys()) - set(references.keys())

        if missing_predictions:
            print(f"\nWarning: {len(missing_predictions)} reference files have no matching predictions:")
            for key in sorted(missing_predictions)[:10]:
                print(f"  - {key}")
            if len(missing_predictions) > 10:
                print(f"  ... and {len(missing_predictions) - 10} more")

        if extra_predictions:
            print(f"\nWarning: {len(extra_predictions)} prediction files have no matching references:")
            for key in sorted(extra_predictions)[:10]:
                print(f"  - {key}")
            if len(extra_predictions) > 10:
                print(f"  ... and {len(extra_predictions) - 10} more")

        # Evaluate matching pairs
        print(f"\nEvaluating {len(common_keys)} file pairs...")
        results = {}
        errors = []

        for key in tqdm(sorted(common_keys), desc="Computing CodeBLEU"):
            prediction_file = predictions[key]
            reference_file = references[key]

            result = self.evaluate_pair(prediction_file, reference_file)
            if "error" in result:
                errors.append({"file": key, "error": result["error"]})
            else:
                results[key] = {
                    "prediction_file": str(prediction_file.relative_to(prediction_dir)),
                    "reference_file": str(reference_file.relative_to(data_dir)),
                    **result,
                }

        # Compute summary statistics
        if results:
            codebleu_scores = [r["codebleu"] for r in results.values() if "codebleu" in r]
            ngram_scores = [r.get("ngram_match_score", 0) for r in results.values()]
            weighted_ngram_scores = [r.get("weighted_ngram_match_score", 0) for r in results.values()]
            syntax_scores = [r.get("syntax_match_score", 0) for r in results.values()]
            dataflow_scores = [r.get("dataflow_match_score", 0) for r in results.values()]

            summary = {
                "num_files": len(results),
                "num_errors": len(errors),
                "mean_codebleu": sum(codebleu_scores) / len(codebleu_scores) if codebleu_scores else 0.0,
                "mean_ngram_match": sum(ngram_scores) / len(ngram_scores) if ngram_scores else 0.0,
                "mean_weighted_ngram_match": sum(weighted_ngram_scores) / len(weighted_ngram_scores)
                if weighted_ngram_scores
                else 0.0,
                "mean_syntax_match": sum(syntax_scores) / len(syntax_scores) if syntax_scores else 0.0,
                "mean_dataflow_match": sum(dataflow_scores) / len(dataflow_scores) if dataflow_scores else 0.0,
            }
        else:
            summary = {
                "num_files": 0,
                "num_errors": len(errors),
                "mean_codebleu": 0.0,
                "mean_ngram_match": 0.0,
                "mean_weighted_ngram_match": 0.0,
                "mean_syntax_match": 0.0,
                "mean_dataflow_match": 0.0,
            }

        output = {
            "summary": summary,
            "results": results,
            "errors": errors,
            "config": {
                "lang": self.lang,
                "weights": self.evaluator.weights,
                "data_dir": str(data_dir),
                "prediction_dir": str(prediction_dir),
            },
        }

        # Save to file if requested
        if output_file:
            output_file.parent.mkdir(parents=True, exist_ok=True)
            with open(output_file, "w", encoding="utf-8") as f:
                json.dump(output, f, indent=2)
            print(f"\nResults saved to {output_file}")

        return output

    def print_summary(self, results: Dict[str, Any]):
        """Print a formatted summary of evaluation results."""
        summary = results["summary"]
        print("\n" + "=" * 60)
        print("CodeBLEU Evaluation Summary")
        print("=" * 60)
        print(f"Files evaluated: {summary['num_files']}")
        print(f"Errors: {summary['num_errors']}")
        print()
        print("Mean Scores:")
        print(f"  CodeBLEU:              {summary['mean_codebleu']:.4f}")
        print(f"  N-gram match:          {summary['mean_ngram_match']:.4f}")
        print(f"  Weighted n-gram match: {summary['mean_weighted_ngram_match']:.4f}")
        print(f"  Syntax match:          {summary['mean_syntax_match']:.4f}")
        print(f"  Data-flow match:       {summary['mean_dataflow_match']:.4f}")
        print("=" * 60)

        if summary["num_files"] > 0:
            # Show top and bottom performers
            file_scores = [
                (key, r["codebleu"]) for key, r in results["results"].items() if "codebleu" in r
            ]
            file_scores.sort(key=lambda x: x[1], reverse=True)

            print("\nTop 5 files by CodeBLEU score:")
            for key, score in file_scores[:5]:
                print(f"  {key:30s} {score:.4f}")

            if len(file_scores) > 5:
                print("\nBottom 5 files by CodeBLEU score:")
                for key, score in file_scores[-5:]:
                    print(f"  {key:30s} {score:.4f}")

        if results["errors"]:
            print(f"\nErrors ({len(results['errors'])}):")
            for error in results["errors"][:5]:
                print(f"  {error['file']}: {error['error']}")
            if len(results["errors"]) > 5:
                print(f"  ... and {len(results['errors']) - 5} more errors")


def run_benchmark(
    data_dir: str,
    prediction_dir: Optional[str] = None,
    output_file: Optional[str] = None,
    lang: str = "python",
    weights: Tuple[float, float, float, float] = (0.25, 0.25, 0.25, 0.25),
) -> Dict[str, Any]:
    """
    Convenience function to run CodeBLEU benchmark.

    Args:
        data_dir: Path to directory containing reference files.
        prediction_dir: Optional path to directory containing prediction files.
            If None, uses data_dir.
        output_file: Optional path to save results JSON.
        lang: Programming language (default: "python").
        weights: CodeBLEU weights (default: equal).

    Returns:
        Dictionary with evaluation results.

    Example:
        >>> results = run_benchmark("data/", "output/modernized-python/")
        >>> print(results["summary"]["mean_codebleu"])
    """
    benchmark = CodeBLEUBenchmark(lang=lang, weights=weights)
    results = benchmark.evaluate_directory(
        data_dir=Path(data_dir),
        prediction_dir=Path(prediction_dir) if prediction_dir else None,
        output_file=Path(output_file) if output_file else None,
    )
    benchmark.print_summary(results)
    return results

