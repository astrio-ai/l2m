"""CA benchmark runner for evaluating computational accuracy on datasets."""

import json
from pathlib import Path
from typing import Any, Dict, List, Optional

from tqdm import tqdm

from evals.ca.evaluator import CAEvaluator


class CABenchmark:
    """
    Benchmark runner for CA evaluation on data directory.
    
    Matches prediction files with reference files and computes CA scores
    by running both and comparing outputs.
    """
    
    def __init__(
        self,
        timeout: int = 30,
        normalize: bool = True,
        strict_match: bool = False,
    ):
        """
        Initialize CA benchmark.
        
        Args:
            timeout: Timeout in seconds for code execution.
            normalize: Whether to normalize outputs before comparison.
            strict_match: Whether to require exact match.
        """
        self.evaluator = CAEvaluator(
            timeout=timeout,
            normalize=normalize,
            strict_match=strict_match,
        )
        self.timeout = timeout
        self.normalize = normalize
        self.strict_match = strict_match
    
    def find_reference_files(
        self,
        data_dir: Path,
        pattern: str = "*.py",
        exclude_patterns: Optional[List[str]] = None,
    ) -> Dict[str, Path]:
        """
        Find reference files in data directory.
        
        Args:
            data_dir: Root directory containing reference files.
            pattern: File pattern to match (default: "*.py").
            exclude_patterns: Optional list of filename patterns to exclude.
            
        Returns:
            Dictionary mapping file stem to Path.
        """
        if exclude_patterns is None:
            exclude_patterns = ["test_*.py", "__pycache__"]
        
        references = {}
        for ref_file in data_dir.rglob(pattern):
            # Skip excluded patterns
            if any(ref_file.match(p) for p in exclude_patterns):
                continue
            
            # Skip test files and cache directories
            if ref_file.name.startswith("test_") or "__pycache__" in str(ref_file):
                continue
            
            # Use stem as key (filename without extension)
            stem = ref_file.stem
            if stem not in references:
                references[stem] = ref_file
            else:
                # Handle duplicates by preferring files in subdirectories closer to root
                if len(ref_file.parts) < len(references[stem].parts):
                    references[stem] = ref_file
        
        return references
    
    def find_prediction_files(
        self,
        prediction_dir: Path,
        reference_keys: Optional[List[str]] = None,
        pattern: str = "*.py",
    ) -> Dict[str, Path]:
        """
        Find prediction files in a directory.
        
        Args:
            prediction_dir: Directory containing prediction files.
            reference_keys: Optional list of expected file stems. If provided, only matches these.
            pattern: File pattern to match (default: "*.py").
            
        Returns:
            Dictionary mapping file stem to Path.
        """
        predictions = {}
        for pred_file in prediction_dir.rglob(pattern):
            stem = pred_file.stem
            if reference_keys is None or stem in reference_keys:
                if stem not in predictions:
                    predictions[stem] = pred_file
        
        return predictions
    
    def evaluate_pair(
        self,
        prediction_file: Path,
        reference_file: Path,
        input_data: Optional[str] = None,
    ) -> Dict[str, Any]:
        """
        Evaluate a single prediction-reference pair.
        
        Args:
            prediction_file: Path to prediction file.
            reference_file: Path to reference file.
            input_data: Optional input to provide via stdin.
            
        Returns:
            Dictionary with CA metrics.
        """
        try:
            result = self.evaluator.evaluate_files(
                groundtruth_file=reference_file,
                prediction_file=prediction_file,
                input_data=input_data,
            )
            return result
        except Exception as e:
            return {
                "error": str(e),
                "ca_score": 0.0,
            }
    
    def evaluate_directory(
        self,
        data_dir: Path,
        prediction_dir: Optional[Path] = None,
        output_file: Optional[Path] = None,
        exclude_patterns: Optional[List[str]] = None,
        input_data_map: Optional[Dict[str, str]] = None,
    ) -> Dict[str, Any]:
        """
        Evaluate predictions against references in data directory.
        
        Args:
            data_dir: Directory containing reference files (groundtruth).
            prediction_dir: Directory containing prediction files. If None, uses data_dir.
            output_file: Optional path to save results as JSON.
            exclude_patterns: Optional patterns to exclude from references.
            input_data_map: Optional dictionary mapping file stems to input data.
            
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
        predictions = self.find_prediction_files(
            prediction_dir,
            reference_keys=list(references.keys()),
        )
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
        
        for key in tqdm(sorted(common_keys), desc="Computing CA"):
            prediction_file = predictions[key]
            reference_file = references[key]
            
            # Get input data if provided
            input_data = input_data_map.get(key) if input_data_map else None
            
            result = self.evaluate_pair(
                prediction_file=prediction_file,
                reference_file=reference_file,
                input_data=input_data,
            )
            
            if "error" in result and result["error"] not in ["groundtruth_timeout", "prediction_timeout"]:
                errors.append({"file": key, "error": result["error"]})
            
            results[key] = {
                "prediction_file": str(prediction_file.relative_to(prediction_dir)),
                "reference_file": str(reference_file.relative_to(data_dir)),
                **result,
            }
        
        # Compute summary statistics
        if results:
            ca_scores = [
                r["ca_score"] for r in results.values()
                if "ca_score" in r and "error" not in r
            ]
            exact_matches = [
                r.get("exact_match", False) for r in results.values()
                if "exact_match" in r
            ]
            normalized_matches = [
                r.get("normalized_match", False) for r in results.values()
                if "normalized_match" in r
            ]
            returncode_matches = [
                r.get("returncode_match", False) for r in results.values()
                if "returncode_match" in r
            ]
            
            summary = {
                "num_files": len(results),
                "num_errors": len(errors),
                "mean_ca_score": sum(ca_scores) / len(ca_scores) if ca_scores else 0.0,
                "exact_match_rate": sum(exact_matches) / len(exact_matches) if exact_matches else 0.0,
                "normalized_match_rate": sum(normalized_matches) / len(normalized_matches) if normalized_matches else 0.0,
                "returncode_match_rate": sum(returncode_matches) / len(returncode_matches) if returncode_matches else 0.0,
                "perfect_matches": sum(1 for s in ca_scores if s == 1.0) if ca_scores else 0,
                "partial_matches": sum(1 for s in ca_scores if 0 < s < 1.0) if ca_scores else 0,
                "failures": sum(1 for s in ca_scores if s == 0.0) if ca_scores else 0,
            }
        else:
            summary = {
                "num_files": 0,
                "num_errors": len(errors),
                "mean_ca_score": 0.0,
                "exact_match_rate": 0.0,
                "normalized_match_rate": 0.0,
                "returncode_match_rate": 0.0,
                "perfect_matches": 0,
                "partial_matches": 0,
                "failures": 0,
            }
        
        output = {
            "summary": summary,
            "results": results,
            "errors": errors,
            "config": {
                "timeout": self.timeout,
                "normalize": self.normalize,
                "strict_match": self.strict_match,
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
        print("Computational Accuracy (CA) Evaluation Summary")
        print("=" * 60)
        print(f"Files evaluated: {summary['num_files']}")
        print(f"Errors: {summary['num_errors']}")
        print()
        print("CA Scores:")
        print(f"  Mean CA Score:        {summary['mean_ca_score']:.4f}")
        print(f"  Exact Match Rate:     {summary['exact_match_rate']:.4f}")
        print(f"  Normalized Match Rate: {summary['normalized_match_rate']:.4f}")
        print(f"  Return Code Match:    {summary['returncode_match_rate']:.4f}")
        print()
        print("Match Distribution:")
        print(f"  Perfect Matches (1.0): {summary['perfect_matches']}")
        print(f"  Partial Matches (0-1): {summary['partial_matches']}")
        print(f"  Failures (0.0):        {summary['failures']}")
        print("=" * 60)
        
        if summary["num_files"] > 0:
            # Show top and bottom performers
            file_scores = [
                (key, r["ca_score"]) for key, r in results["results"].items()
                if "ca_score" in r and "error" not in r
            ]
            file_scores.sort(key=lambda x: x[1], reverse=True)
            
            print("\nTop 5 files by CA score:")
            for key, score in file_scores[:5]:
                print(f"  {key:30s} {score:.4f}")
            
            if len(file_scores) > 5:
                print("\nBottom 5 files by CA score:")
                for key, score in file_scores[-5:]:
                    print(f"  {key:30s} {score:.4f}")
        
        if results["errors"]:
            print(f"\nErrors ({len(results['errors'])}):")
            for error in results["errors"][:5]:
                print(f"  {error['file']}: {error['error']}")
            if len(results["errors"]) > 5:
                print(f"  ... and {len(results['errors']) - 5} more errors")


def run_ca_benchmark(
    data_dir: str,
    prediction_dir: Optional[str] = None,
    output_file: Optional[str] = None,
    timeout: int = 30,
    normalize: bool = True,
    strict_match: bool = False,
    input_data_map: Optional[Dict[str, str]] = None,
) -> Dict[str, Any]:
    """
    Convenience function to run CA benchmark.
    
    Args:
        data_dir: Path to directory containing reference files.
        prediction_dir: Optional path to directory containing prediction files.
            If None, uses data_dir.
        output_file: Optional path to save results JSON.
        timeout: Timeout in seconds for code execution.
        normalize: Whether to normalize outputs before comparison.
        strict_match: Whether to require exact match.
        input_data_map: Optional dictionary mapping file stems to input data.
        
    Returns:
        Dictionary with evaluation results.
        
    Example:
        >>> results = run_ca_benchmark("data/groundtruth-python/", "data/output/")
        >>> print(results["summary"]["mean_ca_score"])
    """
    benchmark = CABenchmark(
        timeout=timeout,
        normalize=normalize,
        strict_match=strict_match,
    )
    results = benchmark.evaluate_directory(
        data_dir=Path(data_dir),
        prediction_dir=Path(prediction_dir) if prediction_dir else None,
        output_file=Path(output_file) if output_file else None,
        input_data_map=input_data_map,
    )
    benchmark.print_summary(results)
    return results

