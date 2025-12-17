"""Computational Accuracy (CA) evaluator for measuring functional equivalence.

CA measures whether modernized code produces the same outputs as groundtruth
code when given the same inputs.
"""

import subprocess
import sys
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Union, Any
import tempfile
import os


def normalize_output(output: str) -> str:
    """
    Normalize output for comparison.
    
    Removes extra whitespace, normalizes line endings, and converts to lowercase.
    This allows for flexible comparison while still catching meaningful differences.
    
    Args:
        output: Raw output string.
        
    Returns:
        Normalized output string.
    """
    # Normalize line endings
    output = output.replace('\r\n', '\n').replace('\r', '\n')
    
    # Split into lines and strip each line
    lines = [line.strip() for line in output.split('\n')]
    
    # Remove empty lines
    lines = [line for line in lines if line]
    
    # Join with single space and convert to lowercase
    normalized = ' '.join(lines).lower()
    
    # Remove multiple spaces
    while '  ' in normalized:
        normalized = normalized.replace('  ', ' ')
    
    return normalized.strip()


def run_python_code(
    code: str,
    input_data: Optional[str] = None,
    timeout: Optional[int] = None,
    cwd: Optional[Path] = None,
) -> Tuple[str, str, int]:
    """
    Run Python code and capture output.
    
    Args:
        code: Python code to execute.
        input_data: Optional input to provide via stdin.
        timeout: Optional timeout in seconds.
        cwd: Optional working directory.
        
    Returns:
        Tuple of (stdout, stderr, returncode).
    """
    with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
        temp_file = Path(f.name)
        f.write(code)
    
    try:
        result = subprocess.run(
            [sys.executable, str(temp_file)],
            input=input_data,
            capture_output=True,
            text=True,
            timeout=timeout,
            cwd=str(cwd) if cwd else None,
        )
        return result.stdout, result.stderr, result.returncode
    finally:
        if temp_file.exists():
            temp_file.unlink()


def run_python_file(
    file_path: Path,
    input_data: Optional[str] = None,
    timeout: Optional[int] = None,
) -> Tuple[str, str, int]:
    """
    Run a Python file and capture output.
    
    Args:
        file_path: Path to Python file.
        input_data: Optional input to provide via stdin.
        timeout: Optional timeout in seconds.
        
    Returns:
        Tuple of (stdout, stderr, returncode).
    """
    file_path = Path(file_path).resolve()
    result = subprocess.run(
        [sys.executable, str(file_path)],
        input=input_data,
        capture_output=True,
        text=True,
        timeout=timeout,
        cwd=str(file_path.parent),
    )
    return result.stdout, result.stderr, result.returncode


class CAEvaluator:
    """
    Computational Accuracy evaluator.
    
    Measures functional equivalence by comparing outputs of groundtruth and
    prediction code when given the same inputs.
    
    Attributes:
        timeout: Timeout in seconds for code execution (default: 30).
        normalize: Whether to normalize outputs before comparison (default: True).
        strict_match: Whether to require exact match (default: False, uses normalized).
    """
    
    def __init__(
        self,
        timeout: int = 30,
        normalize: bool = True,
        strict_match: bool = False,
    ):
        """
        Initialize CA evaluator.
        
        Args:
            timeout: Timeout in seconds for code execution.
            normalize: Whether to normalize outputs before comparison.
            strict_match: If True, requires exact match (ignores normalize).
        """
        self.timeout = timeout
        self.normalize = normalize
        self.strict_match = strict_match
    
    def evaluate_outputs(
        self,
        groundtruth_output: str,
        prediction_output: str,
        groundtruth_stderr: str = "",
        prediction_stderr: str = "",
        groundtruth_returncode: int = 0,
        prediction_returncode: int = 0,
    ) -> Dict[str, Any]:
        """
        Compare outputs and compute CA metrics.
        
        Args:
            groundtruth_output: Output from groundtruth code.
            prediction_output: Output from prediction code.
            groundtruth_stderr: Stderr from groundtruth code.
            prediction_stderr: Stderr from prediction code.
            groundtruth_returncode: Return code from groundtruth code.
            prediction_returncode: Return code from prediction code.
            
        Returns:
            Dictionary with CA metrics:
            - exact_match: Boolean, exact output match
            - normalized_match: Boolean, normalized output match
            - returncode_match: Boolean, return codes match
            - ca_score: Float, overall CA score (1.0 if all match, 0.0 otherwise)
            - groundtruth_output: Normalized groundtruth output
            - prediction_output: Normalized prediction output
            - error: Optional error message if execution failed
        """
        # Check return codes
        returncode_match = groundtruth_returncode == prediction_returncode
        
        # Normalize outputs if requested
        if self.normalize and not self.strict_match:
            gt_normalized = normalize_output(groundtruth_output)
            pred_normalized = normalize_output(prediction_output)
            exact_match = gt_normalized == pred_normalized
            normalized_match = exact_match
        else:
            exact_match = groundtruth_output == prediction_output
            normalized_match = exact_match
            gt_normalized = groundtruth_output
            pred_normalized = prediction_output
        
        # Compute CA score
        # Perfect match if outputs match and return codes match
        if exact_match and returncode_match:
            ca_score = 1.0
        elif normalized_match and returncode_match:
            ca_score = 0.9  # Close match
        elif returncode_match:
            ca_score = 0.5  # Same return code but different output
        else:
            ca_score = 0.0  # Different return codes or execution failure
        
        return {
            "exact_match": exact_match,
            "normalized_match": normalized_match,
            "returncode_match": returncode_match,
            "ca_score": ca_score,
            "groundtruth_output": gt_normalized,
            "prediction_output": pred_normalized,
            "groundtruth_stderr": groundtruth_stderr,
            "prediction_stderr": prediction_stderr,
            "groundtruth_returncode": groundtruth_returncode,
            "prediction_returncode": prediction_returncode,
        }
    
    def evaluate_code(
        self,
        groundtruth_code: str,
        prediction_code: str,
        input_data: Optional[str] = None,
    ) -> Dict[str, Any]:
        """
        Evaluate CA by running both code snippets.
        
        Args:
            groundtruth_code: Groundtruth Python code.
            prediction_code: Prediction Python code.
            input_data: Optional input to provide via stdin.
            
        Returns:
            Dictionary with CA metrics (see evaluate_outputs).
        """
        # Run groundtruth code
        try:
            gt_stdout, gt_stderr, gt_rc = run_python_code(
                groundtruth_code,
                input_data=input_data,
                timeout=self.timeout,
            )
        except subprocess.TimeoutExpired:
            return {
                "error": "groundtruth_timeout",
                "ca_score": 0.0,
            }
        except Exception as e:
            return {
                "error": f"groundtruth_error: {str(e)}",
                "ca_score": 0.0,
            }
        
        # Run prediction code
        try:
            pred_stdout, pred_stderr, pred_rc = run_python_code(
                prediction_code,
                input_data=input_data,
                timeout=self.timeout,
            )
        except subprocess.TimeoutExpired:
            return {
                "error": "prediction_timeout",
                "ca_score": 0.0,
                "groundtruth_output": normalize_output(gt_stdout) if self.normalize else gt_stdout,
            }
        except Exception as e:
            return {
                "error": f"prediction_error: {str(e)}",
                "ca_score": 0.0,
                "groundtruth_output": normalize_output(gt_stdout) if self.normalize else gt_stdout,
            }
        
        return self.evaluate_outputs(
            groundtruth_output=gt_stdout,
            prediction_output=pred_stdout,
            groundtruth_stderr=gt_stderr,
            prediction_stderr=pred_stderr,
            groundtruth_returncode=gt_rc,
            prediction_returncode=pred_rc,
        )
    
    def evaluate_files(
        self,
        groundtruth_file: Path,
        prediction_file: Path,
        input_data: Optional[str] = None,
    ) -> Dict[str, Any]:
        """
        Evaluate CA by running both files.
        
        Args:
            groundtruth_file: Path to groundtruth Python file.
            prediction_file: Path to prediction Python file.
            input_data: Optional input to provide via stdin.
            
        Returns:
            Dictionary with CA metrics (see evaluate_outputs).
        """
        # Run groundtruth file
        try:
            gt_stdout, gt_stderr, gt_rc = run_python_file(
                Path(groundtruth_file),
                input_data=input_data,
                timeout=self.timeout,
            )
        except subprocess.TimeoutExpired:
            return {
                "error": "groundtruth_timeout",
                "ca_score": 0.0,
            }
        except Exception as e:
            return {
                "error": f"groundtruth_error: {str(e)}",
                "ca_score": 0.0,
            }
        
        # Run prediction file
        try:
            pred_stdout, pred_stderr, pred_rc = run_python_file(
                Path(prediction_file),
                input_data=input_data,
                timeout=self.timeout,
            )
        except subprocess.TimeoutExpired:
            return {
                "error": "prediction_timeout",
                "ca_score": 0.0,
                "groundtruth_output": normalize_output(gt_stdout) if self.normalize else gt_stdout,
            }
        except Exception as e:
            return {
                "error": f"prediction_error: {str(e)}",
                "ca_score": 0.0,
                "groundtruth_output": normalize_output(gt_stdout) if self.normalize else gt_stdout,
            }
        
        return self.evaluate_outputs(
            groundtruth_output=gt_stdout,
            prediction_output=pred_stdout,
            groundtruth_stderr=gt_stderr,
            prediction_stderr=pred_stderr,
            groundtruth_returncode=gt_rc,
            prediction_returncode=pred_rc,
        )
    
    def batch_evaluate(
        self,
        groundtruth_prediction_pairs: List[Tuple[Union[str, Path], Union[str, Path]]],
        input_data_list: Optional[List[Optional[str]]] = None,
    ) -> List[Dict[str, Any]]:
        """
        Evaluate multiple groundtruth-prediction pairs.
        
        Args:
            groundtruth_prediction_pairs: List of (groundtruth, prediction) tuples.
                Each can be a string (code) or Path (file).
            input_data_list: Optional list of input data (one per pair).
                If None, no input is provided.
                
        Returns:
            List of CA metric dictionaries.
        """
        if input_data_list is None:
            input_data_list = [None] * len(groundtruth_prediction_pairs)
        
        if len(input_data_list) != len(groundtruth_prediction_pairs):
            raise ValueError(
                f"input_data_list length ({len(input_data_list)}) must match "
                f"pairs length ({len(groundtruth_prediction_pairs)})"
            )
        
        results = []
        for (gt, pred), input_data in zip(groundtruth_prediction_pairs, input_data_list):
            if isinstance(gt, Path) and isinstance(pred, Path):
                result = self.evaluate_files(gt, pred, input_data=input_data)
            elif isinstance(gt, str) and isinstance(pred, str):
                result = self.evaluate_code(gt, pred, input_data=input_data)
            else:
                result = {
                    "error": "type_mismatch",
                    "ca_score": 0.0,
                }
            results.append(result)
        
        return results


def evaluate_ca(
    groundtruth: Union[str, Path],
    prediction: Union[str, Path],
    input_data: Optional[str] = None,
    timeout: int = 30,
    normalize: bool = True,
    strict_match: bool = False,
) -> Dict[str, Any]:
    """
    Convenience function to evaluate CA.
    
    Args:
        groundtruth: Groundtruth code (string) or file path.
        prediction: Prediction code (string) or file path.
        input_data: Optional input to provide via stdin.
        timeout: Timeout in seconds (default: 30).
        normalize: Whether to normalize outputs (default: True).
        strict_match: Whether to require exact match (default: False).
        
    Returns:
        Dictionary with CA metrics.
        
    Example:
        >>> gt_code = "print('Hello, World!')"
        >>> pred_code = "print('Hello, World!')"
        >>> result = evaluate_ca(gt_code, pred_code)
        >>> print(result["ca_score"])  # 1.0
    """
    evaluator = CAEvaluator(timeout=timeout, normalize=normalize, strict_match=strict_match)
    
    if isinstance(groundtruth, Path) and isinstance(prediction, Path):
        return evaluator.evaluate_files(groundtruth, prediction, input_data=input_data)
    elif isinstance(groundtruth, str) and isinstance(prediction, str):
        return evaluator.evaluate_code(groundtruth, prediction, input_data=input_data)
    else:
        raise ValueError("groundtruth and prediction must be both strings or both Paths")

