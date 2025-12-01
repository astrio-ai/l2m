"""CodeBLEU evaluator for measuring code similarity between predictions and references."""

from pathlib import Path
from typing import Dict, List, Optional, Tuple, Union

try:
    from codebleu import calc_codebleu
except ImportError as e:
    raise ImportError(
        "codebleu package is required. Install it with: pip install codebleu"
    ) from e


def _check_tree_sitter_python():
    """Check if tree-sitter-python is available, either directly or via grep-ast."""
    # Try direct import first (standard codebleu way)
    try:
        import tree_sitter_python as tspython  # type: ignore[import-untyped]  # noqa: F401
        return True
    except ImportError:
        pass

    # Try via grep-ast (which Atlas already uses)
    try:
        from grep_ast.tsl import get_parser
        parser = get_parser("python")
        if parser is not None:
            return True
    except Exception:
        pass

    return False


class CodeBLEUEvaluator:
    """
    CodeBLEU evaluator for measuring code similarity.

    CodeBLEU is a weighted combination of:
    - n-gram match (BLEU)
    - weighted n-gram match (BLEU-weighted)
    - AST match (syntactic similarity)
    - data-flow match (semantic similarity)

    Attributes:
        lang: Programming language for evaluation (default: "python").
        weights: Weights for [ngram, weighted_ngram, syntax, dataflow] scores (default: equal weights).
        tokenizer: Optional custom tokenizer function. If None, uses s.split().
    """

    AVAILABLE_LANGS = [
        "python",
        "c",
        "c_sharp",
        "cpp",
        "java",
        "javascript",
        "php",
        "go",
        "ruby",
        "rust",
    ]

    def __init__(
        self,
        lang: str = "python",
        weights: Tuple[float, float, float, float] = (0.25, 0.25, 0.25, 0.25),
        tokenizer: Optional[callable] = None,
    ):
        """
        Initialize CodeBLEU evaluator.

        Args:
            lang: Programming language code. Must be one of AVAILABLE_LANGS.
            weights: Tuple of 4 floats for [ngram, weighted_ngram, syntax, dataflow] weights.
                Must sum to 1.0.
            tokenizer: Optional function to tokenize code strings. If None, uses str.split().

        Raises:
            ValueError: If lang is not supported or weights don't sum to 1.0.
            ImportError: If required tree-sitter language grammar is not available.
        """
        if lang not in self.AVAILABLE_LANGS:
            raise ValueError(
                f"Unsupported language: {lang}. "
                f"Available languages: {self.AVAILABLE_LANGS}"
            )

        if abs(sum(weights) - 1.0) > 1e-6:
            raise ValueError(f"Weights must sum to 1.0, got {sum(weights)}")

        # Check if tree-sitter grammar is available (for Python, check if grep-ast provides it)
        if lang == "python" and not _check_tree_sitter_python():
            raise ImportError(
                "tree-sitter-python grammar is required for CodeBLEU evaluation. "
                "Install it with: pip install tree-sitter-python\n"
                "Note: If you're using grep-ast (which Atlas uses), ensure it's properly configured."
            )

        self.lang = lang
        self.weights = weights
        self.tokenizer = tokenizer

    def evaluate(
        self,
        predictions: Union[str, List[str]],
        references: Union[str, List[str], List[List[str]]],
    ) -> Dict[str, float]:
        """
        Evaluate predictions against references using CodeBLEU.

        Args:
            predictions: Single prediction string or list of prediction strings.
            references: Single reference string, list of reference strings, or list of
                lists (for multiple references per prediction).

        Returns:
            Dictionary with scores:
            - codebleu: Final CodeBLEU score
            - ngram_match_score: n-gram match (BLEU) score
            - weighted_ngram_match_score: Weighted n-gram match score
            - syntax_match_score: AST match score
            - dataflow_match_score: Data-flow match score
        """
        # Normalize inputs to lists
        if isinstance(predictions, str):
            predictions = [predictions]

        if isinstance(references, str):
            references = [[references]]
        elif isinstance(references, list) and len(references) > 0:
            if isinstance(references[0], str):
                # Single reference per prediction
                references = [[ref] for ref in references]
            # Otherwise, references is already List[List[str]]

        if len(predictions) != len(references):
            raise ValueError(
                f"Number of predictions ({len(predictions)}) must match "
                f"number of reference groups ({len(references)})"
            )

        # Calculate CodeBLEU
        try:
            result = calc_codebleu(
                references=references,
                predictions=predictions,
                lang=self.lang,
                weights=self.weights,
                tokenizer=self.tokenizer,
            )
            return result
        except TypeError as e:
            # Handle tree-sitter compatibility issues
            if "an integer is required" in str(e):
                raise ImportError(
                    f"Tree-sitter compatibility issue: {e}\n"
                    "Please install compatible tree-sitter version: "
                    "pip install 'tree-sitter>=0.24.0' tree-sitter-python"
                ) from e
            raise

    def evaluate_files(
        self,
        prediction_paths: Union[Path, List[Path]],
        reference_paths: Union[Path, List[Path], List[List[Path]]],
    ) -> Dict[str, float]:
        """
        Evaluate code from files using CodeBLEU.

        Args:
            prediction_paths: Path to prediction file or list of paths.
            reference_paths: Path to reference file, list of paths, or list of lists
                (for multiple references per prediction).

        Returns:
            Dictionary with CodeBLEU scores (same format as evaluate()).
        """
        # Normalize to lists
        if isinstance(prediction_paths, (str, Path)):
            prediction_paths = [Path(prediction_paths)]

        if isinstance(reference_paths, (str, Path)):
            reference_paths = [[Path(reference_paths)]]
        elif isinstance(reference_paths, list):
            if len(reference_paths) > 0 and isinstance(reference_paths[0], (str, Path)):
                # Single reference per prediction
                reference_paths = [[Path(ref)] for ref in reference_paths]
            else:
                # Multiple references per prediction
                reference_paths = [
                    [Path(p) for p in ref_group] for ref_group in reference_paths
                ]

        # Read files
        predictions = [Path(p).read_text(encoding="utf-8") for p in prediction_paths]

        references: List[List[str]] = []
        for ref_group in reference_paths:
            ref_group_texts = [
                Path(ref).read_text(encoding="utf-8") for ref in ref_group
            ]
            references.append(ref_group_texts)

        return self.evaluate(predictions, references)

    def batch_evaluate(
        self,
        prediction_reference_pairs: List[Tuple[str, Union[str, List[str]]]],
    ) -> List[Dict[str, float]]:
        """
        Evaluate multiple prediction-reference pairs.

        Args:
            prediction_reference_pairs: List of (prediction, reference) tuples.
                Reference can be a string or list of strings.

        Returns:
            List of score dictionaries (one per pair).
        """
        results = []
        for prediction, reference in prediction_reference_pairs:
            if isinstance(reference, str):
                ref_list = [[reference]]
            else:
                # reference is List[str], wrap in another list
                ref_list = [reference]
            result = self.evaluate([prediction], ref_list)
            results.append(result)
        return results


def evaluate_codebleu(
    predictions: Union[str, List[str]],
    references: Union[str, List[str], List[List[str]]],
    lang: str = "python",
    weights: Tuple[float, float, float, float] = (0.25, 0.25, 0.25, 0.25),
    tokenizer: Optional[callable] = None,
) -> Dict[str, float]:
    """
    Convenience function to evaluate CodeBLEU scores.

    Args:
        predictions: Single prediction string or list of prediction strings.
        references: Single reference string, list of reference strings, or list of
            lists (for multiple references per prediction).
        lang: Programming language (default: "python").
        weights: Weights for [ngram, weighted_ngram, syntax, dataflow] (default: equal).
        tokenizer: Optional tokenizer function (default: str.split()).

    Returns:
        Dictionary with CodeBLEU scores.

    Example:
        >>> prediction = "def add(a, b):\\n    return a + b"
        >>> reference = "def sum(x, y):\\n    return x + y"
        >>> result = evaluate_codebleu(prediction, reference, lang="python")
        >>> print(result["codebleu"])
    """
    evaluator = CodeBLEUEvaluator(lang=lang, weights=weights, tokenizer=tokenizer)
    return evaluator.evaluate(predictions, references)

