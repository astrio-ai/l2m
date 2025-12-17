# CodeBLEU Evaluation

CodeBLEU measures **code similarity** by combining multiple metrics that evaluate syntactic structure, semantic similarity, and surface-level similarity between prediction and reference code.

## Overview

CodeBLEU is a comprehensive metric designed specifically for evaluating code generation quality. Unlike metrics that only measure text similarity (like BLEU), CodeBLEU considers:
- Surface-level similarity (n-gram matching)
- Syntactic structure (AST matching)
- Semantic similarity (data-flow matching)

This makes CodeBLEU particularly well-suited for evaluating code modernization tasks where the goal is to generate functionally equivalent code that may differ in implementation details.

## Key Features

- **Multi-component Evaluation**: Combines n-gram, weighted n-gram, AST syntax, and data-flow matching
- **Language Support**: Supports Python, C, C++, Java, JavaScript, PHP, Go, Ruby, Rust, and C#
- **Batch Evaluation**: Evaluate entire directories of files with automatic matching
- **Automated Workflow**: Modernize COBOL files and evaluate in a single step
- **Visualization**: Generate charts and plots from evaluation results
- **Detailed Metrics**: Provides component-level scores for deep analysis
- **Flexible Configuration**: Customize weights for different evaluation priorities

## CodeBLEU Score Calculation

CodeBLEU is a weighted combination of four components:

1. **N-gram Match (BLEU)**: Measures token-level n-gram overlap (default weight: 0.25)
2. **Weighted N-gram Match**: Similar to BLEU but with token frequency weighting (default weight: 0.25)
3. **Syntax Match (AST)**: Compares abstract syntax trees for structural similarity (default weight: 0.25)
4. **Data-flow Match**: Compares data-flow graphs for semantic similarity (default weight: 0.25)

Final score: `weight₁ × ngram + weight₂ × weighted_ngram + weight₃ × syntax + weight₄ × dataflow`

All scores range from 0.0 to 1.0, with 1.0 indicating perfect similarity.

## Usage

### Basic Usage

```python
from evals.codebleu import evaluate_codebleu

# Evaluate code strings
prediction = "def add(a, b):\n    return a + b"
reference = "def sum(x, y):\n    return x + y"
result = evaluate_codebleu(prediction, reference)
print(f"CodeBLEU Score: {result['codebleu']:.4f}")
print(f"Syntax Match: {result['syntax_match_score']:.4f}")

# Evaluate files
from pathlib import Path
result = evaluate_codebleu(
    Path("prediction.py"),
    Path("reference.py"),
)
```

### Batch Evaluation

```python
from evals.codebleu import run_benchmark

results = run_benchmark(
    data_dir="data/groundtruth-python/",
    prediction_dir="data/output/modernized/",
    output_file="results/codebleu_results.json",
)
print(f"Mean CodeBLEU: {results['summary']['mean_codebleu']:.4f}")
```

### Command Line

```bash
# Evaluate directory
python -m evals.codebleu data/groundtruth-python/ data/output/modernized/ -o results.json

# Evaluate with custom weights
python -m evals.codebleu data/ output/ --weights 0.3 0.3 0.2 0.2

# Evaluate only references in data/ (compare files within data/)
python -m evals.codebleu data/

# Different programming language
python -m evals.codebleu data/ output/ --lang java
```

### Automated Modernization and Evaluation

The `modernize_and_evaluate` workflow automates the entire process:

```bash
# Modernize COBOL files and evaluate against groundtruth
python -m evals.codebleu.modernize_and_evaluate data/cobol/ data/output/ -o results.json

# Evaluate existing generated files (skip modernization)
python -m evals.codebleu.modernize_and_evaluate data/ data/output/ --skip-modernization

# Custom message for Atlas modernization
python -m evals.codebleu.modernize_and_evaluate data/ data/output/ --message "Convert to Python 3.10+"
```

## Integration with CA Evaluation

CodeBLEU complements Computational Accuracy (CA) by measuring different aspects:

- **CodeBLEU**: Code similarity (syntax, structure, semantics)
- **CA**: Functional equivalence (output correctness)

Use both metrics together for comprehensive evaluation:

```python
from evals.codebleu import evaluate_codebleu
from evals.ca import evaluate_ca

# CodeBLEU for code similarity
codebleu_result = evaluate_codebleu(prediction_code, groundtruth_code)

# CA for functional equivalence
ca_result = evaluate_ca(groundtruth_code, prediction_code)

print(f"CodeBLEU: {codebleu_result['codebleu']:.4f}")
print(f"CA Score: {ca_result['ca_score']:.4f}")
```

## Configuration Options

### Component Weights

Customize the importance of each component:

```python
from evals.codebleu import CodeBLEUEvaluator

# Emphasize syntax and data-flow over n-grams
evaluator = CodeBLEUEvaluator(
    weights=(0.1, 0.1, 0.4, 0.4)  # [ngram, weighted_ngram, syntax, dataflow]
)
```

### Language Selection

```python
# Evaluate Java code
evaluator = CodeBLEUEvaluator(lang="java")

# Supported languages: python, c, cpp, java, javascript, php, go, ruby, rust, c_sharp
```

### Custom Tokenizer

```python
def my_tokenizer(code: str) -> list[str]:
    # Custom tokenization logic
    return code.split()

evaluator = CodeBLEUEvaluator(tokenizer=my_tokenizer)
```

## Output Format

Results include:

- `codebleu`: Final CodeBLEU score (0.0 to 1.0)
- `ngram_match_score`: N-gram BLEU score
- `weighted_ngram_match_score`: Weighted n-gram score
- `syntax_match_score`: AST match score
- `dataflow_match_score`: Data-flow match score

Batch evaluation results also include:
- `summary`: Aggregate statistics (mean, min, max scores)
- `results`: Per-file scores
- `config`: Evaluation configuration

## Visualization

Generate visualizations from evaluation results:

```python
from evals.codebleu import load_results, create_all_visualizations
from pathlib import Path

results = load_results(Path("results.json"))
create_all_visualizations(
    results,
    output_dir=Path("visuals/"),
)
```

Or use the command line:

```bash
python -m evals.codebleu.visualize results.json --output-dir visuals/
```

Visualizations include:
- CodeBLEU score distribution
- Component comparison charts
- Score distribution histograms

## Limitations

1. **Language Grammar Required**: Requires tree-sitter language grammars for AST and data-flow analysis
2. **Perfect Match Assumption**: High scores assume reference code is "correct" - may not reflect functional equivalence
3. **Variable Naming**: Code with different variable names but same logic may score lower
4. **Style Differences**: Code formatting and style differences can affect n-gram scores
5. **Platform Dependencies**: Some language features may require platform-specific handling

## Best Practices

1. **Use with CA**: Combine CodeBLEU with CA evaluation for comprehensive assessment
2. **Customize Weights**: Adjust component weights based on evaluation priorities
3. **Multiple References**: When available, use multiple reference implementations for better evaluation
4. **Language-Specific Tuning**: Different languages may benefit from different weight configurations
5. **Interpret Components**: Examine individual component scores to understand strengths/weaknesses
6. **Visualize Results**: Use visualization tools to identify patterns and outliers

## Example: COBOL Modernization Evaluation

```python
from evals.codebleu.modernize_and_evaluate import modernize_and_evaluate
from pathlib import Path

# Modernize and evaluate COBOL files
results = modernize_and_evaluate(
    cobol_dir=Path("data/cobol/"),
    output_dir=Path("data/output/modernized/"),
    groundtruth_dir=Path("data/groundtruth-python/"),
)

# Check results
summary = results["summary"]
print(f"Files processed: {summary['total_files']}")
print(f"Mean CodeBLEU: {summary['mean_codebleu']:.4f}")
```

## Dependencies

- `codebleu`: Core CodeBLEU implementation
- `tree-sitter-python`: For Python AST analysis (or via `grep-ast`)
- `matplotlib` (optional): For visualization
- `numpy`, `scipy` (optional): For statistical analysis in visualizations

## Installation Notes

For Python evaluation, ensure tree-sitter is properly installed:

```bash
pip install codebleu tree-sitter-python
```

If using `grep-ast` (which Atlas uses), tree-sitter-python may be provided indirectly, but explicit installation is recommended for reliability.

Note: There may be version conflicts between `codebleu` and `tree-sitter`. If you encounter compatibility issues, try:

```bash
pip install codebleu
pip install 'tree-sitter>=0.24.0' --upgrade
```
