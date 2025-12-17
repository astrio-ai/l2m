# Computational Accuracy (CA) Evaluation

Computational Accuracy (CA) measures **functional equivalence** by comparing the outputs of groundtruth and modernized code when given the same inputs.

## Overview

Unlike CodeBLEU which measures code similarity (syntax, structure), CA measures whether the code produces the same results. This is critical for modernization tools where the goal is functional equivalence, not code similarity.

## Key Features

- **Output Comparison**: Runs both groundtruth and prediction code and compares outputs
- **Normalized Matching**: Flexible comparison that handles whitespace/formatting differences
- **Return Code Checking**: Verifies both programs exit with the same status
- **Timeout Protection**: Prevents hanging programs from blocking evaluation
- **Batch Evaluation**: Evaluate entire directories of files
- **Detailed Metrics**: Provides exact match, normalized match, and overall CA scores

## CA Score Calculation

The CA score is computed as:
- **1.0**: Perfect match (outputs match exactly and return codes match)
- **0.9**: Close match (normalized outputs match and return codes match)
- **0.5**: Partial match (return codes match but outputs differ)
- **0.0**: Failure (different return codes or execution errors)

## Usage

### Basic Usage

```python
from evals.ca import evaluate_ca

# Evaluate code strings
gt_code = "print('Hello, World!')"
pred_code = "print('Hello, World!')"
result = evaluate_ca(gt_code, pred_code)
print(f"CA Score: {result['ca_score']}")  # 1.0

# Evaluate files
from pathlib import Path
result = evaluate_ca(
    Path("groundtruth.py"),
    Path("prediction.py"),
)
```

### Batch Evaluation

```python
from evals.ca import run_ca_benchmark

results = run_ca_benchmark(
    data_dir="data/groundtruth-python/",
    prediction_dir="data/output/modernized/",
    output_file="results/ca_results.json",
)
print(f"Mean CA Score: {results['summary']['mean_ca_score']}")
```

### Command Line

```bash
# Evaluate directory
python -m evals.ca data/groundtruth-python/ data/output/modernized/ -o results.json

# With custom timeout
python -m evals.ca data/groundtruth-python/ data/output/ --timeout 60

# Strict mode (exact match only)
python -m evals.ca data/groundtruth-python/ data/output/ --strict
```

## Integration with CodeBLEU

CA complements CodeBLEU by measuring different aspects:

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

## Input Handling

For programs that require input (e.g., `input()` in Python), provide input data:

```python
result = evaluate_ca(
    groundtruth_code,
    prediction_code,
    input_data="test123\n",  # Input for stdin
)
```

Or use `input_data_map` in batch evaluation:

```python
input_map = {
    "exercise1": "input1\n",
    "exercise2": "input2\n",
}
results = run_ca_benchmark(
    data_dir="data/",
    prediction_dir="output/",
    input_data_map=input_map,
)
```

## Configuration Options

- **timeout**: Maximum execution time in seconds (default: 30)
- **normalize**: Normalize outputs before comparison (default: True)
  - Removes extra whitespace, normalizes line endings, converts to lowercase
- **strict_match**: Require exact match (default: False)
  - If True, ignores normalization and requires byte-for-byte match

## Output Format

Results include:
- `ca_score`: Overall CA score (0.0 to 1.0)
- `exact_match`: Boolean, exact output match
- `normalized_match`: Boolean, normalized output match
- `returncode_match`: Boolean, return codes match
- `groundtruth_output`: Normalized groundtruth output
- `prediction_output`: Normalized prediction output
- `error`: Optional error message if execution failed

## Limitations

1. **Deterministic Output Required**: CA assumes programs produce deterministic outputs for the same inputs
2. **No Side Effects**: Best for pure functions or programs without external dependencies
3. **Input Requirements**: Programs requiring interactive input need explicit input data
4. **Platform Differences**: May need adjustment for platform-specific outputs (e.g., file paths)

## Best Practices

1. **Use with CodeBLEU**: Combine CA and CodeBLEU for comprehensive evaluation
2. **Normalize Outputs**: Use normalization for flexible comparison (handles formatting differences)
3. **Set Appropriate Timeouts**: Prevent infinite loops from blocking evaluation
4. **Handle Errors Gracefully**: Check for execution errors before comparing outputs
5. **Test with Multiple Inputs**: For programs with inputs, test with various inputs

## Example: PyNative Dataset

```python
from evals.ca import run_ca_benchmark

# Evaluate pynative exercises
results = run_ca_benchmark(
    data_dir="data/pynative-python/basic-exercises/",
    prediction_dir="data/output/modernized-pynative/",
    output_file="results/pynative_ca.json",
)

# Check results
summary = results["summary"]
print(f"Perfect Matches: {summary['perfect_matches']}/{summary['num_files']}")
print(f"Mean CA Score: {summary['mean_ca_score']:.4f}")
```

