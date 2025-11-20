# AGENTS.md

## Setup commands

- Install package: `pip install l2m`
- Set up environment: `cp .env.example .env` (add `OPENAI_API_KEY`)

## Coding style

- Python 3.10+ with type hints
- Use `black` for formatting.
- Use `ruff` for linting.
- Follow existing project conventions

## Testing instructions

- Run tests: `pytest tests/ -v`
- Run specific test: `pytest tests/path/to/test_file.py::test_name -v`
- Run with coverage: `pytest --cov=src --cov=cli tests/`
- All tests must pass before committing

## Project structure

- Main code: `src/` and `cli/`
- Tests: `tests/`
- Documentation: `docs/`
- Entry point: `cli/main.py` (CLI command: `l2m`)
- Evaluations: `evals/` (CodeBLEU and Harbor benchmarks)
- Data: `data/` (COBOL files, groundtruth Python, evaluation outputs)
- Output directories:
  - Generated Python: `data/output/` or specified via script args
  - Evaluation results: `data/output/*.json`
  - Visualizations: `data/output/visuals/`

## Important rules

- Do not commit secrets or API keys (use `.env`)
- Add tests for behavior changes
- Run `black` and `ruff` before committing
- Keep changes focused and small
- Update `docs/` for public API changes

## PR instructions

- Title format: `<type>: <description>` (e.g., `fix: resolve import error`)
- Run `pytest` and `ruff` before opening PR
- Include test results in PR description if CI fails

## Evaluation workflows

### CodeBLEU Evaluation

- **Location**: `evals/codebleu/`
- **Purpose**: Measure code quality of L2M-generated Python vs groundtruth using CodeBLEU metric
- **Key files**:
  - `modernize_and_evaluate.py`: Automated workflow (modernize COBOL → evaluate)
  - `evaluator.py`: CodeBLEU evaluation wrapper
  - `benchmark.py`: Batch evaluation runner
  - `visualize.py`: Generate charts from results JSON
- **Usage**:
  ```bash
  # Full workflow: modernize and evaluate
  python -m evals.codebleu.modernize_and_evaluate data/cobol/ data/output/ -o results.json
  
  # Evaluate existing files
  python -m evals.codebleu data/groundtruth/ data/output/generated/
  
  # Visualize results
  python -m evals.codebleu.visualize data/output/results.json
  ```
- **Important**: 
  - Groundtruth Python files must be in same directory or specified `groundtruth_dir`
  - L2M modernization uses `--yes-always`, `--no-stream`, `--no-cache-prompts`, `--map-refresh manual` for speed
  - Visualization saves PNGs to `data/output/visuals/` by default

### Harbor Evaluation

- **Location**: `evals/harbor/`
- **Purpose**: Benchmark L2M agent in containerized environments
- **Note**: Separate optional dependency (`harbor>=0.1.18`)

## Known issues and gotchas

### Dependency conflicts

- **tree-sitter version conflict**: 
  - `codebleu>=0.7.0` requires `tree-sitter<0.23.0`
  - But we need `tree-sitter>=0.24.0` to fix "an integer is required" compatibility issue
  - **Workaround**: Install codebleu first, then upgrade tree-sitter:
    ```bash
    pip install -r requirements.txt
    pip install 'tree-sitter>=0.24.0' --upgrade
    ```

### L2M modernization optimization

- Use `"Convert"` message instead of `"Modernize"` to avoid multi-agent pipeline
- Pass file path explicitly as argument to L2M
- Use optimization flags: `--no-stream`, `--no-cache-prompts`, `--map-refresh manual`
- Set `stdin=subprocess.DEVNULL` for non-interactive mode
- Typical speed: ~2 minutes per COBOL file (not 10+ minutes)

### File organization

- COBOL files: Usually in `data/cobol/` or `data/aws-samples-*/*.cbl`
- Groundtruth Python: `data/groundtruth-python/` or alongside COBOL files
- Generated Python: `data/output/` (maintains subdirectory structure)
- Results JSON: `data/output/*_results.json`
- Visualizations: `data/output/visuals/*.png`

### Environment variables

- See `.env.example` for comprehensive list of supported variables
- Key vars: `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `L2M_MODEL`, `OPENROUTER_API_KEY`
- Rate limiting: Free-tier models have strict limits (30s timeout); paid models allow longer

## When to ask for help

- Tests fail persistently after reasonable fixes.
- You need secrets, credentials, or network access that cannot be securely provided.
- The requested change is large, ambiguous, or touches security-sensitive areas.
- You are unsure about intended behavior or high-level architecture decisions.
