# AGENTS.md

## Setup commands

- Install deps: `pip install -r requirements.txt`
- Install package: `pip install -e .`
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

## When to ask for help

- Tests fail persistently after reasonable fixes.
- You need secrets, credentials, or network access that cannot be securely provided.
- The requested change is large, ambiguous, or touches security-sensitive areas.
- You are unsure about intended behavior or high-level architecture decisions.
