# AGENTS.md

This file is the agent-facing README for the Legacy2Modern (L2M) repository. It provides concise, machine-friendly rules, constraints, and workflows so automated or semi-automated AI agents — and humans operating as agents — can safely and productively make changes to this Python project.

Read this file first. For longer explanations and examples, see `l2m/docs/agents.md`.

---

## Quick summary

- Project purpose: L2M is a Python multi-agent framework for analyzing and modernizing legacy COBOL into modern code (primarily Python).
- Primary code: `l2m/src/`
- Agent implementations: `l2m/src/agents/`
- Docs: `l2m/docs/` (`l2m/docs/agents.md` is the human-facing agent doc)
- Tests: `l2m/tests/`
- Fixtures / sample data: `l2m/data/`
- Dependency manifests: `l2m/pyproject.toml`, `l2m/requirements.txt`
- CI config: `.github/workflows/ci.yml`

If you are an autonomous agent, follow these rules exactly. When uncertain, stop and ask a human reviewer.

---

## Critical rules (MUST follow)

- The orchestrator must delegate specialized tasks (analysis, translation, review, testing, refactor) to the respective agents as implemented in `l2m/src/agents/orchestrator_agent.py`. Do NOT bypass these agents to perform their work yourself when handoffs are enabled.
- The Tester Agent MUST be invoked to generate and run tests for changes that affect behavior. Tests are mandatory for translations and behavior-changing fixes.
- Do not commit secrets or API keys. Use `.env` (copy from `.env.example`) or secure secret stores for credentials.
- Do not start long-running servers, watchers, or interactive processes in automated runs (for example, avoid `npm run dev`, `python -m http.server`, or any process that doesn't terminate).
- Make small, focused changes. Avoid monolithic PRs that touch many unrelated files. Each PR should address one logical change.
- Preserve observable behavior unless the change is explicitly intended to alter behavior and is documented.
- Respect privacy: do not write secrets or private data into session logs, test outputs, or persistent session storage.

---

## How to operate (recommended workflow)

1. Read `l2m/AGENTS.md` and `l2m/docs/agents.md`.
2. Identify the minimal set of files to change under `l2m/src/`.
3. Add or update tests under `l2m/tests/` for any behavioral change.
4. Run local linters and tests (see "Local commands" below).
5. Update documentation in `l2m/docs/` if the change affects public behavior or APIs.
6. Open a PR with:
   - A clear description of what changed and why
   - Relevant test results / failing logs if CI fails
   - Any manual review notes for maintainers

---

## Local commands and tooling

(Only run these when you have the required environment and permissions.)

- Create local env: `cp .env.example .env` and add `OPENAI_API_KEY` (do not commit).
- Install dependencies:
  - `pip install -r requirements.txt`
  - or use project tooling if specified in `pyproject.toml`.
- Run tests:
  - `pytest -q`
  - Run a specific test file: `pytest tests/path/to/test_file.py::test_name -q`
- Linters & formatters (as used in CI):
  - `ruff .` (lint)
  - `black .` (format)
  - `mypy l2m` (optional static typing checks)
- Run package locally (if needed):
  - `pip install -e .`
  - `l2m` or execute `python -m l2m.src.main` per project instructions

Notes:
- Install required helper commands before running steps. Do not assume they are preinstalled on CI or remote sandboxes.
- Do not run non-terminating or interactive commands in automated sessions.

---

## Testing & CI expectations

- CI runs are configured in `.github/workflows/ci.yml`. Ensure your changes do not break CI.
- Unit tests are mandatory for behavior changes. Prefer deterministic tests; mock network I/O and heavy external dependencies.
- If you modify code used by many modules, run the relevant subset of tests locally before opening a PR.
- When fixing flakiness or adding integration tests, include a rationale in the PR.

---

## Handoffs & context passing

- Pass minimal structured context between agents: analysis JSON, file path(s), unified diff patch, and a short task description. Avoid dumping entire files or repo state when a smaller artifact will do.
- Sessions/history may be persisted (see `l2m/data/` for sample storage). Avoid persisting secrets or PII.
- If a handoff fails, retry with clearer or narrower context; if it still fails, stop and request human intervention.

---

## Coding style & conventions (Python)

- Follow existing project conventions. Prefer small, testable functions and clear naming.
- Add docstrings for new public functions and classes.
- Keep changes file-local where possible to reduce review scope.
- Run `black` and `ruff` locally before committing.
- If adding CLI entry points, ensure they are covered by unit tests or integration tests.

---

## Agent prompt templates (concise)

Use these minimal templates when calling the specialized agents. They are intended to be machine-readable and to limit scope.

- Analyzer:
  - "Analyze `path/to/file.cbl`. Return a JSON object with program structure, variables, sections, procedures, and a short summary. Keep output machine-readable."
- Translator:
  - "Given the analyzer JSON and `file.cbl`, generate a Python module that preserves business logic. Make functions small and testable; include a `main()` only if appropriate."
- Reviewer:
  - "Review the Python module for correctness, edge cases, style, and maintainability. Return a prioritized list of fixes with exact file and line ranges and suggested diffs."
- Tester:
  - "Generate pytest-compatible deterministic unit tests for the module. Do not use network access in tests; mock external dependencies. Return the test code and instructions to run it."
- Refactor:
  - "Refactor the provided Python module to improve readability and maintainability while preserving behavior. Keep changes small and include unit tests for refactored code."
- Orchestrator:
  - "Coordinate: Analyzer → Translator → Reviewer → Tester → Refactor. Provide minimal context to each step; validate outputs; stop and request human help on persistent failures."

---

## Checklists

Pre-change checklist
- [ ] I read `l2m/AGENTS.md` and `l2m/docs/agents.md`.
- [ ] I scoped the minimal files to change under `l2m/src/`.
- [ ] I did not include secrets in any changes.
- [ ] I added or updated unit tests covering changed behavior.
- [ ] I ran `black` and `ruff` locally (or equivalent formatters/linters).

Pre-PR checklist
- [ ] Unit tests for the changed areas pass locally.
- [ ] Linting/formatting applied and clean.
- [ ] Documentation updated for public-facing changes (`l2m/docs/`).
- [ ] PR description explains the motivation and links to failing tests (if applicable).

---

## When to stop and ask a human

Stop and request human help if any of the following apply:
- Tests fail persistently after reasonable fixes.
- You need secrets, credentials, or network access that cannot be securely provided.
- The requested change is large, ambiguous, or touches security-sensitive areas.
- You are unsure about intended behavior or high-level architecture decisions.

When asking for help, include:
- A one-paragraph summary of what you attempted
- Relevant failing test logs
- The minimal reproduction steps (file paths and commands)

---

## Where to get more information

- Agent system docs: `l2m/docs/agents.md`
- Project README: `l2m/README.md`
- Contribution guide: `l2m/CONTRIBUTING.md`
- CI config: `.github/workflows/ci.yml`
- Agent implementations: `l2m/src/agents/`

---

Keep this file concise and relevant. Update `l2m/docs/agents.md` for longer, human-focused explanations when you change major agent behaviors. Agents should prefer this file for machine-friendly rules and `l2m/docs/agents.md` for examples and extended guidance.