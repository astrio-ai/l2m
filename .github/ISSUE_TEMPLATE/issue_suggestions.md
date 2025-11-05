# GitHub Issue Suggestions for L2M

## Good First Issues (Beginner Friendly)

### 1. Add More COBOL Sample Files
**Labels**: `good first issue`, `enhancement`, `documentation`
**Priority**: Low
**Description**:
Add more diverse COBOL sample files to `data/samples/` to help users test the system. Include examples of:
- File I/O operations
- Complex data structures
- Nested PERFORM loops
- CALL statements
- COPY statements

**Acceptance Criteria**:
- Add 5+ new COBOL sample files
- Include expected Python output for each
- Update documentation with sample descriptions

---

### 2. Improve Error Messages
**Labels**: `good first issue`, `enhancement`, `ux`
**Priority**: Medium
**Description**:
Make error messages more user-friendly and actionable. Currently, some errors might be too technical.

**Acceptance Criteria**:
- Review all error messages in agents and tools
- Add context to error messages
- Include suggestions for resolution
- Add tests for error message formatting

---

### 3. Add Code Examples to Documentation
**Labels**: `good first issue`, `documentation`, `enhancement`
**Priority**: Low
**Description**:
Add more code examples to documentation showing real-world usage patterns.

**Files to Update**:
- `docs/getting_started.md`
- `docs/workflows.md`
- `docs/agents.md`

**Acceptance Criteria**:
- Add 3+ code examples per documentation file
- Include examples for common use cases
- Show error handling patterns

---

## Feature Enhancements

### 4. Add Support for Multiple COBOL Files
**Labels**: `enhancement`, `feature`
**Priority**: High
**Description**:
Currently, the pipeline processes one COBOL file at a time. Add support for batch processing multiple files.

**Requirements**:
- Modify `ModernizationPipeline` to accept list of files
- Process files in parallel or sequentially (configurable)
- Aggregate results
- Handle errors per-file (continue on error option)

**Acceptance Criteria**:
- CLI accepts multiple files
- API supports batch processing
- Progress tracking for batch operations
- Tests for batch processing

---

### 5. Add Progress Tracking/Callback System
**Labels**: `enhancement`, `feature`
**Priority**: Medium
**Description**:
Add progress tracking and callback system so users can monitor long-running modernization tasks.

**Requirements**:
- Add progress callbacks to pipeline
- Track progress through each agent step
- Optional: Add progress bar for CLI
- Store progress in session for resumable operations

**Acceptance Criteria**:
- Callback system for each pipeline step
- Progress percentage/tracking
- CLI progress bar
- Tests for progress tracking

---

### 6. Add Support for Other Legacy Languages
**Labels**: `enhancement`, `feature`, `roadmap`
**Priority**: Low
**Description**:
Extend the system to support modernization from other legacy languages (Fortran, Pascal, etc.).

**Requirements**:
- Abstract language-specific parsers
- Add Fortran parser/analyzer
- Add Pascal parser/analyzer
- Create language-agnostic translation layer

**Acceptance Criteria**:
- Support at least one additional language
- Maintain backward compatibility with COBOL
- Update documentation
- Add language-specific tests

---

### 7. Add Code Diff Visualization
**Labels**: `enhancement`, `feature`, `ux`
**Priority**: Medium
**Description**:
Add ability to visualize differences between original COBOL and generated Python code.

**Requirements**:
- Use `difflib` or similar for diff generation
- CLI option to show diff
- HTML diff output option
- Highlight significant changes

**Acceptance Criteria**:
- Diff generation for COBOL → Python
- CLI flag `--show-diff`
- HTML output option
- Tests for diff generation

---

## Technical Improvements

### 8. Add Caching Layer for Analysis Results
**Labels**: `enhancement`, `performance`
**Priority**: Medium
**Description**:
Cache analysis results to avoid re-analyzing similar COBOL files, improving performance.

**Requirements**:
- Add caching layer (Redis or in-memory)
- Cache based on file hash/content
- Configurable cache TTL
- Cache invalidation strategy

**Acceptance Criteria**:
- Caching implementation
- Performance improvement demonstrated
- Cache configuration options
- Tests for caching behavior

---

### 9. Improve Test Coverage
**Labels**: `enhancement`, `testing`
**Priority**: Medium
**Description**:
Increase test coverage, especially for edge cases and error handling.

**Current Coverage**: Check with `pytest --cov`
**Target**: 80%+ coverage

**Areas Needing More Tests**:
- Error handling in agents
- Edge cases in COBOL parsing
- Guardrail validation edge cases
- Session management edge cases

**Acceptance Criteria**:
- Coverage report shows 80%+ coverage
- All critical paths tested
- Edge cases covered
- Error paths tested

---

### 10. Add Retry Logic for API Calls
**Labels**: `enhancement`, `reliability`
**Priority**: High
**Description**:
Add retry logic with exponential backoff for OpenAI API calls to handle transient failures.

**Requirements**:
- Retry on rate limits (429)
- Retry on network errors
- Exponential backoff
- Configurable retry attempts
- Log retry attempts

**Acceptance Criteria**:
- Retry logic implemented
- Handles rate limits gracefully
- Configurable retry settings
- Tests for retry behavior

---

### 11. Refactor Tool Functions
**Labels**: `refactor`, `code quality`
**Priority**: Low
**Description**:
Some tool functions could be better organized and more reusable. Refactor for better structure.

**Files to Review**:
- `src/tools/cobol_parser_tool.py`
- `src/tools/python_synth_tool.py`
- `src/tools/code_quality_tool.py`

**Acceptance Criteria**:
- Better function organization
- Improved reusability
- No breaking changes
- Tests still pass

---

## Documentation & UX

### 12. Add CLI Command Reference
**Labels**: `documentation`, `enhancement`
**Priority**: Medium
**Description**:
Create comprehensive CLI command reference documentation.

**Requirements**:
- Document all CLI commands
- Include examples for each command
- Add command reference to docs/
- Update README with CLI section

**Acceptance Criteria**:
- CLI reference documentation
- Examples for all commands
- Integration with existing docs
- Helpful error messages

---

### 13. Add Interactive Tutorial
**Labels**: `documentation`, `enhancement`, `good first issue`
**Priority**: Low
**Description**:
Create an interactive tutorial or walkthrough for new users.

**Options**:
- Jupyter notebook tutorial
- Step-by-step guide with screenshots
- Video tutorial script
- Interactive CLI wizard

**Acceptance Criteria**:
- Tutorial created
- Covers basic usage
- Accessible to beginners
- Linked from README

---

### 14. Improve Logging and Debugging
**Labels**: `enhancement`, `developer experience`
**Priority**: Medium
**Description**:
Improve logging to make debugging easier. Add structured logging, log levels, and better formatting.

**Requirements**:
- Structured logging (JSON option)
- Better log levels
- Agent-specific logging
- Debug mode for verbose output

**Acceptance Criteria**:
- Structured logging implemented
- Better log formatting
- Debug mode added
- Log rotation/configurable output

---

## Integration & DevOps

### 15. Add Docker Support
**Labels**: `enhancement`, `devops`
**Priority**: Medium
**Description**:
Add Docker support for easy deployment and consistent environments.

**Requirements**:
- Dockerfile for the application
- docker-compose.yml for development
- Documentation for Docker usage
- CI/CD integration

**Acceptance Criteria**:
- Dockerfile created
- docker-compose.yml working
- Documentation updated
- Tests pass in Docker

---

### 16. Add Pre-commit Hooks
**Labels**: `enhancement`, `developer experience`
**Priority**: Low
**Description**:
Add pre-commit hooks for code quality checks (formatting, linting, tests).

**Requirements**:
- Pre-commit configuration
- Hooks for black, ruff, mypy
- Optional: test hook
- Documentation

**Acceptance Criteria**:
- Pre-commit hooks working
- Automatic formatting
- Linting on commit
- Documentation updated

---

### 17. Add GitHub Actions for Releases
**Labels**: `enhancement`, `devops`
**Priority**: Low
**Description**:
Add GitHub Actions workflow for automated releases and PyPI publishing.

**Requirements**:
- Release workflow
- Version bump automation
- PyPI publishing (optional)
- Release notes generation

**Acceptance Criteria**:
- Automated release workflow
- Version management
- Release notes auto-generated
- Documentation updated

---

## Research & Exploration

### 18. Evaluate Alternative LLM Providers
**Labels**: `research`, `enhancement`
**Priority**: Low
**Description**:
Research and evaluate support for alternative LLM providers (Anthropic Claude, Google Gemini, etc.).

**Requirements**:
- Abstract LLM provider interface
- Implement Claude provider
- Compare quality/cost
- Document findings

**Acceptance Criteria**:
- Abstract provider interface
- At least one alternative provider
- Comparison documentation
- Tests for provider abstraction

---

### 19. Add Code Quality Metrics
**Labels**: `enhancement`, `feature`
**Priority**: Medium
**Description**:
Add metrics to measure code quality improvements (complexity reduction, LOC changes, etc.).

**Requirements**:
- Metrics calculation
- Before/after comparison
- Metrics reporting
- Visualization options

**Acceptance Criteria**:
- Metrics implemented
- Comparison reports
- CLI/API access to metrics
- Documentation

---

## Bug Fixes (Create as Discovered)

### 20. Template: Bug Report
**Labels**: `bug`
**Description Template**:
When reporting bugs, include:
- COBOL file that triggers the bug
- Expected behavior
- Actual behavior
- Error messages/logs
- Environment details

---

## Quick Wins (Can be done in < 1 hour)

- Add more badges to README
- Add table of contents to long docs
- Add emoji to issue templates
- Improve README formatting
- Add more examples to docstrings
- Add type hints to remaining functions
- Add docstrings to all public functions

---

## How to Use These Suggestions

1. **Copy the issue title and description** from above
2. **Create a new GitHub issue** using the appropriate template
3. **Add the suggested labels**
4. **Set priority** if using project boards
5. **Link related issues** if applicable

**Note**: Adjust priorities and descriptions based on your project's current needs and roadmap.

