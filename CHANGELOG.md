# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Comprehensive documentation with Mermaid diagrams
- Test suite with 47+ tests covering agents, tools, guardrails, and workflows
- Issue templates for bug reports and feature requests
- Code of Conduct and Contributing guidelines
- Security policy

## [0.1.0] - 2025-11-05

### Added
- Initial release of Legacy2Modern (L2M)
- Multi-agent architecture using OpenAI Agents SDK
- Orchestrator Agent for workflow coordination
- Analyzer Agent for COBOL code analysis
- Translator Agent for COBOL to Python conversion
- Reviewer Agent for code quality review
- Tester Agent for test generation and execution
- Refactor Agent for code improvement
- Session management with SQLite
- Tool integration (COBOL parser, Python synthesis, code quality, test runner)
- Guardrails for input/output validation
- Sequential and handoff workflow modes
- Comprehensive documentation
- Test suite

### Infrastructure
- Project structure with modular design
- Configuration management with Pydantic
- Logging system
- Environment variable support

[Unreleased]: https://github.com/astrio-ai/l2m/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/astrio-ai/l2m/releases/tag/v0.1.0

