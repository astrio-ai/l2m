# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 11-14-2025

### Added
- Initial release of Legacy2Modern (L2M)
- Interactive CLI for COBOL to Python modernization
- Single-agent architecture with tool calling
- Codebase indexing and repo map generation using tree-sitter
- Support for multiple LLM providers via LiteLLM
- Git integration for tracking changes
- Session management with persistent chat history
- Comprehensive test suite
- Docker support with `.devcontainer/` configuration for VS Code Dev Containers
- Package data configuration for resource files (model-settings.yml, query files)
- Default git configuration in Docker to suppress warnings
- MANIFEST.in for proper package data inclusion
- GitHub Actions workflow for automated release builds across multiple platforms
- Helpful command examples displayed on startup
- Model name displayed in brand color
- Architecture changes documentation

### Fixed
- Missing `oslex` dependency in package configuration
- Resource files not being included in installed package
- Git user configuration warnings in Docker containers
- Exception handling TypeError in LiteLLM error catching
- LiteLLM exception tuple handling issues
- Removed deleted files from git tracking (repo-map errors)

### Changed
- **Major**: Simplified from multi-agent to single-agent architecture for better maintainability
- Complete TUI redesign with Codex-style minimalist interface
- Startup display now shows helpful commands instead of technical details
- Updated README to reflect current architecture and features

### Removed
- Multi-agent system components (agents, orchestrator, workflows)
- Old TUI implementation files
- Textual TUI experiment files
- Unused evaluation and workflow files

### Infrastructure
- Python 3.10+ support
- Modular package structure
- Configuration management
- Logging system
- Environment variable support

[Unreleased]: https://github.com/astrio-ai/l2m/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/astrio-ai/l2m/releases/tag/v0.1.0
