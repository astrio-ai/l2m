# Contributing to L2M

Thank you for contributing to L2M! We're an exciting open source project built to translate, refactor, and modernize legacy software systems into modern, maintainable programming languages using AI-powered agents, and we'd love to have you contribute! Here's some resources and guidance to help you get started:

## Table of Contents

1. [Getting Started](#getting-started)
2. [Development Setup](#development-setup)
3. [Issues](#issues)
4. [Pull Requests](#pull-requests)
5. [Code Style](#code-style)
6. [Testing](#testing)

## Getting Started

To ensure a positive and inclusive environment, please read our [Code of Conduct](CODE_OF_CONDUCT.md) before contributing.

## Issues

If you find a bug or have a feature request, please create an Issue and we'll triage it.

### Before Creating an Issue

- **Search existing Issues** before creating a new one to avoid duplicates
- **Include a clear description** of the problem with steps to reproduce
- **Provide examples** - include COBOL code samples, error messages, and screenshots if applicable
- **Specify environment** - Python version, OS, and any relevant configuration

### Issue Labels

- `good first issue` - Great for newcomers to the project
- `bug` - Something isn't working
- `enhancement` - New feature or request
- `documentation` - Improvements or additions to documentation
- `agent` - Related to agent functionality

## Pull Requests

We actively welcome your Pull Requests! A couple of things to keep in mind before you submit:

### Before Submitting

- **Check existing PRs** - Make sure someone else hasn't already created a PR fixing the same issue
- **Link to Issues** - If you're fixing an Issue, make sure to link your PR to the related Issue(s)
- **First viable PR wins** - We will always try to accept the first viable PR that resolves the Issue
- **Good first issues** - If you're new, we encourage you to take a look at issues tagged with `good first issue`
- **Feature discussions** - If you're submitting a new feature, please open a Discussion first to discuss the design before opening a PR. We'd love to accept your hard work, but unfortunately if a feature hasn't gone through a proper design process, your PR may be closed.

### PR Requirements

- **Use clear commit messages** - Follow conventional commit format: `feat:`, `fix:`, `docs:`, `test:`, etc.
- **Provide detailed context** - Use the PR template and explain what problem you're solving
- **Keep PRs focused** - One PR should address one issue or feature
- **Update documentation** - If you're adding features, update relevant docs

**PRs without clear problem statements will be closed.**

## Testing

### Running Tests

```bash
# Run all tests
pytest tests/

# Run with coverage
pytest --cov=src tests/

# Run specific test file
pytest tests/test_agents.py

# Run with verbose output
pytest -v tests/
```

### Writing Tests

- Write tests for new features and bug fixes
- Use `pytest` conventions
- Include both positive and negative test cases
- Test agent functionality with mock LLM responses when possible
- Test tools independently

## Areas for Contribution

We welcome contributions in these areas:

1. **Agent Improvements**
   - Better COBOL parsing and analysis
   - Enhanced translation accuracy
   - Improved code review capabilities

2. **Tool Development**
   - COBOL parser enhancements
   - Python code generation improvements
   - Test generation tools

3. **Documentation**
   - API documentation
   - Tutorials and examples
   - Architecture diagrams

4. **Testing**
   - More comprehensive test coverage
   - Integration tests
   - Performance benchmarks

5. **Examples**
   - More COBOL sample files
   - Complex transformation examples
   - Real-world use cases

## Questions?

- **Discord**: Join our [Discord community](https://discord.gg/2BVwAUzW)
- **GitHub Discussions**: [Start a discussion](https://github.com/astrio-ai/l2m/discussions)
- **Email**: [naingoolwin.astrio@gmail.com](mailto:naingoolwin.astrio@gmail.com)

## License

By contributing, you agree that your contributions will be licensed under the Apache-2.0 License.

---

Thank you for helping make Legacy2Modern better! 🚀

