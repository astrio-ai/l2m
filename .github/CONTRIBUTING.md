# Contributing to Legacy2Modern (L2M) CLI

Thank you for your interest in contributing to L2M CLI! This document provides guidelines and information for contributors.

## 🤝 How to Contribute

We welcome contributions from the community! Here are the main ways you can help:

- 🐛 **Report Bugs** - Use GitHub Issues to report bugs
- 💡 **Suggest Features** - Propose new features or improvements
- 📝 **Improve Documentation** - Help make our docs better
- 🔧 **Fix Issues** - Submit pull requests to fix bugs
- ✨ **Add Features** - Implement new functionality
- 🧪 **Write Tests** - Add test coverage for existing or new code

## 🚀 Getting Started

### Prerequisites

- Python 3.10 or higher
- Git
- pip (Python package manager)

### Development Setup

1. **Fork the repository**
   ```bash
   # Fork on GitHub, then clone your fork
   git clone https://github.com/YOUR_USERNAME/legacy2modern.git
   cd legacy2modern
   ```

2. **Set up the development environment**
   ```bash
   # Create a virtual environment
   python -m venv .venv
   source .venv/bin/activate  # On Windows: .venv\Scripts\activate
   
   # Install dependencies
   pip install -r requirements.txt
   
   # Install in development mode
   pip install -e .
   ```

3. **Set up environment variables**
   ```bash
   # Copy the template
   cp .env.template .env
   
   # Edit .env with your API keys
   # LLM_API_KEY=your_api_key_here
   ```

## 📋 Development Guidelines

### Code Style

We follow PEP 8 style guidelines:

- **Indentation**: 4 spaces (no tabs)
- **Line length**: 79 characters maximum
- **Naming**: Use descriptive names, follow Python conventions
- **Comments**: Write clear, helpful comments
- **Docstrings**: Use Google-style docstrings

### Project Structure

```
legacy2modern/
├── engine/                         # Core engine components
│   ├── cli/                        # CLI interface
│   ├── agents/                     # AI-powered analysis
│   ├── functionality_mapper/       # Functionality equivalence
│   └── modernizers/                # Core transpilation engine
├── examples/                       # COBOL examples
├── tests/                          # Test suite
├── docs/                           # Documentation
└── scripts/                        # Utility scripts
```

### Testing

Run tests before submitting:

```bash
# Run all tests
python -m pytest tests/

# Run with coverage
python -m pytest tests/ --cov=packages

# Run specific test file
python -m pytest tests/test_cobol_transpiler.py
```

### Code Quality

We use several tools to maintain code quality:

```bash
# Format code with black
black packages/ tests/

# Check with flake8
flake8 packages/ tests/

# Type checking with mypy
mypy packages/
```

## 🔄 Contribution Workflow

### 1. Create a Feature Branch

```bash
# Create and switch to a new branch
git checkout -b feature/your-feature-name

# Or for bug fixes
git checkout -b fix/issue-description
```

### 2. Make Your Changes

- Write clear, focused commits
- Include tests for new functionality
- Update documentation if needed
- Follow the code style guidelines

### 3. Test Your Changes

```bash
# Run the full test suite
python -m pytest tests/

# Test the CLI
python run_cli.py --help

# Test transpilation
python run_cli.py examples/cobol/HELLO.cobol
```

### 4. Submit a Pull Request

1. **Push your branch**
   ```bash
   git push origin feature/your-feature-name
   ```

2. **Create a Pull Request**
   - Use the PR template
   - Describe your changes clearly
   - Link any related issues
   - Include screenshots for UI changes

3. **PR Title Format**
   ```
   feat: Add new transpilation feature
   fix: Resolve CLI import error
   docs: Update installation instructions
   test: Add tests for file I/O rules
   ```

## 📝 Commit Message Guidelines

Use conventional commit format:

```
type(scope): description

[optional body]

[optional footer]
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, etc.)
- `refactor`: Code refactoring
- `test`: Adding or updating tests
- `chore`: Maintenance tasks

**Examples:**
```
feat(cli): Add interactive mode with command history
fix(transpiler): Resolve COBOL PERFORM statement parsing
docs(readme): Update installation instructions for Homebrew
test(parser): Add comprehensive test suite for COBOL grammar
```

## 🐛 Reporting Issues

When reporting issues, please include:

- **Clear description** of the problem
- **Steps to reproduce** the issue
- **Expected vs actual behavior**
- **Environment details** (OS, Python version, etc.)
- **Error messages** and stack traces
- **Sample COBOL code** (if applicable)

## 🎯 Areas for Contribution

### High Priority
- **COBOL Grammar Support** - Extend parser for more COBOL constructs
- **Test Coverage** - Add more comprehensive tests
- **Documentation** - Improve user guides and API docs
- **Performance** - Optimize transpilation speed

### Medium Priority
- **CLI Features** - Add more interactive commands
- **LLM Integration** - Enhance AI-powered analysis
- **Error Handling** - Improve error messages and recovery
- **Examples** - Add more COBOL program examples

### Low Priority
- **UI Improvements** - Enhance CLI interface
- **Tooling** - Add development tools and scripts
- **Benchmarks** - Performance benchmarking tools

## 🤝 Code Review Process

1. **Automated Checks** - CI/CD will run tests and linting
2. **Review Request** - At least one maintainer will review
3. **Feedback** - Address any review comments
4. **Merge** - Once approved, your PR will be merged

## 📞 Getting Help

- **GitHub Issues** - For bugs and feature requests
- **GitHub Discussions** - For questions and general discussion
- **Documentation** - Check the docs folder for guides

## 🏆 Recognition

Contributors will be:
- Listed in the README.md
- Mentioned in release notes
- Added to the project's contributors list

## 📄 License

By contributing to L2M CLI, you agree that your contributions will be licensed under the Apache-2.0 License.

---

Thank you for contributing to L2M CLI! 🚀 