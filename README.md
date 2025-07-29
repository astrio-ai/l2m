# Legacy2Modern (L2M) — AI Legacy Code Transpilation Engine

<div align="center">

<!-- Keep the gap above this line, otherwise they won't render correctly! -->
[![GitHub Repo stars](https://img.shields.io/github/stars/astrio-ai/openlegacy)](https://github.com/astrio-ai/openlegacy) 
[![Join us on Discord](https://img.shields.io/discord/1396038465002405948?logo=discord&logoColor=white&label=discord)](https://discord.gg/2BVwAUzW)
[![Contributing Guide](https://img.shields.io/badge/Contributing-Guide-informational)](https://github.com/openrewrite/.github/blob/main/CONTRIBUTING.md)
</div>

![L2M CLI Screenshot](./docs/assets/l2m-screenshot.png)

Welcome to **Legacy2Modern (L2M)**, an open-source engine for transforming legacy source code into modern, maintainable software.

L2M specializes in **intelligent transpilation** of legacy languages (starting with COBOL) into modern languages like Python. It combines the precision of AST-based parsing with the flexibility of template-driven code generation, optionally augmented by Large Language Models (LLMs).

Whether you're modernizing COBOL business systems, migrating legacy applications to the cloud, or transforming decades-old enterprise code — L2M is built to help you do it **safely**, **accurately**, and **transparently**.

## ✨ Features

* 🔄 **COBOL to Python Transpilation**  
  Translate COBOL programs into modern Python code with multiple transpilation approaches.

* 🖥️ **Modern CLI Interface**  
  Beautiful, interactive command-line interface with natural language commands and AI-powered analysis.

* 🧠 **Lossless Semantic Tree (LST) Parsing**  
  ANTLR4-based parser that retains all source code information including comments, whitespace, and semantic context.

* 🏗️ **Intermediate Representation (IR) System**  
  Language-agnostic IR that enables extensibility to other source and target languages.

* 📝 **Template-Based Code Generation**  
  Jinja2-powered template system for clean, maintainable code generation.

* 🤖 **AI-Powered Analysis & Optimization**  
  LLM integration for code analysis, review, and optimization suggestions.

* 🎯 **Multiple Transpilation Approaches**  
  - Direct COBOL → Python transpilation
  - IR-based transpilation with templates
  - Hybrid transpilation with LLM augmentation
  - Extensible architecture for future languages

* 🧪 **Comprehensive Testing Framework**  
  Unit tests, integration tests, and validation for all transpilation components.

* 📦 **Easy Installation Options**  
  - Homebrew installation: `brew install legacy2modern-cli`
  - Direct installation: `pip install -e .`
  - Run without installation: `python run_cli.py`

## 🚀 Quickstart

### Prerequisites

- Python 3.10+
- Git (for cloning the repository)
- Homebrew (for Option 3)

### Option 1: Quick Install (Recommended)

```bash
# Clone the repository
git clone https://github.com/astrio-ai/legacy2modern.git
cd legacy2modern

# Run the installation script
./install.sh
```

### Option 2: Manual Installation

```bash
# Clone the repository
git clone https://github.com/astrio-ai/legacy2modern.git
cd legacy2modern

# Install dependencies
pip install -r requirements.txt

# Install the CLI
pip install -e .
```

### Option 3: Homebrew Installation (macOS)

```bash
# Install via Homebrew
brew install legacy2modern-cli

# Run the CLI
legacy2modern
```

### Option 4: Run Directly (No Installation)

```bash
# Clone the repository
git clone https://github.com/astrio-ai/legacy2modern.git
cd legacy2modern

# Install dependencies
pip install -r requirements.txt

# Run the CLI directly
python run_cli.py
```

### Using the CLI

Once installed, you can use the CLI in several ways:

```bash
# Start the interactive CLI
legacy2modern

# Or use the short command
l2m

# Run directly without installation
python run_cli.py
```

**Note**: Homebrew installation provides the most convenient way to install and use the CLI on macOS.

### Examples

```bash
# Start the CLI
legacy2modern

# In the interactive mode:
> transpile examples/cobol/HELLO.cobol
> /transpile examples/cobol/HELLO.cobol
> analyze the generated Python code
> /help
```

### Run Tests

```bash
# Run all tests
pytest tests/

# Run specific test
pytest tests/test_cobol_lst.py
```

## 📋 Supported COBOL Constructs

### **Data Division**
- Variable declarations with PIC clauses
- Level numbers (01, 05, 77)
- Type inference (PIC X → str, PIC 9 → int/float)

### **Procedure Division**
- `DISPLAY` → `print()`
- `ACCEPT` → `input()`
- `MOVE` → Python assignment (`=`)
- `ADD`/`SUBTRACT` → Python arithmetic (`+=`, `-=`)
- `PERFORM UNTIL` → `while` loops
- `INSPECT` → String operations
- `GOBACK` → `return`

### **Example Transformation**

**Input (COBOL):**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD!'.
           GOBACK.
```

**Output (Python):**
```python
# Generated Python code from main

def main():
    print('HELLO WORLD!')
    return

if __name__ == '__main__':
    main()
```

## 🔧 Development

### **Project Structure**

```
legacy2modern/
├── packages/
│   ├── transpiler/          # Core transpilation engine
│   │   ├── engine/          # Transpilation components
│   │   ├── grammars/        # ANTLR grammar files
│   │   ├── llm-helpers/     # LLM integration helpers
│   │   └── rules/           # Transformation rules
│   ├── cli/                 # Modern CLI interface
│   └── llm-agent/           # LLM agent components
├── examples/
│   └── cobol/               # Sample COBOL programs
├── tests/                   # Test suite
├── docs/                    # Documentation
├── scripts/                 # CLI script wrappers
├── Formula/                 # Homebrew formula
├── install.sh               # Installation script
├── run_cli.py               # Direct CLI runner
└── setup.py                 # Package configuration
```

### **Adding New Features**

1. **New Language Support**: Add grammar files and IR translators
2. **New Templates**: Create Jinja2 templates for target languages
3. **New Rules**: Implement transformation rules in the rules directory
4. **LLM Integration**: Extend llm-helpers for AI-powered suggestions
5. **CLI Enhancements**: Add new commands and interactive features
6. **Installation Methods**: Add support for new package managers

## 🧪 Testing

```bash
# Run all tests
pytest

# Run specific test file
pytest tests/test_cobol_lst.py

# Run with coverage
pytest --cov=packages/transpiler
```

## 📄 License
This project is licensed under the Apache-2.0 License. See the [LICENSE](./LICENSE) file for details.

## 🤝 Contributing
We welcome all contributions — from fixing typos to adding new language support!
See [CONTRIBUTING.md](./CONTRIBUTING.md) for setup instructions, coding guidelines, and how to submit PRs.

### Good First Issues
* Add support for more COBOL constructs
* Create templates for other target languages (JavaScript, C++)
* Improve error handling and reporting
* Add more comprehensive test cases

## 💬 Community & Support
* 📢 Follow our project updates on [X](https://x.com/nolan-lwin)
* 👾 Join our [Discord](https://discord.gg/2BVwAUzW)
* 🧑‍💻 Join the discussion: [GitHub Discussions](https://github.com/astrio-ai/legacy2modern/discussions)
* 🧪 Report bugs: [GitHub Issues](https://github.com/astrio-ai/legacy2modern/issues)

## 📬 Contact Us
For partnership inquiries or professional use cases:

📧 **[naingoolwin.astrio@gmail.com](mailto:naingoolwin.astrio@gmail.com)**
