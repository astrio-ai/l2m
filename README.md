# Legacy2Modern — AI-Powered Legacy Code Transpilation Engine

<div align="center">

<!-- Keep the gap above this line, otherwise they won't render correctly! -->
[![GitHub Repo stars](https://img.shields.io/github/stars/astrio-ai/openlegacy)](https://github.com/astrio-ai/openlegacy) 
[![Join us on Discord](https://img.shields.io/discord/1396038465002405948?logo=discord&logoColor=white&label=discord)](https://discord.gg/2BVwAUzW)
[![Contributing Guide](https://img.shields.io/badge/Contributing-Guide-informational)](https://github.com/openrewrite/.github/blob/main/CONTRIBUTING.md)
</div>

<img width="1080" height="640" alt="Legacy2Modern CLI" src="https://github.com/user-attachments/assets/4fac234b-45e5-4c48-aadb-9dbc6d0851d7" />

Welcome to **Legacy2Modern**, an open-source engine for transforming legacy source code into modern, maintainable software.

Legacy2Modern specializes in **intelligent transpilation** of legacy languages (starting with COBOL) into modern languages like Python. It combines the precision of AST-based parsing with the flexibility of template-driven code generation, optionally augmented by Large Language Models (LLMs).

Whether you're modernizing COBOL business systems, migrating legacy applications to the cloud, or transforming decades-old enterprise code — Legacy2Modern is built to help you do it **safely**, **accurately**, and **transparently**.

## ✨ Features

* 🔄 **COBOL to Python Transpilation**  
  Translate COBOL programs into modern Python code with multiple transpilation approaches.

* 🧠 **Lossless Semantic Tree (LST) Parsing**  
  ANTLR4-based parser that retains all source code information including comments, whitespace, and semantic context.

* 🏗️ **Intermediate Representation (IR) System**  
  Language-agnostic IR that enables extensibility to other source and target languages.

* 📝 **Template-Based Code Generation**  
  Jinja2-powered template system for clean, maintainable code generation.

* 🎯 **Multiple Transpilation Approaches**  
  - Direct COBOL → Python transpilation
  - IR-based transpilation with templates
  - Extensible architecture for future languages

* 🧪 **Comprehensive Testing Framework**  
  Unit tests, integration tests, and validation for all transpilation components.

## 🏗️ Architecture

```
packages/transpiler/engine/
├── parser/
│   └── cobol_lst.py              # COBOL LST parser & semantic analysis
├── ir/
│   ├── ir.py                     # IR definitions
│   ├── cobol_to_ir.py            # COBOL → IR translator
│   └── ir_transpiler.py          # IR-based transpiler
├── generator/
│   └── template_generator.py      # Jinja2 template generator
├── templates/
│   └── python/
│       ├── main.py.j2            # Main Python template
│       └── expression.py.j2      # Expression template
├── cobol_transpiler.py           # Direct COBOL → Python transpiler
└── ir_template_transpiler.py     # Template-based transpiler
```

### **Transpilation Pipeline**

1. **Parsing**: COBOL source → Lossless Semantic Tree (LST)
2. **Semantic Analysis**: Symbol tables, type resolution, control flow
3. **IR Translation**: LST → Language-agnostic Intermediate Representation
4. **Code Generation**: IR → Target language (Python) via templates

## 🚀 Quickstart

### Prerequisites

- Python 3.10+
- Git (for cloning the repository)

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

### Option 3: Run Directly (No Installation)

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

The CLI provides an interactive interface similar to Gemini CLI where you can:
- Transpile COBOL files to Python using natural language commands
- Get AI-powered analysis and optimization suggestions
- View code previews with syntax highlighting
- Use slash commands for quick actions

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

### 4. Run Tests

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
│   ├── transpiler/           # Core transpilation engine
│   │   ├── engine/          # Transpilation components
│   │   ├── grammars/        # ANTLR grammar files
│   │   ├── llm-helpers/     # LLM integration helpers
│   │   └── rules/           # Transformation rules
│   ├── cli/                 # Command-line interface
│   └── llm-agent/           # LLM agent components
├── examples/
│   └── cobol/              # Sample COBOL programs
├── tests/                  # Test suite
└── docs/                   # Documentation
```

### **Adding New Features**

1. **New Language Support**: Add grammar files and IR translators
2. **New Templates**: Create Jinja2 templates for target languages
3. **New Rules**: Implement transformation rules in the rules directory
4. **LLM Integration**: Extend llm-helpers for AI-powered suggestions

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
