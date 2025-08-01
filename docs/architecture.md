# 🏗️ Architecture

Legacy2Modern (L2M) is built with a modular, extensible architecture that combines traditional transpilation techniques with modern AI capabilities.

## 📁 Project Structure

```
legacy2modern/
├── packages/
│   ├── transpiler/               # Core transpilation engine
│   │   ├── engine/
│   │   │   ├── parser/
│   │   │   │   └── cobol_lst.py              # COBOL LST parser & semantic analysis
│   │   │   ├── ir/
│   │   │   │   ├── ir.py                     # IR definitions
│   │   │   │   ├── cobol_to_ir.py            # COBOL → IR translator
│   │   │   │   └── ir_transpiler.py          # IR-based transpiler
│   │   │   ├── generator/
│   │   │   │   └── template_generator.py      # Jinja2 template generator
│   │   │   ├── templates/
│   │   │   │   └── python/
│   │   │   │       ├── main.py.j2            # Main Python template
│   │   │   │       └── expression.py.j2      # Expression template
│   │   │   ├── cobol_transpiler.py           # Direct COBOL → Python transpiler
│   │   │   ├── hybrid_transpiler.py          # Hybrid transpiler with LLM
│   │   │   ├── llm_augmentor.py              # LLM integration
│   │   │   └── edge_case_detector.py         # Edge case detection
│   │   ├── grammars/             # ANTLR grammar files
│   │   ├── llm-helpers/          # LLM integration helpers
│   │   └── rules/                # Transformation rules
│   ├── cli/                      # Modern CLI interface
│   │   └── cli.py               # Main CLI with Gemini-style design
│   └── llm-agent/                # LLM agent components
│       ├── agent.py              # Main LLM agent
│       ├── code_analyzer.py      # Code analysis
│       ├── optimizer.py          # Code optimization
│       └── reviewer.py           # Code review
├── examples/
│   └── cobol/                   # Sample COBOL programs
├── tests/                       # Test suite
├── docs/                        # Documentation
├── scripts/                     # CLI script wrappers
├── Formula/                     # Homebrew formula
├── install.sh                   # Installation script
├── run_cli.py                   # Direct CLI runner
└── setup.py                     # Package configuration
```

## 🔄 Transpilation Pipeline

The Legacy2Modern transpilation process follows a sophisticated 7-step pipeline:

1. **Parsing**: COBOL source → Lossless Semantic Tree (LST)
2. **Semantic Analysis**: Symbol tables, type resolution, control flow
3. **Edge Case Detection**: Identify complex patterns requiring special handling
4. **LLM Augmentation**: AI-powered analysis for complex transformations
5. **IR Translation**: LST → Language-agnostic Intermediate Representation
6. **Code Generation**: IR → Target language (Python) via templates
7. **AI Analysis**: Code review, optimization suggestions, and quality assessment

## 🧠 Core Components

### Transpiler Engine
- **Parser**: ANTLR4-based COBOL parser with lossless semantic tree generation
- **IR System**: Language-agnostic intermediate representation for extensibility
- **Template Engine**: Jinja2-powered code generation with customizable templates
- **Rule Engine**: Configurable transformation rules for different language constructs

### CLI Interface
- **Modern Design**: Gemini-style interface with rich terminal formatting
- **Interactive Mode**: Natural language commands and real-time feedback
- **AI Integration**: Seamless LLM-powered analysis and suggestions
- **Multi-platform**: Support for Homebrew, pip, and direct installation

### LLM Agent System
- **Code Analysis**: Intelligent code complexity and maintainability assessment
- **Optimization**: AI-powered code improvement suggestions
- **Review System**: Automated code review with confidence scoring
- **Documentation**: Automatic documentation generation

## 🔧 Key Design Principles

### Modularity
Each component is designed to be independent and replaceable, allowing for easy extension and customization.

### Extensibility
The IR system enables support for new source and target languages without major architectural changes.

### AI-First
LLM integration is built into the core architecture, not as an afterthought, enabling intelligent transpilation.

### User Experience
The CLI provides a modern, intuitive interface that makes complex transpilation accessible to all users.

## 🚀 Installation Architecture

### Homebrew Integration
- **Formula**: Automated installation with dependency management
- **Script Wrappers**: Executable scripts for `legacy2modern` and `l2m` commands
- **Path Management**: Automatic PATH configuration

### Python Package
- **Entry Points**: Console scripts for easy command-line access
- **Dependencies**: Comprehensive dependency management via requirements.txt
- **Development Mode**: Editable installation for development

### Direct Execution
- **No Installation**: Run directly from source for testing and development
- **Portable**: Self-contained execution without system-wide installation