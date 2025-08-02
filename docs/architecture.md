# 🏗️ Architecture

Legacy2Modern (L2M) is built with a modular, extensible architecture that combines traditional transpilation techniques with modern AI capabilities for both COBOL transpilation and website modernization.

## 📁 Project Structure

```
legacy2modern/
├── engine/                       # Core engine components
│   ├── agents/                   # LLM agent system
│   │   ├── agent.py             # Main LLM agent with multi-provider support
│   │   ├── code_analyzer.py     # Code analysis and complexity assessment
│   │   ├── optimizer.py         # Code optimization and improvement
│   │   ├── reviewer.py          # Code review and quality assessment
│   │   └── results.py           # Result data classes
│   ├── cli/                      # Modern CLI interface
│   │   └── cli.py               # Main CLI with interactive design
│   └── modernizers/              # Language-specific modernizers
│       ├── cobol_system/         # COBOL transpilation system
│       │   ├── parser/           # ANTLR4-based COBOL parser
│       │   ├── ir/               # Intermediate representation
│       │   ├── transpilers/      # Transpilation engines
│       │   ├── rules/            # Transformation rules
│       │   ├── templates/        # Jinja2 templates
│       │   └── generators/       # Code generators
│       └── static_site/          # Website modernization system
│           ├── parser/            # HTML/CSS/JS parser
│           ├── transpilers/       # Website transpilation engines
│           ├── rules/             # Modernization rules
│           ├── templates/         # Framework templates
│           └── transformers/      # Code transformers
├── examples/
│   ├── cobol/                   # Sample COBOL programs
│   └── website/                 # Sample legacy websites
├── tests/                       # Test suite
│   ├── cobol_system/            # COBOL transpilation tests
│   └── static_site/             # Website modernization tests
├── docs/                        # Documentation
├── scripts/                     # CLI script wrappers
├── formula/                     # Homebrew formula
├── install.sh                   # Installation script
├── run_cli.py                   # Direct CLI runner
└── setup.py                     # Package configuration
```

## 🔄 Transpilation Pipeline

The Legacy2Modern transpilation process follows sophisticated pipelines for both COBOL and website modernization:

### COBOL Transpilation Pipeline
1. **Parsing**: COBOL source → Lossless Semantic Tree (LST)
2. **Semantic Analysis**: Symbol tables, type resolution, control flow
3. **Edge Case Detection**: Identify complex patterns requiring special handling
4. **LLM Augmentation**: AI-powered analysis for complex transformations
5. **IR Translation**: LST → Language-agnostic Intermediate Representation
6. **Code Generation**: IR → Target language (Python) via templates
7. **AI Analysis**: Code review, optimization suggestions, and quality assessment

### Website Modernization Pipeline
1. **HTML Parsing**: Parse legacy HTML structure and content
2. **CSS Analysis**: Extract and modernize Bootstrap/jQuery styles
3. **JavaScript Transformation**: Convert jQuery to React hooks
4. **Framework Selection**: Choose target framework (React/Next.js/Astro)
5. **Component Generation**: Create modern component structure
6. **Style Modernization**: Convert to Tailwind CSS or modern CSS
7. **AI Enhancement**: LLM-powered optimization and best practices

## 🧠 Core Components

### Engine Architecture
- **Agents**: LLM agent system with multi-provider support (Claude, OpenAI, Local)
- **Modernizers**: Language-specific transpilation engines
- **CLI Interface**: Modern, interactive command-line interface

### COBOL System
- **Parser**: ANTLR4-based COBOL parser with lossless semantic tree generation
- **IR System**: Language-agnostic intermediate representation for extensibility
- **Template Engine**: Jinja2-powered code generation with customizable templates
- **Rule Engine**: Configurable transformation rules for different language constructs
- **Hybrid Transpiler**: Combines traditional transpilation with LLM augmentation

### Static Site Modernizer
- **HTML Parser**: Semantic HTML analysis and structure extraction
- **CSS Transformer**: Bootstrap to Tailwind CSS conversion
- **JavaScript Modernizer**: jQuery to React hooks transformation
- **Framework Generator**: Multi-framework template system
- **LLM Agent**: AI-powered website analysis and optimization

### CLI Interface
- **Modern Design**: Rich terminal interface with progress indicators
- **Interactive Mode**: Natural language commands and real-time feedback
- **AI Integration**: Seamless LLM-powered analysis and suggestions
- **Multi-language Support**: COBOL transpilation and website modernization

### LLM Agent System
- **Multi-Provider Support**: Claude, OpenAI, and local LLM integration
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

### Multi-Language Support
Unified architecture supports both COBOL transpilation and website modernization with shared components.

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

## 🌐 Supported Frameworks

### Website Modernization
- **React**: Component-based architecture with hooks
- **Next.js**: Full-stack React framework with SSR
- **Astro**: Content-focused static site generator
- **Tailwind CSS**: Utility-first CSS framework

### LLM Providers
- **Anthropic Claude**: Primary LLM for code analysis and generation
- **OpenAI GPT**: Alternative LLM provider
- **Local LLMs**: Ollama integration for offline processing

## 🔄 Output Organization

### COBOL Transpilation
- **Output Directory**: `output/modernized-python/`
- **File Structure**: Preserves original file organization
- **Generated Files**: Python files with modern syntax and structure

### Website Modernization
- **Output Directory**: `output/modernized-{project-name}/`
- **Framework Structure**: Complete project setup for target framework
- **Generated Files**: Full application with components, styles, and configuration