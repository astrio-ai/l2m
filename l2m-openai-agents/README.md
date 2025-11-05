# L2M OpenAI Agents - COBOL to Python Modernization

A modern, AI-powered system for transforming legacy COBOL code into Python using the [OpenAI Agents SDK](https://github.com/openai/openai-agents-python).

## 🎯 Overview

L2M (Legacy2Modern) uses specialized AI agents to automate the modernization of COBOL codebases. Built on OpenAI's Agents SDK, it provides a flexible, extensible framework for code transformation.

## ✨ Features

- **Multi-Agent Architecture**: Specialized agents for analysis, translation, review, testing, and refactoring
- **OpenAI Agents SDK**: Built on the official OpenAI Agents framework for reliable agent workflows
- **Session Management**: Persistent conversation history across agent interactions
- **Tool Integration**: Extensible tools for COBOL parsing, Python synthesis, and code quality
- **Tracing Support**: Built-in tracing for debugging and monitoring agent behavior

## 🚀 Quick Start

### Prerequisites

- Python 3.10+
- OpenAI API Key

### Installation

```bash
# Clone the repository
git clone https://github.com/astrio-ai/legacy2modern.git
cd legacy2modern/l2m-openai-agents

# Install dependencies
pip install -r requirements.txt  # or use: uv add 'openai-agents'

# Set up environment
cp .env.example .env
# Edit .env and add your OPENAI_API_KEY
```

### Basic Usage

```python
from src.workflows.modernization_pipeline import ModernizationPipeline

pipeline = ModernizationPipeline()
result = await pipeline.run("examples/sample.cbl")
print(result)
```

## 📁 Project Structure

```
l2m-openai-agents/
├── src/
│   ├── agents/          # Specialized AI agents
│   ├── tools/           # COBOL parsing, Python synthesis tools
│   ├── workflows/       # Agent orchestration workflows
│   ├── sessions/        # Session management
│   ├── guardrails/      # Input/output validation
│   ├── tracing/         # Tracing configuration
│   └── utils/           # Utilities
├── data/                # Sample COBOL files and outputs
├── tests/               # Test suite
└── examples/           # Usage examples
```

## 🤖 Agents

- **Orchestrator Agent**: Manages overall pipeline and agent handoffs
- **Analyzer Agent**: Parses COBOL and extracts logic
- **Translator Agent**: Converts COBOL to Python
- **Reviewer Agent**: Reviews translated code quality
- **Tester Agent**: Creates and runs unit tests
- **Refactor Agent**: Improves code structure and readability

## 📚 Documentation

- [OpenAI Agents SDK Docs](https://openai.github.io/openai-agents-python/)
- [Agent Patterns](examples/)
- [Architecture](docs/architecture.md)

## 🧪 Testing

```bash
pytest tests/
```

## 📄 License

Apache-2.0

## 🤝 Contributing

Contributions welcome! See [CONTRIBUTING.md](../CONTRIBUTING.md) for guidelines.

## 📬 Contact

**naingoolwin.astrio@gmail.com**

---

**Note**: This is a migration from LangGraph-based multi-agent system to OpenAI Agents SDK. See `archive/v1-langgraph-multi-agent` branch for previous implementation.

