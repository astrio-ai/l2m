# Legacy2Modern (L2M)

**AI-Powered COBOL to Python Modernization System**

[![GitHub Repo stars](https://img.shields.io/github/stars/astrio-ai/legacy2modern)](https://github.com/astrio-ai/legacy2modern)

## 🎯 Overview

Legacy2Modern (L2M) is a research project for modernizing legacy COBOL code into modern Python using AI-powered agents. Built on OpenAI's Agents SDK, it provides a flexible, extensible framework for code transformation.

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
cd legacy2modern

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
result = await pipeline.run("data/samples/sample1.cbl")
print(result)
```

## 📁 Project Structure

```
legacy2modern/
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
├── examples/            # Usage examples
├── docs/                # Documentation
└── evals/               # Evaluation benchmarks
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

## 🔄 Migration History

### v1 → v2 Migration

We migrated from LangGraph-based multi-agent system to OpenAI Agents SDK for:
- **Simpler API**: More intuitive agent creation and management
- **Built-in Sessions**: Automatic conversation history management
- **Better Tooling**: Native support for function tools and handoffs
- **Active Development**: Official OpenAI framework with regular updates

### Accessing Previous Versions

- **LangGraph Implementation**: Checkout `archive/v1-langgraph-multi-agent` branch
- **All commits preserved**: Full git history available

## 📄 License

Apache-2.0

## 🤝 Contributing

Contributions welcome! See [CONTRIBUTING.md](./CONTRIBUTING.md) for guidelines.

## 📬 Contact

**naingoolwin.astrio@gmail.com**

## 🙏 Acknowledgments

- [OpenAI Agents SDK](https://github.com/openai/openai-agents-python) - Multi-agent framework
- [LangGraph](https://github.com/langchain-ai/langgraph) - Previous orchestration framework

---

**Note**: This repository maintains multiple versions for research purposes. The current active development uses OpenAI Agents SDK. Previous LangGraph implementation is archived in `archive/v1-langgraph-multi-agent` branch.
