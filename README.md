# Legacy2Modern (L2M)

**AI-Powered COBOL to Python Modernization System**

[![GitHub Repo stars](https://img.shields.io/github/stars/astrio-ai/legacy2modern)](https://github.com/astrio-ai/legacy2modern)

## 🎯 Overview

Legacy2Modern (L2M) is a research project for modernizing legacy COBOL code into modern Python using AI-powered agents. The project has evolved through multiple iterations:

- **v1 (Archived)**: Multi-agent system using LangGraph - See `archive/v1-langgraph-multi-agent` branch
- **v2 (Current)**: OpenAI Agents SDK implementation - See `l2m-openai-agents/` directory

## 📁 Repository Structure

```
legacy2modern/
├── l2m-openai-agents/          # Current implementation (OpenAI Agents SDK)
│   ├── src/                   # Source code
│   ├── tests/                 # Test suite
│   ├── examples/              # Usage examples
│   └── README.md             # Detailed documentation
├── archive/                   # Archived versions
│   └── v1-langgraph-multi-agent/  # LangGraph-based implementation
└── README.md                  # This file
```

## 🚀 Quick Start (Current Version)

### Prerequisites

- Python 3.10+
- OpenAI API Key

### Installation

```bash
# Navigate to the current implementation
cd l2m-openai-agents

# Install dependencies
pip install -r requirements.txt  # or: uv add 'openai-agents'

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

See [l2m-openai-agents/README.md](./l2m-openai-agents/README.md) for detailed documentation.

## 📚 Documentation

- **Current Implementation**: [l2m-openai-agents/README.md](./l2m-openai-agents/README.md)
- **OpenAI Agents SDK**: [Official Documentation](https://openai.github.io/openai-agents-python/)
- **Archived v1**: See `archive/v1-langgraph-multi-agent` branch

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

## 🧪 Testing

```bash
cd l2m-openai-agents
pytest tests/
```

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

**Note**: This repository maintains multiple versions for research purposes. The current active development is in `l2m-openai-agents/`.

