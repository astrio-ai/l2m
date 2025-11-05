# Legacy2Modern (L2M)

<div align="center">

<!-- Keep the gap above this line, otherwise they won't render correctly! -->
[![GitHub Repo stars](https://img.shields.io/github/stars/astrio-ai/l2m)](https://github.com/astrio-ai/l2m) 
[![Join us on Discord](https://img.shields.io/discord/1396038465002405948?logo=discord&logoColor=white&label=discord)](https://discord.gg/2BVwAUzW)
[![Contributing Guide](https://img.shields.io/badge/Contributing-Guide-informational)](https://github.com/astrio-ai/l2m/CONTRIBUTING.md)
</div>

Legacy2Modern (L2M) is an open-source, AI-powered multi-agent framework that automatically analyzes, translates, refactors, and modernizes legacy codebases into modern programming languages.

## ✨ Features

- **Multi-Agent Architecture**: Specialized agents for analysis, translation, review, testing, and refactoring
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
git clone https://github.com/astrio-ai/l2m.git
cd l2m

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

## 🤖 Agents

- **Orchestrator Agent**: Manages overall pipeline and agent handoffs
- **Analyzer Agent**: Parses COBOL and extracts logic
- **Translator Agent**: Converts COBOL to Python
- **Reviewer Agent**: Reviews translated code quality
- **Tester Agent**: Creates and runs unit tests
- **Refactor Agent**: Improves code structure and readability

## 📚 Documentation

- [Architecture](docs/architecture.md)

## 🧪 Testing

**Important: Make sure your virtual environment is activated before running tests.**

```bash
# Activate virtual environment first
source .venv/bin/activate

# Then run tests (use python -m pytest to ensure venv Python is used)
python -m pytest tests/
```

If you see `ModuleNotFoundError: No module named 'agents'`, it means the virtual environment isn't activated. Make sure to run `source .venv/bin/activate` first.

## 📄 License
This project is licensed under the Apache-2.0 License. See the [LICENSE](./LICENSE) file for details.

## 🤝 Contributing
We welcome all contributions — from fixing typos to adding new language support!
See [CONTRIBUTING.md](./CONTRIBUTING.md) for setup instructions, coding guidelines, and how to submit PRs.

## 💬 Community & Support
* 📢 Follow our project updates on [X](https://x.com/astrioai)
* 👾 Join our [Discord](https://discord.gg/2BVwAUzW)
* 🧑‍💻 Join the discussion: [GitHub Discussions](https://github.com/astrio-ai/l2m/discussions)
* 🧪 Report bugs: [GitHub Issues](https://github.com/astrio-ai/l2m/issues)

## 📬 Contact Us
For partnership inquiries or professional use cases:

📧 **[naingoolwin.astrio@gmail.com](mailto:naingoolwin.astrio@gmail.com)**