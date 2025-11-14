# Legacy2Modern (L2M)

<div align="left">

<!-- Keep the gap above this line, otherwise they won't render correctly! -->
[![GitHub Repo stars](https://img.shields.io/github/stars/astrio-ai/l2m)](https://github.com/astrio-ai/l2m)
[![Follow us on X](https://img.shields.io/twitter/follow/AstrioAI)](https://www.x.com/AstrioAI)
[![Join us on Discord](https://img.shields.io/discord/1396038465002405948?logo=discord&logoColor=white&label=discord)](https://discord.gg/2BVwAUzW)
[![Contributing Guide](https://img.shields.io/badge/Contributing-Guide-informational)](https://github.com/astrio-ai/l2m/CONTRIBUTING.md)
</div>

Legacy2Modern (L2M) is an open-source, AI-powered multi-agent framework that automatically analyzes, translates, refactors, and modernizes legacy codebases into modern programming languages.

## Features

- **Multi-Agent Architecture**: Specialized agents for analysis, translation, review, testing, and refactoring
- **Batch Processing**: Process multiple COBOL files with real-time progress tracking and comprehensive reporting
- **Session Management**: Persistent conversation history across agent interactions
- **Tool Integration**: Extensible tools for COBOL parsing, Python synthesis, and code quality
- **Tracing Support**: Built-in tracing for debugging and monitoring agent behavior

## Quick Start

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

#### Interactive CLI (Preview)

```bash
# Ensure your environment is activated and the package is installed
pip install -e .

# Start the interactive CLI
l2m
```

## Documentation

- [Getting Started](docs/getting_started.md) - Installation and quick start guide
- [Full Documentation](docs/README.md) - Complete documentation index

## License
This project is licensed under the Apache-2.0 License. See the [LICENSE](./LICENSE) file for details.

## Security
For security vulnerabilities, please email [naingoolwin.astrio@gmail.com](mailto:naingoolwin.astrio@gmail.com) instead of using the issue tracker. See [SECURITY.md](.github/SECURITY.md) for details.

## Contributing
We welcome all contributions — from fixing typos to adding new language support!
See [CONTRIBUTING.md](./CONTRIBUTING.md) for setup instructions, coding guidelines, and how to submit PRs.

## Contributors

<a href="https://github.com/astrio-ai/l2m/graphs/contributors">
  <img alt="contributors" src="https://contrib.rocks/image?repo=astrio-ai/l2m&v=1" />

</a>

## Community & Support
* Follow our project updates on [X](https://x.com/astrioai)
* Join our [Discord](https://discord.gg/2BVwAUzW)
* Join the discussion: [GitHub Discussions](https://github.com/astrio-ai/l2m/discussions)
* Report bugs: [GitHub Issues](https://github.com/astrio-ai/l2m/issues)

## Contact Us
For partnership inquiries or professional use cases:

📧 **[naingoolwin.astrio@gmail.com](mailto:naingoolwin.astrio@gmail.com)**
