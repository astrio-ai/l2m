# Legacy2Modern (L2M)

<div align="left">

<!-- Keep the gap above this line, otherwise they won't render correctly! -->
[![GitHub Repo stars](https://img.shields.io/github/stars/astrio-ai/l2m?cacheSeconds=3600)](https://github.com/astrio-ai/l2m)
[![Follow us on X](https://img.shields.io/twitter/follow/AstrioAI)](https://www.x.com/AstrioAI)
[![Join us on Discord](https://img.shields.io/discord/1396038465002405948?logo=discord&logoColor=white&label=discord)](https://discord.gg/2BVwAUzW)
[![Contributing Guide](https://img.shields.io/badge/Contributing-Guide-informational)](https://github.com/astrio-ai/l2m/CONTRIBUTING.md)
</div>

Legacy2Modern (L2M) is an open-source, AI coding agent that helps you modernize legacy codebases into modern programming languages within your terminal.

## Features

- **Modern TUI**: Clean, Codex-style terminal interface with brand-colored UI elements
- **Multi-Provider Support**: Works with OpenAI, Anthropic, DeepSeek, Gemini, and 100+ other LLM providers via LiteLLM
- **Interactive Chat**: Natural conversation with your codebase - ask questions, request changes, and get AI assistance
- **File Management**: Add files to context, drop them when done, view what's in your chat session
- **Git Integration**: Automatic commits, undo support, and repository-aware context
- **Streaming Responses**: Real-time AI responses with markdown rendering
- **Session History**: Persistent conversation history across sessions

## Quick Start

### Prerequisites

- Python 3.10+
- BYOK for your preferred LLM provider (OpenAI, Anthropic, etc.)

### Installation

```bash
# Install L2M
pip install l2m

# Set up environment
l2m --help  # Verify installation
```

### Set Up API Keys
To set up your API key, create a `.env` file at the root of your project and add your provider key(s):

```env
# Example for OpenAI:
OPENAI_API_KEY=sk-...

# Example for Anthropic:
ANTHROPIC_API_KEY=sk-ant-...

# Example for DeepSeek:
DEEPSEEK_API_KEY=...

# Add other providers as needed
```

You can quickly start by copying the example environment file:

```bash
cp .env.example .env
```

### Usage

```bash
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
