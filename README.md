# ATLAS: Autonomous Transpilation for Legacy Application Systems

<div align="left">

<!-- Keep the gap above this line, otherwise they won't render correctly! -->
[![GitHub Repo stars](https://img.shields.io/github/stars/astrio-ai/atlas?cacheSeconds=3600)](https://github.com/astrio-ai/atlas)
[![Follow us on X](https://img.shields.io/twitter/follow/AstrioAI)](https://www.x.com/AstrioAI)
[![Join us on Discord](https://img.shields.io/discord/1396038465002405948?logo=discord&logoColor=white&label=discord)](https://discord.gg/gESuZkdD5R)
[![Contributing Guide](https://img.shields.io/badge/Contributing-Guide-informational)](https://github.com/astrio-ai/atlas/CONTRIBUTING.md)
</div>

**ATLAS** is an open-source, AI coding agent that helps you modernize legacy codebases into modern programming languages within your terminal.

**Status**: Paper in progress

<p align="left">
  <img src="./.github/atlas-cli.png" alt="ATLAS CLI" width="80%" />
</p>

## Features

- **Modern TUI**: Clean terminal interface with brand-colored UI elements
- **Multi-Provider Support**: Works with OpenAI, Anthropic, DeepSeek, Gemini, and 100+ other LLM providers via LiteLLM
- **Interactive Chat**: Natural conversation with your codebase - ask questions, request changes, and get AI assistance
- **File Management**: Add files to context, drop them when done, view what's in your chat session
- **Git Integration**: Automatic commits, undo support, and repository-aware context
- **Streaming Responses**: Real-time AI responses with markdown rendering
- **Session History**: Persistent conversation history across sessions

## Quick Start

### Prerequisites

- Python 3.14+
- BYOK for your preferred LLM provider (OpenAI, Anthropic, etc.)

### Installation

```bash
curl -fsSL https://astrio.app/atlas/install | bash
```

or

```bash
pip install astrio-atlas
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
atlas
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

## Community & Support
* Follow our project updates on [X](https://x.com/astrioai)
* Join our [Discord](https://discord.gg/2BVwAUzW)
* Join the discussion: [GitHub Discussions](https://github.com/astrio-ai/atlas/discussions)
* Report bugs: [GitHub Issues](https://github.com/astrio-ai/atlas/issues)

## Contact Us
For partnership inquiries or professional use cases:

📧 **[nolanlwin@astrio.app](mailto:nolanlwin@astrio.app)**
