# Documentation

Welcome to the Atlas (Atlas) documentation!

## Documentation Index

### Getting Started
- **[Getting Started Guide](getting_started.md)** - Quick start and installation instructions

### Architecture & Design
- **[Architecture](architecture.md)** - System architecture, components, and data flow

## Quick Links

- **Installation**: See [Getting Started](getting_started.md#installation)
- **Basic Usage**: See [Getting Started](getting_started.md#basic-usage)
- **CLI Usage**: Run `atlas` for interactive terminal interface
- **Architecture**: See [Architecture](architecture.md) for system design

## Documentation Structure

```
docs/
├── README.md              # This file
├── getting_started.md     # Installation and quick start
└── architecture.md        # System architecture and design
```

## For Different Audiences

### Developers
- Start with [Getting Started](getting_started.md)
- Read [Architecture](architecture.md) for system design
- Explore the codebase in `src/coders/` and `cli/` for implementation details

### Contributors
- Read [Architecture](architecture.md) to understand the system
- Check `src/coders/` for different edit format implementations
- Review `cli/commands.py` for CLI command structure
- See [CONTRIBUTING.md](../CONTRIBUTING.md) for contribution guidelines

### Users
- Follow [Getting Started](getting_started.md) for setup
- Use the interactive CLI: `atlas`
- Type `help` in the CLI for available commands
- See the main [README.md](../README.md) for feature overview

## Common Tasks

### Installation
```bash
# Install the package via pypi
pip install astrio-atlas
# or from curl
curl -fsSL https://astrio.app/atlas/install | bash

# Configure environment
cp .env.example .env
# Edit .env with your API key (OPENAI_API_KEY, ANTHROPIC_API_KEY, etc.)
```

### Usage
```bash
atlas
```

### Understanding the System
- **Coder System**: See `src/coders/` for different edit formats (wholefile, editblock, udiff, etc.)
- **LLM Integration**: See `src/core/` for model management and LiteLLM integration
- **Git Integration**: See `src/git/` for repository tracking and commits
- **CLI Interface**: See `cli/` for terminal UI and command handling

## Documentation Standards

All documentation follows these principles:
- **Clear and concise** - Easy to understand
- **Code examples** - Practical examples included
- **Visual diagrams** - ASCII diagrams for visualization
- **Up-to-date** - Kept current with codebase

## Contributing to Documentation

To improve documentation:
1. Edit markdown files in `docs/`
2. Follow existing format and style
3. Include code examples
4. Update this README if adding new docs
5. Submit PR with documentation changes

## Support

- **GitHub Issues**: [Report documentation issues](https://github.com/astrio-ai/atlas/issues)
- **Discord**: [Get help](https://discord.gg/2BVwAUzW)
- **Email**: naingoolwin.astrio@gmail.com

