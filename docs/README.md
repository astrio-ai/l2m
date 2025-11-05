# Documentation

Welcome to the Legacy2Modern (L2M) documentation!

## 📚 Documentation Index

### Getting Started
- **[Getting Started Guide](getting_started.md)** - Quick start and installation instructions

### Architecture & Design
- **[Architecture](architecture.md)** - System architecture, components, and data flow
- **[Agent System](agents.md)** - Detailed documentation of all agents
- **[Workflows](workflows.md)** - Workflow documentation and execution modes

## 🚀 Quick Links

- **Installation**: See [Getting Started](getting_started.md#installation)
- **Basic Usage**: See [Getting Started](getting_started.md#basic-usage)
- **Agent Details**: See [Agent System](agents.md)
- **Workflow Modes**: See [Workflows](workflows.md#execution-modes)

## 📖 Documentation Structure

```
docs/
├── README.md              # This file
├── getting_started.md     # Installation and quick start
├── architecture.md         # System architecture and design
├── agents.md              # Agent system documentation
└── workflows.md           # Workflow documentation
```

## 🎯 For Different Audiences

### 👨‍💻 Developers
- Start with [Getting Started](getting_started.md)
- Read [Architecture](architecture.md) for system design
- Explore [Agent System](agents.md) for implementation details

### 🔧 Contributors
- Read [Architecture](architecture.md) to understand the system
- Check [Agent System](agents.md) for adding new agents
- Review [Workflows](workflows.md) for extending workflows

### 📊 Users
- Follow [Getting Started](getting_started.md) for setup
- Use [Workflows](workflows.md) for usage examples
- Refer to [Agent System](agents.md) to understand capabilities

## 🔍 Common Tasks

### Setting Up
```bash
# Install dependencies
pip install -r requirements.txt

# Configure environment
cp .env.example .env
# Edit .env with your API key
```

### Running Modernization
```python
from src.workflows.modernization_pipeline import ModernizationPipeline

pipeline = ModernizationPipeline()
results = await pipeline.run("file.cbl")
```

### Understanding Agents
See [Agent System](agents.md) for:
- Agent purposes and capabilities
- Tool descriptions
- Usage examples
- Adding new agents

### Customizing Workflows
See [Workflows](workflows.md) for:
- Sequential vs handoff modes
- Custom workflow creation
- Performance optimization

## 📝 Documentation Standards

All documentation follows these principles:
- **Clear and concise** - Easy to understand
- **Code examples** - Practical examples included
- **Visual diagrams** - ASCII diagrams for visualization
- **Up-to-date** - Kept current with codebase

## 🤝 Contributing to Documentation

To improve documentation:
1. Edit markdown files in `docs/`
2. Follow existing format and style
3. Include code examples
4. Update this README if adding new docs
5. Submit PR with documentation changes

## 📞 Support

- **GitHub Issues**: [Report documentation issues](https://github.com/astrio-ai/l2m/issues)
- **Discord**: [Get help](https://discord.gg/2BVwAUzW)
- **Email**: naingoolwin.astrio@gmail.com

