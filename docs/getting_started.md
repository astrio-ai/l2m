# Getting Started Guide

Get up and running with Atlas under 1 minute!

## Installation

```bash
pip install astrio-atlas
```

Or install from source:
```bash
git clone https://github.com/astrio-ai/atlas.git
cd atlas
pip install -r requirements.txt
pip install -e .
```

## Configuration

Create a `.env` file in your project directory and add your API key:

```env
OPENAI_API_KEY=sk-your-key-here
# or
ANTHROPIC_API_KEY=sk-ant-your-key-here
# or any provider supported by LiteLLM
```

## Quick Start

Start the interactive CLI:

```bash
atlas
```

Then in the terminal:

```bash
/add data/hello/HELLO.cbl
Modernize this COBOL file to Python
/diff          # Review changes
/commit        # Commit when satisfied
```

## Common Commands

- `/add <file>` - Add files to context
- `/drop <file>` - Remove files from context
- `/ls` - List files in context
- `/diff` - Show changes
- `/undo` - Undo last change
- `/commit` - Commit changes
- `/help` - Get help
- `/exit` or `Ctrl+D` - Exit

## Non-Interactive Mode

Run with a single message:

```bash
atlas --message "Modernize data/hello/HELLO.cbl to Python"
atlas data/hello/HELLO.cbl --message "Convert to Python"
```

## Configuration Options

Specify model:
```bash
atlas --model gpt-4o
```

Or in `.env`:
```env
L2M_MODEL=gpt-4o
```

## Troubleshooting

**Command not found**: `pip install astrio-atlas` or ensure it's in your PATH

**API key error**: Check `.env` file has correct key

**Model not found**: Use `atlas --list-models` to see available models

**Git errors**: Run `git init` or use `atlas --no-git`

## Next Steps

- Read [Architecture Documentation](architecture.md) for system design
- Check the main [README.md](../README.md) for feature overview
- Join [Discord](https://discord.gg/2BVwAUzW) for help

## Getting Help

- **GitHub Issues**: [Report bugs](https://github.com/astrio-ai/atlas/issues)
- **Discord**: [Join community](https://discord.gg/2BVwAUzW)
- **Email**: naingoolwin.astrio@gmail.com
