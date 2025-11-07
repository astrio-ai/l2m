# Legacy2Modern (L2M)

<div align="left">

<!-- Keep the gap above this line, otherwise they won't render correctly! -->
[![GitHub Repo stars](https://img.shields.io/github/stars/astrio-ai/l2m)](https://github.com/astrio-ai/l2m) 
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

### Basic Usage

#### Command Line Interface (CLI)

The easiest way to run the modernization pipeline is through the CLI:

```bash
# Activate virtual environment (if not already activated)
source .venv/bin/activate

# Run modernization on a COBOL file
python -m src.main <path_to_cobol_file>

# Example:
python -m src.main data/hello/HELLO.cbl
```

The pipeline will:
1. Analyze the COBOL code structure
2. Translate it to modern Python
3. Review the translated code
4. Generate unit tests
5. Refactor for best practices
6. Save the generated Python file and test file to `data/output/`

**Output:**
- Generated Python file: `data/output/<filename>.py`
- Generated test file: `data/output/test_<filename>.py`

#### Batch Processing

Process multiple COBOL files at once with real-time progress tracking:

```bash
# Process all COBOL files in a directory (recursive)
python -m src.main --directory data/aws-samples-aws-mainframe-modernization-carddemo/ --batch-delay 10.0

# Process multiple specific files
python -m src.main file1.cbl file2.cbl file3.cbl

# Process files matching a pattern
python -m src.main --pattern "**/*.cbl"

# Process files from a list file
python -m src.main --file-list files.txt
```

**Batch Processing Features:**
- **Real-time Progress**: Shows progress bar, ETA, and current step for each file
- **Progress Display**: `[████████░░░░░░░░] 20% (3/15) | ETA: 45 minutes | [3/15] Processing COACTUPC.cbl (4237 lines)... Analyzing...`
- **Error Handling**: Continues processing on errors (configurable)
- **Rate Limiting**: Configurable delays between files to avoid API rate limits
- **Summary Report**: JSON report with detailed results saved to `data/output/batch_report_YYYYMMDD_HHMMSS.json`

**Batch Options:**
- `--batch-delay <seconds>`: Delay between files (default: 5.0s)
- `--max-concurrent <n>`: Maximum concurrent files (default: 1)
- `--no-continue-on-error`: Stop on first error (default: continue)

**Example Output:**
```
======================================================================
Starting batch modernization of 15 file(s)
======================================================================
Output directory: data/output
Delay between files: 10.0s
Continue on error: True
----------------------------------------------------------------------

[████████░░░░░░░░] 20% (3/15) | ETA: 45 minutes | [3/15] Processing COACTUPC.cbl (4237 lines)... Translating...

======================================================================
BATCH MODERNIZATION SUMMARY
======================================================================
Total files processed: 15
Successful: 12 (80.0%)
Failed: 3 (20.0%)
Total duration: 2h 15m 30s
...
```

## Agents

- **Orchestrator Agent**: Manages overall pipeline and agent handoffs
- **Analyzer Agent**: Parses COBOL and extracts logic
- **Translator Agent**: Converts COBOL to Python
- **Reviewer Agent**: Reviews translated code quality
- **Tester Agent**: Creates and runs unit tests
- **Refactor Agent**: Improves code structure and readability

## Documentation

- [Getting Started](docs/getting_started.md) - Installation and quick start guide
- [Architecture](docs/architecture.md) - System architecture and design
- [Agent System](docs/agents.md) - Detailed agent documentation
- [Workflows](docs/workflows.md) - Workflow documentation and usage
- [Full Documentation](docs/README.md) - Complete documentation index

## Testing

**Important: Make sure your virtual environment is activated before running tests.**

```bash
# Activate virtual environment first
source .venv/bin/activate

# Then run tests (use python -m pytest to ensure venv Python is used)
python -m pytest tests/
```

If you see `ModuleNotFoundError: No module named 'agents'`, it means the virtual environment isn't activated. Make sure to run `source .venv/bin/activate` first.

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
* Join the discussion: [GitHub Discussions](https://github.com/astrio-ai/l2m/discussions)
* Report bugs: [GitHub Issues](https://github.com/astrio-ai/l2m/issues)

## Contact Us
For partnership inquiries or professional use cases:

📧 **[naingoolwin.astrio@gmail.com](mailto:naingoolwin.astrio@gmail.com)**
