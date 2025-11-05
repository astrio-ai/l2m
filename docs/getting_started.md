# Getting Started Guide

## Quick Start

Get up and running with L2M in 5 minutes!

### 1. Installation

```bash
# Clone the repository
git clone https://github.com/astrio-ai/l2m.git
cd l2m

# Create virtual environment
python -m venv .venv
source .venv/bin/activate  # On Windows: .venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt
```

### 2. Configuration

```bash
# Create .env file
cp .env.example .env

# Edit .env and add your OpenAI API key
# OPENAI_API_KEY=sk-your-key-here
```

### 3. Run Your First Modernization

```python
import asyncio
from src.workflows.modernization_pipeline import ModernizationPipeline

async def main():
    # Create pipeline
    pipeline = ModernizationPipeline()
    
    # Modernize a COBOL file
    results = await pipeline.run("data/samples/sample1.cbl")
    
    # Print results
    print(results["translation"])

asyncio.run(main())
```

## Detailed Setup

### Prerequisites

- **Python 3.10+**: Required for type hints and modern features
- **OpenAI API Key**: Get one from [platform.openai.com](https://platform.openai.com)
- **Git**: For cloning the repository

### Step-by-Step Installation

#### 1. Clone Repository

```bash
git clone https://github.com/astrio-ai/l2m.git
cd l2m
```

#### 2. Set Up Virtual Environment

**macOS/Linux**:
```bash
python3 -m venv .venv
source .venv/bin/activate
```

**Windows**:
```bash
python -m venv .venv
.venv\Scripts\activate
```

#### 3. Install Dependencies

```bash
pip install -r requirements.txt
```

Key dependencies:
- `openai-agents>=0.4.0` - Multi-agent framework
- `pydantic>=2.0.0` - Configuration management
- `python-dotenv>=1.0.0` - Environment variable loading

#### 4. Configure Environment

Create `.env` file in project root:

```bash
# .env
OPENAI_API_KEY=sk-your-key-here
OPENAI_MODEL=gpt-4o
OPENAI_TEMPERATURE=0.7
MAX_TURNS=50
```

#### 5. Verify Installation

```bash
# Run tests
python -m pytest tests/ -v

# Check imports
python -c "from src.workflows.modernization_pipeline import ModernizationPipeline; print('✅ Installation successful')"
```

## Basic Usage

### Simple Example

```python
import asyncio
from src.workflows.modernization_pipeline import ModernizationPipeline

async def main():
    pipeline = ModernizationPipeline()
    results = await pipeline.run("data/samples/sample1.cbl")
    print(results["translation"])

asyncio.run(main())
```

### Sequential Mode

Execute agents one after another:

```python
pipeline = ModernizationPipeline(use_handoffs=False)
results = await pipeline.run("file.cbl")

# Access individual step results
print("Analysis:", results["analysis"])
print("Translation:", results["translation"])
print("Review:", results["review"])
```

### Handoff Mode

Let orchestrator route tasks intelligently:

```python
pipeline = ModernizationPipeline(use_handoffs=True)
results = await pipeline.run("file.cbl")
print("Output:", results["orchestrator_output"])
```

### With Session

Maintain conversation history:

```python
pipeline = ModernizationPipeline(session_id="my_session")
results = await pipeline.run("file1.cbl")
results = await pipeline.run("file2.cbl")  # Uses same session context
```

## Working with COBOL Files

### File Requirements

- **File Extension**: `.cbl`, `.cob`, or `.cobol`
- **Encoding**: UTF-8 (with error handling)
- **Format**: Standard COBOL syntax

### Sample COBOL File

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD!'.
           GOBACK.
```

Save as `data/samples/hello.cbl`

### Running Modernization

```python
from pathlib import Path

cobol_file = Path("data/samples/hello.cbl")
pipeline = ModernizationPipeline()
results = await pipeline.run(str(cobol_file))
```

## Understanding Results

### Result Structure

```python
results = {
    "cobol_file": "path/to/file.cbl",
    "analysis": "Analysis report from Analyzer Agent",
    "translation": "Python code from Translator Agent",
    "review": "Review report from Reviewer Agent",
    "tests": "Test results from Tester Agent",
    "refactored": "Refactored Python code from Refactor Agent",
    "error": None  # or error message if failed
}
```

### Accessing Results

```python
results = await pipeline.run("file.cbl")

if "error" in results:
    print(f"Error: {results['error']}")
else:
    print("Analysis:", results["analysis"])
    print("\nTranslation:", results["translation"])
    print("\nReview:", results["review"])
```

## Configuration Options

### Model Selection

```python
from src.config import get_settings

settings = get_settings()
settings.openai_model = "gpt-3.5-turbo"  # Faster, cheaper
# or
settings.openai_model = "gpt-4o"  # Better quality (default)
```

### Temperature Control

```python
settings.openai_temperature = 0.3  # More deterministic
# or
settings.openai_temperature = 0.7  # More creative (default)
```

### Session Database

```python
settings.db_path = Path("custom/path/sessions.db")
```

## Troubleshooting

### Common Issues

#### 1. ModuleNotFoundError: No module named 'agents'

**Solution**: Activate virtual environment
```bash
source .venv/bin/activate
```

#### 2. API Key Error

**Solution**: Check `.env` file has correct API key
```bash
cat .env | grep OPENAI_API_KEY
```

#### 3. File Not Found

**Solution**: Verify file path is correct
```python
from pathlib import Path
file_path = Path("data/samples/sample1.cbl")
assert file_path.exists(), f"File not found: {file_path}"
```

#### 4. Import Errors

**Solution**: Install dependencies
```bash
pip install -r requirements.txt
```

## Next Steps

- Read [Architecture Documentation](architecture.md) for system design
- Explore [Agent System](agents.md) for agent details
- Check [Workflow Documentation](workflows.md) for advanced usage
- Review [Contributing Guide](../CONTRIBUTING.md) to contribute

## Getting Help

- **GitHub Issues**: [Report bugs](https://github.com/astrio-ai/l2m/issues)
- **Discord**: [Join community](https://discord.gg/2BVwAUzW)
- **Email**: naingoolwin.astrio@gmail.com

