# Getting Started with Legacy2Modern

Welcome to Legacy2Modern, a multi-agent system for modernizing legacy codebases using LangGraph workflows and specialized AI agents.

## Quick Start

### Installation

1. Clone the repository:
```bash
git clone https://github.com/your-org/legacy2modern.git
cd legacy2modern
```

2. Install dependencies:
```bash
pip install -r requirements.txt
```

3. Set up environment variables:
```bash
cp .env.example .env
# Edit .env with your configuration
```

### Basic Usage

#### Using the CLI

1. Analyze a legacy codebase:
```bash
python -m src.cli analyze /path/to/legacy/code
```

2. Modernize a codebase:
```bash
python -m src.cli modernize /path/to/legacy/code python
```

3. Generate tests:
```bash
python -m src.cli test /path/to/modernized/code
```

4. Validate modernized code:
```bash
python -m src.cli validate /path/to/modernized/code
```

#### Using the API

1. Start the API server:
```bash
python -m src.api.server
```

2. Make API requests:
```bash
curl -X POST "http://localhost:8000/api/v1/agents/run" \
  -H "Content-Type: application/json" \
  -d '{"agent_type": "analyzer", "parameters": {"codebase_path": "/path/to/code"}}'
```

## Supported Languages

### Legacy Languages
- COBOL (.cobol, .cbl, .cob)
- FORTRAN (.f, .f90)
- Pascal (.pas)
- Assembly (.asm)

### Target Languages
- Python (.py)
- Java (.java)
- C# (.cs)
- C (.c)

## Configuration

### Environment Variables

- `LLM_PROVIDER`: LLM provider (anthropic, openai, azure, local)
- `LLM_MODEL`: LLM model name
- `LLM_API_KEY`: API key for LLM provider
- `API_KEY`: API key for authentication
- `DEBUG`: Enable debug mode

### Configuration Files

Create a `config.yaml` file:

```yaml
llm:
  provider: anthropic
  model: claude-3-sonnet
  api_key: your-api-key

agents:
  timeout: 300
  retry_count: 3
  parallel_limit: 5

workflows:
  timeout: 3600
  retry_count: 2
  parallel_limit: 3
```

## Examples

### COBOL to Python

1. Analyze COBOL codebase:
```bash
python -m src.cli analyze examples/cobol --output analysis.json
```

2. Modernize to Python:
```bash
python -m src.cli modernize examples/cobol python --output modernized/
```

3. Generate tests:
```bash
python -m src.cli test modernized/ --output tests/
```

4. Validate results:
```bash
python -m src.cli validate modernized/ --output validation.json
```

### API Workflow

1. Start the server:
```bash
python -m src.api.server
```

2. Analyze codebase:
```bash
curl -X POST "http://localhost:8000/api/v1/analysis/analyze" \
  -H "Content-Type: application/json" \
  -d '{"codebase_path": "/path/to/code", "analysis_type": "full"}'
```

3. Modernize codebase:
```bash
curl -X POST "http://localhost:8000/api/v1/modernization/modernize" \
  -H "Content-Type: application/json" \
  -d '{"codebase_path": "/path/to/code", "target_language": "python", "modernization_goals": ["maintainability"]}'
```

## Troubleshooting

### Common Issues

1. **Import errors**: Make sure all dependencies are installed
2. **API key errors**: Check your environment variables
3. **File permission errors**: Ensure proper file permissions
4. **Memory errors**: Increase available memory for large codebases

### Debug Mode

Enable debug mode for detailed logging:

```bash
python -m src.cli --verbose analyze /path/to/code
```

### Logs

Check logs in the `logs/` directory for detailed information about execution.

## Next Steps

- Read the [Architecture Documentation](architecture.md)
- Learn about [Agent Documentation](agents.md)
- Explore [Workflow Documentation](workflows.md)
- Check the [API Documentation](api.md)
