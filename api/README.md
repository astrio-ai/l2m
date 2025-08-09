# Legacy2Modern API

A FastAPI-based REST API that provides programmatic access to the Legacy2Modern CLI functionality for transpiling legacy code to modern languages and frameworks.

## Features

- **COBOL Transpilation**: Convert COBOL code to Python with AI assistance
- **Website Modernization**: Transform legacy HTML sites to modern frameworks (React, Next.js, Astro)
- **Code Analysis**: Analyze code complexity and structure
- **File Upload Support**: Upload files directly through the API
- **Health Monitoring**: Built-in health checks and status monitoring
- **Interactive Documentation**: Auto-generated API docs with Swagger UI

## Quick Start

### 1. Install Dependencies

The API requires the following additional dependencies (already added to `requirements.txt`):

```bash
pip install fastapi uvicorn python-multipart pydantic
```

### 2. Start the Server

```bash
# From the project root
python api/run_server.py

# Or directly with uvicorn
uvicorn api.main:app --host 0.0.0.0 --port 8001 --reload
```

The API will be available at:
- **API Base**: http://localhost:8001
- **Interactive Docs**: http://localhost:8001/docs
- **ReDoc**: http://localhost:8001/redoc

### 3. Test the API

```bash
# Health check
curl http://localhost:8001/health

# Get supported frameworks
curl http://localhost:8001/frameworks
```

## API Endpoints

### Health & Status

- `GET /` - API information and links
- `GET /health` - Health status of all components
- `GET /frameworks` - List of supported target frameworks

### COBOL Transpilation

#### Text-based Transpilation
```http
POST /transpile/cobol
Content-Type: application/json

{
  "source_code": "IDENTIFICATION DIVISION...",
  "output_file": "/path/to/output.py",
  "llm_config": {
    "model": "claude-3-sonnet",
    "temperature": 0.1
  }
}
```

#### File Upload Transpilation
```http
POST /transpile/cobol/file
Content-Type: multipart/form-data

file: [COBOL file]
output_file: /path/to/output.py
llm_config: {"model": "claude-3-sonnet"}
```

### Website Modernization

#### Path-based Modernization
```http
POST /modernize/website
Content-Type: application/json

{
  "input_path": "/path/to/legacy-site.html",
  "output_dir": "/path/to/output",
  "target_framework": "react",
  "analyze_only": false
}
```

#### File Upload Modernization
```http
POST /modernize/website/file
Content-Type: multipart/form-data

file: [HTML file]
output_dir: /path/to/output
target_framework: nextjs
analyze_only: false
```

### Analysis

#### Website Analysis
```http
POST /analyze/website
Content-Type: application/json

{
  "input_path": "/path/to/website.html"
}
```

#### Code Analysis
```http
POST /analyze/code
Content-Type: application/json

{
  "source_code": "def hello(): print('Hello')",
  "target_code": "const hello = () => console.log('Hello')"
}
```

## Request/Response Models

### TranspilationRequest
```python
{
  "source_code": "string",           # Optional: Source code content
  "source_file_path": "string",      # Optional: Path to source file
  "target_framework": "react",       # Optional: Target framework
  "output_directory": "string",      # Optional: Output directory
  "analyze_only": false,             # Only analyze without generating code
  "llm_config": {}                   # LLM configuration for AI assistance
}
```

### TranspilationResponse
```python
{
  "success": true,                    # Whether the operation succeeded
  "message": "string",               # Human-readable message
  "generated_code": "string",        # Generated target code (if applicable)
  "analysis": {},                    # Analysis results
  "errors": [],                      # List of errors (if any)
  "warnings": [],                    # List of warnings (if any)
  "metadata": {}                     # Additional metadata
}
```

### FrameworkType Enum
- `react` - React.js framework
- `nextjs` - Next.js framework  
- `astro` - Astro framework

## Configuration

### LLM Configuration

The API supports configurable LLM settings for AI-assisted transpilation:

```python
{
  "model": "claude-3-sonnet",       # LLM model to use
  "temperature": 0.1,                # Creativity level (0.0-1.0)
  "max_tokens": 4000,               # Maximum output tokens
  "api_key": "your-api-key"         # API key for the LLM service
}
```

### Environment Variables

The API automatically loads LLM configuration from environment variables:
- `CLAUDE_API_KEY` - Claude API key
- `OPENAI_API_KEY` - OpenAI API key
- `ANTHROPIC_API_KEY` - Anthropic API key

## Error Handling

The API provides comprehensive error handling:

- **400 Bad Request**: Invalid input parameters
- **404 Not Found**: File or resource not found
- **500 Internal Server Error**: Server-side processing errors

All errors include detailed error messages and error types for debugging.

## Testing

### Manual Testing

```bash
# Test the API models and service layer
python -c "
import sys; sys.path.insert(0, '.')
from api.models import FrameworkType
from api.services import Legacy2ModernService
print('API components working correctly!')
"
```

### Automated Testing

```bash
# Run API tests (requires proper PYTHONPATH)
PYTHONPATH=/path/to/project python -m pytest tests/api/
```

## Architecture

The API follows a clean architecture pattern:

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   FastAPI App   │───▶│  Service Layer  │───▶│  Engine Layer   │
│   (main.py)     │    │  (services.py)  │    │  (CLI Engine)   │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         ▼                       ▼                       ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Pydantic      │    │   Error         │    │   File I/O      │
│   Models        │    │   Handling      │    │   Operations    │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## Development

### Adding New Endpoints

1. Define the request/response models in `models.py`
2. Implement the business logic in `services.py`
3. Add the endpoint in `main.py`
4. Update tests in `tests/api/`

### Running in Development Mode

```bash
# Start with auto-reload
uvicorn api.main:app --reload --host 0.0.0.0 --port 8001

# Start with debug logging
uvicorn api.main:app --reload --log-level debug
```

## Production Deployment

### Using Gunicorn

```bash
gunicorn api.main:app -w 4 -k uvicorn.workers.UvicornWorker --bind 0.0.0.0:8001
```

### Docker Deployment

```dockerfile
FROM python:3.9-slim

WORKDIR /app
COPY requirements.txt .
RUN pip install -r requirements.txt

COPY . .
EXPOSE 8001

CMD ["uvicorn", "api.main:app", "--host", "0.0.0.0", "--port", "8001"]
```

## Troubleshooting

### Common Issues

1. **Import Errors**: Ensure `PYTHONPATH` includes the project root
2. **LLM Configuration**: Check environment variables and API keys
3. **File Permissions**: Ensure proper read/write permissions for file operations
4. **Port Conflicts**: Change the port if 8001 is already in use

### Debug Mode

Enable debug logging by setting the log level:

```bash
uvicorn api.main:app --log-level debug
```

## Contributing

When contributing to the API:

1. Follow the existing code structure
2. Add comprehensive error handling
3. Include proper type hints
4. Write tests for new functionality
5. Update this README for new features

## License

This API is part of the Legacy2Modern CLI project and follows the same license terms. 