# API Documentation

This document describes the REST API endpoints for the Legacy2Modern multi-agent system.

## Overview

The system provides a REST API for interacting with agents, workflows, and the modernization process. The API is built using FastAPI and provides comprehensive endpoints for all system functionality.

## Base URL

```
http://localhost:8000
```

## Authentication

The API uses API key authentication. Include your API key in the request headers:

```
X-API-Key: your-api-key
```

## Endpoints

### Agents

#### List Agents

```http
GET /api/v1/agents/
```

**Description**: List all available agents.

**Response**:
```json
{
  "agents": [
    {
      "id": "analyzer",
      "name": "Analyzer Agent",
      "description": "Analyzes legacy codebases"
    }
  ]
}
```

#### Run Agent

```http
POST /api/v1/agents/run
```

**Description**: Run a specific agent.

**Request Body**:
```json
{
  "agent_type": "analyzer",
  "parameters": {
    "codebase_path": "/path/to/code",
    "analysis_type": "full"
  }
}
```

**Response**:
```json
{
  "agent_id": "analyzer",
  "agent_type": "analyzer",
  "status": "completed",
  "result": {
    "success": true,
    "analysis_results": {...}
  }
}
```

#### Get Agent Status

```http
GET /api/v1/agents/{agent_id}/status
```

**Description**: Get the status of a specific agent.

**Response**:
```json
{
  "agent_id": "analyzer",
  "status": "idle",
  "last_run": null,
  "next_run": null
}
```

#### Get Agent Tools

```http
GET /api/v1/agents/{agent_id}/tools
```

**Description**: Get the tools available to a specific agent.

**Response**:
```json
{
  "agent_id": "analyzer",
  "tools": ["code_analyzer", "dependency_analyzer"]
}
```

### Workflows

#### List Workflows

```http
GET /api/v1/workflows/
```

**Description**: List all available workflows.

**Response**:
```json
{
  "workflows": [
    {
      "id": "main",
      "name": "Main Workflow",
      "description": "Complete modernization workflow"
    }
  ]
}
```

#### Run Workflow

```http
POST /api/v1/workflows/run
```

**Description**: Run a specific workflow.

**Request Body**:
```json
{
  "workflow_type": "main",
  "parameters": {
    "codebase_path": "/path/to/code",
    "target_language": "python"
  }
}
```

**Response**:
```json
{
  "workflow_id": "main",
  "workflow_type": "main",
  "status": "completed",
  "result": {
    "success": true,
    "files_processed": 10
  }
}
```

#### Run Workflow Async

```http
POST /api/v1/workflows/run/async
```

**Description**: Run a workflow asynchronously.

**Request Body**:
```json
{
  "workflow_type": "main",
  "parameters": {
    "codebase_path": "/path/to/code",
    "target_language": "python"
  }
}
```

**Response**:
```json
{
  "workflow_id": "main",
  "workflow_type": "main",
  "status": "running",
  "result": null
}
```

#### Get Workflow Status

```http
GET /api/v1/workflows/{workflow_id}/status
```

**Description**: Get the status of a specific workflow.

**Response**:
```json
{
  "workflow_id": "main",
  "status": "running",
  "last_run": "2024-01-01T00:00:00Z",
  "next_run": null
}
```

### Analysis

#### Analyze Codebase

```http
POST /api/v1/analysis/analyze
```

**Description**: Analyze a legacy codebase.

**Request Body**:
```json
{
  "codebase_path": "/path/to/code",
  "analysis_type": "full",
  "options": {}
}
```

**Response**:
```json
{
  "analysis_id": "analysis_123",
  "analysis_type": "full",
  "status": "completed",
  "result": {
    "structure": {...},
    "dependencies": {...},
    "patterns": {...}
  }
}
```

#### Get Legacy Patterns

```http
GET /api/v1/analysis/patterns
```

**Description**: Get known legacy code patterns.

**Response**:
```json
{
  "patterns": [
    {
      "id": "cobol_file_io",
      "name": "COBOL File I/O",
      "description": "File input/output operations in COBOL",
      "language": "cobol"
    }
  ]
}
```

#### Analyze Dependencies

```http
GET /api/v1/analysis/dependencies?codebase_path=/path/to/code
```

**Description**: Analyze dependencies in a codebase.

**Response**:
```json
{
  "dependencies": {
    "external": [],
    "internal": [],
    "circular": []
  }
}
```

#### Analyze Complexity

```http
GET /api/v1/analysis/complexity?codebase_path=/path/to/code
```

**Description**: Analyze code complexity.

**Response**:
```json
{
  "complexity": {
    "cyclomatic_complexity": 10,
    "cognitive_complexity": 15,
    "maintainability_index": 85
  }
}
```

### Modernization

#### Modernize Codebase

```http
POST /api/v1/modernization/modernize
```

**Description**: Modernize a legacy codebase.

**Request Body**:
```json
{
  "codebase_path": "/path/to/code",
  "target_language": "python",
  "modernization_goals": ["maintainability", "readability"]
}
```

**Response**:
```json
{
  "modernization_id": "modernization_123",
  "target_language": "python",
  "status": "completed",
  "result": {
    "success": true,
    "files_processed": 10,
    "files_successful": 9,
    "files_failed": 1
  }
}
```

#### Modernize Codebase Async

```http
POST /api/v1/modernization/modernize/async
```

**Description**: Modernize a codebase asynchronously.

**Request Body**:
```json
{
  "codebase_path": "/path/to/code",
  "target_language": "python",
  "modernization_goals": ["maintainability"]
}
```

**Response**:
```json
{
  "modernization_id": "modernization_123",
  "target_language": "python",
  "status": "running",
  "result": null
}
```

#### Get Supported Languages

```http
GET /api/v1/modernization/languages
```

**Description**: Get supported target languages.

**Response**:
```json
{
  "languages": [
    {
      "id": "python",
      "name": "Python",
      "description": "Modern Python code",
      "extensions": [".py"]
    }
  ]
}
```

#### Get Modernization Status

```http
GET /api/v1/modernization/{modernization_id}/status
```

**Description**: Get the status of a modernization process.

**Response**:
```json
{
  "modernization_id": "modernization_123",
  "status": "completed",
  "progress": 100,
  "start_time": "2024-01-01T00:00:00Z",
  "end_time": "2024-01-01T00:01:00Z"
}
```

## Error Handling

### Error Response Format

```json
{
  "error_code": "VALIDATION_ERROR",
  "message": "Invalid input parameters",
  "details": {
    "field": "codebase_path",
    "issue": "Path does not exist"
  }
}
```

### Common Error Codes

- `VALIDATION_ERROR`: Input validation failed
- `CONFIGURATION_ERROR`: Configuration error
- `AGENT_ERROR`: Agent execution error
- `WORKFLOW_ERROR`: Workflow execution error
- `API_ERROR`: API error
- `UNKNOWN_ERROR`: Unknown error

## Rate Limiting

The API implements rate limiting:

- **Requests per minute**: 60 requests
- **Requests per hour**: 1000 requests
- **Rate limit headers**: Included in responses

## Response Codes

- `200 OK`: Request successful
- `201 Created`: Resource created
- `400 Bad Request`: Invalid request
- `401 Unauthorized`: Authentication required
- `403 Forbidden`: Access denied
- `404 Not Found`: Resource not found
- `429 Too Many Requests`: Rate limit exceeded
- `500 Internal Server Error`: Server error

## Examples

### Complete Modernization Workflow

1. **Analyze Codebase**:
```bash
curl -X POST "http://localhost:8000/api/v1/analysis/analyze" \
  -H "Content-Type: application/json" \
  -H "X-API-Key: your-api-key" \
  -d '{"codebase_path": "/path/to/cobol", "analysis_type": "full"}'
```

2. **Modernize Codebase**:
```bash
curl -X POST "http://localhost:8000/api/v1/modernization/modernize" \
  -H "Content-Type: application/json" \
  -H "X-API-Key: your-api-key" \
  -d '{"codebase_path": "/path/to/cobol", "target_language": "python", "modernization_goals": ["maintainability"]}'
```

3. **Check Status**:
```bash
curl -X GET "http://localhost:8000/api/v1/modernization/modernization_123/status" \
  -H "X-API-Key: your-api-key"
```

## SDK and Client Libraries

The API can be used with any HTTP client, but we recommend:

- **Python**: `requests` library
- **JavaScript**: `axios` or `fetch`
- **cURL**: Command-line tool
- **Postman**: GUI tool for testing

## WebSocket Support

For real-time updates, the API supports WebSocket connections:

```
ws://localhost:8000/ws
```

WebSocket messages include:
- Workflow progress updates
- Agent status changes
- Error notifications
- Completion notifications
