# Architecture Documentation

## Overview

Legacy2Modern (L2M) is built on a **multi-agent architecture** using the OpenAI Agents SDK. The system orchestrates specialized AI agents that work together to transform legacy COBOL code into modern Python.

## System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Modernization Pipeline                    │
│                                                              │
│  ┌──────────────┐                                           │
│  │ Orchestrator │  ◄─── Coordinates all agents              │
│  │    Agent     │                                            │
│  └──────┬───────┘                                            │
│         │                                                     │
│         ├──────────┬──────────┬──────────┬──────────┐        │
│         │          │          │          │          │        │
│    ┌────▼────┐ ┌──▼───┐ ┌────▼────┐ ┌──▼───┐ ┌────▼────┐   │
│    │Analyzer │ │Trans │ │Reviewer │ │Tester│ │Refactor│   │
│    │ Agent   │ │Agent │ │ Agent   │ │Agent │ │ Agent  │   │
│    └────┬────┘ └──┬───┘ └────┬────┘ └──┬───┘ └────┬────┘   │
│         │          │          │          │          │        │
│         └──────────┴──────────┴──────────┴──────────┘        │
│                    │                                            │
│                    ▼                                            │
│         ┌──────────────────────┐                               │
│         │  Modernized Python   │                               │
│         └──────────────────────┘                               │
└─────────────────────────────────────────────────────────────────┘
```

## Core Components

### 1. Modernization Pipeline

The `ModernizationPipeline` class orchestrates the entire workflow. It supports two execution modes:

- **Sequential Mode**: Agents run one after another in a fixed order
- **Handoff Mode**: Orchestrator agent dynamically routes tasks to specialized agents

**Location**: `src/workflows/modernization_pipeline.py`

### 2. Agent System

Each agent is a specialized AI assistant with specific capabilities:

| Agent | Purpose | Tools | Output |
|-------|---------|-------|--------|
| **Orchestrator** | Coordinates workflow and handoffs | None | Routes tasks to appropriate agents |
| **Analyzer** | Parses COBOL structure | `analyze_cobol` | Analysis report with structure, variables, procedures |
| **Translator** | Converts COBOL to Python | `translate_to_python` | Python code equivalent |
| **Reviewer** | Reviews code quality | `review_code` | Code review with suggestions |
| **Tester** | Generates and runs tests | `create_tests`, `execute_tests` | Test suite and results |
| **Refactor** | Improves code structure | `refactor_code` | Refactored Python code |

**Location**: `src/agents/`

### 3. Tools

Tools are functions that agents can call to perform specific tasks:

- **COBOL Parser** (`src/tools/cobol_parser_tool.py`): Extracts structure from COBOL files
- **Python Synthesis** (`src/tools/python_synth_tool.py`): Generates and validates Python code
- **Code Quality** (`src/tools/code_quality_tool.py`): Checks code quality metrics
- **Test Runner** (`src/tools/test_runner_tool.py`): Executes generated tests

### 4. Guardrails

Guardrails validate inputs and outputs:

- **COBOL Input Guard** (`src/guardrails/cobol_input_guard.py`): Validates COBOL file before processing
- **Python Output Guard** (`src/guardrails/python_output_guard.py`): Ensures generated Python code is safe and valid

### 5. Session Management

The system uses SQLite sessions to maintain conversation history across agent interactions:

- **Session Storage**: `data/sessions.db`
- **Session ID**: Unique identifier for each modernization run
- **Context Preservation**: Agents can reference previous analysis and translations

**Location**: `src/sessions/`

### 6. Configuration

Centralized configuration using Pydantic settings:

- **Settings** (`src/config/settings.py`): Model configuration, API keys, paths
- **Environment Variables**: Loaded from `.env` file
- **Default Values**: Sensible defaults for all settings

## Data Flow

```
COBOL File
    │
    ▼
┌─────────────────┐
│ Input Guardrail │  ← Validates file exists, is readable
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Analyzer Agent │  ← Parses structure, extracts logic
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│Translator Agent │  ← Converts COBOL → Python
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Output Guardrail│  ← Validates Python syntax
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Reviewer Agent  │  ← Reviews code quality
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Tester Agent   │  ← Generates & runs tests
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Refactor Agent  │  ← Improves structure
└────────┬────────┘
         │
         ▼
  Python Code
```

## Execution Modes

### Sequential Mode

Agents execute in a fixed order:

1. **Analyzer** → Analyzes COBOL file
2. **Translator** → Translates to Python
3. **Reviewer** → Reviews translated code
4. **Tester** → Generates and runs tests
5. **Refactor** → Refactors code

**Use Case**: When you want predictable, step-by-step execution.

### Handoff Mode

Orchestrator agent dynamically routes tasks:

```
User Request
    │
    ▼
Orchestrator Agent
    │
    ├─→ Analyzer (if analysis needed)
    ├─→ Translator (if translation needed)
    ├─→ Reviewer (if review needed)
    ├─→ Tester (if testing needed)
    └─→ Refactor (if refactoring needed)
```

**Use Case**: When you want the orchestrator to intelligently decide which agents to use.

## Technology Stack

- **OpenAI Agents SDK**: Multi-agent orchestration framework
- **OpenAI GPT Models**: Language models for code understanding and generation
- **SQLite**: Session storage and conversation history
- **Pydantic**: Configuration and data validation
- **Python 3.10+**: Runtime environment

## Extensibility

The architecture is designed for extensibility:

1. **Add New Agents**: Create a new agent class in `src/agents/`
2. **Add New Tools**: Create tool functions and register with agents
3. **Add New Guardrails**: Create validation functions in `src/guardrails/`
4. **Custom Workflows**: Create new pipeline classes in `src/workflows/`

## Security Considerations

- **Input Validation**: All COBOL files validated before processing
- **Output Validation**: Generated Python code validated for syntax and safety
- **API Key Management**: Secure storage via environment variables
- **Session Isolation**: Each session maintains separate conversation history

