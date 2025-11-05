# Agent System Documentation

## Overview

L2M uses a **multi-agent system** where specialized AI agents collaborate to modernize legacy code. Each agent has a specific role and set of tools.

## Agent Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Agent System                             │
│                                                              │
│  Each Agent:                                                 │
│  ┌──────────────┐                                           │
│  │  Instructions │  ← Defines agent's role and behavior      │
│  └──────┬───────┘                                            │
│         │                                                     │
│  ┌──────▼───────┐                                            │
│  │     Tools     │  ← Functions agent can call               │
│  └──────┬───────┘                                            │
│         │                                                     │
│  ┌──────▼───────┐                                            │
│  │  LLM Model   │  ← OpenAI GPT model for reasoning          │
│  └──────────────┘                                            │
└─────────────────────────────────────────────────────────────┘
```

## Agent Details

### 1. Orchestrator Agent

**Purpose**: Coordinates the overall modernization workflow and routes tasks to specialized agents.

**Capabilities**:
- Routes tasks to appropriate agents
- Maintains workflow context
- Coordinates handoffs between agents

**Handoffs To**:
- Analyzer Agent
- Translator Agent
- Reviewer Agent
- Tester Agent
- Refactor Agent

**Location**: `src/agents/orchestrator_agent.py`

**Example Usage**:
```python
orchestrator = OrchestratorAgent(handoff_agents=[analyzer, translator, ...])
result = await orchestrator.run("Modernize this COBOL file: program.cbl")
```

---

### 2. Analyzer Agent

**Purpose**: Parses COBOL code and extracts structure, variables, and business logic.

**Instructions**:
- Parse COBOL source files to understand structure
- Extract variable declarations (WORKING-STORAGE, DATA DIVISION)
- Identify procedures and their logic flow
- Map dependencies between procedures
- Identify business logic patterns

**Tools**:
- `analyze_cobol(code_path: str)`: Analyzes COBOL file and returns structure report

**Output**: Analysis report with:
- Program structure (PROGRAM-ID, divisions)
- Variable declarations
- Procedures and logic flow
- Dependencies between sections

**Location**: `src/agents/analyzer_agent.py`

**Example Output**:
```json
{
  "program_id": "HELLO",
  "working_storage": [...],
  "procedures": ["MAIN", "DISPLAY-HELLO"],
  "display_statements": ["'HELLO WORLD!'"]
}
```

---

### 3. Translator Agent

**Purpose**: Converts analyzed COBOL code into modern Python equivalents.

**Instructions**:
- Convert COBOL syntax to Python
- Map COBOL data types to Python types
- Translate control flow structures
- Maintain business logic equivalence
- Follow Python best practices

**Tools**:
- `translate_to_python(cobol_code: str, analysis: str)`: Translates COBOL to Python

**Output**: Python code equivalent to the COBOL source

**Location**: `src/agents/translator_agent.py`

**Example**:
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO.
PROCEDURE DIVISION.
    DISPLAY 'HELLO WORLD!'.
    GOBACK.
```

**Translates to**:
```python
def main():
    print('HELLO WORLD!')
    return

if __name__ == '__main__':
    main()
```

---

### 4. Reviewer Agent

**Purpose**: Reviews translated Python code for quality, correctness, and best practices.

**Instructions**:
- Check code quality and style
- Verify correctness of translation
- Suggest improvements
- Identify potential bugs
- Ensure Python best practices

**Tools**:
- `review_code(python_code: str)`: Reviews Python code and returns suggestions

**Output**: Review report with:
- Code quality score
- Issues found
- Suggestions for improvement
- Best practice recommendations

**Location**: `src/agents/reviewer_agent.py`

---

### 5. Tester Agent

**Purpose**: Generates and executes unit tests for translated Python code.

**Instructions**:
- Generate comprehensive unit tests
- Test edge cases and error handling
- Execute tests and report results
- Ensure test coverage

**Tools**:
- `create_tests(python_code: str, requirements: str)`: Generates test code
- `execute_tests(test_code: str)`: Runs tests and returns results

**Output**: Test suite and execution results

**Location**: `src/agents/tester_agent.py`

**Example Test Generation**:
```python
def test_main():
    """Test main function."""
    # Capture stdout
    import io
    import sys
    old_stdout = sys.stdout
    sys.stdout = buffer = io.StringIO()
    
    main()
    
    output = buffer.getvalue()
    sys.stdout = old_stdout
    
    assert 'HELLO WORLD!' in output
```

---

### 6. Refactor Agent

**Purpose**: Improves code structure, readability, and maintainability.

**Instructions**:
- Improve code organization
- Enhance readability
- Optimize structure
- Apply design patterns where appropriate
- Maintain functionality while improving quality

**Tools**:
- `refactor_code(python_code: str, suggestions: str)`: Refactors Python code

**Output**: Refactored Python code with improved structure

**Location**: `src/agents/refactor_agent.py`

---

## Agent Communication

### Handoff Mechanism

When using handoff mode, agents communicate through the orchestrator:

```
User Request
    │
    ▼
Orchestrator
    │
    ├─→ Analyzer: "Analyze this COBOL file"
    │   └─→ Returns: Analysis report
    │
    ├─→ Translator: "Translate using this analysis"
    │   └─→ Returns: Python code
    │
    ├─→ Reviewer: "Review this Python code"
    │   └─→ Returns: Review report
    │
    └─→ Refactor: "Refactor based on review"
        └─→ Returns: Refactored code
```

### Session Context

All agents share the same session context, allowing them to:
- Reference previous agent outputs
- Maintain conversation history
- Build upon previous analyses

**Session Storage**: SQLite database at `data/sessions.db`

---

## Adding a New Agent

To add a new agent:

1. **Create Agent Class** (`src/agents/new_agent.py`):
```python
from agents import Agent, Runner, function_tool
from agents.model_settings import ModelSettings
from src.config import get_settings

@function_tool
def new_tool(input: str) -> str:
    """Tool description."""
    # Tool implementation
    return result

class NewAgent:
    def __init__(self):
        self.settings = get_settings()
        model_settings = ModelSettings(
            temperature=self.settings.openai_temperature,
        )
        
        self.agent = Agent(
            name="New Agent",
            instructions="Agent instructions...",
            model=self.settings.openai_model,
            model_settings=model_settings,
            tools=[new_tool],
        )
    
    async def run(self, input_text: str, session=None) -> str:
        result = await Runner.run(
            self.agent,
            input_text,
            session=session,
            max_turns=self.settings.max_turns,
        )
        return result.final_output
```

2. **Add to Pipeline** (`src/workflows/modernization_pipeline.py`):
```python
from src.agents.new_agent import NewAgent

class ModernizationPipeline:
    def __init__(self, ...):
        # ...
        self.new_agent = NewAgent()
```

3. **Update Orchestrator** (if using handoffs):
```python
handoff_agents = [
    # ... existing agents
    self.new_agent.agent,
]
```

---

## Best Practices

1. **Clear Instructions**: Provide specific, clear instructions for each agent
2. **Tool Design**: Make tools focused and reusable
3. **Error Handling**: Agents should handle errors gracefully
4. **Context Preservation**: Use sessions to maintain context
5. **Testing**: Test each agent independently and in the pipeline

