# Agent Architecture & Orchestration Visuals

## Main Workflow Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                    Legacy2Modern Multi-Agent System                  │
│                         (LangGraph Orchestration)                    │
└─────────────────────────────────────────────────────────────────────┘

START
  │
  ▼
┌─────────────────┐
│  Analyzer Agent │ ◄─────────────────────┐
│                 │                        │
│ • Code Analysis │                        │
│ • Dependency    │  ┌─────────────────┐   │
│   Mapping       │  │                 │   │
│ • Pattern       │  │  GraphState     │   │
│   Detection     │  │  (Shared State) │   │
└────────┬────────┘  │                 │   │
         │           │ • Messages      │   │
         ▼           │ • Analysis      │   │
┌─────────────────┐  │ • Plans         │   │
│  Planner Agent  │  │ • Code          │   │
│                 │  │ • Reviews       │   │
│ • Strategy Dev  │  │ • Tests         │   │
│ • Risk Assess   │  │ • Validation    │   │
│ • Rule Gen      │  └─────────────────┘   │
└────────┬────────┘         ▲              │
         │                  │              │
         ▼                  │              │
┌─────────────────┐         │              │
│ Executor Agent  │         │              │
│                 │         │              │
│ • Transform     │         │              │
│ • Generate Code │         │              │
│ • Pattern Match │         │              │
└────────┬────────┘         │              │
         │                  │              │
         ▼                  │              │
┌─────────────────┐         │              │
│ Reviewer Agent  │         │              │
│                 │         │              │
│ • Quality Check │─────────┘              │
│ • Standards     │                        │
│ • Best Practices│                        │
└────────┬────────┘                        │
         │                                 │
         ▼                                 │
┌─────────────────┐                        │
│  Tester Agent   │                        │
│                 │                        │
│ • Test Gen      │────────────────────────┘
│ • Coverage      │
│ • Validation    │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Validator Agent │
│                 │
│ • Final Check   │
│ • Compliance    │
│ • Integration   │
└────────┬────────┘
         │
         ▼
       END
```

## Workflow Variants

### 1. Main Workflow (Complete Modernization)
```
Analyzer → Planner → Executor → Reviewer → Tester → Validator
```
**Use Case**: Full COBOL modernization with comprehensive validation

### 2. Analysis Workflow (Code Understanding)
```
Analyzer → END
```
**Use Case**: Quick codebase assessment and documentation

### 3. Modernization Workflow (Code Transformation)
```
Planner → Executor → Validator
```
**Use Case**: Code transformation without full quality review

### 4. Validation Workflow (Quality Assurance)
```
Reviewer → Tester → Validator
```
**Use Case**: Testing and validation of transformed code

## Agent Communication Pattern

### State-Based Communication
```
┌─────────────┐
│   Agent A   │
│             │
│  Processes  │──┐
│   Input     │  │
└─────────────┘  │
                 │ Updates State
                 ▼
        ┌────────────────┐
        │   GraphState   │
        │                │
        │ • Messages     │
        │ • Results      │
        │ • Metadata     │
        │ • Errors       │
        └────────┬───────┘
                 │ Reads State
                 │
        ┌────────▼───────┐
        │     Agent B    │
        │                │
        │   Processes    │
        │   State        │
        └────────────────┘
```

## Data Flow Architecture

```
┌────────────────────────────────────────────────────────────────┐
│                       Input Layer                               │
│  • COBOL Source Code                                           │
│  • Target Language (Python)                                     │
│  • Modernization Goals                                          │
└────────────────────┬───────────────────────────────────────────┘
                     │
                     ▼
┌────────────────────────────────────────────────────────────────┐
│                    Processing Layer                             │
│                                                                 │
│  ┌─────────────┐   ┌──────────────┐   ┌──────────────┐        │
│  │   Agent 1   │──▶│  GraphState  │◀──│   Agent 2    │        │
│  │  Analyzer   │   │              │   │   Planner    │        │
│  └─────────────┘   │              │   └──────────────┘        │
│                    │  Shared      │                            │
│  ┌─────────────┐   │  State       │   ┌──────────────┐        │
│  │   Agent 3   │──▶│  Management  │◀──│   Agent 4    │        │
│  │  Executor   │   │              │   │  Reviewer    │        │
│  └─────────────┘   └──────────────┘   └──────────────┘        │
│                                                                 │
│  ┌─────────────┐                     ┌──────────────┐         │
│  │   Agent 5   │────────────────────▶│   Agent 6    │         │
│  │   Tester    │                     │  Validator   │         │
│  └─────────────┘                     └──────────────┘         │
└────────────────────────────────────────────────────────────────┘
                     │
                     ▼
┌────────────────────────────────────────────────────────────────┐
│                       Output Layer                              │
│  • transformed_code.py (Modern Python)                          │
│  • cobol_to_python_mapping.py (Reference)                       │
│  • test_transformed_code.py (Test Suite)                        │
│  • Quality Metrics                                              │
└────────────────────────────────────────────────────────────────┘
```

## LangGraph Configuration

```python
# Main Workflow
workflow = StateGraph(GraphState)

# Add Nodes (Agents)
workflow.add_node("analyzer", run_analyzer)
workflow.add_node("planner", run_planner)
workflow.add_node("executor", run_executor)
workflow.add_node("reviewer", run_reviewer)
workflow.add_node("tester", run_tester)
workflow.add_node("validator", run_validator)

# Define Edges (Flow)
workflow.set_entry_point("analyzer")
workflow.add_edge("analyzer", "planner")
workflow.add_edge("planner", "executor")
workflow.add_edge("executor", "reviewer")
workflow.add_edge("reviewer", "tester")
workflow.add_edge("tester", "validator")
workflow.add_edge("validator", END)

# Compile & Execute
compiled_graph = workflow.compile()
result = await compiled_graph.ainvoke(initial_state)
```

## Agent Responsibilities Matrix

| Agent | Primary Role | Key Operations | Outputs |
|-------|-------------|----------------|---------|
| **Analyzer** | Code Understanding | Parse, analyze structure, map dependencies | Analysis report, file structure |
| **Planner** | Strategy Design | Generate rules, assess risks, plan phases | Modernization plan, transformation rules |
| **Executor** | Code Transformation | Transform code, generate templates, apply rules | Transformed code, backup info |
| **Reviewer** | Quality Assessment | Review code quality, check standards | Quality report, recommendations |
| **Tester** | Test Generation | Generate tests, run coverage analysis | Test suite, coverage metrics |
| **Validator** | Final Validation | Integration tests, compliance check | Validation report, success status |

## Technology Stack

```
┌────────────────────────────────────────┐
│         LangGraph 0.6+                  │  ← Orchestration
├────────────────────────────────────────┤
│         LangChain Core                  │  ← LLM Integration
├────────────────────────────────────────┤
│    OpenAI GPT-4 / Anthropic Claude     │  ← AI Intelligence
├────────────────────────────────────────┤
│              ANTLR4                     │  ← COBOL Parsing
├────────────────────────────────────────┤
│              Jinja2                     │  ← Template Engine
├────────────────────────────────────────┤
│             Python 3.10+                │  ← Runtime
└────────────────────────────────────────┘
```

