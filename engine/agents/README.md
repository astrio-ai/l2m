# Agents Directory Organization

This directory contains the AutoGen-based modernization system for legacy2modern. The code has been organized into logical subdirectories for better maintainability and clarity.

## Directory Structure

### `core_agents/`
Contains the main agent implementations and base classes:

- **`base_agent.py`** - Abstract base class for all agents
- **`base_execution_env.py`** - Base execution environment
- **`base_memory.py`** - Base memory management
- **`parser_agent.py`** - Agent for parsing legacy code
- **`modernizer_agent.py`** - Agent for modernizing code
- **`refactor_agent.py`** - Agent for refactoring code
- **`qa_agent.py`** - Agent for quality assurance
- **`coordinator_agent.py`** - Agent for coordinating other agents

### `utilities/`
Contains supporting utilities and helper modules:

- **`ai.py`** - AI integration utilities
- **`chat_to_files.py`** - Chat to file conversion utilities
- **`check_env.py`** - Environment checking utilities
- **`diff.py`** - Diff processing utilities
- **`files_dict.py`** - File dictionary management
- **`git.py`** - Git integration utilities
- **`linting.py`** - Linting utilities
- **`preprompts_holder.py`** - Preprompt management
- **`project_config.py`** - Project configuration management
- **`prompt.py`** - Prompt building utilities
- **`token_usage.py`** - Token usage tracking
- **`version_manager.py`** - Version management utilities

### `autogen_integration/`
Contains AutoGen framework integration:

- **`autogen_wrapper.py`** - Wrapper for AutoGen integration
- **`group_chat_coordinator.py`** - Group chat coordination
- **`debug_autogen.py`** - AutoGen debugging utilities
- **`demo_group_chat.py`** - Demo group chat functionality
- **`migration_example.py`** - Migration example implementation

### `preprompts/`
Contains preprompt templates and configurations.

### `templates/`
Contains template files for various frameworks.

## Usage

The main entry point is through the `__init__.py` file, which exports all the necessary classes and functions:

```python
from engine.agents import (
    # Core agents
    BaseAgent, ParserAgent, ModernizerAgent, RefactorAgent, QAAgent, CoordinatorAgent,
    
    # Utilities
    AI, ProjectConfig, PromptBuilder, TokenUsageTracker,
    
    # AutoGen integration
    AutoGenAgentWrapper, GroupChatCoordinator
)
```

## Benefits of This Organization

1. **Clear Separation of Concerns**: Core agents, utilities, and integration code are clearly separated
2. **Easier Maintenance**: Related functionality is grouped together
3. **Better Discoverability**: Developers can quickly find the code they need
4. **Reduced Coupling**: Each subdirectory has a specific purpose
5. **Improved Testing**: Test files are organized at the project root level for better test discovery

## Migration Notes

If you're updating existing code that imports from this directory, you may need to update import statements to reflect the new structure. The main `__init__.py` file maintains backward compatibility by re-exporting all the necessary classes and functions. 