"""
AutoGen-based modernization system for legacy2modern.

This module provides a multi-agent system for modernizing legacy codebases
using AutoGen framework for AI agent coordination and communication.
"""

# Core agents
from .core_agents import (
    BaseAgent, BaseExecutionEnv, BaseMemory,
    ParserAgent, ModernizerAgent, RefactorAgent, QAAgent, CoordinatorAgent
)

# Utilities
from .utilities import (
    AI, ChatToFiles, DiffProcessor, FilesDict,
    GitManager, LintingManager, PrepromptsHolder, ProjectConfig,
    PromptBuilder, TokenUsageTracker, VersionManager
)

# AutoGen integration
from .autogen_integration import (
    AutoGenAgentWrapper, GroupChatCoordinator
)

__all__ = [
    # Core agents
    'BaseAgent',
    'BaseExecutionEnv', 
    'BaseMemory',
    'ParserAgent',
    'ModernizerAgent',
    'RefactorAgent',
    'QAAgent',
    'CoordinatorAgent',
    
    # Utilities
    'AI',
    'ChatToFiles',
    # 'check_environment',  # check_env.py is a standalone script
    'DiffProcessor',
    'FilesDict',
    'GitManager',
    'LintingManager',
    'PrepromptsHolder',
    'ProjectConfig',
    'PromptBuilder',
    'TokenUsageTracker',
    'VersionManager',
    
    # AutoGen integration
    'AutoGenAgentWrapper',
    'GroupChatCoordinator',
    # 'debug_autogen_setup',  # debug_autogen.py is a standalone script
    # 'demo_group_chat',
    # 'run_migration_example'
] 