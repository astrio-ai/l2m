"""
AutoGen-based modernization system for legacy2modern.

This module provides a multi-agent system for modernizing legacy codebases
using AutoGen framework for AI agent coordination and communication.
"""

from .ai import AI
from .base_agent import BaseAgent
from .base_execution_env import BaseExecutionEnv
from .base_memory import BaseMemory
from .chat_to_files import ChatToFiles
from .diff import DiffProcessor
from .files_dict import FilesDict
from .git import GitManager
from .linting import LintingManager
from .preprompts_holder import PrepromptsHolder
from .project_config import ProjectConfig
from .prompt import PromptBuilder
from .token_usage import TokenUsageTracker
from .version_manager import VersionManager

from .parser_agent import ParserAgent
from .modernizer_agent import ModernizerAgent
from .refactor_agent import RefactorAgent
from .qa_agent import QAAgent
from .coordinator_agent import CoordinatorAgent

__all__ = [
    'AI',
    'BaseAgent',
    'BaseExecutionEnv', 
    'BaseMemory',
    'ChatToFiles',
    'DiffProcessor',
    'FilesDict',
    'GitManager',
    'LintingManager',
    'PrepromptsHolder',
    'ProjectConfig',
    'PromptBuilder',
    'TokenUsageTracker',
    'VersionManager',
    'ParserAgent',
    'ModernizerAgent',
    'RefactorAgent',
    'QAAgent',
    'CoordinatorAgent'
] 