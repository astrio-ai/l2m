"""
Utility modules for the AutoGen modernization system.

This package contains supporting utilities for AI integration, file handling,
project configuration, prompt management, and other helper functionality.
"""

from .ai import AI
from .chat_to_files import ChatToFiles
# check_env.py is a standalone script, not a module to import
from .diff import DiffProcessor
from .files_dict import FilesDict
from .git import GitManager
from .linting import LintingManager
from .preprompts_holder import PrepromptsHolder
from .project_config import ProjectConfig
from .prompt import PromptBuilder
from .token_usage import TokenUsageTracker
from .version_manager import VersionManager

__all__ = [
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
    'VersionManager'
] 