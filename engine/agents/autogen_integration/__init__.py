"""
AutoGen integration modules for the modernization system.

This package contains classes and utilities for integrating with the AutoGen
framework, including wrappers, group chat coordination, debugging tools,
and sandbox execution capabilities.
"""

from .autogen_wrapper import AutoGenAgentWrapper
from .group_chat_coordinator import GroupChatCoordinator
from .sandbox_executor import (
    SandboxConfig, SandboxExecutor, SandboxAgent,
    create_sandbox_executor, create_sandbox_agent, execute_in_sandbox
)
# debug_autogen.py is a standalone script
# from .demo_group_chat import demo_group_chat
# from .migration_example import run_migration_example

__all__ = [
    'AutoGenAgentWrapper',
    'GroupChatCoordinator',
    'SandboxConfig',
    'SandboxExecutor', 
    'SandboxAgent',
    'create_sandbox_executor',
    'create_sandbox_agent',
    'execute_in_sandbox',
    # 'debug_autogen_setup',  # debug_autogen.py is a standalone script
    # 'demo_group_chat',
    # 'run_migration_example'
] 