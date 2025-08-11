"""
Core agent implementations for the AutoGen modernization system.

This package contains the main agent classes that implement specific
functionality for parsing, modernizing, refactoring, QA, and coordination.
"""

from .base_agent import BaseAgent, AgentRole, AgentState
from .base_execution_env import BaseExecutionEnv
from .base_memory import BaseMemory
from .parser_agent import ParserAgent
from .modernizer_agent import ModernizerAgent
from .refactor_agent import RefactorAgent
from .qa_agent import QAAgent
from .coordinator_agent import CoordinatorAgent

__all__ = [
    'BaseAgent',
    'AgentRole', 
    'AgentState',
    'BaseExecutionEnv',
    'BaseMemory',
    'ParserAgent',
    'ModernizerAgent',
    'RefactorAgent',
    'QAAgent',
    'CoordinatorAgent'
] 