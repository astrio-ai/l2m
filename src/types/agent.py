"""
Agent type definitions.

This module contains type definitions for agents
in the multi-agent system.
"""

from typing import Dict, Any, List, Optional, Callable, Awaitable
from enum import Enum
from dataclasses import dataclass
from abc import ABC, abstractmethod


class AgentType(str, Enum):
    """Agent type enumeration."""
    ANALYZER = "analyzer"
    PLANNER = "planner"
    EXECUTOR = "executor"
    REVIEWER = "reviewer"
    TESTER = "tester"
    VALIDATOR = "validator"


class AgentStatus(str, Enum):
    """Agent status enumeration."""
    IDLE = "idle"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


@dataclass
class AgentCapabilities:
    """Agent capabilities definition."""
    can_analyze: bool = False
    can_plan: bool = False
    can_execute: bool = False
    can_review: bool = False
    can_test: bool = False
    can_validate: bool = False
    can_transform: bool = False
    can_optimize: bool = False


@dataclass
class AgentMetadata:
    """Agent metadata definition."""
    name: str
    description: str
    version: str
    author: str
    capabilities: AgentCapabilities
    tools: List[str]
    options: Dict[str, Any]


class AgentInterface(ABC):
    """Interface for all agents."""
    
    @abstractmethod
    async def run(self, parameters: Dict[str, Any]) -> Dict[str, Any]:
        """Run the agent with the given parameters."""
        pass
    
    @abstractmethod
    def get_metadata(self) -> AgentMetadata:
        """Get agent metadata."""
        pass
    
    @abstractmethod
    def get_capabilities(self) -> AgentCapabilities:
        """Get agent capabilities."""
        pass
    
    @abstractmethod
    def get_tools(self) -> List[str]:
        """Get available tools."""
        pass


class AgentResult:
    """Agent execution result."""
    
    def __init__(
        self,
        success: bool,
        data: Optional[Dict[str, Any]] = None,
        error: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None
    ):
        """Initialize the agent result."""
        self.success = success
        self.data = data or {}
        self.error = error
        self.metadata = metadata or {}
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert result to dictionary."""
        return {
            "success": self.success,
            "data": self.data,
            "error": self.error,
            "metadata": self.metadata
        }


class AgentContext:
    """Context for agent execution."""
    
    def __init__(
        self,
        agent_id: str,
        execution_id: str,
        parameters: Dict[str, Any],
        options: Optional[Dict[str, Any]] = None
    ):
        """Initialize the agent context."""
        self.agent_id = agent_id
        self.execution_id = execution_id
        self.parameters = parameters
        self.options = options or {}
        self.start_time = None
        self.end_time = None
        self.duration = None
    
    def start(self):
        """Start the agent execution."""
        from datetime import datetime
        self.start_time = datetime.now()
    
    def end(self):
        """End the agent execution."""
        from datetime import datetime
        self.end_time = datetime.now()
        if self.start_time:
            self.duration = (self.end_time - self.start_time).total_seconds()
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert context to dictionary."""
        return {
            "agent_id": self.agent_id,
            "execution_id": self.execution_id,
            "parameters": self.parameters,
            "options": self.options,
            "start_time": self.start_time.isoformat() if self.start_time else None,
            "end_time": self.end_time.isoformat() if self.end_time else None,
            "duration": self.duration
        }
