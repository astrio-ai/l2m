"""
Tool type definitions.

This module contains type definitions for agent tools
and their capabilities.
"""

from typing import Dict, Any, List, Optional, Callable, Awaitable
from enum import Enum
from dataclasses import dataclass
from abc import ABC, abstractmethod


class ToolType(str, Enum):
    """Tool type enumeration."""
    FILE_OPERATION = "file_operation"
    CODE_ANALYSIS = "code_analysis"
    CODE_TRANSFORMATION = "code_transformation"
    TEST_GENERATION = "test_generation"
    VALIDATION = "validation"
    SEARCH = "search"
    DOCUMENTATION = "documentation"


class ToolStatus(str, Enum):
    """Tool status enumeration."""
    IDLE = "idle"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


@dataclass
class ToolCapabilities:
    """Tool capabilities definition."""
    can_read_files: bool = False
    can_write_files: bool = False
    can_analyze_code: bool = False
    can_transform_code: bool = False
    can_generate_tests: bool = False
    can_validate_code: bool = False
    can_search_code: bool = False
    can_generate_docs: bool = False


@dataclass
class ToolMetadata:
    """Tool metadata definition."""
    name: str
    description: str
    version: str
    author: str
    tool_type: ToolType
    capabilities: ToolCapabilities
    parameters: List[Dict[str, Any]]
    options: Dict[str, Any]


class ToolInterface(ABC):
    """Interface for all tools."""
    
    @abstractmethod
    async def run(self, **kwargs) -> Any:
        """Run the tool with the given parameters."""
        pass
    
    @abstractmethod
    def get_metadata(self) -> ToolMetadata:
        """Get tool metadata."""
        pass
    
    @abstractmethod
    def get_capabilities(self) -> ToolCapabilities:
        """Get tool capabilities."""
        pass
    
    @abstractmethod
    def get_parameters(self) -> List[Dict[str, Any]]:
        """Get tool parameters."""
        pass
    
    @abstractmethod
    def validate_parameters(self, **kwargs) -> bool:
        """Validate tool parameters."""
        pass


class ToolResult:
    """Tool execution result."""
    
    def __init__(
        self,
        success: bool,
        data: Optional[Any] = None,
        error: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None
    ):
        """Initialize the tool result."""
        self.success = success
        self.data = data
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


class ToolContext:
    """Context for tool execution."""
    
    def __init__(
        self,
        tool_id: str,
        execution_id: str,
        parameters: Dict[str, Any],
        options: Optional[Dict[str, Any]] = None
    ):
        """Initialize the tool context."""
        self.tool_id = tool_id
        self.execution_id = execution_id
        self.parameters = parameters
        self.options = options or {}
        self.start_time = None
        self.end_time = None
        self.duration = None
    
    def start(self):
        """Start the tool execution."""
        from datetime import datetime
        self.start_time = datetime.now()
    
    def end(self):
        """End the tool execution."""
        from datetime import datetime
        self.end_time = datetime.now()
        if self.start_time:
            self.duration = (self.end_time - self.start_time).total_seconds()
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert context to dictionary."""
        return {
            "tool_id": self.tool_id,
            "execution_id": self.execution_id,
            "parameters": self.parameters,
            "options": self.options,
            "start_time": self.start_time.isoformat() if self.start_time else None,
            "end_time": self.end_time.isoformat() if self.end_time else None,
            "duration": self.duration
        }


class ToolParameter:
    """Tool parameter definition."""
    
    def __init__(
        self,
        name: str,
        parameter_type: type,
        description: str,
        required: bool = True,
        default: Any = None,
        choices: Optional[List[Any]] = None,
        validation: Optional[Callable] = None
    ):
        """Initialize the tool parameter."""
        self.name = name
        self.parameter_type = parameter_type
        self.description = description
        self.required = required
        self.default = default
        self.choices = choices
        self.validation = validation
    
    def validate(self, value: Any) -> bool:
        """Validate the parameter value."""
        if self.required and value is None:
            return False
        
        if value is not None and not isinstance(value, self.parameter_type):
            return False
        
        if self.choices and value not in self.choices:
            return False
        
        if self.validation and not self.validation(value):
            return False
        
        return True
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert parameter to dictionary."""
        return {
            "name": self.name,
            "type": self.parameter_type.__name__,
            "description": self.description,
            "required": self.required,
            "default": self.default,
            "choices": self.choices
        }
