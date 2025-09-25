"""
Workflow type definitions.

This module contains type definitions for workflows
and their execution.
"""

from typing import Dict, Any, List, Optional, Union
from enum import Enum
from dataclasses import dataclass
from abc import ABC, abstractmethod


class WorkflowType(str, Enum):
    """Workflow type enumeration."""
    MAIN = "main"
    LEGACY_ANALYSIS = "legacy_analysis"
    MODERNIZATION = "modernization"
    VALIDATION = "validation"


class WorkflowStatus(str, Enum):
    """Workflow status enumeration."""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


class WorkflowStepType(str, Enum):
    """Workflow step type enumeration."""
    AGENT = "agent"
    TOOL = "tool"
    CONDITION = "condition"
    MERGE = "merge"
    SPLIT = "split"


@dataclass
class WorkflowStep:
    """Workflow step definition."""
    step_id: str
    step_type: WorkflowStepType
    name: str
    description: str
    agent_id: Optional[str] = None
    tool_id: Optional[str] = None
    condition: Optional[str] = None
    options: Optional[Dict[str, Any]] = None


@dataclass
class WorkflowStepResult:
    """Workflow step result definition."""
    step_id: str
    success: bool
    data: Optional[Dict[str, Any]] = None
    error: Optional[str] = None
    duration: Optional[float] = None
    metadata: Optional[Dict[str, Any]] = None


@dataclass
class WorkflowExecution:
    """Workflow execution definition."""
    execution_id: str
    workflow_type: WorkflowType
    status: WorkflowStatus
    steps: List[WorkflowStep]
    step_results: List[WorkflowStepResult]
    start_time: Optional[str] = None
    end_time: Optional[str] = None
    duration: Optional[float] = None
    error: Optional[str] = None
    metadata: Optional[Dict[str, Any]] = None
    
    def __post_init__(self):
        """Initialize default values."""
        if self.step_results is None:
            self.step_results = []
        if self.metadata is None:
            self.metadata = {}


class WorkflowInterface(ABC):
    """Interface for all workflow implementations."""
    
    @abstractmethod
    async def run(self, parameters: Dict[str, Any]) -> Dict[str, Any]:
        """Run the workflow with the given parameters."""
        pass
    
    @abstractmethod
    def get_steps(self) -> List[WorkflowStep]:
        """Get all steps in the workflow."""
        pass
    
    @abstractmethod
    def get_entry_step(self) -> str:
        """Get the entry step."""
        pass
    
    @abstractmethod
    def get_exit_steps(self) -> List[str]:
        """Get all exit steps."""
        pass
    
    @abstractmethod
    def get_next_steps(self, current_step: str) -> List[str]:
        """Get the next steps from the current step."""
        pass


class WorkflowManager:
    """Workflow execution manager."""
    
    def __init__(self, workflow_id: str, execution_id: str):
        """Initialize the workflow manager."""
        self.workflow_id = workflow_id
        self.execution_id = execution_id
        self.status = WorkflowStatus.PENDING
        self.current_step = None
        self.completed_steps = []
        self.step_results = []
        self.execution_data = {}
        self.start_time = None
        self.end_time = None
        self.duration = None
        self.error = None
    
    def start(self):
        """Start the workflow execution."""
        from datetime import datetime
        self.start_time = datetime.now()
        self.status = WorkflowStatus.RUNNING
    
    def end(self, success: bool = True, error: Optional[str] = None):
        """End the workflow execution."""
        from datetime import datetime
        self.end_time = datetime.now()
        if self.start_time:
            self.duration = (self.end_time - self.start_time).total_seconds()
        
        if success:
            self.status = WorkflowStatus.COMPLETED
        else:
            self.status = WorkflowStatus.FAILED
            self.error = error
    
    def set_current_step(self, step_id: str):
        """Set the current step."""
        self.current_step = step_id
    
    def complete_step(self, step_id: str, result: WorkflowStepResult):
        """Mark a step as completed."""
        if step_id not in self.completed_steps:
            self.completed_steps.append(step_id)
        
        self.step_results.append(result)
    
    def add_execution_data(self, key: str, value: Any):
        """Add data to the execution."""
        self.execution_data[key] = value
    
    def get_execution_data(self, key: str, default: Any = None) -> Any:
        """Get data from the execution."""
        return self.execution_data.get(key, default)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert execution to dictionary."""
        return {
            "workflow_id": self.workflow_id,
            "execution_id": self.execution_id,
            "status": self.status.value,
            "current_step": self.current_step,
            "completed_steps": self.completed_steps,
            "step_results": [result.__dict__ for result in self.step_results],
            "execution_data": self.execution_data,
            "start_time": self.start_time.isoformat() if self.start_time else None,
            "end_time": self.end_time.isoformat() if self.end_time else None,
            "duration": self.duration,
            "error": self.error
        }
