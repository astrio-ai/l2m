"""
Workflow models for API endpoints.

This module contains Pydantic models for workflow-related
API operations and data validation.
"""

from pydantic import BaseModel, Field
from typing import Dict, Any, List, Optional
from datetime import datetime
from enum import Enum


class WorkflowStatus(str, Enum):
    """Workflow status enumeration."""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


class WorkflowType(str, Enum):
    """Workflow type enumeration."""
    MAIN = "main"
    LEGACY_ANALYSIS = "legacy_analysis"
    MODERNIZATION = "modernization"
    VALIDATION = "validation"


class WorkflowStep(BaseModel):
    """Model for a workflow step."""
    step_id: str = Field(..., description="ID of the workflow step")
    step_name: str = Field(..., description="Name of the workflow step")
    step_type: str = Field(..., description="Type of the workflow step")
    status: WorkflowStatus = Field(..., description="Status of the workflow step")
    start_time: Optional[datetime] = Field(default=None, description="Start time of the step")
    end_time: Optional[datetime] = Field(default=None, description="End time of the step")
    duration: Optional[float] = Field(default=None, description="Duration of the step in seconds")
    result: Optional[Dict[str, Any]] = Field(default=None, description="Result of the step")
    error: Optional[str] = Field(default=None, description="Error message if any")


class WorkflowExecution(BaseModel):
    """Model for a workflow execution."""
    execution_id: str = Field(..., description="ID of the workflow execution")
    workflow_type: WorkflowType = Field(..., description="Type of workflow")
    status: WorkflowStatus = Field(..., description="Status of the workflow execution")
    start_time: Optional[datetime] = Field(default=None, description="Start time of the execution")
    end_time: Optional[datetime] = Field(default=None, description="End time of the execution")
    duration: Optional[float] = Field(default=None, description="Duration of the execution in seconds")
    steps: List[WorkflowStep] = Field(default_factory=list, description="Workflow steps")
    result: Optional[Dict[str, Any]] = Field(default=None, description="Overall result of the execution")
    error: Optional[str] = Field(default=None, description="Error message if any")


class WorkflowRequest(BaseModel):
    """Request model for workflow operations."""
    workflow_type: WorkflowType = Field(..., description="Type of workflow to run")
    parameters: Dict[str, Any] = Field(default_factory=dict, description="Workflow parameters")
    options: Optional[Dict[str, Any]] = Field(default=None, description="Additional options")


class WorkflowResponse(BaseModel):
    """Response model for workflow operations."""
    execution_id: str = Field(..., description="ID of the workflow execution")
    workflow_type: WorkflowType = Field(..., description="Type of workflow")
    status: WorkflowStatus = Field(..., description="Status of the workflow execution")
    result: Optional[Dict[str, Any]] = Field(default=None, description="Workflow execution result")
    error: Optional[str] = Field(default=None, description="Error message if any")
