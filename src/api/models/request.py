"""
Request models for API endpoints.

This module contains Pydantic models for API request validation
and data serialization.
"""

from pydantic import BaseModel, Field
from typing import Dict, Any, List, Optional
from datetime import datetime


class BaseRequest(BaseModel):
    """Base request model with common fields."""
    request_id: Optional[str] = None
    timestamp: Optional[datetime] = None
    
    class Config:
        json_encoders = {
            datetime: lambda v: v.isoformat()
        }


class AgentRequest(BaseRequest):
    """Request model for agent operations."""
    agent_type: str = Field(..., description="Type of agent to run")
    parameters: Dict[str, Any] = Field(default_factory=dict, description="Agent parameters")
    options: Optional[Dict[str, Any]] = Field(default=None, description="Additional options")


class WorkflowRequest(BaseRequest):
    """Request model for workflow operations."""
    workflow_type: str = Field(..., description="Type of workflow to run")
    parameters: Dict[str, Any] = Field(default_factory=dict, description="Workflow parameters")
    options: Optional[Dict[str, Any]] = Field(default=None, description="Additional options")


class AnalysisRequest(BaseRequest):
    """Request model for analysis operations."""
    codebase_path: str = Field(..., description="Path to the codebase to analyze")
    analysis_type: str = Field(default="full", description="Type of analysis to perform")
    options: Optional[Dict[str, Any]] = Field(default=None, description="Analysis options")


class ModernizationRequest(BaseRequest):
    """Request model for modernization operations."""
    codebase_path: str = Field(..., description="Path to the codebase to modernize")
    target_language: str = Field(..., description="Target programming language")
    modernization_goals: List[str] = Field(default_factory=list, description="Modernization goals")
    options: Optional[Dict[str, Any]] = Field(default=None, description="Modernization options")


class FileUploadRequest(BaseRequest):
    """Request model for file upload operations."""
    file_path: str = Field(..., description="Path to the file to upload")
    file_type: str = Field(..., description="Type of file being uploaded")
    options: Optional[Dict[str, Any]] = Field(default=None, description="Upload options")
