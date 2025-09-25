"""
Response models for API endpoints.

This module contains Pydantic models for API response serialization
and data validation.
"""

from pydantic import BaseModel, Field
from typing import Dict, Any, List, Optional
from datetime import datetime


class BaseResponse(BaseModel):
    """Base response model with common fields."""
    success: bool = Field(..., description="Whether the operation was successful")
    message: Optional[str] = Field(default=None, description="Response message")
    timestamp: Optional[datetime] = Field(default_factory=datetime.now, description="Response timestamp")
    
    class Config:
        json_encoders = {
            datetime: lambda v: v.isoformat()
        }


class AgentResponse(BaseResponse):
    """Response model for agent operations."""
    agent_id: str = Field(..., description="ID of the agent")
    agent_type: str = Field(..., description="Type of agent")
    status: str = Field(..., description="Status of the agent operation")
    result: Optional[Dict[str, Any]] = Field(default=None, description="Agent operation result")
    error: Optional[str] = Field(default=None, description="Error message if any")


class WorkflowResponse(BaseResponse):
    """Response model for workflow operations."""
    workflow_id: str = Field(..., description="ID of the workflow")
    workflow_type: str = Field(..., description="Type of workflow")
    status: str = Field(..., description="Status of the workflow operation")
    result: Optional[Dict[str, Any]] = Field(default=None, description="Workflow operation result")
    error: Optional[str] = Field(default=None, description="Error message if any")


class AnalysisResponse(BaseResponse):
    """Response model for analysis operations."""
    analysis_id: str = Field(..., description="ID of the analysis")
    analysis_type: str = Field(..., description="Type of analysis performed")
    status: str = Field(..., description="Status of the analysis")
    result: Optional[Dict[str, Any]] = Field(default=None, description="Analysis result")
    error: Optional[str] = Field(default=None, description="Error message if any")


class ModernizationResponse(BaseResponse):
    """Response model for modernization operations."""
    modernization_id: str = Field(..., description="ID of the modernization")
    target_language: str = Field(..., description="Target programming language")
    status: str = Field(..., description="Status of the modernization")
    result: Optional[Dict[str, Any]] = Field(default=None, description="Modernization result")
    error: Optional[str] = Field(default=None, description="Error message if any")


class ErrorResponse(BaseResponse):
    """Response model for error conditions."""
    error_code: str = Field(..., description="Error code")
    error_details: Optional[Dict[str, Any]] = Field(default=None, description="Additional error details")
    
    def __init__(self, **data):
        super().__init__(success=False, **data)
