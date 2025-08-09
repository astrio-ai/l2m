"""
Pydantic models for the Legacy2Modern API
"""

from typing import Dict, Any, List, Optional, Union
from pydantic import BaseModel, Field
from enum import Enum


class FrameworkType(str, Enum):
    """Supported target frameworks for website modernization."""
    REACT = "react"
    NEXTJS = "nextjs"
    ASTRO = "astro"


class TranspilationRequest(BaseModel):
    """Request model for transpilation operations."""
    source_code: Optional[str] = Field(None, description="Source code content")
    source_file_path: Optional[str] = Field(None, description="Path to source file")
    target_framework: Optional[FrameworkType] = Field(None, description="Target framework for website modernization")
    output_directory: Optional[str] = Field(None, description="Output directory for generated code")
    analyze_only: bool = Field(False, description="Only analyze without generating code")
    llm_config: Optional[Dict[str, Any]] = Field(None, description="LLM configuration for AI-assisted translation")


class WebsiteModernizationRequest(BaseModel):
    """Request model for website modernization."""
    input_path: str = Field(..., description="Path to HTML file or ZIP archive")
    output_dir: str = Field(..., description="Output directory for modern website")
    target_framework: FrameworkType = Field(FrameworkType.REACT, description="Target framework")
    analyze_only: bool = Field(False, description="Only analyze without generating code")


class CobolTranspilationRequest(BaseModel):
    """Request model for COBOL transpilation."""
    source_code: Optional[str] = Field(None, description="COBOL source code content")
    source_file_path: Optional[str] = Field(None, description="Path to COBOL file")
    output_file: Optional[str] = Field(None, description="Output file path for generated Python code")
    llm_config: Optional[Dict[str, Any]] = Field(None, description="LLM configuration for AI-assisted translation")


class AnalysisRequest(BaseModel):
    """Request model for code analysis."""
    source_code: str = Field(..., description="Source code to analyze")
    target_code: Optional[str] = Field(None, description="Target code for comparison analysis")


class TranspilationResponse(BaseModel):
    """Response model for transpilation operations."""
    success: bool = Field(..., description="Whether the operation was successful")
    message: str = Field(..., description="Human-readable message")
    generated_code: Optional[str] = Field(None, description="Generated target code")
    analysis: Optional[Dict[str, Any]] = Field(None, description="Analysis results")
    errors: Optional[List[str]] = Field(None, description="List of errors if any")
    warnings: Optional[List[str]] = Field(None, description="List of warnings if any")
    metadata: Optional[Dict[str, Any]] = Field(None, description="Additional metadata")


class WebsiteAnalysisResponse(BaseModel):
    """Response model for website analysis."""
    success: bool = Field(..., description="Whether the analysis was successful")
    message: str = Field(..., description="Human-readable message")
    analysis: Optional[Dict[str, Any]] = Field(None, description="Website analysis results")
    parsed_data: Optional[Dict[str, Any]] = Field(None, description="Parsed website data")
    errors: Optional[List[str]] = Field(None, description="List of errors if any")
    metadata: Optional[Dict[str, Any]] = Field(None, description="Additional metadata")


class HealthResponse(BaseModel):
    """Response model for health check."""
    status: str = Field(..., description="Service status")
    version: str = Field(..., description="API version")
    timestamp: str = Field(..., description="Current timestamp")
    components: Dict[str, str] = Field(..., description="Status of individual components")


class ErrorResponse(BaseModel):
    """Response model for errors."""
    error: str = Field(..., description="Error message")
    error_type: str = Field(..., description="Type of error")
    details: Optional[Dict[str, Any]] = Field(None, description="Additional error details") 