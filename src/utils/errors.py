"""
Error handling utilities for the multi-agent system.

This module provides custom exception classes and error handling
utilities for the modernization system.
"""

from typing import Optional, Dict, Any
from enum import Enum


class ErrorCode(str, Enum):
    """Error code enumeration."""
    UNKNOWN_ERROR = "UNKNOWN_ERROR"
    VALIDATION_ERROR = "VALIDATION_ERROR"
    CONFIGURATION_ERROR = "CONFIGURATION_ERROR"
    AGENT_ERROR = "AGENT_ERROR"
    WORKFLOW_ERROR = "WORKFLOW_ERROR"
    PARSER_ERROR = "PARSER_ERROR"
    MODERNIZER_ERROR = "MODERNIZER_ERROR"
    TRANSFORMER_ERROR = "TRANSFORMER_ERROR"
    API_ERROR = "API_ERROR"
    CLI_ERROR = "CLI_ERROR"


class Legacy2ModernError(Exception):
    """Base exception class for all Legacy2Modern errors."""
    
    def __init__(
        self,
        message: str,
        error_code: ErrorCode = ErrorCode.UNKNOWN_ERROR,
        details: Optional[Dict[str, Any]] = None
    ):
        """Initialize the error."""
        super().__init__(message)
        self.message = message
        self.error_code = error_code
        self.details = details or {}
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert error to dictionary."""
        return {
            "error_code": self.error_code.value,
            "message": self.message,
            "details": self.details
        }


class ValidationError(Legacy2ModernError):
    """Exception for validation errors."""
    
    def __init__(self, message: str, details: Optional[Dict[str, Any]] = None):
        super().__init__(message, ErrorCode.VALIDATION_ERROR, details)


class ConfigurationError(Legacy2ModernError):
    """Exception for configuration errors."""
    
    def __init__(self, message: str, details: Optional[Dict[str, Any]] = None):
        super().__init__(message, ErrorCode.CONFIGURATION_ERROR, details)


class AgentError(Legacy2ModernError):
    """Exception for agent errors."""
    
    def __init__(self, message: str, agent_name: str, details: Optional[Dict[str, Any]] = None):
        super().__init__(message, ErrorCode.AGENT_ERROR, details)
        self.agent_name = agent_name


class WorkflowError(Legacy2ModernError):
    """Exception for workflow errors."""
    
    def __init__(self, message: str, workflow_name: str, details: Optional[Dict[str, Any]] = None):
        super().__init__(message, ErrorCode.WORKFLOW_ERROR, details)
        self.workflow_name = workflow_name


class ParserError(Legacy2ModernError):
    """Exception for parser errors."""
    
    def __init__(self, message: str, language: str, details: Optional[Dict[str, Any]] = None):
        super().__init__(message, ErrorCode.PARSER_ERROR, details)
        self.language = language


class ModernizerError(Legacy2ModernError):
    """Exception for modernizer errors."""
    
    def __init__(self, message: str, source_language: str, target_language: str, details: Optional[Dict[str, Any]] = None):
        super().__init__(message, ErrorCode.MODERNIZER_ERROR, details)
        self.source_language = source_language
        self.target_language = target_language


class TransformerError(Legacy2ModernError):
    """Exception for transformer errors."""
    
    def __init__(self, message: str, transformation_type: str, details: Optional[Dict[str, Any]] = None):
        super().__init__(message, ErrorCode.TRANSFORMER_ERROR, details)
        self.transformation_type = transformation_type


class APIError(Legacy2ModernError):
    """Exception for API errors."""
    
    def __init__(self, message: str, status_code: int, details: Optional[Dict[str, Any]] = None):
        super().__init__(message, ErrorCode.API_ERROR, details)
        self.status_code = status_code


class CLIError(Legacy2ModernError):
    """Exception for CLI errors."""
    
    def __init__(self, message: str, command: str, details: Optional[Dict[str, Any]] = None):
        super().__init__(message, ErrorCode.CLI_ERROR, details)
        self.command = command


def handle_error(error: Exception) -> Dict[str, Any]:
    """Handle an error and return error information."""
    if isinstance(error, Legacy2ModernError):
        return error.to_dict()
    else:
        return {
            "error_code": ErrorCode.UNKNOWN_ERROR.value,
            "message": str(error),
            "details": {"exception_type": type(error).__name__}
        }
