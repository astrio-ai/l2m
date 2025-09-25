"""
Input validation utilities for the multi-agent system.

This module provides validation functions for input data,
configuration, and API requests.
"""

from typing import Any, Dict, List, Optional, Union
from pathlib import Path
import re

from src.utils.errors import ValidationError
from src.utils.logger import get_logger

logger = get_logger(__name__)


def validate_file_path(file_path: str) -> bool:
    """Validate that a file path exists and is accessible."""
    try:
        path = Path(file_path)
        return path.exists() and path.is_file()
    except Exception:
        return False


def validate_directory_path(directory_path: str) -> bool:
    """Validate that a directory path exists and is accessible."""
    try:
        path = Path(directory_path)
        return path.exists() and path.is_dir()
    except Exception:
        return False


def validate_language(language: str, supported_languages: List[str]) -> bool:
    """Validate that a language is supported."""
    return language.lower() in [lang.lower() for lang in supported_languages]


def validate_file_extension(file_path: str, supported_extensions: List[str]) -> bool:
    """Validate that a file has a supported extension."""
    try:
        path = Path(file_path)
        return path.suffix.lower() in [ext.lower() for ext in supported_extensions]
    except Exception:
        return False


def validate_workflow_type(workflow_type: str, supported_types: List[str]) -> bool:
    """Validate that a workflow type is supported."""
    return workflow_type.lower() in [wt.lower() for wt in supported_types]


def validate_agent_type(agent_type: str, supported_types: List[str]) -> bool:
    """Validate that an agent type is supported."""
    return agent_type.lower() in [at.lower() for at in supported_types]


def validate_api_key(api_key: str) -> bool:
    """Validate that an API key is properly formatted."""
    if not api_key:
        return False
    
    # Basic validation - should be non-empty and contain valid characters
    return len(api_key) >= 8 and re.match(r'^[a-zA-Z0-9_-]+$', api_key)


def validate_email(email: str) -> bool:
    """Validate that an email address is properly formatted."""
    if not email:
        return False
    
    pattern = r'^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$'
    return re.match(pattern, email) is not None


def validate_url(url: str) -> bool:
    """Validate that a URL is properly formatted."""
    if not url:
        return False
    
    pattern = r'^https?://[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}(/.*)?$'
    return re.match(pattern, url) is not None


def validate_positive_integer(value: Any) -> bool:
    """Validate that a value is a positive integer."""
    try:
        int_value = int(value)
        return int_value > 0
    except (ValueError, TypeError):
        return False


def validate_positive_float(value: Any) -> bool:
    """Validate that a value is a positive float."""
    try:
        float_value = float(value)
        return float_value > 0
    except (ValueError, TypeError):
        return False


def validate_range(value: Any, min_value: float, max_value: float) -> bool:
    """Validate that a value is within a specified range."""
    try:
        float_value = float(value)
        return min_value <= float_value <= max_value
    except (ValueError, TypeError):
        return False


def validate_required_fields(data: Dict[str, Any], required_fields: List[str]) -> bool:
    """Validate that all required fields are present in data."""
    for field in required_fields:
        if field not in data or data[field] is None:
            return False
    return True


def validate_field_types(data: Dict[str, Any], field_types: Dict[str, type]) -> bool:
    """Validate that fields have the correct types."""
    for field, expected_type in field_types.items():
        if field in data and not isinstance(data[field], expected_type):
            return False
    return True


def validate_choice(value: Any, choices: List[Any]) -> bool:
    """Validate that a value is one of the allowed choices."""
    return value in choices


def validate_string_length(value: str, min_length: int = 0, max_length: int = None) -> bool:
    """Validate that a string has the correct length."""
    if not isinstance(value, str):
        return False
    
    if len(value) < min_length:
        return False
    
    if max_length is not None and len(value) > max_length:
        return False
    
    return True


def validate_list_length(value: List[Any], min_length: int = 0, max_length: int = None) -> bool:
    """Validate that a list has the correct length."""
    if not isinstance(value, list):
        return False
    
    if len(value) < min_length:
        return False
    
    if max_length is not None and len(value) > max_length:
        return False
    
    return True


def validate_dict_structure(data: Dict[str, Any], structure: Dict[str, Any]) -> bool:
    """Validate that a dictionary has the correct structure."""
    for key, expected_type in structure.items():
        if key not in data:
            return False
        
        if not isinstance(data[key], expected_type):
            return False
    
    return True


def validate_configuration(config: Dict[str, Any], required_config: Dict[str, Any]) -> bool:
    """Validate that a configuration has all required fields."""
    for key, expected_type in required_config.items():
        if key not in config:
            return False
        
        if not isinstance(config[key], expected_type):
            return False
    
    return True


def validate_modernization_goals(goals: List[str], supported_goals: List[str]) -> bool:
    """Validate that modernization goals are supported."""
    if not isinstance(goals, list):
        return False
    
    for goal in goals:
        if goal not in supported_goals:
            return False
    
    return True


def validate_workflow_parameters(parameters: Dict[str, Any], required_parameters: List[str]) -> bool:
    """Validate that workflow parameters contain all required fields."""
    for param in required_parameters:
        if param not in parameters:
            return False
    
    return True


def validate_agent_parameters(parameters: Dict[str, Any], required_parameters: List[str]) -> bool:
    """Validate that agent parameters contain all required fields."""
    for param in required_parameters:
        if param not in parameters:
            return False
    
    return True
