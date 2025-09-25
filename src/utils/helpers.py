"""
General utility functions and helpers.

This module provides general utility functions for common tasks
in the modernization system.
"""

import os
import json
import yaml
from typing import Any, Dict, List, Optional, Union
from pathlib import Path
from datetime import datetime
import hashlib
import uuid

from src.utils.logger import get_logger

logger = get_logger(__name__)


def generate_id() -> str:
    """Generate a unique ID."""
    return str(uuid.uuid4())


def generate_hash(data: str) -> str:
    """Generate a hash for the given data."""
    return hashlib.sha256(data.encode()).hexdigest()


def get_timestamp() -> str:
    """Get current timestamp as ISO string."""
    return datetime.now().isoformat()


def format_duration(seconds: float) -> str:
    """Format duration in seconds to human-readable format."""
    if seconds < 60:
        return f"{seconds:.1f}s"
    elif seconds < 3600:
        minutes = seconds / 60
        return f"{minutes:.1f}m"
    else:
        hours = seconds / 3600
        return f"{hours:.1f}h"


def format_file_size(size_bytes: int) -> str:
    """Format file size in bytes to human-readable format."""
    if size_bytes < 1024:
        return f"{size_bytes}B"
    elif size_bytes < 1024 * 1024:
        return f"{size_bytes / 1024:.1f}KB"
    elif size_bytes < 1024 * 1024 * 1024:
        return f"{size_bytes / (1024 * 1024):.1f}MB"
    else:
        return f"{size_bytes / (1024 * 1024 * 1024):.1f}GB"


def safe_json_loads(data: str, default: Any = None) -> Any:
    """Safely load JSON data with error handling."""
    try:
        return json.loads(data)
    except (json.JSONDecodeError, TypeError):
        return default


def safe_json_dumps(data: Any, default: str = "{}") -> str:
    """Safely dump data to JSON with error handling."""
    try:
        return json.dumps(data, indent=2, default=str)
    except (TypeError, ValueError):
        return default


def safe_yaml_load(data: str, default: Any = None) -> Any:
    """Safely load YAML data with error handling."""
    try:
        return yaml.safe_load(data)
    except (yaml.YAMLError, TypeError):
        return default


def safe_yaml_dump(data: Any, default: str = "") -> str:
    """Safely dump data to YAML with error handling."""
    try:
        return yaml.dump(data, default_flow_style=False)
    except (TypeError, ValueError):
        return default


def ensure_directory(directory_path: str) -> bool:
    """Ensure that a directory exists."""
    try:
        Path(directory_path).mkdir(parents=True, exist_ok=True)
        return True
    except Exception as e:
        logger.error(f"Error creating directory {directory_path}: {e}")
        return False


def ensure_file(file_path: str, content: str = "") -> bool:
    """Ensure that a file exists with the given content."""
    try:
        path = Path(file_path)
        path.parent.mkdir(parents=True, exist_ok=True)
        
        if not path.exists():
            path.write_text(content)
        
        return True
    except Exception as e:
        logger.error(f"Error creating file {file_path}: {e}")
        return False


def read_file_safely(file_path: str, encoding: str = "utf-8") -> Optional[str]:
    """Safely read a file with error handling."""
    try:
        with open(file_path, 'r', encoding=encoding) as f:
            return f.read()
    except Exception as e:
        logger.error(f"Error reading file {file_path}: {e}")
        return None


def write_file_safely(file_path: str, content: str, encoding: str = "utf-8") -> bool:
    """Safely write content to a file with error handling."""
    try:
        path = Path(file_path)
        path.parent.mkdir(parents=True, exist_ok=True)
        
        with open(file_path, 'w', encoding=encoding) as f:
            f.write(content)
        
        return True
    except Exception as e:
        logger.error(f"Error writing file {file_path}: {e}")
        return False


def get_file_info(file_path: str) -> Optional[Dict[str, Any]]:
    """Get information about a file."""
    try:
        path = Path(file_path)
        if not path.exists():
            return None
        
        stat = path.stat()
        return {
            "path": str(path),
            "name": path.name,
            "size": stat.st_size,
            "modified": datetime.fromtimestamp(stat.st_mtime).isoformat(),
            "is_file": path.is_file(),
            "is_directory": path.is_directory()
        }
    except Exception as e:
        logger.error(f"Error getting file info for {file_path}: {e}")
        return None


def list_files(directory_path: str, pattern: str = "*", recursive: bool = False) -> List[str]:
    """List files in a directory matching a pattern."""
    try:
        path = Path(directory_path)
        if not path.exists():
            return []
        
        if recursive:
            files = list(path.rglob(pattern))
        else:
            files = list(path.glob(pattern))
        
        return [str(f) for f in files if f.is_file()]
    except Exception as e:
        logger.error(f"Error listing files in {directory_path}: {e}")
        return []


def copy_file_safely(source_path: str, dest_path: str) -> bool:
    """Safely copy a file with error handling."""
    try:
        import shutil
        shutil.copy2(source_path, dest_path)
        return True
    except Exception as e:
        logger.error(f"Error copying file from {source_path} to {dest_path}: {e}")
        return False


def move_file_safely(source_path: str, dest_path: str) -> bool:
    """Safely move a file with error handling."""
    try:
        import shutil
        shutil.move(source_path, dest_path)
        return True
    except Exception as e:
        logger.error(f"Error moving file from {source_path} to {dest_path}: {e}")
        return False


def delete_file_safely(file_path: str) -> bool:
    """Safely delete a file with error handling."""
    try:
        Path(file_path).unlink()
        return True
    except Exception as e:
        logger.error(f"Error deleting file {file_path}: {e}")
        return False


def get_environment_variable(name: str, default: str = None) -> Optional[str]:
    """Get an environment variable with error handling."""
    try:
        return os.getenv(name, default)
    except Exception as e:
        logger.error(f"Error getting environment variable {name}: {e}")
        return default


def set_environment_variable(name: str, value: str) -> bool:
    """Set an environment variable with error handling."""
    try:
        os.environ[name] = value
        return True
    except Exception as e:
        logger.error(f"Error setting environment variable {name}: {e}")
        return False


def merge_dictionaries(dict1: Dict[str, Any], dict2: Dict[str, Any]) -> Dict[str, Any]:
    """Merge two dictionaries, with dict2 taking precedence."""
    result = dict1.copy()
    result.update(dict2)
    return result


def deep_merge_dictionaries(dict1: Dict[str, Any], dict2: Dict[str, Any]) -> Dict[str, Any]:
    """Deep merge two dictionaries, with dict2 taking precedence."""
    result = dict1.copy()
    
    for key, value in dict2.items():
        if key in result and isinstance(result[key], dict) and isinstance(value, dict):
            result[key] = deep_merge_dictionaries(result[key], value)
        else:
            result[key] = value
    
    return result


def flatten_dictionary(data: Dict[str, Any], separator: str = ".") -> Dict[str, Any]:
    """Flatten a nested dictionary."""
    result = {}
    
    def _flatten(obj, prefix=""):
        for key, value in obj.items():
            new_key = f"{prefix}{separator}{key}" if prefix else key
            
            if isinstance(value, dict):
                _flatten(value, new_key)
            else:
                result[new_key] = value
    
    _flatten(data)
    return result


def unflatten_dictionary(data: Dict[str, Any], separator: str = ".") -> Dict[str, Any]:
    """Unflatten a flattened dictionary."""
    result = {}
    
    for key, value in data.items():
        keys = key.split(separator)
        current = result
        
        for k in keys[:-1]:
            if k not in current:
                current[k] = {}
            current = current[k]
        
        current[keys[-1]] = value
    
    return result
