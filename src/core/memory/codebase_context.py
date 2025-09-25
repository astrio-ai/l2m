"""
Codebase context management.

This module handles codebase context and metadata for the
multi-agent system.
"""

from typing import Dict, Any, List, Optional
from pathlib import Path
import json

from src.utils.logger import get_logger

logger = get_logger(__name__)


class CodebaseContext:
    """Manages codebase context and metadata."""
    
    def __init__(self, codebase_path: str):
        """Initialize codebase context."""
        self.codebase_path = Path(codebase_path)
        self.context: Dict[str, Any] = {}
        self.logger = get_logger(__name__)
    
    def set_context(self, key: str, value: Any):
        """Set context value."""
        self.context[key] = value
        self.logger.debug(f"Set context {key}: {type(value).__name__}")
    
    def get_context(self, key: str, default: Any = None) -> Any:
        """Get context value."""
        return self.context.get(key, default)
    
    def update_context(self, updates: Dict[str, Any]):
        """Update multiple context values."""
        self.context.update(updates)
        self.logger.debug(f"Updated context with {len(updates)} values")
    
    def get_codebase_info(self) -> Dict[str, Any]:
        """Get codebase information."""
        return {
            "path": str(self.codebase_path),
            "exists": self.codebase_path.exists(),
            "is_directory": self.codebase_path.is_dir(),
            "size": self._get_codebase_size(),
            "file_count": self._get_file_count(),
            "languages": self._get_detected_languages()
        }
    
    def _get_codebase_size(self) -> int:
        """Get total size of codebase."""
        if not self.codebase_path.exists():
            return 0
        
        total_size = 0
        for file_path in self.codebase_path.rglob("*"):
            if file_path.is_file():
                total_size += file_path.stat().st_size
        
        return total_size
    
    def _get_file_count(self) -> int:
        """Get total file count."""
        if not self.codebase_path.exists():
            return 0
        
        return len([f for f in self.codebase_path.rglob("*") if f.is_file()])
    
    def _get_detected_languages(self) -> List[str]:
        """Get detected programming languages."""
        # This would implement actual language detection
        # For now, return placeholder
        return ["python", "javascript", "java"]
    
    def save_context(self, file_path: str):
        """Save context to file."""
        try:
            with open(file_path, 'w') as f:
                json.dump(self.context, f, indent=2)
            self.logger.info(f"Context saved to {file_path}")
        except Exception as e:
            self.logger.error(f"Error saving context: {e}")
    
    def load_context(self, file_path: str):
        """Load context from file."""
        try:
            with open(file_path, 'r') as f:
                self.context = json.load(f)
            self.logger.info(f"Context loaded from {file_path}")
        except Exception as e:
            self.logger.error(f"Error loading context: {e}")
