"""
Base transformer interface for all code transformers.

This module provides the foundational transformer class that all specialized
transformers inherit from, providing common functionality and interfaces.
"""

from abc import ABC, abstractmethod
from typing import Dict, Any, List, Optional
from pathlib import Path

from src.utils.logger import get_logger

logger = get_logger(__name__)


class BaseTransformer(ABC):
    """Base class for all code transformers."""
    
    def __init__(self, transformation_type: str):
        """Initialize the base transformer."""
        self.transformation_type = transformation_type
        self.logger = get_logger(f"{self.__class__.__name__}")
    
    @abstractmethod
    async def transform(self, code: str, context: Dict[str, Any] = None) -> str:
        """
        Transform code according to the transformer's rules.
        
        Args:
            code: Source code to transform
            context: Additional context for transformation
            
        Returns:
            Transformed code
        """
        pass
    
    @abstractmethod
    def get_transformation_rules(self) -> List[Dict[str, Any]]:
        """Get the transformation rules for this transformer."""
        pass
    
    def validate_input(self, code: str) -> bool:
        """Validate input code."""
        return isinstance(code, str) and len(code.strip()) > 0
    
    def log_transformation_activity(self, activity: str, details: Dict[str, Any] = None):
        """Log transformation activity."""
        self.logger.info(f"{self.transformation_type} transformer: {activity}")
        if details:
            self.logger.debug(f"Details: {details}")
