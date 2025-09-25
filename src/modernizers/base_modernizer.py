"""
Base modernizer interface for all language modernizers.

This module provides the foundational modernizer class that all language-specific
modernizers inherit from, providing common functionality and interfaces.
"""

from abc import ABC, abstractmethod
from typing import Dict, Any, List, Optional
from pathlib import Path

from src.utils.logger import get_logger

logger = get_logger(__name__)


class BaseModernizer(ABC):
    """Base class for all language modernizers."""
    
    def __init__(self, source_language: str, target_language: str):
        """Initialize the base modernizer."""
        self.source_language = source_language
        self.target_language = target_language
        self.logger = get_logger(f"{self.__class__.__name__}")
    
    @abstractmethod
    async def modernize_file(self, file_path: str, output_path: str) -> Dict[str, Any]:
        """
        Modernize a single file.
        
        Args:
            file_path: Path to the source file
            output_path: Path to the output file
            
        Returns:
            Modernization results
        """
        pass
    
    @abstractmethod
    async def modernize_codebase(self, codebase_path: str, output_path: str) -> Dict[str, Any]:
        """
        Modernize an entire codebase.
        
        Args:
            codebase_path: Path to the source codebase
            output_path: Path to the output codebase
            
        Returns:
            Modernization results
        """
        pass
    
    @abstractmethod
    def get_supported_source_extensions(self) -> List[str]:
        """Get list of supported source file extensions."""
        pass
    
    @abstractmethod
    def get_target_extension(self) -> str:
        """Get the target file extension."""
        pass
    
    def validate_source_file(self, file_path: str) -> bool:
        """Validate if source file can be modernized."""
        file_path = Path(file_path)
        return file_path.suffix.lower() in self.get_supported_source_extensions()
    
    def log_modernization_activity(self, activity: str, details: Dict[str, Any] = None):
        """Log modernization activity."""
        self.logger.info(f"{self.source_language} -> {self.target_language} modernizer: {activity}")
        if details:
            self.logger.debug(f"Details: {details}")
