"""
Base parser interface for all language parsers.

This module provides the foundational parser class that all language-specific
parsers inherit from, providing common functionality and interfaces.
"""

from abc import ABC, abstractmethod
from typing import Dict, Any, List, Optional
from pathlib import Path

from src.utils.logger import get_logger

logger = get_logger(__name__)


class BaseParser(ABC):
    """Base class for all language parsers."""
    
    def __init__(self, language: str):
        """Initialize the base parser."""
        self.language = language
        self.logger = get_logger(f"{self.__class__.__name__}")
    
    @abstractmethod
    async def parse_file(self, file_path: str) -> Dict[str, Any]:
        """
        Parse a single file.
        
        Args:
            file_path: Path to the file to parse
            
        Returns:
            Parsed file structure
        """
        pass
    
    @abstractmethod
    async def parse_codebase(self, codebase_path: str) -> Dict[str, Any]:
        """
        Parse an entire codebase.
        
        Args:
            codebase_path: Path to the codebase to parse
            
        Returns:
            Parsed codebase structure
        """
        pass
    
    @abstractmethod
    def get_supported_extensions(self) -> List[str]:
        """Get list of supported file extensions."""
        pass
    
    def validate_file(self, file_path: str) -> bool:
        """Validate if file can be parsed by this parser."""
        file_path = Path(file_path)
        return file_path.suffix.lower() in self.get_supported_extensions()
    
    def log_parsing_activity(self, activity: str, details: Dict[str, Any] = None):
        """Log parsing activity."""
        self.logger.info(f"{self.language} parser: {activity}")
        if details:
            self.logger.debug(f"Details: {details}")
