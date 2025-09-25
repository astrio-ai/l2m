"""
Base tool class for all agent tools.

This module provides the foundational tool class that all specialized
tools inherit from, providing common functionality and interfaces.
"""

from abc import ABC, abstractmethod
from typing import Any, Dict, Optional
from langchain_core.tools import BaseTool as LangChainBaseTool

from src.utils.logger import get_logger

logger = get_logger(__name__)


class BaseAgentTool(ABC):
    """Base class for all agent tools."""
    
    def __init__(self, name: str, description: str):
        """Initialize the base tool."""
        self.name = name
        self.description = description
        self.logger = get_logger(f"{self.__class__.__name__}")
    
    @abstractmethod
    async def run(self, **kwargs) -> Any:
        """
        Run the tool with the given parameters.
        
        Args:
            **kwargs: Tool-specific parameters
            
        Returns:
            Tool execution result
        """
        pass
    
    def get_langchain_tool(self) -> LangChainBaseTool:
        """Get the LangChain tool representation."""
        # This would be implemented to return a LangChain tool
        # For now, return None as placeholder
        return None
    
    def validate_parameters(self, **kwargs) -> bool:
        """Validate tool parameters."""
        # Override in subclasses for specific validation
        return True
    
    def log_usage(self, parameters: Dict[str, Any], result: Any):
        """Log tool usage."""
        self.logger.info(f"Tool {self.name} executed with parameters: {parameters}")
        if result:
            self.logger.debug(f"Tool {self.name} result: {result}")
