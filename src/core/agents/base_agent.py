"""
Base agent class for all modernization agents.

This module provides the foundational agent class that all specialized
agents inherit from, providing common functionality and interfaces.
"""

from abc import ABC, abstractmethod
from typing import Dict, Any, List
from langchain_core.messages import BaseMessage
from langchain_core.tools import BaseTool

from src.core.state.agent_state import AgentState
from src.core.tools.base_tool import BaseAgentTool
from src.utils.logger import get_logger

logger = get_logger(__name__)


class BaseAgent(ABC):
    """Base class for all modernization agents."""
    
    def __init__(self, settings, tools: List[BaseAgentTool] = None):
        """Initialize the base agent."""
        self.settings = settings
        self.tools = tools or []
        self.logger = get_logger(self.__class__.__name__)
    
    @abstractmethod
    async def run(self, state: AgentState) -> AgentState:
        """
        Run the agent with the given state.
        
        Args:
            state: The current agent state
            
        Returns:
            Updated agent state
        """
        pass
    
    @abstractmethod
    def get_system_prompt(self) -> str:
        """Get the system prompt for this agent."""
        pass
    
    async def process_messages(self, messages: List[BaseMessage]) -> List[BaseMessage]:
        """Process a list of messages and return responses."""
        # This would be implemented with the actual LLM provider
        # For now, return empty list as placeholder
        return []
    
    def get_available_tools(self) -> List[BaseTool]:
        """Get the list of available tools for this agent."""
        return [tool.get_langchain_tool() for tool in self.tools]
    
    async def use_tool(self, tool_name: str, **kwargs) -> Any:
        """Use a specific tool with the given parameters."""
        for tool in self.tools:
            if tool.name == tool_name:
                return await tool.run(**kwargs)
        
        raise ValueError(f"Tool '{tool_name}' not found")
    
    def log_activity(self, activity: str, details: Dict[str, Any] = None):
        """Log agent activity."""
        self.logger.info(f"{self.__class__.__name__}: {activity}")
        if details:
            self.logger.debug(f"Details: {details}")
