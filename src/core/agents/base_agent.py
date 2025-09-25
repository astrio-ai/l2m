"""
Base agent class for all modernization agents.

This module provides the foundational agent class that all specialized
agents inherit from, providing common functionality and interfaces.
"""

from abc import ABC, abstractmethod
from typing import Dict, Any, List, Optional
from langchain_core.messages import BaseMessage, HumanMessage, AIMessage, SystemMessage
from langchain_core.tools import BaseTool

from src.core.state.agent_state import AgentState
from src.core.tools.base_tool import BaseAgentTool
from src.core.llm.provider_factory import LLMProviderFactory
from src.config.settings import Settings
from src.utils.logger import get_logger

logger = get_logger(__name__)


class BaseAgent(ABC):
    """Base class for all modernization agents."""
    
    def __init__(self, settings: Settings, tools: List[BaseAgentTool] = None):
        """Initialize the base agent."""
        self.settings = settings
        self.tools = tools or []
        self.logger = get_logger(self.__class__.__name__)
        
        # Initialize LLM provider
        try:
            self.llm_provider = LLMProviderFactory.create_provider(settings)
            self.logger.info(f"Initialized {self.__class__.__name__} with {settings.llm_provider} provider")
        except Exception as e:
            self.logger.error(f"Failed to initialize LLM provider: {e}")
            self.llm_provider = None
    
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
        if not self.llm_provider:
            self.logger.error("LLM provider not initialized")
            return []
        
        try:
            # Add system message if not present
            if not any(isinstance(msg, SystemMessage) for msg in messages):
                system_prompt = self.get_system_prompt()
                if system_prompt:
                    messages = [SystemMessage(content=system_prompt)] + messages
            
            # Generate response using LLM provider
            response_text = await self.llm_provider.generate_response(messages)
            
            # Create AI message response
            response_message = AIMessage(content=response_text)
            
            self.logger.info(f"Generated response with {len(response_text)} characters")
            return [response_message]
            
        except Exception as e:
            self.logger.error(f"Error processing messages: {e}")
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
    
    async def generate_response(self, prompt: str, context: str = None) -> str:
        """Generate a response for a given prompt."""
        if not self.llm_provider:
            self.logger.error("LLM provider not initialized")
            return ""
        
        try:
            # Prepare messages
            messages = []
            
            # Add system prompt
            system_prompt = self.get_system_prompt()
            if system_prompt:
                messages.append(SystemMessage(content=system_prompt))
            
            # Add context if provided
            if context:
                messages.append(HumanMessage(content=f"Context: {context}"))
            
            # Add the main prompt
            messages.append(HumanMessage(content=prompt))
            
            # Generate response
            response_text = await self.llm_provider.generate_response(messages)
            
            self.logger.info(f"Generated response for prompt: {prompt[:50]}...")
            return response_text
            
        except Exception as e:
            self.logger.error(f"Error generating response: {e}")
            return ""
    
    async def generate_streaming_response(self, prompt: str, context: str = None):
        """Generate a streaming response for a given prompt."""
        if not self.llm_provider:
            self.logger.error("LLM provider not initialized")
            return
        
        try:
            # Prepare messages
            messages = []
            
            # Add system prompt
            system_prompt = self.get_system_prompt()
            if system_prompt:
                messages.append(SystemMessage(content=system_prompt))
            
            # Add context if provided
            if context:
                messages.append(HumanMessage(content=f"Context: {context}"))
            
            # Add the main prompt
            messages.append(HumanMessage(content=prompt))
            
            # Generate streaming response
            async for chunk in self.llm_provider.generate_streaming_response(messages):
                yield chunk
                
        except Exception as e:
            self.logger.error(f"Error generating streaming response: {e}")
    
    async def health_check(self) -> bool:
        """Check if the agent is healthy and ready to work."""
        if not self.llm_provider:
            return False
        
        try:
            return await self.llm_provider.health_check()
        except Exception as e:
            self.logger.error(f"Health check failed: {e}")
            return False
    
    def get_llm_info(self) -> Dict[str, Any]:
        """Get information about the LLM provider."""
        if not self.llm_provider:
            return {"status": "not_initialized"}
        
        return {
            "status": "initialized",
            "provider": self.llm_provider.get_model_info()
        }
