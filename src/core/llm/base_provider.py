"""
Base LLM provider class.

This module provides the base class for all LLM providers,
defining the common interface and functionality.
"""

from abc import ABC, abstractmethod
from typing import List, Dict, Any, Optional
from langchain_core.messages import BaseMessage, HumanMessage, AIMessage, SystemMessage
from langchain_core.language_models import BaseLanguageModel
from langchain_core.callbacks import CallbackManagerForLLMRun

from src.utils.logger import get_logger

logger = get_logger(__name__)


class BaseLLMProvider(ABC):
    """Base class for all LLM providers."""
    
    def __init__(self, api_key: str, model: str, temperature: float = 0.7, max_tokens: int = 4000):
        """Initialize the LLM provider."""
        self.api_key = api_key
        self.model = model
        self.temperature = temperature
        self.max_tokens = max_tokens
        self.logger = get_logger(self.__class__.__name__)
    
    @abstractmethod
    async def generate_response(self, messages: List[BaseMessage], **kwargs) -> str:
        """
        Generate a response from the LLM.
        
        Args:
            messages: List of messages to send to the LLM
            **kwargs: Additional parameters
            
        Returns:
            Generated response text
        """
        pass
    
    @abstractmethod
    async def generate_streaming_response(self, messages: List[BaseMessage], **kwargs):
        """
        Generate a streaming response from the LLM.
        
        Args:
            messages: List of messages to send to the LLM
            **kwargs: Additional parameters
            
        Yields:
            Response chunks
        """
        pass
    
    def format_messages(self, messages: List[BaseMessage]) -> List[Dict[str, str]]:
        """Format messages for the LLM provider."""
        formatted = []
        for message in messages:
            if isinstance(message, SystemMessage):
                formatted.append({"role": "system", "content": message.content})
            elif isinstance(message, HumanMessage):
                formatted.append({"role": "user", "content": message.content})
            elif isinstance(message, AIMessage):
                formatted.append({"role": "assistant", "content": message.content})
        
        return formatted
    
    def validate_messages(self, messages: List[BaseMessage]) -> bool:
        """Validate that messages are properly formatted."""
        if not messages:
            return False
        
        for message in messages:
            if not hasattr(message, 'content') or not message.content:
                return False
        
        return True
    
    def get_model_info(self) -> Dict[str, Any]:
        """Get information about the current model."""
        return {
            "provider": self.__class__.__name__,
            "model": self.model,
            "temperature": self.temperature,
            "max_tokens": self.max_tokens
        }
    
    def get_model(self) -> str:
        """Get the model name for LangChain compatibility."""
        return self.model
