"""
OpenAI LLM provider.

This module provides the OpenAI LLM provider implementation
for the Legacy2Modern system.
"""

import asyncio
from typing import List, Dict, Any, Optional, AsyncGenerator
from langchain_core.messages import BaseMessage, HumanMessage, AIMessage, SystemMessage

try:
    from openai import AsyncOpenAI
except ImportError:
    AsyncOpenAI = None

from .base_provider import BaseLLMProvider
from src.utils.logger import get_logger

logger = get_logger(__name__)


class OpenAIProvider(BaseLLMProvider):
    """OpenAI LLM provider."""
    
    def __init__(self, api_key: str, model: str = "gpt-4", 
                 temperature: float = 0.7, max_tokens: int = 4000):
        """Initialize the OpenAI provider."""
        super().__init__(api_key, model, temperature, max_tokens)
        
        if AsyncOpenAI is None:
            raise ImportError("openai package is required. Install with: pip install openai")
        
        self.client = AsyncOpenAI(api_key=api_key)
    
    async def generate_response(self, messages: List[BaseMessage], **kwargs) -> str:
        """Generate a response from OpenAI."""
        try:
            # Format messages for OpenAI
            formatted_messages = self.format_messages(messages)
            
            # Prepare the request
            request_params = {
                "model": self.model,
                "messages": formatted_messages,
                "temperature": self.temperature,
                "max_tokens": self.max_tokens,
                **kwargs
            }
            
            # Make the request
            response = await self.client.chat.completions.create(**request_params)
            
            # Extract the response content
            if response.choices and len(response.choices) > 0:
                return response.choices[0].message.content
            else:
                return ""
                
        except Exception as e:
            self.logger.error(f"Error generating OpenAI response: {e}")
            raise
    
    async def generate_streaming_response(self, messages: List[BaseMessage], **kwargs) -> AsyncGenerator[str, None]:
        """Generate a streaming response from OpenAI."""
        try:
            # Format messages for OpenAI
            formatted_messages = self.format_messages(messages)
            
            # Prepare the request
            request_params = {
                "model": self.model,
                "messages": formatted_messages,
                "temperature": self.temperature,
                "max_tokens": self.max_tokens,
                "stream": True,
                **kwargs
            }
            
            # Make the streaming request
            stream = await self.client.chat.completions.create(**request_params)
            
            async for chunk in stream:
                if chunk.choices and len(chunk.choices) > 0:
                    delta = chunk.choices[0].delta
                    if delta.content:
                        yield delta.content
                        
        except Exception as e:
            self.logger.error(f"Error generating streaming OpenAI response: {e}")
            raise
    
    async def health_check(self) -> bool:
        """Check if the OpenAI API is accessible."""
        try:
            # Simple test request
            test_messages = [HumanMessage(content="Hello")]
            await self.generate_response(test_messages, max_tokens=10)
            return True
        except Exception as e:
            self.logger.error(f"OpenAI health check failed: {e}")
            return False
