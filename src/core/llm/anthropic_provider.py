"""
Anthropic Claude LLM provider.

This module provides the Anthropic Claude LLM provider implementation
for the Legacy2Modern system.
"""

import asyncio
from typing import List, Dict, Any, Optional, AsyncGenerator
from langchain_core.messages import BaseMessage, HumanMessage, AIMessage, SystemMessage

try:
    import anthropic
except ImportError:
    anthropic = None

from .base_provider import BaseLLMProvider
from src.utils.logger import get_logger

logger = get_logger(__name__)


class AnthropicProvider(BaseLLMProvider):
    """Anthropic Claude LLM provider."""
    
    def __init__(self, api_key: str, model: str = "claude-3-sonnet-20240229", 
                 temperature: float = 0.7, max_tokens: int = 4000):
        """Initialize the Anthropic provider."""
        super().__init__(api_key, model, temperature, max_tokens)
        
        if anthropic is None:
            raise ImportError("anthropic package is required. Install with: pip install anthropic")
        
        self.client = anthropic.AsyncAnthropic(api_key=api_key)
    
    async def generate_response(self, messages: List[BaseMessage], **kwargs) -> str:
        """Generate a response from Claude."""
        try:
            # Format messages for Claude
            formatted_messages = self._format_for_claude(messages)
            
            # Prepare the request
            request_params = {
                "model": self.model,
                "max_tokens": self.max_tokens,
                "temperature": self.temperature,
                "messages": formatted_messages,
                **kwargs
            }
            
            # Make the request
            response = await self.client.messages.create(**request_params)
            
            # Extract the response content
            if response.content and len(response.content) > 0:
                return response.content[0].text
            else:
                return ""
                
        except Exception as e:
            self.logger.error(f"Error generating Claude response: {e}")
            raise
    
    async def generate_streaming_response(self, messages: List[BaseMessage], **kwargs) -> AsyncGenerator[str, None]:
        """Generate a streaming response from Claude."""
        try:
            # Format messages for Claude
            formatted_messages = self._format_for_claude(messages)
            
            # Prepare the request
            request_params = {
                "model": self.model,
                "max_tokens": self.max_tokens,
                "temperature": self.temperature,
                "messages": formatted_messages,
                "stream": True,
                **kwargs
            }
            
            # Make the streaming request
            async with self.client.messages.stream(**request_params) as stream:
                async for chunk in stream:
                    if chunk.type == "content_block_delta":
                        yield chunk.delta.text
                        
        except Exception as e:
            self.logger.error(f"Error generating streaming Claude response: {e}")
            raise
    
    def _format_for_claude(self, messages: List[BaseMessage]) -> List[Dict[str, str]]:
        """Format messages for Claude API."""
        formatted = []
        
        for message in messages:
            if isinstance(message, SystemMessage):
                # Claude uses system parameter instead of system messages
                continue
            elif isinstance(message, HumanMessage):
                formatted.append({"role": "user", "content": message.content})
            elif isinstance(message, AIMessage):
                formatted.append({"role": "assistant", "content": message.content})
        
        return formatted
    
    def get_system_message(self, messages: List[BaseMessage]) -> Optional[str]:
        """Extract system message from messages."""
        for message in messages:
            if isinstance(message, SystemMessage):
                return message.content
        return None
    
    async def health_check(self) -> bool:
        """Check if the Anthropic API is accessible."""
        try:
            # Simple test request
            test_messages = [HumanMessage(content="Hello")]
            await self.generate_response(test_messages, max_tokens=10)
            return True
        except Exception as e:
            self.logger.error(f"Anthropic health check failed: {e}")
            return False
