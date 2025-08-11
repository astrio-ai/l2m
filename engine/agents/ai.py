"""
AI wrapper for LLM calls with rate limiting, retries, and conversation state.
"""

import asyncio
import time
import logging
from typing import Dict, List, Optional, Any, Union
from dataclasses import dataclass, field
import openai
from openai import AsyncOpenAI
import anthropic
from anthropic import AsyncAnthropic

logger = logging.getLogger(__name__)

@dataclass
class ConversationState:
    """Tracks conversation state and history."""
    messages: List[Dict[str, str]] = field(default_factory=list)
    token_count: int = 0
    cost_estimate: float = 0.0
    last_request_time: float = 0.0

class AI:
    """
    Wrapper for LLM calls with rate limiting, retries, and conversation management.
    Supports both OpenAI and Anthropic APIs.
    """
    
    def __init__(
        self,
        api_key: str,
        provider: str = "openai",
        model: str = "gpt-4",
        max_retries: int = 3,
        rate_limit_delay: float = 1.0,
        max_tokens: int = 4000,
        temperature: float = 0.7
    ):
        self.api_key = api_key
        self.provider = provider.lower()
        self.model = model
        self.max_retries = max_retries
        self.rate_limit_delay = rate_limit_delay
        self.max_tokens = max_tokens
        self.temperature = temperature
        
        # Initialize API clients
        if self.provider == "openai":
            self.client = AsyncOpenAI(api_key=api_key)
        elif self.provider == "anthropic":
            self.client = AsyncAnthropic(api_key=api_key)
        else:
            raise ValueError(f"Unsupported provider: {provider}")
        
        # Conversation state
        self.conversation = ConversationState()
        
        # Rate limiting
        self._last_request = 0.0
        
    async def _rate_limit(self):
        """Ensure we don't exceed rate limits."""
        now = time.time()
        time_since_last = now - self._last_request
        if time_since_last < self.rate_limit_delay:
            await asyncio.sleep(self.rate_limit_delay - time_since_last)
        self._last_request = time.time()
    
    async def _make_request(self, messages: List[Dict[str, str]]) -> str:
        """Make API request with retry logic."""
        for attempt in range(self.max_retries):
            try:
                await self._rate_limit()
                
                if self.provider == "openai":
                    response = await self.client.chat.completions.create(
                        model=self.model,
                        messages=messages,
                        max_tokens=self.max_tokens,
                        temperature=self.temperature
                    )
                    return response.choices[0].message.content
                    
                elif self.provider == "anthropic":
                    # Convert OpenAI format to Anthropic format
                    system_message = ""
                    user_messages = []
                    
                    for msg in messages:
                        if msg["role"] == "system":
                            system_message = msg["content"]
                        else:
                            user_messages.append(msg["content"])
                    
                    user_content = "\n".join(user_messages)
                    
                    response = await self.client.messages.create(
                        model=self.model,
                        max_tokens=self.max_tokens,
                        temperature=self.temperature,
                        system=system_message,
                        messages=[{"role": "user", "content": user_content}]
                    )
                    return response.content[0].text
                    
            except Exception as e:
                logger.warning(f"API request failed (attempt {attempt + 1}/{self.max_retries}): {e}")
                if attempt == self.max_retries - 1:
                    raise
                await asyncio.sleep(2 ** attempt)  # Exponential backoff
    
    async def chat(
        self,
        messages: List[Dict[str, str]],
        system_prompt: Optional[str] = None,
        update_conversation: bool = True
    ) -> str:
        """
        Send a chat request to the LLM.
        
        Args:
            messages: List of message dictionaries with 'role' and 'content'
            system_prompt: Optional system prompt to prepend
            update_conversation: Whether to update conversation state
            
        Returns:
            LLM response as string
        """
        # Prepare messages
        if system_prompt:
            full_messages = [{"role": "system", "content": system_prompt}] + messages
        else:
            full_messages = messages.copy()
        
        # Make request
        response = await self._make_request(full_messages)
        
        # Update conversation state
        if update_conversation:
            self.conversation.messages.extend(messages)
            self.conversation.messages.append({"role": "assistant", "content": response})
            self.conversation.last_request_time = time.time()
            
            # Rough token estimation (4 chars per token)
            total_chars = sum(len(msg["content"]) for msg in full_messages + [{"content": response}])
            self.conversation.token_count += total_chars // 4
        
        return response
    
    async def chat_simple(self, prompt: str, system_prompt: Optional[str] = None) -> str:
        """Simple chat interface for single prompts."""
        messages = [{"role": "user", "content": prompt}]
        return await self.chat(messages, system_prompt)
    
    def get_conversation_summary(self) -> Dict[str, Any]:
        """Get summary of conversation state."""
        return {
            "message_count": len(self.conversation.messages),
            "token_count": self.conversation.token_count,
            "cost_estimate": self.conversation.cost_estimate,
            "last_request_time": self.conversation.last_request_time
        }
    
    def reset_conversation(self):
        """Reset conversation state."""
        self.conversation = ConversationState()
    
    def estimate_cost(self, tokens: int) -> float:
        """Estimate cost based on token count and model."""
        # Rough cost estimates (per 1K tokens)
        costs = {
            "gpt-4": 0.03,  # Input
            "gpt-4-turbo": 0.01,
            "gpt-3.5-turbo": 0.001,
            "claude-3-opus": 0.015,
            "claude-3-sonnet": 0.003,
            "claude-3-haiku": 0.00025
        }
        
        base_cost = costs.get(self.model, 0.01)
        return (tokens / 1000) * base_cost 