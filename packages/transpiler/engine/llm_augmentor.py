"""
LLM Augmentation for COBOL Transpilation

This module provides AI-assisted translation for complex COBOL constructs
that cannot be handled by the rule-based system.
"""

import os
import logging
import json
import hashlib
import time
from typing import Dict, Any, List, Optional, Union
from dataclasses import dataclass, field
from abc import ABC, abstractmethod
import openai
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

@dataclass
class LLMConfig:
    """Configuration for LLM API calls."""
    api_key: str
    model: str
    temperature: float
    max_tokens: int = 2000
    provider: str = "openai"  # openai, anthropic, local
    cache_enabled: bool = True
    cache_ttl: int = 3600  # 1 hour
    retry_attempts: int = 3
    retry_delay: float = 1.0
    
    @classmethod
    def from_env(cls):
        """Create config from environment variables."""
        # Handle invalid temperature gracefully
        try:
            temperature = float(os.getenv('DEFAULT_LLM_TEMPERATURE', '0.1'))
        except ValueError:
            temperature = 0.1  # Default fallback
        
        return cls(
            api_key=os.getenv('LLM_API_KEY', ''),
            model=os.getenv('LLM_MODEL', 'gpt-4'),
            temperature=temperature,
            provider=os.getenv('LLM_PROVIDER', 'openai'),
            cache_enabled=os.getenv('LLM_CACHE_ENABLED', 'true').lower() == 'true',
            cache_ttl=int(os.getenv('LLM_CACHE_TTL', '3600')),
            retry_attempts=int(os.getenv('LLM_RETRY_ATTEMPTS', '3')),
            retry_delay=float(os.getenv('LLM_RETRY_DELAY', '1.0'))
        )


class LLMProvider(ABC):
    """Abstract base class for LLM providers."""
    
    @abstractmethod
    def generate_response(self, messages: List[Dict[str, str]], config: LLMConfig) -> str:
        """Generate response from LLM."""
        pass


class OpenAIProvider(LLMProvider):
    """OpenAI API provider."""
    
    def generate_response(self, messages: List[Dict[str, str]], config: LLMConfig) -> str:
        """Generate response using OpenAI API."""
        from openai import OpenAI
        
        client = OpenAI(api_key=config.api_key)
        response = client.chat.completions.create(
            model=config.model,
            messages=messages,
            temperature=config.temperature,
            max_tokens=config.max_tokens
        )
        return response.choices[0].message.content.strip()


class AnthropicProvider(LLMProvider):
    """Anthropic Claude API provider."""
    
    def generate_response(self, messages: List[Dict[str, str]], config: LLMConfig) -> str:
        """Generate response using Anthropic API."""
        import anthropic
        
        client = anthropic.Anthropic(api_key=config.api_key)
        
        # Convert OpenAI format to Anthropic format
        prompt = ""
        for msg in messages:
            if msg["role"] == "system":
                prompt += f"System: {msg['content']}\n\n"
            elif msg["role"] == "user":
                prompt += f"Human: {msg['content']}\n\n"
            elif msg["role"] == "assistant":
                prompt += f"Assistant: {msg['content']}\n\n"
        
        prompt += "Assistant:"
        
        response = client.messages.create(
            model=config.model,
            max_tokens=config.max_tokens,
            temperature=config.temperature,
            messages=[{"role": "user", "content": prompt}]
        )
        
        return response.content[0].text.strip()


class LocalProvider(LLMProvider):
    """Local LLM provider (e.g., Ollama)."""
    
    def generate_response(self, messages: List[Dict[str, str]], config: LLMConfig) -> str:
        """Generate response using local LLM."""
        import requests
        import logging
        
        logger = logging.getLogger(__name__)
        logger.info(f"Generating response with local LLM: {config.model}")
        
        # Convert to Ollama format
        prompt = ""
        for msg in messages:
            if msg["role"] == "system":
                prompt += f"System: {msg['content']}\n\n"
            elif msg["role"] == "user":
                prompt += f"User: {msg['content']}\n\n"
            elif msg["role"] == "assistant":
                prompt += f"Assistant: {msg['content']}\n\n"
        
        logger.info(f"Sending request to Ollama with prompt length: {len(prompt)}")
        
        response = requests.post(
            "http://localhost:11434/api/generate",
            json={
                "model": config.model,
                "prompt": prompt,
                "stream": False
            },
            timeout=60  # 60 second timeout
        )
        
        if response.status_code == 200:
            result = response.json()
            if "response" in result:
                response_text = result["response"].strip()
                # Clean up markdown code blocks if present
                if "```python" in response_text:
                    # Extract code from markdown blocks
                    import re
                    python_blocks = re.findall(r'```python\s*\n(.*?)\n```', response_text, re.DOTALL)
                    if python_blocks:
                        return python_blocks[0].strip()
                    # Fallback: remove markdown formatting
                    response_text = re.sub(r'```python\s*\n', '', response_text)
                    response_text = re.sub(r'\n```', '', response_text)
                return response_text
            else:
                raise Exception(f"Local LLM response missing 'response' field: {result}")
        else:
            raise Exception(f"Local LLM request failed: {response.status_code} - {response.text}")


class ResponseCache:
    """Simple in-memory cache for LLM responses."""
    
    def __init__(self, ttl: int = 3600):
        self.cache: Dict[str, Dict[str, Any]] = {}
        self.ttl = ttl
    
    def get(self, key: str) -> Optional[str]:
        """Get cached response if not expired."""
        if key in self.cache:
            entry = self.cache[key]
            if time.time() - entry['timestamp'] < self.ttl:
                return entry['response']
            else:
                del self.cache[key]
        return None
    
    def set(self, key: str, response: str):
        """Cache a response."""
        self.cache[key] = {
            'response': response,
            'timestamp': time.time()
        }
    
    def clear(self):
        """Clear all cached responses."""
        self.cache.clear()


class LLMAugmentor:
    """
    Handles AI-assisted translation of complex COBOL constructs.
    """
    
    def __init__(self, config: Optional[LLMConfig] = None):
        self.config = config or LLMConfig.from_env()
        self.logger = logging.getLogger(__name__)
        self.cache = ResponseCache(self.config.cache_ttl) if self.config.cache_enabled else None
        
        # Set up provider
        self.provider = self._create_provider()
        
        # Set up OpenAI client for backward compatibility
        if self.config.provider == "openai" and self.config.api_key:
            openai.api_key = self.config.api_key
        elif not self.config.api_key and self.config.provider != "local":
            self.logger.warning("No LLM API key provided. AI augmentation will be disabled.")
    
    def _create_provider(self) -> Optional[LLMProvider]:
        """Create the appropriate LLM provider."""
        # For local providers, API key is not required
        if self.config.provider == "local":
            return LocalProvider()
        
        # For cloud providers, API key is required
        if not self.config.api_key:
            return None
        
        providers = {
            "openai": OpenAIProvider,
            "anthropic": AnthropicProvider,
            "local": LocalProvider
        }
        
        provider_class = providers.get(self.config.provider)
        if provider_class:
            return provider_class()
        else:
            self.logger.error(f"Unknown LLM provider: {self.config.provider}")
            return None
    
    def can_augment(self) -> bool:
        """Check if LLM augmentation is available."""
        # For local providers, no API key is needed
        if self.config.provider == "local":
            return bool(self.provider)
        # For cloud providers, API key is required
        return bool(self.config.api_key and self.provider)
    
    def _generate_cache_key(self, edge_case: Dict[str, Any]) -> str:
        """Generate a cache key for an edge case."""
        # Create a hash of the edge case data
        data = json.dumps(edge_case, sort_keys=True)
        return hashlib.md5(data.encode()).hexdigest()
    
    def _create_enhanced_prompt(self, edge_case: Dict[str, Any]) -> str:
        """
        Create an enhanced prompt for COBOL to Python translation.
        """
        edge_type = edge_case.get('type', 'unknown')
        tokens = edge_case.get('all_tokens', [])
        context = edge_case.get('context', '')
        severity = edge_case.get('severity', 'medium')
        
        # Enhanced prompt with better context and examples
        prompt = f"""You are an expert COBOL to Python translator specializing in legacy code modernization.

TASK: Translate the following COBOL construct to modern, maintainable Python code.

CONTEXT:
- Edge Case Type: {edge_type}
- Severity: {severity}
- Context: {context}
- COBOL Tokens: {tokens[:30]}  # First 30 tokens for context

REQUIREMENTS:
1. Generate valid, executable Python code
2. Use snake_case for variable names (convert from COBOL naming)
3. Handle COBOL-specific constructs appropriately
4. Add clear comments explaining the translation
5. Ensure the code follows Python best practices
6. Handle edge cases and error conditions gracefully
7. Use modern Python features where appropriate

COBOL TO PYTHON MAPPING GUIDELINES:
- DISPLAY -> print()
- ACCEPT -> input()
- MOVE TO -> assignment (=)
- ADD/SUBTRACT -> arithmetic operators (+, -, *, /)
- PERFORM UNTIL -> while loop
- IF/ELSE -> if/elif/else
- COMPUTE -> mathematical expressions
- SEARCH -> list operations or dictionary lookups
- INSPECT -> string methods
- GOBACK -> return

EXAMPLES:
COBOL: DISPLAY 'HELLO WORLD'
Python: print('HELLO WORLD')

COBOL: MOVE 10 TO COUNTER
Python: counter = 10

COBOL: PERFORM UNTIL COUNTER > 0
Python: while counter > 0:

Now translate this COBOL construct to Python:"""

        return prompt
    
    def _create_system_message(self) -> str:
        """Create the system message for the LLM."""
        return """You are an expert COBOL to Python translator. Your task is to convert COBOL code to modern, maintainable Python code.

Key responsibilities:
1. Understand COBOL syntax and semantics
2. Generate equivalent Python code
3. Maintain code functionality while improving readability
4. Handle edge cases and complex constructs
5. Add appropriate comments and documentation
6. Follow Python best practices and PEP 8 guidelines

IMPORTANT: Provide ONLY the Python code without any markdown formatting, explanations, or code blocks. Return clean, executable Python code."""
    
    def translate_edge_case(self, edge_case: Dict[str, Any]) -> Optional[str]:
        """
        Translate a complex COBOL construct using LLM.
        
        Args:
            edge_case: The edge case to translate
            
        Returns:
            Translated Python code or None if translation failed
        """
        if not self.can_augment():
            self.logger.warning("LLM augmentation not available")
            return None
        
        # Check cache first
        if self.cache:
            cache_key = self._generate_cache_key(edge_case)
            cached_response = self.cache.get(cache_key)
            if cached_response:
                self.logger.info(f"Using cached translation for edge case: {edge_case.get('type', 'unknown')}")
                return cached_response
        
        try:
            # Create enhanced prompt
            prompt = self._create_enhanced_prompt(edge_case)
            
            # Prepare messages
            messages = [
                {"role": "system", "content": self._create_system_message()},
                {"role": "user", "content": prompt}
            ]
            
            # Generate response with retry logic
            translated_code = self._generate_with_retry(messages)
            
            if translated_code:
                # Cache the response
                if self.cache:
                    cache_key = self._generate_cache_key(edge_case)
                    self.cache.set(cache_key, translated_code)
                
                self.logger.info(f"Successfully translated edge case: {edge_case.get('type', 'unknown')}")
                return translated_code
            
        except Exception as e:
            self.logger.error(f"Failed to translate edge case: {e}")
            return None
    
    def _generate_with_retry(self, messages: List[Dict[str, str]]) -> Optional[str]:
        """Generate response with retry logic."""
        for attempt in range(self.config.retry_attempts):
            try:
                return self.provider.generate_response(messages, self.config)
            except Exception as e:
                self.logger.warning(f"LLM request failed (attempt {attempt + 1}/{self.config.retry_attempts}): {e}")
                if attempt < self.config.retry_attempts - 1:
                    time.sleep(self.config.retry_delay * (2 ** attempt))  # Exponential backoff
                else:
                    raise e
        return None
    
    def batch_translate_edge_cases(self, edge_cases: List[Dict[str, Any]]) -> Dict[str, str]:
        """
        Translate multiple edge cases in batch.
        
        Args:
            edge_cases: List of edge cases to translate
            
        Returns:
            Dictionary mapping edge case IDs to translated code
        """
        if not self.can_augment():
            return {}
        
        translations = {}
        
        for i, edge_case in enumerate(edge_cases):
            self.logger.info(f"Translating edge case {i+1}/{len(edge_cases)}: {edge_case.get('type', 'unknown')}")
            
            translated = self.translate_edge_case(edge_case)
            if translated:
                edge_case_id = f"edge_case_{i}_{edge_case.get('type', 'unknown')}"
                translations[edge_case_id] = translated
        
        return translations
    
    def generate_integration_code(self, translations: Dict[str, str]) -> str:
        """
        Generate code to integrate AI-translated snippets.
        
        Args:
            translations: Dictionary of translated code snippets
            
        Returns:
            Integration code
        """
        if not translations:
            return ""
        
        integration_code = []
        integration_code.append("# AI-Generated Code Snippets")
        integration_code.append("# =========================")
        integration_code.append("# These snippets were generated by AI to handle complex COBOL constructs")
        integration_code.append("")
        
        for edge_case_id, translated_code in translations.items():
            integration_code.append(f"# {edge_case_id}")
            integration_code.append("# Generated by AI for complex COBOL construct")
            integration_code.append(translated_code)
            integration_code.append("")
        
        return "\n".join(integration_code)
    
    def clear_cache(self):
        """Clear the response cache."""
        if self.cache:
            self.cache.clear()
            self.logger.info("Response cache cleared")
    
    def get_cache_stats(self) -> Dict[str, Any]:
        """Get cache statistics."""
        if not self.cache:
            return {"enabled": False}
        
        return {
            "enabled": True,
            "size": len(self.cache.cache),
            "ttl": self.cache.ttl
        } 