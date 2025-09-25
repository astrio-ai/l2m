"""
LLM provider factory.

This module provides a factory for creating LLM providers
based on configuration settings.
"""

from typing import Dict, Any, Optional
from .base_provider import BaseLLMProvider
from .anthropic_provider import AnthropicProvider
from .openai_provider import OpenAIProvider
from src.config.settings import Settings
from src.utils.logger import get_logger

logger = get_logger(__name__)


class LLMProviderFactory:
    """Factory for creating LLM providers."""
    
    _providers = {
        "anthropic": AnthropicProvider,
        "openai": OpenAIProvider,
    }
    
    @classmethod
    def create_provider(cls, settings: Settings) -> BaseLLMProvider:
        """
        Create an LLM provider based on settings.
        
        Args:
            settings: Application settings
            
        Returns:
            Configured LLM provider
        """
        provider_name = settings.llm_provider.lower()
        
        if provider_name not in cls._providers:
            raise ValueError(f"Unsupported LLM provider: {provider_name}")
        
        provider_class = cls._providers[provider_name]
        
        # Get provider-specific configuration
        config = cls._get_provider_config(settings, provider_name)
        
        try:
            provider = provider_class(**config)
            logger.info(f"Created {provider_name} provider with model {settings.llm_model}")
            return provider
        except Exception as e:
            logger.error(f"Failed to create {provider_name} provider: {e}")
            raise
    
    @classmethod
    def _get_provider_config(cls, settings: Settings, provider_name: str) -> Dict[str, Any]:
        """Get provider-specific configuration."""
        # Get the appropriate API key
        api_key = None
        if provider_name == "anthropic":
            api_key = settings.anthropic_api_key or settings.llm_api_key
        elif provider_name == "openai":
            api_key = settings.openai_api_key or settings.llm_api_key
        else:
            api_key = settings.llm_api_key
        
        base_config = {
            "api_key": api_key,
            "model": settings.llm_model,
            "temperature": settings.llm_temperature,
            "max_tokens": settings.llm_max_tokens,
        }
        
        # Provider-specific configurations
        if provider_name == "anthropic":
            # Anthropic-specific model mapping
            model_mapping = {
                "claude-3-sonnet": "claude-3-5-sonnet-20241022",
                "claude-3-haiku": "claude-3-haiku-20240307",
                "claude-3-opus": "claude-3-opus-20240229",
            }
            
            if settings.llm_model in model_mapping:
                base_config["model"] = model_mapping[settings.llm_model]
        
        elif provider_name == "openai":
            # OpenAI-specific model mapping
            model_mapping = {
                "gpt-4": "gpt-4",
                "gpt-4-turbo": "gpt-4-turbo-preview",
                "gpt-3.5-turbo": "gpt-3.5-turbo",
            }
            
            if settings.llm_model in model_mapping:
                base_config["model"] = model_mapping[settings.llm_model]
        
        return base_config
    
    @classmethod
    def get_available_providers(cls) -> list:
        """Get list of available providers."""
        return list(cls._providers.keys())
    
    @classmethod
    def register_provider(cls, name: str, provider_class: type):
        """Register a new provider."""
        cls._providers[name.lower()] = provider_class
        logger.info(f"Registered new provider: {name}")
    
    @classmethod
    async def test_provider(cls, settings: Settings) -> bool:
        """Test if the configured provider is working."""
        try:
            provider = cls.create_provider(settings)
            return await provider.health_check()
        except Exception as e:
            logger.error(f"Provider test failed: {e}")
            return False
