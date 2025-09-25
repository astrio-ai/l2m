"""
LLM model configuration settings.

This module provides configuration settings for different
LLM models and providers.
"""

from typing import Dict, Any, List, Optional
from pydantic import BaseModel, Field
from enum import Enum

from src.utils.logger import get_logger

logger = get_logger(__name__)


class LLMProvider(str, Enum):
    """LLM provider enumeration."""
    ANTHROPIC = "anthropic"
    OPENAI = "openai"
    AZURE = "azure"
    LOCAL = "local"


class LLMModel(BaseModel):
    """Configuration for an LLM model."""
    
    name: str = Field(..., description="Model name")
    provider: LLMProvider = Field(..., description="Model provider")
    api_key: Optional[str] = Field(default=None, description="API key for the model")
    base_url: Optional[str] = Field(default=None, description="Base URL for the model")
    temperature: float = Field(default=0.7, description="Model temperature")
    max_tokens: int = Field(default=4000, description="Maximum tokens")
    timeout: int = Field(default=300, description="Request timeout in seconds")
    retry_count: int = Field(default=3, description="Retry count for failed requests")
    options: Dict[str, Any] = Field(default_factory=dict, description="Additional model options")


class AnthropicModel(LLMModel):
    """Configuration for Anthropic models."""
    
    provider: LLMModel = LLMProvider.ANTHROPIC
    base_url: str = "https://api.anthropic.com/v1"
    options: Dict[str, Any] = {
        "max_retries": 3,
        "retry_delay": 1.0,
        "streaming": False
    }


class OpenAIModel(LLMModel):
    """Configuration for OpenAI models."""
    
    provider: LLMModel = LLMProvider.OPENAI
    base_url: str = "https://api.openai.com/v1"
    options: Dict[str, Any] = {
        "max_retries": 3,
        "retry_delay": 1.0,
        "streaming": False
    }


class AzureModel(LLMModel):
    """Configuration for Azure models."""
    
    provider: LLMModel = LLMProvider.AZURE
    base_url: str = "https://your-resource.openai.azure.com/"
    options: Dict[str, Any] = {
        "api_version": "2023-12-01-preview",
        "max_retries": 3,
        "retry_delay": 1.0
    }


class LocalModel(LLMModel):
    """Configuration for local models."""
    
    provider: LLMModel = LLMProvider.LOCAL
    base_url: str = "http://localhost:11434"
    options: Dict[str, Any] = {
        "max_retries": 1,
        "retry_delay": 0.5,
        "streaming": True
    }


class ModelConfigManager:
    """Manager for LLM model configurations."""
    
    def __init__(self):
        """Initialize the model configuration manager."""
        self.models = {
            "claude-3-sonnet": AnthropicModel(
                name="claude-3-sonnet-20240229",
                temperature=0.7,
                max_tokens=4000
            ),
            "claude-3-haiku": AnthropicModel(
                name="claude-3-haiku-20240307",
                temperature=0.7,
                max_tokens=4000
            ),
            "gpt-4": OpenAIModel(
                name="gpt-4",
                temperature=0.7,
                max_tokens=4000
            ),
            "gpt-3.5-turbo": OpenAIModel(
                name="gpt-3.5-turbo",
                temperature=0.7,
                max_tokens=4000
            ),
            "llama2": LocalModel(
                name="llama2",
                temperature=0.7,
                max_tokens=4000
            )
        }
        self.logger = get_logger(__name__)
    
    def get_model(self, model_name: str) -> Optional[LLMModel]:
        """Get configuration for a specific model."""
        return self.models.get(model_name)
    
    def get_all_models(self) -> Dict[str, LLMModel]:
        """Get all model configurations."""
        return self.models
    
    def add_model(self, model_name: str, model: LLMModel):
        """Add a new model configuration."""
        self.models[model_name] = model
        self.logger.info(f"Added model configuration: {model_name}")
    
    def update_model(self, model_name: str, model: LLMModel):
        """Update configuration for a specific model."""
        self.models[model_name] = model
        self.logger.info(f"Updated model configuration: {model_name}")
    
    def get_models_by_provider(self, provider: LLMProvider) -> Dict[str, LLMModel]:
        """Get models by provider."""
        return {
            name: model for name, model in self.models.items()
            if model.provider == provider
        }
    
    def get_default_model(self, provider: LLMProvider) -> Optional[LLMModel]:
        """Get the default model for a provider."""
        models = self.get_models_by_provider(provider)
        if models:
            return list(models.values())[0]
        return None
