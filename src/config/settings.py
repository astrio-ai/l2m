"""
Application settings and configuration.

This module provides the main settings class for the
multi-agent modernization system.
"""

import os
from typing import List, Optional, Dict, Any
from pathlib import Path
from pydantic import Field, ConfigDict
from pydantic_settings import BaseSettings


class Settings(BaseSettings):
    """Application settings."""
    
    # Application settings
    app_name: str = Field(default="Legacy2Modern", description="Application name")
    app_version: str = Field(default="2.0.0", description="Application version")
    debug: bool = Field(default=False, description="Debug mode")
    
    # API settings
    api_host: str = Field(default="0.0.0.0", description="API host")
    api_port: int = Field(default=8000, description="API port")
    api_key: Optional[str] = Field(default=None, description="API key for authentication")
    allowed_origins: List[str] = Field(default=["*"], description="Allowed CORS origins")
    allowed_hosts: List[str] = Field(default=["*"], description="Allowed hosts")
    
    # Database settings
    database_url: Optional[str] = Field(default=None, description="Database URL")
    database_pool_size: int = Field(default=10, description="Database pool size")
    
    # LLM settings
    llm_provider: str = Field(default="anthropic", description="LLM provider")
    llm_model: str = Field(default="claude-3-sonnet", description="LLM model")
    llm_api_key: Optional[str] = Field(default=None, description="LLM API key")
    llm_temperature: float = Field(default=0.7, description="LLM temperature")
    llm_max_tokens: int = Field(default=4000, description="LLM max tokens")
    
    # Specific API keys
    anthropic_api_key: Optional[str] = Field(default=None, alias="ANTHROPIC_API_KEY", description="Anthropic API key")
    openai_api_key: Optional[str] = Field(default=None, alias="OPENAI_API_KEY", description="OpenAI API key")
    
    # Agent settings
    agent_timeout: int = Field(default=300, description="Agent timeout in seconds")
    agent_retry_count: int = Field(default=3, description="Agent retry count")
    agent_parallel_limit: int = Field(default=5, description="Agent parallel limit")
    
    # Workflow settings
    workflow_timeout: int = Field(default=3600, description="Workflow timeout in seconds")
    workflow_retry_count: int = Field(default=2, description="Workflow retry count")
    workflow_parallel_limit: int = Field(default=3, description="Workflow parallel limit")
    
    # File settings
    max_file_size: int = Field(default=100 * 1024 * 1024, description="Max file size in bytes")
    supported_extensions: List[str] = Field(
        default=[".cobol", ".cbl", ".cob", ".f", ".f90", ".pas", ".asm"],
        description="Supported file extensions"
    )
    
    # Output settings
    output_directory: str = Field(default="output", description="Output directory")
    backup_directory: str = Field(default="backup", description="Backup directory")
    log_directory: str = Field(default="logs", description="Log directory")
    
    # Logging settings
    log_level: str = Field(default="INFO", description="Log level")
    log_format: str = Field(default="%(asctime)s - %(name)s - %(levelname)s - %(message)s", description="Log format")
    log_file: Optional[str] = Field(default=None, description="Log file path")
    
    # Security settings
    secret_key: Optional[str] = Field(default=None, description="Secret key for encryption")
    jwt_secret: Optional[str] = Field(default=None, description="JWT secret key")
    jwt_algorithm: str = Field(default="HS256", description="JWT algorithm")
    jwt_expiration: int = Field(default=3600, description="JWT expiration in seconds")
    
    model_config = ConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        case_sensitive=False,
        extra="ignore",
        populate_by_name=True
    )
    
    def load_from_file(self, config_file: str):
        """Load settings from a configuration file."""
        try:
            config_path = Path(config_file)
            if config_path.exists():
                # This would implement actual configuration loading
                # For now, just return success
                return True
            else:
                return False
        except Exception as e:
            return False
    
    def get_llm_config(self) -> Dict[str, Any]:
        """Get LLM configuration."""
        return {
            "provider": self.llm_provider,
            "model": self.llm_model,
            "api_key": self.llm_api_key,
            "temperature": self.llm_temperature,
            "max_tokens": self.llm_max_tokens
        }
    
    def get_agent_config(self) -> Dict[str, Any]:
        """Get agent configuration."""
        return {
            "timeout": self.agent_timeout,
            "retry_count": self.agent_retry_count,
            "parallel_limit": self.agent_parallel_limit
        }
    
    def get_workflow_config(self) -> Dict[str, Any]:
        """Get workflow configuration."""
        return {
            "timeout": self.workflow_timeout,
            "retry_count": self.workflow_retry_count,
            "parallel_limit": self.workflow_parallel_limit
        }
