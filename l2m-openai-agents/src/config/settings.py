"""
Configuration settings for L2M OpenAI Agents.

Handles model configuration, system constants, and logging setup.
"""

import os
import logging
from pathlib import Path
from typing import Optional
from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    """Application settings loaded from environment variables."""
    
    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        case_sensitive=False,
        extra="ignore"
    )
    
    # OpenAI Configuration
    openai_api_key: str
    openai_model: str = "gpt-4o"
    openai_temperature: float = 0.7
    openai_max_tokens: int = 4096
    
    # Database Configuration
    db_path: str = "./data/sessions.db"
    redis_url: Optional[str] = None
    
    # Logging Configuration
    log_level: str = "INFO"
    log_file: Optional[str] = "./logs/l2m.log"
    
    # Tracing Configuration
    enable_tracing: bool = True
    tracing_provider: Optional[str] = None  # logfire, agentops, braintrust, etc.
    
    # Path Configuration
    output_dir: str = "./data/output"
    backup_dir: str = "./data/backup"
    
    # Agent Configuration
    max_turns: int = 50
    enable_handoffs: bool = True
    
    @property
    def base_dir(self) -> Path:
        """Get the base directory of the project."""
        return Path(__file__).parent.parent.parent
    
    @property
    def output_path(self) -> Path:
        """Get the output directory path."""
        return self.base_dir / self.output_dir
    
    @property
    def backup_path(self) -> Path:
        """Get the backup directory path."""
        return self.base_dir / self.backup_dir


_settings: Optional[Settings] = None


def get_settings() -> Settings:
    """Get or create the global settings instance."""
    global _settings
    if _settings is None:
        _settings = Settings()
        _setup_logging(_settings)
    return _settings


def _setup_logging(settings: Settings) -> None:
    """Configure logging based on settings."""
    log_level = getattr(logging, settings.log_level.upper(), logging.INFO)
    
    # Create logs directory if it doesn't exist
    if settings.log_file:
        log_file_path = Path(settings.log_file)
        log_file_path.parent.mkdir(parents=True, exist_ok=True)
    
    # Configure logging
    logging.basicConfig(
        level=log_level,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        handlers=[
            logging.StreamHandler(),
            *(logging.FileHandler(settings.log_file) if settings.log_file else []),
        ],
    )
    
    logger = logging.getLogger(__name__)
    logger.info(f"Logging configured at level {settings.log_level}")

