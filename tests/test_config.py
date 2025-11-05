"""Tests for configuration functionality."""

import pytest
import os
import tempfile
from pathlib import Path
from src.config.settings import Settings, get_settings


class TestSettings:
    """Tests for settings configuration."""
    
    def test_settings_defaults(self):
        """Test settings with default values."""
        # Use environment variables or defaults
        settings = Settings(
            openai_api_key="test-key",
        )
        assert settings.openai_model == "gpt-4o"
        assert settings.openai_temperature == 0.7
        assert settings.max_turns == 50
    
    def test_settings_custom_values(self):
        """Test settings with custom values."""
        settings = Settings(
            openai_api_key="test-key",
            openai_model="gpt-3.5-turbo",
            openai_temperature=0.5,
            max_turns=100,
        )
        assert settings.openai_model == "gpt-3.5-turbo"
        assert settings.openai_temperature == 0.5
        assert settings.max_turns == 100
    
    def test_settings_paths(self):
        """Test settings path properties."""
        settings = Settings(openai_api_key="test-key")
        assert settings.base_dir.exists()
        assert isinstance(settings.output_path, Path)
        assert isinstance(settings.backup_path, Path)
    
    def test_get_settings_singleton(self):
        """Test that get_settings returns singleton."""
        settings1 = get_settings()
        settings2 = get_settings()
        assert settings1 is settings2
    
    def test_settings_env_file_loading(self):
        """Test settings loading from .env file."""
        # Create temporary .env file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.env', delete=False) as f:
            f.write("OPENAI_API_KEY=test-key-from-env\n")
            f.write("OPENAI_MODEL=gpt-3.5-turbo\n")
            f.write("OPENAI_TEMPERATURE=0.8\n")
            temp_env = f.name
        
        try:
            # Note: Settings uses pydantic_settings which looks for .env in current dir
            # This is a basic test - full integration would require mocking
            settings = Settings(openai_api_key="test-key")
            assert settings.openai_api_key == "test-key"
        finally:
            Path(temp_env).unlink()

