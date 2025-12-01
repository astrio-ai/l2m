"""Pytest configuration and fixtures."""

import os
import sys
from pathlib import Path

# Add src to path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root))

# Set test environment variables
os.environ.setdefault("OPENAI_API_KEY", "test-key-for-tests")
os.environ.setdefault("OPENAI_MODEL", "gpt-4o")
os.environ.setdefault("LOG_LEVEL", "WARNING")  # Reduce log noise in tests
os.environ.setdefault("ATLAS_CHECK_UPDATE", "false")
os.environ.setdefault("ATLAS_ANALYTICS", "false")

