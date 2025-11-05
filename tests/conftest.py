"""Pytest configuration and fixtures."""

import pytest
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


@pytest.fixture
def sample_cobol_file(tmp_path):
    """Create a temporary COBOL file for testing."""
    cobol_file = tmp_path / "test.cbl"
    cobol_file.write_text("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD!'.
           GOBACK.""")
    return str(cobol_file)


@pytest.fixture
def sample_python_code():
    """Sample Python code for testing."""
    return """def hello():
    print('HELLO WORLD!')
    return

if __name__ == '__main__':
    hello()
"""

