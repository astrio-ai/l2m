"""
Pytest tests for hello.py ground truth Python equivalent.

Tests verify the functionality matches the COBOL program behavior.
"""

import pytest
import sys
import io
from contextlib import redirect_stdout
from pathlib import Path

# Add parent directory to path to import hello
test_dir = Path(__file__).parent
sys.path.insert(0, str(test_dir))

# Import with explicit file name handling
import importlib.util
spec = importlib.util.spec_from_file_location("hello", test_dir / "hello.py")
hello_module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(hello_module)

# Import function from the loaded module
hello = hello_module.hello


class TestHelloFunction:
    """Tests for hello() function."""
    
    def test_hello_output(self):
        """Test that hello() prints the correct message."""
        f = io.StringIO()
        with redirect_stdout(f):
            hello()
        
        output = f.getvalue().strip()
        assert output == 'HELLO WORLD!'
    
    def test_hello_returns_none(self):
        """Test that hello() returns None."""
        f = io.StringIO()
        with redirect_stdout(f):
            result = hello()
        
        assert result is None
    
    def test_hello_multiple_calls(self):
        """Test calling hello() multiple times."""
        f = io.StringIO()
        with redirect_stdout(f):
            hello()
            hello()
            hello()
        
        output = f.getvalue().strip()
        lines = output.split('\n')
        assert len(lines) == 3
        assert all(line == 'HELLO WORLD!' for line in lines)


class TestMainExecution:
    """Tests for main execution."""
    
    def test_main_execution(self):
        """Test that the module can be executed as main."""
        import subprocess
        import sys
        
        # Run the script and capture output
        result = subprocess.run(
            [sys.executable, str(test_dir / "hello.py")],
            capture_output=True,
            text=True
        )
        
        assert result.returncode == 0
        assert result.stdout.strip() == 'HELLO WORLD!'
        assert result.stderr == ''


class TestIntegration:
    """Integration tests."""
    
    def test_hello_functionality(self):
        """Test complete hello functionality."""
        f = io.StringIO()
        with redirect_stdout(f):
            hello()
        
        output = f.getvalue().strip()
        
        # Verify output matches expected COBOL behavior
        assert output == 'HELLO WORLD!'
        assert len(output) == 12  # Length of "HELLO WORLD!"
        assert output.startswith('HELLO')
        assert output.endswith('!')

