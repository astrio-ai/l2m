"""Tests for CLI functionality."""

import pytest
import tempfile
from pathlib import Path
from src.cli.interactive_shell import _expand_path, CLIContext


class TestExpandPath:
    """Tests for _expand_path function."""
    
    def test_expand_path_relative(self):
        """Test expanding relative paths."""
        with tempfile.TemporaryDirectory() as temp_dir:
            base = Path(temp_dir)
            context = CLIContext(cwd=base)
            
            # Test relative path
            result = _expand_path(base, "test.txt")
            expected = base / "test.txt"
            assert result == expected.resolve()
    
    def test_expand_path_absolute(self):
        """Test expanding absolute paths."""
        with tempfile.TemporaryDirectory() as temp_dir:
            base = Path(temp_dir)
            context = CLIContext(cwd=base)
            
            # Test absolute path
            abs_path = base / "absolute.txt"
            result = _expand_path(base, str(abs_path))
            assert result == abs_path.resolve()
    
    def test_expand_path_with_quotes(self):
        """Test expanding paths with quotes."""
        with tempfile.TemporaryDirectory() as temp_dir:
            base = Path(temp_dir)
            context = CLIContext(cwd=base)
            
            # Test with single quotes
            result1 = _expand_path(base, "'test.txt'")
            expected1 = base / "test.txt"
            assert result1 == expected1.resolve()
            
            # Test with double quotes
            result2 = _expand_path(base, '"test.txt"')
            expected2 = base / "test.txt"
            assert result2 == expected2.resolve()
    
    def test_expand_path_user_home(self):
        """Test expanding paths with user home (~)."""
        with tempfile.TemporaryDirectory() as temp_dir:
            base = Path(temp_dir)
            context = CLIContext(cwd=base)
            
            # Test user home expansion
            result = _expand_path(base, "~/test.txt")
            expected = Path.home() / "test.txt"
            assert result == expected.resolve()
    
    def test_expand_path_empty(self):
        """Test expanding empty path."""
        with tempfile.TemporaryDirectory() as temp_dir:
            base = Path(temp_dir)
            context = CLIContext(cwd=base)
            
            # Test empty path
            result = _expand_path(base, "")
            assert result == base
    
    def test_expand_path_windows_drive_letter(self):
        """Test expanding Windows drive letter paths."""
        import os
        if os.name == 'nt':  # Windows
            with tempfile.TemporaryDirectory() as temp_dir:
                base = Path(temp_dir)
                context = CLIContext(cwd=base)
                
                # Test drive letter path (Windows specific)
                drive_path = "C:\\test.txt"
                result = _expand_path(base, drive_path)
                expected = Path(drive_path)
                assert result == expected.resolve()
    
    def test_expand_path_backslashes(self):
        """Test that backslashes in paths are handled correctly."""
        with tempfile.TemporaryDirectory() as temp_dir:
            base = Path(temp_dir)
            context = CLIContext(cwd=base)
            
            # Test path with backslashes (should work on both platforms)
            path_with_backslash = "subdir\\test.txt"
            result = _expand_path(base, path_with_backslash)
            expected = base / "subdir" / "test.txt"
            assert result == expected.resolve()
