"""
Unit tests for file tools.

This module contains unit tests for file operation tools
in the multi-agent system.
"""

import pytest
from unittest.mock import Mock, patch, mock_open
from pathlib import Path
from src.core.tools.file_tools import FileReaderTool, FileWriterTool, DirectoryScannerTool, BackupTool


class TestFileReaderTool:
    """Test cases for the file reader tool."""
    
    @pytest.fixture
    def file_reader(self):
        """Create file reader tool fixture."""
        return FileReaderTool()
    
    @pytest.mark.asyncio
    async def test_read_file_success(self, file_reader):
        """Test successful file reading."""
        with patch("builtins.open", mock_open(read_data="test content")):
            result = await file_reader.run("test.txt")
            assert result == "test content"
    
    @pytest.mark.asyncio
    async def test_read_file_error(self, file_reader):
        """Test file reading error handling."""
        with patch("builtins.open", side_effect=FileNotFoundError("File not found")):
            with pytest.raises(FileNotFoundError):
                await file_reader.run("nonexistent.txt")
    
    def test_validate_parameters(self, file_reader):
        """Test parameter validation."""
        assert file_reader.validate_parameters(file_path="test.txt")
        assert file_reader.validate_parameters(file_path="test.txt", encoding="utf-8")


class TestFileWriterTool:
    """Test cases for the file writer tool."""
    
    @pytest.fixture
    def file_writer(self):
        """Create file writer tool fixture."""
        return FileWriterTool()
    
    @pytest.mark.asyncio
    async def test_write_file_success(self, file_writer):
        """Test successful file writing."""
        with patch("builtins.open", mock_open()) as mock_file:
            with patch("os.makedirs"):
                result = await file_writer.run("test.txt", "test content")
                assert result is True
                mock_file.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_write_file_error(self, file_writer):
        """Test file writing error handling."""
        with patch("builtins.open", side_effect=PermissionError("Permission denied")):
            with pytest.raises(PermissionError):
                await file_writer.run("test.txt", "test content")
    
    def test_validate_parameters(self, file_writer):
        """Test parameter validation."""
        assert file_writer.validate_parameters(file_path="test.txt", content="test")
        assert file_writer.validate_parameters(file_path="test.txt", content="test", encoding="utf-8")


class TestDirectoryScannerTool:
    """Test cases for the directory scanner tool."""
    
    @pytest.fixture
    def directory_scanner(self):
        """Create directory scanner tool fixture."""
        return DirectoryScannerTool()
    
    @pytest.mark.asyncio
    async def test_scan_directory_success(self, directory_scanner):
        """Test successful directory scanning."""
        with patch("pathlib.Path.rglob") as mock_rglob:
            mock_rglob.return_value = [Path("file1.txt"), Path("file2.txt")]
            
            result = await directory_scanner.run("/test/dir", "*.txt", recursive=True)
            assert len(result) == 2
            assert "file1.txt" in result
            assert "file2.txt" in result
    
    @pytest.mark.asyncio
    async def test_scan_directory_error(self, directory_scanner):
        """Test directory scanning error handling."""
        with patch("pathlib.Path.rglob", side_effect=OSError("Directory not found")):
            with pytest.raises(OSError):
                await directory_scanner.run("/nonexistent/dir", "*.txt")
    
    def test_validate_parameters(self, directory_scanner):
        """Test parameter validation."""
        assert directory_scanner.validate_parameters(directory_path="/test/dir")
        assert directory_scanner.validate_parameters(directory_path="/test/dir", pattern="*.txt", recursive=True)


class TestBackupTool:
    """Test cases for the backup tool."""
    
    @pytest.fixture
    def backup_tool(self):
        """Create backup tool fixture."""
        return BackupTool()
    
    @pytest.mark.asyncio
    async def test_create_backup_success(self, backup_tool):
        """Test successful backup creation."""
        with patch("pathlib.Path.exists", return_value=True):
            with patch("pathlib.Path.is_file", return_value=True):
                with patch("shutil.copy2") as mock_copy:
                    result = await backup_tool.run("/test/file.txt", "/backup/")
                    assert result is not None
                    mock_copy.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_create_backup_error(self, backup_tool):
        """Test backup creation error handling."""
        with patch("pathlib.Path.exists", return_value=False):
            with pytest.raises(Exception):
                await backup_tool.run("/nonexistent/file.txt", "/backup/")
    
    def test_validate_parameters(self, backup_tool):
        """Test parameter validation."""
        assert backup_tool.validate_parameters(source_path="/test/file.txt", backup_location="/backup/")
