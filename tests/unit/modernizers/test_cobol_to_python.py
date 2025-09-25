"""
Unit tests for the COBOL to Python modernizer.

This module contains unit tests for the COBOL to Python modernizer
functionality and behavior.
"""

import pytest
from unittest.mock import Mock, patch, AsyncMock
from pathlib import Path
from src.modernizers.cobol_to_python import CobolToPythonModernizer


class TestCobolToPythonModernizer:
    """Test cases for the COBOL to Python modernizer."""
    
    @pytest.fixture
    def modernizer(self):
        """Create modernizer fixture."""
        return CobolToPythonModernizer()
    
    def test_modernizer_initialization(self, modernizer):
        """Test modernizer initialization."""
        assert modernizer is not None
        assert modernizer.source_language == "cobol"
        assert modernizer.target_language == "python"
        assert modernizer.ast_visitor is not None
    
    def test_get_supported_source_extensions(self, modernizer):
        """Test supported source extensions."""
        extensions = modernizer.get_supported_source_extensions()
        assert isinstance(extensions, list)
        assert ".cobol" in extensions
        assert ".cbl" in extensions
        assert ".cob" in extensions
    
    def test_get_target_extension(self, modernizer):
        """Test target extension."""
        extension = modernizer.get_target_extension()
        assert extension == ".py"
    
    def test_validate_source_file(self, modernizer):
        """Test source file validation."""
        # Test with valid extensions
        assert modernizer.validate_source_file("test.cobol")
        assert modernizer.validate_source_file("test.cbl")
        assert modernizer.validate_source_file("test.cob")
        
        # Test with invalid extensions
        assert not modernizer.validate_source_file("test.txt")
        assert not modernizer.validate_source_file("test.py")
    
    @pytest.mark.asyncio
    async def test_modernize_file_success(self, modernizer):
        """Test successful file modernization."""
        with patch.object(modernizer, '_parse_cobol_file') as mock_parse:
            with patch.object(modernizer, '_transform_to_python') as mock_transform:
                with patch.object(modernizer, '_write_python_file') as mock_write:
                    # Mock responses
                    mock_parse.return_value = {"programs": [], "procedures": []}
                    mock_transform.return_value = "def main():\n    pass"
                    mock_write.return_value = None
                    
                    result = await modernizer.modernize_file("test.cobol", "test.py")
                    
                    assert result is not None
                    assert result["success"] is True
                    assert result["source_file"] == "test.cobol"
                    assert result["target_file"] == "test.py"
    
    @pytest.mark.asyncio
    async def test_modernize_file_error(self, modernizer):
        """Test file modernization error handling."""
        with patch.object(modernizer, '_parse_cobol_file', side_effect=Exception("Parse error")):
            result = await modernizer.modernize_file("test.cobol", "test.py")
            
            assert result is not None
            assert result["success"] is False
            assert result["error"] == "Parse error"
    
    @pytest.mark.asyncio
    async def test_modernize_codebase_success(self, modernizer):
        """Test successful codebase modernization."""
        with patch.object(modernizer, '_find_cobol_files') as mock_find:
            with patch.object(modernizer, 'modernize_file') as mock_modernize:
                with patch.object(modernizer, '_generate_project_structure') as mock_generate:
                    # Mock responses
                    mock_find.return_value = ["file1.cobol", "file2.cobol"]
                    mock_modernize.return_value = {"success": True}
                    mock_generate.return_value = None
                    
                    result = await modernizer.modernize_codebase("/test/cobol", "/test/python")
                    
                    assert result is not None
                    assert result["success"] is True
                    assert result["source_codebase"] == "/test/cobol"
                    assert result["target_codebase"] == "/test/python"
                    assert result["files_processed"] == 2
    
    @pytest.mark.asyncio
    async def test_modernize_codebase_error(self, modernizer):
        """Test codebase modernization error handling."""
        with patch.object(modernizer, '_find_cobol_files', side_effect=Exception("Find error")):
            result = await modernizer.modernize_codebase("/test/cobol", "/test/python")
            
            assert result is not None
            assert result["success"] is False
            assert result["error"] == "Find error"
    
    @pytest.mark.asyncio
    async def test_parse_cobol_file(self, modernizer):
        """Test COBOL file parsing."""
        result = await modernizer._parse_cobol_file("test.cobol")
        
        assert result is not None
        assert isinstance(result, dict)
        assert "programs" in result
        assert "procedures" in result
        assert "data_items" in result
    
    @pytest.mark.asyncio
    async def test_transform_to_python(self, modernizer):
        """Test transformation to Python."""
        parsed_structure = {"programs": [], "procedures": []}
        result = await modernizer._transform_to_python(parsed_structure)
        
        assert result is not None
        assert isinstance(result, str)
        assert "def main():" in result
    
    @pytest.mark.asyncio
    async def test_write_python_file(self, modernizer):
        """Test Python file writing."""
        with patch("pathlib.Path.mkdir"):
            with patch("builtins.open", mock_open()) as mock_file:
                await modernizer._write_python_file("test.py", "def main():\n    pass")
                mock_file.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_find_cobol_files(self, modernizer):
        """Test COBOL file finding."""
        with patch("pathlib.Path.rglob") as mock_rglob:
            mock_rglob.return_value = [Path("file1.cobol"), Path("file2.cbl")]
            
            result = await modernizer._find_cobol_files("/test/cobol")
            
            assert len(result) == 2
            assert "file1.cobol" in result
            assert "file2.cbl" in result
    
    @pytest.mark.asyncio
    async def test_generate_project_structure(self, modernizer):
        """Test project structure generation."""
        with patch("pathlib.Path.rglob") as mock_rglob:
            with patch("pathlib.Path.exists") as mock_exists:
                with patch("pathlib.Path.write_text") as mock_write:
                    mock_rglob.return_value = [Path("file1.py")]
                    mock_exists.return_value = False
                    
                    await modernizer._generate_project_structure("/test/python")
                    
                    # Verify that files were created
                    assert mock_write.called
    
    def test_log_modernization_activity(self, modernizer):
        """Test modernization activity logging."""
        # This test would verify that logging works correctly
        # For now, just ensure the method exists and can be called
        modernizer.log_modernization_activity("test activity", {"key": "value"})
        assert True  # If we get here, no exception was raised
