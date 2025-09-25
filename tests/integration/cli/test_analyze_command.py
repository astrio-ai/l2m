"""
Integration tests for the analyze CLI command.

This module contains integration tests for the analyze command
functionality and behavior.
"""

import pytest
from unittest.mock import Mock, patch, AsyncMock
from click.testing import CliRunner
from src.cli.cli import cli


class TestAnalyzeCommand:
    """Test cases for the analyze CLI command."""
    
    @pytest.fixture
    def runner(self):
        """Create CLI runner fixture."""
        return CliRunner()
    
    @pytest.fixture
    def mock_codebase_path(self, tmp_path):
        """Create mock codebase path fixture."""
        codebase_dir = tmp_path / "test_codebase"
        codebase_dir.mkdir()
        
        # Create some test files
        (codebase_dir / "test1.cobol").write_text("IDENTIFICATION DIVISION.")
        (codebase_dir / "test2.cobol").write_text("IDENTIFICATION DIVISION.")
        
        return str(codebase_dir)
    
    def test_analyze_command_help(self, runner):
        """Test analyze command help."""
        result = runner.invoke(cli, ["analyze", "--help"])
        
        assert result.exit_code == 0
        assert "Analyze a legacy codebase" in result.output
        assert "codebase_path" in result.output
        assert "--output" in result.output
        assert "--format" in result.output
    
    def test_analyze_command_success(self, runner, mock_codebase_path):
        """Test successful analyze command execution."""
        with patch("src.cli.commands.analyze.analyze_command") as mock_analyze:
            mock_analyze.return_value = AsyncMock()
            
            result = runner.invoke(cli, [
                "analyze",
                mock_codebase_path,
                "--output", "/tmp/output.json",
                "--format", "json"
            ])
            
            assert result.exit_code == 0
    
    def test_analyze_command_invalid_path(self, runner):
        """Test analyze command with invalid path."""
        result = runner.invoke(cli, [
            "analyze",
            "/nonexistent/path",
            "--output", "/tmp/output.json"
        ])
        
        assert result.exit_code != 0
        assert "Path" in result.output or "Error" in result.output
    
    def test_analyze_command_missing_path(self, runner):
        """Test analyze command with missing path."""
        result = runner.invoke(cli, ["analyze"])
        
        assert result.exit_code != 0
        assert "Missing argument" in result.output
    
    def test_analyze_command_invalid_format(self, runner, mock_codebase_path):
        """Test analyze command with invalid format."""
        result = runner.invoke(cli, [
            "analyze",
            mock_codebase_path,
            "--format", "invalid"
        ])
        
        assert result.exit_code != 0
        assert "Invalid value" in result.output
    
    def test_analyze_command_output_file(self, runner, mock_codebase_path, tmp_path):
        """Test analyze command with output file."""
        output_file = tmp_path / "output.json"
        
        with patch("src.cli.commands.analyze.analyze_command") as mock_analyze:
            mock_analyze.return_value = AsyncMock()
            
            result = runner.invoke(cli, [
                "analyze",
                mock_codebase_path,
                "--output", str(output_file),
                "--format", "json"
            ])
            
            assert result.exit_code == 0
    
    def test_analyze_command_verbose(self, runner, mock_codebase_path):
        """Test analyze command with verbose output."""
        with patch("src.cli.commands.analyze.analyze_command") as mock_analyze:
            mock_analyze.return_value = AsyncMock()
            
            result = runner.invoke(cli, [
                "--verbose",
                "analyze",
                mock_codebase_path
            ])
            
            assert result.exit_code == 0
    
    def test_analyze_command_config(self, runner, mock_codebase_path, tmp_path):
        """Test analyze command with config file."""
        config_file = tmp_path / "config.yaml"
        config_file.write_text("debug: true\n")
        
        with patch("src.cli.commands.analyze.analyze_command") as mock_analyze:
            mock_analyze.return_value = AsyncMock()
            
            result = runner.invoke(cli, [
                "--config", str(config_file),
                "analyze",
                mock_codebase_path
            ])
            
            assert result.exit_code == 0
    
    def test_analyze_command_error_handling(self, runner, mock_codebase_path):
        """Test analyze command error handling."""
        with patch("src.cli.commands.analyze.analyze_command") as mock_analyze:
            mock_analyze.side_effect = Exception("Analysis error")
            
            result = runner.invoke(cli, [
                "analyze",
                mock_codebase_path
            ])
            
            assert result.exit_code != 0
            assert "Analysis error" in result.output
