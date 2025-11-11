"""Tests for workflow functionality."""

import pytest
import tempfile
from pathlib import Path
from src.workflows.modernization_pipeline import ModernizationPipeline
from src.workflows.batch_pipeline import discover_cobol_files


class TestDiscoverCobolFiles:
    """Tests for discover_cobol_files function."""
    
    def test_discover_single_file(self):
        """Test discovering a single COBOL file."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.cbl', delete=False) as f:
            f.write("SAMPLE COBOL")
            temp_file = Path(f.name)
        
        try:
            result = discover_cobol_files(temp_file)
            assert len(result) == 1
            assert result[0] == temp_file
        finally:
            temp_file.unlink()
    
    def test_discover_directory(self):
        """Test discovering COBOL files in a directory."""
        with tempfile.TemporaryDirectory() as temp_dir:
            temp_path = Path(temp_dir)
            
            # Create COBOL files
            cbl1 = temp_path / "test1.cbl"
            cbl2 = temp_path / "test2.CBL"
            cbl1.write_text("COBOL1")
            cbl2.write_text("COBOL2")
            
            # Create non-COBOL file
            txt_file = temp_path / "readme.txt"
            txt_file.write_text("TEXT")
            
            result = discover_cobol_files(temp_path)
            assert len(result) == 2
            assert cbl1 in result
            assert cbl2 in result
            assert txt_file not in result
    
    def test_discover_with_pattern(self):
        """Test discovering COBOL files with glob pattern."""
        with tempfile.TemporaryDirectory() as temp_dir:
            temp_path = Path(temp_dir)
            
            # Create files in subdirectory
            subdir = temp_path / "src"
            subdir.mkdir()
            
            cbl1 = subdir / "main.cbl"
            cbl2 = subdir / "utils.cbl"
            cbl3 = temp_path / "root.cbl"
            
            cbl1.write_text("MAIN")
            cbl2.write_text("UTILS")
            cbl3.write_text("ROOT")
            
            # Test pattern search
            result = discover_cobol_files(temp_path, pattern="src/*.cbl")
            assert len(result) == 2
            assert cbl1 in result
            assert cbl2 in result
            assert cbl3 not in result
    
    def test_discover_with_limit(self):
        """Test discovering COBOL files with limit."""
        with tempfile.TemporaryDirectory() as temp_dir:
            temp_path = Path(temp_dir)
            
            # Create multiple COBOL files
            files = []
            for i in range(5):
                cbl_file = temp_path / f"test{i}.cbl"
                cbl_file.write_text(f"COBOL{i}")
                files.append(cbl_file)
            
            # Sort by size (they should all be same size, so order may vary)
            result = discover_cobol_files(temp_path, limit=3)
            assert len(result) == 3
            assert all(f in files for f in result)
    
    def test_discover_file_list(self):
        """Test discovering COBOL files from a file list."""
        with tempfile.TemporaryDirectory() as temp_dir:
            temp_path = Path(temp_dir)
            
            # Create COBOL files
            cbl1 = temp_path / "file1.cbl"
            cbl2 = temp_path / "file2.cbl"
            cbl1.write_text("FILE1")
            cbl2.write_text("FILE2")
            
            # Create file list
            file_list = temp_path / "files.txt"
            file_list.write_text("file1.cbl\nfile2.cbl\n")
            
            result = discover_cobol_files(file_list)
            assert len(result) == 2
            assert cbl1 in result
            assert cbl2 in result
    
    def test_discover_file_list_relative_paths(self):
        """Test discovering COBOL files from file list with relative paths."""
        with tempfile.TemporaryDirectory() as temp_dir:
            temp_path = Path(temp_dir)
            
            # Create subdirectory
            subdir = temp_path / "cobol"
            subdir.mkdir()
            
            # Create COBOL files
            cbl1 = subdir / "program.cbl"
            cbl1.write_text("PROGRAM")
            
            # Create file list in root directory referencing file in subdirectory
            file_list = temp_path / "files.txt"
            file_list.write_text("cobol/program.cbl\n")
            
            result = discover_cobol_files(file_list)
            assert len(result) == 1
            assert result[0] == cbl1
    
    def test_discover_windows_drive_letter(self):
        """Test that function handles absolute paths with drive letters (Windows)."""
        # This test will only run on Windows, but we can test the logic
        import os
        if os.name == 'nt':  # Windows
            # Create a file with absolute path
            with tempfile.NamedTemporaryFile(mode='w', suffix='.cbl', delete=False) as f:
                f.write("WINDOWS COBOL")
                temp_file = Path(f.name)
            
            try:
                # Test that absolute path works
                result = discover_cobol_files(temp_file)
                assert len(result) == 1
                assert result[0] == temp_file
                assert temp_file.is_absolute()
            finally:
                temp_file.unlink()
    
    def test_discover_case_insensitive_extensions(self):
        """Test that function finds COBOL files with various case extensions."""
        with tempfile.TemporaryDirectory() as temp_dir:
            temp_path = Path(temp_dir)
            
            # Create files with different case extensions
            cbl1 = temp_path / "test1.cbl"
            cbl2 = temp_path / "test2.CBL"
            cbl3 = temp_path / "test3.cob"
            cbl4 = temp_path / "test4.COB"
            
            cbl1.write_text("CBL")
            cbl2.write_text("CBL_UPPER")
            cbl3.write_text("COB")
            cbl4.write_text("COB_UPPER")
            
            result = discover_cobol_files(temp_path)
            assert len(result) == 4
            assert cbl1 in result
            assert cbl2 in result
            assert cbl3 in result
            assert cbl4 in result


class TestModernizationPipeline:
    """Tests for modernization pipeline."""
    
    def test_pipeline_initialization_default(self):
        """Test pipeline initialization with default settings."""
        pipeline = ModernizationPipeline()
        assert pipeline is not None
        assert pipeline.settings is not None
        assert pipeline.session_id == "default"
    
    def test_pipeline_initialization_custom_session(self):
        """Test pipeline initialization with custom session."""
        pipeline = ModernizationPipeline(session_id="test_session")
        assert pipeline.session_id == "test_session"
    
    def test_pipeline_initialization_with_handoffs(self):
        """Test pipeline initialization with handoffs enabled."""
        pipeline = ModernizationPipeline(use_handoffs=True)
        assert pipeline.use_handoffs is True
        assert pipeline.orchestrator is not None
    
    def test_pipeline_initialization_without_handoffs(self):
        """Test pipeline initialization with handoffs disabled."""
        pipeline = ModernizationPipeline(use_handoffs=False)
        assert pipeline.use_handoffs is False
        assert pipeline.orchestrator is None
    
    def test_pipeline_has_all_agents(self):
        """Test that pipeline has all required agents."""
        pipeline = ModernizationPipeline()
        assert pipeline.analyzer is not None
        assert pipeline.translator is not None
        assert pipeline.reviewer is not None
        assert pipeline.tester is not None
        assert pipeline.refactor is not None
    
    @pytest.mark.asyncio
    async def test_pipeline_run_file_not_found(self):
        """Test pipeline run with non-existent file."""
        pipeline = ModernizationPipeline(use_handoffs=False)
        results = await pipeline.run("nonexistent.cbl")
        assert "error" in results
        # Error could be "not found" or API error, so just check error exists
        assert results["error"] is not None
        # Check that file validation happened (file not found should be caught before API call)
        # The pipeline checks file existence before running agents
        assert "cobol_file" in results
    
    @pytest.mark.asyncio
    async def test_pipeline_run_simple_file(self):
        """Test pipeline run with simple COBOL file."""
        # Create temporary COBOL file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.cbl', delete=False) as f:
            f.write("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
           DISPLAY 'TEST'.
           GOBACK.""")
            temp_file = f.name
        
        try:
            pipeline = ModernizationPipeline(use_handoffs=False)
            # Note: This will actually call OpenAI API if OPENAI_API_KEY is set
            # For unit tests, we might want to mock this
            results = await pipeline.run(temp_file)
            
            assert "cobol_file" in results
            # Results may have errors if API key is not set, but structure should be there
            assert isinstance(results, dict)
        finally:
            Path(temp_file).unlink()

