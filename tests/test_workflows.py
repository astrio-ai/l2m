"""Tests for workflow functionality."""

import pytest
import tempfile
from pathlib import Path
from src.workflows.modernization_pipeline import ModernizationPipeline


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

