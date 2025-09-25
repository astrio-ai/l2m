"""
End-to-end tests for complete workflows.

This module contains end-to-end tests for complete modernization
workflows from start to finish.
"""

import pytest
from unittest.mock import Mock, patch, AsyncMock
from pathlib import Path
import tempfile
import shutil

from src.core.graph.main_graph import MainWorkflowGraph
from src.config.settings import Settings


class TestFullWorkflows:
    """Test cases for complete workflows."""
    
    @pytest.fixture
    def settings(self):
        """Create settings fixture."""
        return Settings()
    
    @pytest.fixture
    def temp_codebase(self):
        """Create temporary codebase fixture."""
        temp_dir = tempfile.mkdtemp()
        codebase_dir = Path(temp_dir) / "cobol_codebase"
        codebase_dir.mkdir()
        
        # Create sample COBOL files
        (codebase_dir / "hello.cobol").write_text("""
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO.
PROCEDURE DIVISION.
    DISPLAY 'Hello, World!'.
    STOP RUN.
""")
        
        (codebase_dir / "calculator.cobol").write_text("""
IDENTIFICATION DIVISION.
PROGRAM-ID. CALCULATOR.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 NUM1 PIC 9(5).
01 NUM2 PIC 9(5).
01 RESULT PIC 9(10).
PROCEDURE DIVISION.
    ACCEPT NUM1.
    ACCEPT NUM2.
    ADD NUM1 TO NUM2 GIVING RESULT.
    DISPLAY RESULT.
    STOP RUN.
""")
        
        yield str(codebase_dir)
        
        # Cleanup
        shutil.rmtree(temp_dir)
    
    @pytest.fixture
    def temp_output(self):
        """Create temporary output directory fixture."""
        temp_dir = tempfile.mkdtemp()
        yield str(temp_dir)
        
        # Cleanup
        shutil.rmtree(temp_dir)
    
    @pytest.mark.asyncio
    async def test_complete_modernization_workflow(self, settings, temp_codebase, temp_output):
        """Test complete modernization workflow."""
        with patch("src.core.graph.main_graph.MainWorkflowGraph") as mock_workflow_class:
            # Mock workflow instance
            mock_workflow = Mock()
            mock_workflow.run.return_value = AsyncMock(return_value={
                "status": "completed",
                "result": {
                    "success": True,
                    "files_processed": 2,
                    "files_successful": 2,
                    "files_failed": 0
                }
            })
            mock_workflow_class.return_value = mock_workflow
            
            # Create workflow
            workflow = MainWorkflowGraph(settings)
            
            # Prepare initial state
            initial_state = {
                "codebase_path": temp_codebase,
                "target_language": "python",
                "modernization_goals": ["maintainability", "readability"],
                "backup_location": temp_output,
                "test_framework": "pytest"
            }
            
            # Run workflow
            result = await workflow.run(initial_state)
            
            # Verify results
            assert result is not None
            assert result["status"] == "completed"
            assert result["result"]["success"] is True
            assert result["result"]["files_processed"] == 2
            assert result["result"]["files_successful"] == 2
            assert result["result"]["files_failed"] == 0
    
    @pytest.mark.asyncio
    async def test_workflow_with_errors(self, settings, temp_codebase, temp_output):
        """Test workflow with errors."""
        with patch("src.core.graph.main_graph.MainWorkflowGraph") as mock_workflow_class:
            # Mock workflow instance with error
            mock_workflow = Mock()
            mock_workflow.run.return_value = AsyncMock(return_value={
                "status": "failed",
                "error": "Workflow execution failed",
                "result": {
                    "success": False,
                    "files_processed": 2,
                    "files_successful": 1,
                    "files_failed": 1
                }
            })
            mock_workflow_class.return_value = mock_workflow
            
            # Create workflow
            workflow = MainWorkflowGraph(settings)
            
            # Prepare initial state
            initial_state = {
                "codebase_path": temp_codebase,
                "target_language": "python",
                "modernization_goals": ["maintainability"],
                "backup_location": temp_output
            }
            
            # Run workflow
            result = await workflow.run(initial_state)
            
            # Verify results
            assert result is not None
            assert result["status"] == "failed"
            assert result["error"] == "Workflow execution failed"
            assert result["result"]["success"] is False
            assert result["result"]["files_processed"] == 2
            assert result["result"]["files_successful"] == 1
            assert result["result"]["files_failed"] == 1
    
    @pytest.mark.asyncio
    async def test_workflow_partial_success(self, settings, temp_codebase, temp_output):
        """Test workflow with partial success."""
        with patch("src.core.graph.main_graph.MainWorkflowGraph") as mock_workflow_class:
            # Mock workflow instance with partial success
            mock_workflow = Mock()
            mock_workflow.run.return_value = AsyncMock(return_value={
                "status": "completed",
                "result": {
                    "success": True,
                    "files_processed": 2,
                    "files_successful": 1,
                    "files_failed": 1,
                    "warnings": ["Some files could not be processed"]
                }
            })
            mock_workflow_class.return_value = mock_workflow
            
            # Create workflow
            workflow = MainWorkflowGraph(settings)
            
            # Prepare initial state
            initial_state = {
                "codebase_path": temp_codebase,
                "target_language": "python",
                "modernization_goals": ["maintainability"],
                "backup_location": temp_output
            }
            
            # Run workflow
            result = await workflow.run(initial_state)
            
            # Verify results
            assert result is not None
            assert result["status"] == "completed"
            assert result["result"]["success"] is True
            assert result["result"]["files_processed"] == 2
            assert result["result"]["files_successful"] == 1
            assert result["result"]["files_failed"] == 1
            assert "warnings" in result["result"]
    
    @pytest.mark.asyncio
    async def test_workflow_timeout(self, settings, temp_codebase, temp_output):
        """Test workflow timeout handling."""
        with patch("src.core.graph.main_graph.MainWorkflowGraph") as mock_workflow_class:
            # Mock workflow instance with timeout
            mock_workflow = Mock()
            mock_workflow.run.return_value = AsyncMock(return_value={
                "status": "failed",
                "error": "Workflow timeout",
                "result": {
                    "success": False,
                    "timeout": True
                }
            })
            mock_workflow_class.return_value = mock_workflow
            
            # Create workflow
            workflow = MainWorkflowGraph(settings)
            
            # Prepare initial state
            initial_state = {
                "codebase_path": temp_codebase,
                "target_language": "python",
                "modernization_goals": ["maintainability"],
                "backup_location": temp_output
            }
            
            # Run workflow
            result = await workflow.run(initial_state)
            
            # Verify results
            assert result is not None
            assert result["status"] == "failed"
            assert result["error"] == "Workflow timeout"
            assert result["result"]["success"] is False
            assert result["result"]["timeout"] is True
    
    @pytest.mark.asyncio
    async def test_workflow_cancellation(self, settings, temp_codebase, temp_output):
        """Test workflow cancellation."""
        with patch("src.core.graph.main_graph.MainWorkflowGraph") as mock_workflow_class:
            # Mock workflow instance with cancellation
            mock_workflow = Mock()
            mock_workflow.run.return_value = AsyncMock(return_value={
                "status": "cancelled",
                "error": "Workflow cancelled by user",
                "result": {
                    "success": False,
                    "cancelled": True
                }
            })
            mock_workflow_class.return_value = mock_workflow
            
            # Create workflow
            workflow = MainWorkflowGraph(settings)
            
            # Prepare initial state
            initial_state = {
                "codebase_path": temp_codebase,
                "target_language": "python",
                "modernization_goals": ["maintainability"],
                "backup_location": temp_output
            }
            
            # Run workflow
            result = await workflow.run(initial_state)
            
            # Verify results
            assert result is not None
            assert result["status"] == "cancelled"
            assert result["error"] == "Workflow cancelled by user"
            assert result["result"]["success"] is False
            assert result["result"]["cancelled"] is True
