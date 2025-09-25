"""
Workflow management API routes.

This module provides API endpoints for managing and executing
LangGraph workflows.
"""

from fastapi import APIRouter, HTTPException, BackgroundTasks
from typing import Dict, Any, List, Optional
from pydantic import BaseModel

from src.core.graph.main_graph import MainWorkflowGraph
from src.core.graph.legacy_analysis import LegacyAnalysisWorkflow
from src.core.graph.modernization import ModernizationWorkflow
from src.core.graph.validation import ValidationWorkflow
from src.config.settings import Settings
from src.utils.logger import get_logger

logger = get_logger(__name__)
router = APIRouter()


class WorkflowRequest(BaseModel):
    """Request model for workflow operations."""
    workflow_type: str
    parameters: Dict[str, Any]


class WorkflowResponse(BaseModel):
    """Response model for workflow operations."""
    workflow_id: str
    workflow_type: str
    status: str
    result: Optional[Dict[str, Any]] = None
    error: Optional[str] = None


@router.get("/")
async def list_workflows():
    """List all available workflows."""
    workflows = [
        {
            "id": "main",
            "name": "Main Workflow",
            "description": "Complete modernization workflow with all agents"
        },
        {
            "id": "legacy_analysis",
            "name": "Legacy Analysis Workflow",
            "description": "Analyze legacy codebases"
        },
        {
            "id": "modernization",
            "name": "Modernization Workflow",
            "description": "Transform legacy code to modern equivalents"
        },
        {
            "id": "validation",
            "name": "Validation Workflow",
            "description": "Validate modernized code"
        }
    ]
    return {"workflows": workflows}


@router.post("/run")
async def run_workflow(request: WorkflowRequest, background_tasks: BackgroundTasks):
    """Run a specific workflow."""
    try:
        # Get workflow instance based on type
        workflow = _get_workflow_instance(request.workflow_type)
        
        if not workflow:
            raise HTTPException(status_code=404, detail=f"Workflow type '{request.workflow_type}' not found")
        
        # Run the workflow
        result = await workflow.run(request.parameters)
        
        return WorkflowResponse(
            workflow_id=request.workflow_type,
            workflow_type=request.workflow_type,
            status="completed",
            result=result
        )
        
    except Exception as e:
        logger.error(f"Error running workflow {request.workflow_type}: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/run/async")
async def run_workflow_async(request: WorkflowRequest, background_tasks: BackgroundTasks):
    """Run a workflow asynchronously."""
    try:
        # Get workflow instance based on type
        workflow = _get_workflow_instance(request.workflow_type)
        
        if not workflow:
            raise HTTPException(status_code=404, detail=f"Workflow type '{request.workflow_type}' not found")
        
        # Add workflow to background tasks
        background_tasks.add_task(workflow.run, request.parameters)
        
        return WorkflowResponse(
            workflow_id=request.workflow_type,
            workflow_type=request.workflow_type,
            status="running",
            result=None
        )
        
    except Exception as e:
        logger.error(f"Error running workflow {request.workflow_type}: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/{workflow_id}/status")
async def get_workflow_status(workflow_id: str):
    """Get the status of a specific workflow."""
    # This would implement actual workflow status checking
    # For now, return placeholder status
    return {
        "workflow_id": workflow_id,
        "status": "idle",
        "last_run": None,
        "next_run": None
    }


def _get_workflow_instance(workflow_type: str):
    """Get an instance of the specified workflow type."""
    settings = Settings()
    
    workflow_map = {
        "main": MainWorkflowGraph(settings),
        "legacy_analysis": LegacyAnalysisWorkflow(settings),
        "modernization": ModernizationWorkflow(settings),
        "validation": ValidationWorkflow(settings)
    }
    
    return workflow_map.get(workflow_type)
