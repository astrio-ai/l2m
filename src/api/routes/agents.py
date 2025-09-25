"""
Agent management API routes.

This module provides API endpoints for managing and interacting
with the multi-agent system.
"""

from fastapi import APIRouter, HTTPException, Depends
from typing import Dict, Any, List, Optional
from pydantic import BaseModel

from src.core.agents.analyzer_agent import AnalyzerAgent
from src.core.agents.planner_agent import PlannerAgent
from src.core.agents.executor_agent import ExecutorAgent
from src.core.agents.reviewer_agent import ReviewerAgent
from src.core.agents.tester_agent import TesterAgent
from src.core.agents.validator_agent import ValidatorAgent
from src.config.settings import Settings
from src.utils.logger import get_logger

logger = get_logger(__name__)
router = APIRouter()


class AgentRequest(BaseModel):
    """Request model for agent operations."""
    agent_type: str
    parameters: Dict[str, Any]


class AgentResponse(BaseModel):
    """Response model for agent operations."""
    agent_id: str
    agent_type: str
    status: str
    result: Optional[Dict[str, Any]] = None
    error: Optional[str] = None


@router.get("/")
async def list_agents():
    """List all available agents."""
    agents = [
        {"id": "analyzer", "name": "Analyzer Agent", "description": "Analyzes legacy codebases"},
        {"id": "planner", "name": "Planner Agent", "description": "Creates modernization plans"},
        {"id": "executor", "name": "Executor Agent", "description": "Executes code transformations"},
        {"id": "reviewer", "name": "Reviewer Agent", "description": "Reviews transformed code"},
        {"id": "tester", "name": "Tester Agent", "description": "Generates and runs tests"},
        {"id": "validator", "name": "Validator Agent", "description": "Validates final code"}
    ]
    return {"agents": agents}


@router.post("/run")
async def run_agent(request: AgentRequest):
    """Run a specific agent."""
    try:
        # Get agent instance based on type
        agent = _get_agent_instance(request.agent_type)
        
        if not agent:
            raise HTTPException(status_code=404, detail=f"Agent type '{request.agent_type}' not found")
        
        # Run the agent
        result = await agent.run(request.parameters)
        
        return AgentResponse(
            agent_id=request.agent_type,
            agent_type=request.agent_type,
            status="completed",
            result=result
        )
        
    except Exception as e:
        logger.error(f"Error running agent {request.agent_type}: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/{agent_id}/status")
async def get_agent_status(agent_id: str):
    """Get the status of a specific agent."""
    # This would implement actual agent status checking
    # For now, return placeholder status
    return {
        "agent_id": agent_id,
        "status": "idle",
        "last_run": None,
        "next_run": None
    }


@router.get("/{agent_id}/tools")
async def get_agent_tools(agent_id: str):
    """Get the tools available to a specific agent."""
    try:
        agent = _get_agent_instance(agent_id)
        if not agent:
            raise HTTPException(status_code=404, detail=f"Agent '{agent_id}' not found")
        
        tools = agent.get_available_tools()
        return {"agent_id": agent_id, "tools": tools}
        
    except Exception as e:
        logger.error(f"Error getting tools for agent {agent_id}: {e}")
        raise HTTPException(status_code=500, detail=str(e))


def _get_agent_instance(agent_type: str):
    """Get an instance of the specified agent type."""
    settings = Settings()
    
    agent_map = {
        "analyzer": AnalyzerAgent(settings),
        "planner": PlannerAgent(settings),
        "executor": ExecutorAgent(settings),
        "reviewer": ReviewerAgent(settings),
        "tester": TesterAgent(settings),
        "validator": ValidatorAgent(settings)
    }
    
    return agent_map.get(agent_type)
