"""
Code modernization workflow.

This module defines the LangGraph workflow for modernizing legacy code,
including planning, execution, and validation steps.
"""

from typing import Dict, Any
from langgraph.graph import StateGraph, END

from src.core.state.graph_state import GraphState
from src.core.agents.planner_agent import PlannerAgent
from src.core.agents.executor_agent import ExecutorAgent
from src.core.agents.validator_agent import ValidatorAgent
from src.utils.logger import get_logger

logger = get_logger(__name__)


class ModernizationWorkflow:
    """Workflow for modernizing legacy code."""
    
    def __init__(self, settings):
        """Initialize the modernization workflow."""
        self.settings = settings
        self.graph = self._build_graph()
    
    def _build_graph(self) -> StateGraph:
        """Build the modernization workflow graph."""
        planner = PlannerAgent(self.settings)
        executor = ExecutorAgent(self.settings)
        validator = ValidatorAgent(self.settings)
        
        workflow = StateGraph(GraphState)
        workflow.add_node("planner", planner.run)
        workflow.add_node("executor", executor.run)
        workflow.add_node("validator", validator.run)
        
        workflow.set_entry_point("planner")
        workflow.add_edge("planner", "executor")
        workflow.add_edge("executor", "validator")
        workflow.add_edge("validator", END)
        
        return workflow.compile()
    
    async def run(self, initial_state: Dict[str, Any] = None):
        """Run the modernization workflow."""
        if initial_state is None:
            initial_state = {}
        
        logger.info("Starting modernization workflow")
        result = await self.graph.ainvoke(initial_state)
        logger.info("Modernization workflow completed")
        
        return result
