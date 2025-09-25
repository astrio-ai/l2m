"""
Code validation workflow.

This module defines the LangGraph workflow for validating modernized code,
including testing, review, and quality assurance steps.
"""

from typing import Dict, Any
from langgraph.graph import StateGraph, END

from src.core.state.graph_state import GraphState
from src.core.agents.reviewer_agent import ReviewerAgent
from src.core.agents.tester_agent import TesterAgent
from src.core.agents.validator_agent import ValidatorAgent
from src.utils.logger import get_logger

logger = get_logger(__name__)


class ValidationWorkflow:
    """Workflow for validating modernized code."""
    
    def __init__(self, settings):
        """Initialize the validation workflow."""
        self.settings = settings
        self.graph = self._build_graph()
    
    def _build_graph(self) -> StateGraph:
        """Build the validation workflow graph."""
        reviewer = ReviewerAgent(self.settings)
        tester = TesterAgent(self.settings)
        validator = ValidatorAgent(self.settings)
        
        workflow = StateGraph(GraphState)
        workflow.add_node("reviewer", reviewer.run)
        workflow.add_node("tester", tester.run)
        workflow.add_node("validator", validator.run)
        
        workflow.set_entry_point("reviewer")
        workflow.add_edge("reviewer", "tester")
        workflow.add_edge("tester", "validator")
        workflow.add_edge("validator", END)
        
        return workflow.compile()
    
    async def run(self, initial_state: Dict[str, Any] = None):
        """Run the validation workflow."""
        if initial_state is None:
            initial_state = {}
        
        logger.info("Starting validation workflow")
        result = await self.graph.ainvoke(initial_state)
        logger.info("Validation workflow completed")
        
        return result
