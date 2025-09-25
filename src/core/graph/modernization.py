"""
Code modernization workflow.

This module defines the LangGraph workflow for modernizing legacy code,
including planning, execution, and validation steps.
"""

from typing import Dict, Any
from langgraph.graph import StateGraph, END

from src.core.state.graph_state import GraphState
from src.core.agents.analyzer_agent import AnalyzerAgent
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
        analyzer = AnalyzerAgent(self.settings)
        planner = PlannerAgent(self.settings)
        executor = ExecutorAgent(self.settings)
        validator = ValidatorAgent(self.settings)
        
        workflow = StateGraph(GraphState)
        workflow.add_node("analyzer", analyzer.run)
        workflow.add_node("planner", planner.run)
        workflow.add_node("executor", executor.run)
        workflow.add_node("validator", validator.run)
        
        workflow.set_entry_point("analyzer")
        workflow.add_edge("analyzer", "planner")
        workflow.add_edge("planner", "executor")
        workflow.add_edge("executor", "validator")
        workflow.add_edge("validator", END)
        
        return workflow.compile()
    
    async def run(self, initial_state: Dict[str, Any] = None):
        """Run the modernization workflow."""
        if initial_state is None:
            initial_state = {}
        
        # Initialize required state fields
        state = {
            "codebase_path": initial_state.get("codebase_path", ""),
            "target_language": initial_state.get("target_language", "python"),
            "modernization_goals": initial_state.get("modernization_goals", []),
            "backup_location": initial_state.get("backup_location", "/tmp/legacy2modern_backup"),
            "test_framework": initial_state.get("test_framework", "pytest"),
            "analysis_results": None,
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "transformation_results": None,
            "pattern_results": None,
            "backup_path": None,
            "quality_review": None,
            "standards_analysis": None,
            "test_cases": None,
            "test_results": None,
            "coverage_analysis": None,
            "test_validation": None,
            "final_validation": None,
            "compliance_check": None,
            "integration_tests": None,
            "modernization_success": None,
            "error": None,
            "messages": [],
            "workflow_id": None,
            "start_time": None,
            "end_time": None,
            "total_duration": None
        }
        
        logger.info("Starting modernization workflow")
        result = await self.graph.ainvoke(state)
        logger.info("Modernization workflow completed")
        
        return result
