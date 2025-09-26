"""
Modernization workflow using LangGraph.

This module implements a multi-agent system where agents work together
to modernize legacy code using structured handoffs.
"""

from typing import Dict, Any, List
from langgraph.graph import StateGraph, END
from src.core.state.graph_state import GraphState
from src.core.agents.analyzer_agent import AnalyzerAgent
from src.core.agents.planner_agent import PlannerAgent
from src.core.agents.executor_agent import ExecutorAgent
from src.core.agents.reviewer_agent import ReviewerAgent
from src.core.agents.validator_agent import ValidatorAgent
from src.utils.logger import get_logger

logger = get_logger(__name__)


class ModernizationWorkflow:
    """Modernization workflow with structured agent handoffs."""
    
    def __init__(self, settings):
        """Initialize the modernization workflow."""
        self.settings = settings
        # Create agent instances
        self.analyzer = AnalyzerAgent(settings)
        self.planner = PlannerAgent(settings)
        self.executor = ExecutorAgent(settings)
        self.reviewer = ReviewerAgent(settings)
        self.validator = ValidatorAgent(settings)
        self.graph = self._build_graph()
    
    def _build_graph(self) -> StateGraph:
        """Build the modernization workflow graph."""
        # Build the workflow graph
        workflow = StateGraph(GraphState)
        
        # Add nodes with wrapper functions that handle state updates
        workflow.add_node("analyzer", self._run_analyzer)
        workflow.add_node("planner", self._run_planner)
        workflow.add_node("executor", self._run_executor)
        workflow.add_node("reviewer", self._run_reviewer)
        workflow.add_node("validator", self._run_validator)
        
        # Set entry point
        workflow.set_entry_point("analyzer")
        
        # Add edges for the workflow
        workflow.add_edge("analyzer", "planner")
        workflow.add_edge("planner", "executor")
        workflow.add_edge("executor", "reviewer")
        workflow.add_edge("reviewer", "validator")
        workflow.add_edge("validator", END)
        
        return workflow.compile()
    
    async def _run_analyzer(self, state: GraphState) -> GraphState:
        """Run the analyzer agent with proper state handling."""
        logger.info("Running analyzer agent")
        try:
            # Convert GraphState to AgentState format
            agent_state = {
                "codebase_path": state.get("codebase_path", ""),
                "target_language": state.get("target_language", ""),
                "modernization_goals": state.get("modernization_goals", []),
                "analysis_results": state.get("analysis_results"),
                "error": state.get("error"),
                "messages": state.get("messages", [])
            }
            
            # Run the analyzer agent
            result = await self.analyzer.run(agent_state)
            
            # Update state with results
            state["analysis_results"] = result.get("analysis_results")
            state["error"] = result.get("error")
            state["messages"] = result.get("messages", state.get("messages", []))
            
            logger.info("Analyzer agent completed successfully")
            return state
            
        except Exception as e:
            logger.error(f"Error in analyzer agent: {e}")
            state["error"] = str(e)
            return state
    
    async def _run_planner(self, state: GraphState) -> GraphState:
        """Run the planner agent with proper state handling."""
        logger.info("Running planner agent")
        try:
            # Convert GraphState to AgentState format
            agent_state = {
                "codebase_path": state.get("codebase_path", ""),
                "target_language": state.get("target_language", ""),
                "modernization_goals": state.get("modernization_goals", []),
                "analysis_results": state.get("analysis_results"),
                "error": state.get("error"),
                "messages": state.get("messages", [])
            }
            
            # Run the planner agent
            result = await self.planner.run(agent_state)
            
            # Update state with results
            state["modernization_plan"] = result.get("modernization_plan")
            state["risk_assessment"] = result.get("risk_assessment")
            state["transformation_plan"] = result.get("transformation_plan")
            state["llm_transformation_rules"] = result.get("llm_transformation_rules")
            state["planning_summary"] = result.get("planning_summary")
            state["handoff_to_executor"] = result.get("handoff_to_executor")
            state["error"] = result.get("error")
            state["messages"] = result.get("messages", state.get("messages", []))
            
            logger.info("Planner agent completed successfully")
            return state
            
        except Exception as e:
            logger.error(f"Error in planner agent: {e}")
            state["error"] = str(e)
            return state
    
    async def _run_executor(self, state: GraphState) -> GraphState:
        """Run the executor agent with proper state handling."""
        logger.info("Running executor agent")
        try:
            # Convert GraphState to AgentState format
            agent_state = {
                "codebase_path": state.get("codebase_path", ""),
                "target_language": state.get("target_language", ""),
                "modernization_goals": state.get("modernization_goals", []),
                "analysis_results": state.get("analysis_results"),
                "transformation_plan": state.get("transformation_plan"),
                "handoff_to_executor": state.get("handoff_to_executor"),
                "backup_location": state.get("backup_location"),
                "error": state.get("error"),
                "messages": state.get("messages", [])
            }
            
            # Run the executor agent
            result = await self.executor.run(agent_state)
            
            # Update state with results
            state["transformation_results"] = result.get("transformation_results")
            state["pattern_results"] = result.get("pattern_results")
            state["backup_path"] = result.get("backup_path")
            state["error"] = result.get("error")
            state["messages"] = result.get("messages", state.get("messages", []))
            
            logger.info("Executor agent completed successfully")
            return state
            
        except Exception as e:
            logger.error(f"Error in executor agent: {e}")
            state["error"] = str(e)
            return state
    
    async def _run_reviewer(self, state: GraphState) -> GraphState:
        """Run the reviewer agent with proper state handling."""
        logger.info("Running reviewer agent")
        try:
            # Convert GraphState to AgentState format
            agent_state = {
                "codebase_path": state.get("codebase_path", ""),
                "target_language": state.get("target_language", ""),
                "modernization_goals": state.get("modernization_goals", []),
                "analysis_results": state.get("analysis_results"),
                "transformation_results": state.get("transformation_results"),
                "error": state.get("error"),
                "messages": state.get("messages", [])
            }
            
            # Run the reviewer agent
            result = await self.reviewer.run(agent_state)
            
            # Update state with results
            state["quality_review"] = result.get("quality_review")
            state["standards_analysis"] = result.get("standards_analysis")
            state["test_cases"] = result.get("test_cases")
            state["error"] = result.get("error")
            state["messages"] = result.get("messages", state.get("messages", []))
            
            logger.info("Reviewer agent completed successfully")
            return state
            
        except Exception as e:
            logger.error(f"Error in reviewer agent: {e}")
            state["error"] = str(e)
            return state
    
    async def _run_validator(self, state: GraphState) -> GraphState:
        """Run the validator agent with proper state handling."""
        logger.info("Running validator agent")
        try:
            # Convert GraphState to AgentState format
            agent_state = {
                "codebase_path": state.get("codebase_path", ""),
                "target_language": state.get("target_language", ""),
                "modernization_goals": state.get("modernization_goals", []),
                "analysis_results": state.get("analysis_results"),
                "transformation_results": state.get("transformation_results"),
                "test_cases": state.get("test_cases"),
                "error": state.get("error"),
                "messages": state.get("messages", [])
            }
            
            # Run the validator agent
            result = await self.validator.run(agent_state)
            
            # Update state with results
            state["final_validation"] = result.get("final_validation")
            state["compliance_check"] = result.get("compliance_check")
            state["integration_tests"] = result.get("integration_tests")
            state["modernization_success"] = result.get("modernization_success")
            state["workflow_id"] = result.get("workflow_id")
            state["start_time"] = result.get("start_time")
            state["end_time"] = result.get("end_time")
            state["total_duration"] = result.get("total_duration")
            state["error"] = result.get("error")
            state["messages"] = result.get("messages", state.get("messages", []))
            
            logger.info("Validator agent completed successfully")
            return state
            
        except Exception as e:
            logger.error(f"Error in validator agent: {e}")
            state["error"] = str(e)
            return state
    
    async def run(self, codebase_path: str, target_language: str = "python", 
                  modernization_goals: List[str] = None) -> Dict[str, Any]:
        """Run the modernization workflow."""
        if modernization_goals is None:
            modernization_goals = []
        
        try:
            # Initialize state with start time
            from datetime import datetime
            start_time = datetime.utcnow()
            
            state = {
                "codebase_path": codebase_path,
                "target_language": target_language,
                "modernization_goals": modernization_goals,
                "backup_location": "/tmp/legacy2modern_backup",
                "test_framework": "pytest",
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
                "start_time": start_time,
                "end_time": None,
                "total_duration": None
            }
            
            logger.info("Starting modernization workflow")
            result = await self.graph.ainvoke(state)
            logger.info("Modernization workflow completed")
            
            return result
            
        except Exception as e:
            logger.error(f"Error in modernization workflow: {e}")
            return {
                "error": str(e),
                "modernization_success": False
            }