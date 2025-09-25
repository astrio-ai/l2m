"""
Main workflow graph for the multi-agent system.

This module defines the primary LangGraph workflow that orchestrates
all agents in the modernization process.
"""

from typing import Dict, Any, List
from langgraph.graph import StateGraph, END
from langgraph.prebuilt import ToolNode
from langchain_core.messages import HumanMessage, AIMessage

from src.core.state.graph_state import GraphState
from src.core.agents.analyzer_agent import AnalyzerAgent
from src.core.agents.planner_agent import PlannerAgent
from src.core.agents.executor_agent import ExecutorAgent
from src.core.agents.reviewer_agent import ReviewerAgent
from src.core.agents.tester_agent import TesterAgent
from src.core.agents.validator_agent import ValidatorAgent
from src.utils.logger import get_logger

logger = get_logger(__name__)


class MainWorkflowGraph:
    """Main workflow graph orchestrating all modernization agents."""
    
    def __init__(self, settings):
        """Initialize the main workflow graph."""
        self.settings = settings
        self.analyzer = AnalyzerAgent(settings)
        self.planner = PlannerAgent(settings)
        self.executor = ExecutorAgent(settings)
        self.reviewer = ReviewerAgent(settings)
        self.tester = TesterAgent(settings)
        self.validator = ValidatorAgent(settings)
        self.graph = self._build_graph()
    
    def _build_graph(self) -> StateGraph:
        """Build the main workflow graph."""
        # Create the state graph
        workflow = StateGraph(GraphState)
        
        # Add nodes for each agent
        workflow.add_node("analyzer", self._run_analyzer)
        workflow.add_node("planner", self._run_planner)
        workflow.add_node("executor", self._run_executor)
        workflow.add_node("reviewer", self._run_reviewer)
        workflow.add_node("tester", self._run_tester)
        workflow.add_node("validator", self._run_validator)
        
        # Define the workflow edges
        workflow.set_entry_point("analyzer")
        workflow.add_edge("analyzer", "planner")
        workflow.add_edge("planner", "executor")
        workflow.add_edge("executor", "reviewer")
        workflow.add_edge("reviewer", "tester")
        workflow.add_edge("tester", "validator")
        workflow.add_edge("validator", END)
        
        return workflow.compile()
    
    def _run_analyzer(self, state: GraphState) -> GraphState:
        """Run the analyzer agent."""
        logger.info("Running analyzer agent")
        try:
            # Convert state to agent state format
            agent_state = self._convert_to_agent_state(state, "analyzer")
            result = self.analyzer.run(agent_state)
            
            # Update state with results
            state["analysis_results"] = result.get("analysis_results")
            state["messages"] = state.get("messages", []) + [
                HumanMessage(content="Analyzing legacy codebase..."),
                AIMessage(content=f"Analysis completed: {len(result.get('analysis_results', {}).get('files', []))} files analyzed")
            ]
            
            logger.info("Analyzer agent completed successfully")
            return state
            
        except Exception as e:
            logger.error(f"Error in analyzer agent: {e}")
            state["error"] = str(e)
            return state
    
    def _run_planner(self, state: GraphState) -> GraphState:
        """Run the planner agent."""
        logger.info("Running planner agent")
        try:
            agent_state = self._convert_to_agent_state(state, "planner")
            result = self.planner.run(agent_state)
            
            # Update state with results
            state["modernization_plan"] = result.get("modernization_plan")
            state["risk_assessment"] = result.get("risk_assessment")
            state["implementation_strategy"] = result.get("implementation_strategy")
            state["messages"] = state.get("messages", []) + [
                HumanMessage(content="Creating modernization plan..."),
                AIMessage(content=f"Plan created with {len(result.get('modernization_plan', {}).get('phases', []))} phases")
            ]
            
            logger.info("Planner agent completed successfully")
            return state
            
        except Exception as e:
            logger.error(f"Error in planner agent: {e}")
            state["error"] = str(e)
            return state
    
    def _run_executor(self, state: GraphState) -> GraphState:
        """Run the executor agent."""
        logger.info("Running executor agent")
        try:
            agent_state = self._convert_to_agent_state(state, "executor")
            result = self.executor.run(agent_state)
            
            # Update state with results
            state["transformation_results"] = result.get("transformation_results")
            state["pattern_results"] = result.get("pattern_results")
            state["backup_path"] = result.get("backup_path")
            state["messages"] = state.get("messages", []) + [
                HumanMessage(content="Executing code transformations..."),
                AIMessage(content=f"Transformations completed: {len(result.get('transformation_results', []))} files transformed")
            ]
            
            logger.info("Executor agent completed successfully")
            return state
            
        except Exception as e:
            logger.error(f"Error in executor agent: {e}")
            state["error"] = str(e)
            return state
    
    def _run_reviewer(self, state: GraphState) -> GraphState:
        """Run the reviewer agent."""
        logger.info("Running reviewer agent")
        try:
            agent_state = self._convert_to_agent_state(state, "reviewer")
            result = self.reviewer.run(agent_state)
            
            # Update state with results
            state["quality_review"] = result.get("quality_review")
            state["standards_analysis"] = result.get("standards_analysis")
            state["test_cases"] = result.get("test_cases")
            state["messages"] = state.get("messages", []) + [
                HumanMessage(content="Reviewing transformed code..."),
                AIMessage(content=f"Review completed with quality score: {result.get('quality_review', {}).get('overall_score', 'N/A')}")
            ]
            
            logger.info("Reviewer agent completed successfully")
            return state
            
        except Exception as e:
            logger.error(f"Error in reviewer agent: {e}")
            state["error"] = str(e)
            return state
    
    def _run_tester(self, state: GraphState) -> GraphState:
        """Run the tester agent."""
        logger.info("Running tester agent")
        try:
            agent_state = self._convert_to_agent_state(state, "tester")
            result = self.tester.run(agent_state)
            
            # Update state with results
            state["test_results"] = result.get("test_results")
            state["coverage_analysis"] = result.get("coverage_analysis")
            state["test_validation"] = result.get("test_validation")
            state["messages"] = state.get("messages", []) + [
                HumanMessage(content="Generating and running tests..."),
                AIMessage(content=f"Tests completed: {result.get('test_results', {}).get('passed', 0)} passed, {result.get('test_results', {}).get('failed', 0)} failed")
            ]
            
            logger.info("Tester agent completed successfully")
            return state
            
        except Exception as e:
            logger.error(f"Error in tester agent: {e}")
            state["error"] = str(e)
            return state
    
    def _run_validator(self, state: GraphState) -> GraphState:
        """Run the validator agent."""
        logger.info("Running validator agent")
        try:
            agent_state = self._convert_to_agent_state(state, "validator")
            result = self.validator.run(agent_state)
            
            # Update state with results
            state["final_validation"] = result.get("final_validation")
            state["compliance_check"] = result.get("compliance_check")
            state["integration_tests"] = result.get("integration_tests")
            state["modernization_success"] = result.get("modernization_success", False)
            state["messages"] = state.get("messages", []) + [
                HumanMessage(content="Performing final validation..."),
                AIMessage(content=f"Validation completed: {'Success' if result.get('modernization_success', False) else 'Failed'}")
            ]
            
            logger.info("Validator agent completed successfully")
            return state
            
        except Exception as e:
            logger.error(f"Error in validator agent: {e}")
            state["error"] = str(e)
            state["modernization_success"] = False
            return state
    
    def _convert_to_agent_state(self, graph_state: GraphState, agent_type: str) -> Dict[str, Any]:
        """Convert graph state to agent state format."""
        return {
            "agent_id": agent_type,
            "agent_type": agent_type,
            "codebase_path": graph_state.get("codebase_path", ""),
            "target_language": graph_state.get("target_language", ""),
            "modernization_goals": graph_state.get("modernization_goals", []),
            "analysis_results": graph_state.get("analysis_results"),
            "modernization_plan": graph_state.get("modernization_plan"),
            "transformation_results": graph_state.get("transformation_results"),
            "quality_review": graph_state.get("quality_review"),
            "test_results": graph_state.get("test_results"),
            "final_validation": graph_state.get("final_validation"),
            "error": graph_state.get("error"),
            "messages": graph_state.get("messages", [])
        }
    
    async def run(self, initial_state: Dict[str, Any] = None):
        """Run the main workflow with the given initial state."""
        if initial_state is None:
            initial_state = {}
        
        logger.info("Starting main workflow")
        
        # Convert initial state to GraphState format
        graph_state = GraphState(**initial_state)
        
        # Run the workflow
        result = await self.graph.ainvoke(graph_state)
        
        logger.info("Main workflow completed")
        return result
