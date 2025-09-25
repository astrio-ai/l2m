"""
Legacy code analysis workflow.

This module defines the LangGraph workflow specifically for analyzing
legacy codebases and understanding their structure and functionality.
"""

from typing import Dict, Any
from langgraph.graph import StateGraph, END
from langchain_core.messages import HumanMessage, AIMessage

from src.core.state.graph_state import GraphState
from src.core.agents.analyzer_agent import AnalyzerAgent
from src.utils.logger import get_logger

logger = get_logger(__name__)


class LegacyAnalysisWorkflow:
    """Workflow for analyzing legacy codebases."""
    
    def __init__(self, settings):
        """Initialize the legacy analysis workflow."""
        self.settings = settings
        self.analyzer = AnalyzerAgent(settings)
        self.graph = self._build_graph()
    
    def _build_graph(self) -> StateGraph:
        """Build the legacy analysis workflow graph."""
        workflow = StateGraph(GraphState)
        workflow.add_node("analyzer", self._run_analyzer)
        workflow.set_entry_point("analyzer")
        workflow.add_edge("analyzer", END)
        
        return workflow.compile()
    
    def _run_analyzer(self, state: GraphState) -> GraphState:
        """Run the analyzer agent."""
        logger.info("Running analyzer agent for legacy analysis")
        try:
            # Convert state to agent state format
            agent_state = {
                "agent_id": "analyzer",
                "agent_type": "analyzer",
                "codebase_path": state.get("codebase_path", ""),
                "target_language": state.get("target_language", ""),
                "modernization_goals": state.get("modernization_goals", []),
                "analysis_results": state.get("analysis_results"),
                "error": state.get("error"),
                "messages": state.get("messages", [])
            }
            
            result = self.analyzer.run(agent_state)
            
            # Update state with results
            state["analysis_results"] = result.get("analysis_results")
            state["messages"] = state.get("messages", []) + [
                HumanMessage(content="Analyzing legacy codebase structure and functionality..."),
                AIMessage(content=f"Analysis completed: {len(result.get('analysis_results', {}).get('files', []))} files analyzed")
            ]
            
            logger.info("Analyzer agent completed successfully")
            return state
            
        except Exception as e:
            logger.error(f"Error in analyzer agent: {e}")
            state["error"] = str(e)
            return state
    
    async def run(self, initial_state: Dict[str, Any] = None):
        """Run the legacy analysis workflow."""
        if initial_state is None:
            initial_state = {}
        
        logger.info("Starting legacy analysis workflow")
        
        # Convert initial state to GraphState format
        graph_state = GraphState(**initial_state)
        
        # Run the workflow
        result = await self.graph.ainvoke(graph_state)
        
        logger.info("Legacy analysis workflow completed")
        return result
