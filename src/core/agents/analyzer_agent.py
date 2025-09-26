"""
Code analysis agent.

This agent specializes in analyzing legacy codebases to understand
their structure, dependencies, and functionality.
"""

from typing import Dict, Any, List
from src.core.agents.base_agent import BaseAgent
from src.core.state.agent_state import AgentState
from src.core.tools.code_tools import CodeAnalyzerTool, DependencyAnalyzerTool
from src.core.tools.file_tools import FileReaderTool, DirectoryScannerTool
from src.core.tools.search_tools import PatternSearchTool, ReferenceFinderTool, CodeDiscoveryTool
from src.core.tools.handoff_tools import transfer_to_planner
from src.utils.logger import get_logger

logger = get_logger(__name__)


class AnalyzerAgent(BaseAgent):
    """Agent responsible for analyzing legacy codebases."""
    
    def __init__(self, settings):
        """Initialize the analyzer agent."""
        tools = [
            CodeAnalyzerTool(),
            DependencyAnalyzerTool(),
            FileReaderTool(),
            DirectoryScannerTool(),
            PatternSearchTool(),
            ReferenceFinderTool(),
            CodeDiscoveryTool(),
            transfer_to_planner
        ]
        super().__init__(settings, tools)
    
    async def run(self, state: AgentState) -> AgentState:
        """Run the analyzer agent."""
        self.log_activity("Starting code analysis")
        
        try:
            # Step 1: Gather raw data using tools
            structure_analysis = await self.use_tool(
                "analyze_code_structure",
                codebase_path=state["codebase_path"]
            )
            
            dependency_analysis = await self.use_tool(
                "analyze_dependencies",
                codebase_path=state["codebase_path"]
            )
            
            # Step 2: Use LLM to provide intelligent analysis and insights
            analysis_prompt = f"""
            Based on the following code analysis data, provide intelligent insights and recommendations:

            STRUCTURE ANALYSIS:
            {structure_analysis}

            DEPENDENCY ANALYSIS:
            {dependency_analysis}

            Please provide:
            1. Key findings about the codebase structure
            2. Legacy patterns and anti-patterns identified
            3. Complexity assessment and modernization challenges
            4. Recommendations for modernization approach
            5. Risk factors and mitigation strategies
            """
            
            # Use LLM to generate intelligent analysis
            from langchain_core.messages import HumanMessage
            messages = [HumanMessage(content=analysis_prompt)]
            llm_response = await self.process_messages(messages)
            
            # Extract LLM insights
            llm_insights = llm_response[0].content if llm_response else "No LLM analysis available"
            
            # Step 3: Prepare analysis results for handoff
            analysis_results = {
                "structure": structure_analysis,
                "dependencies": dependency_analysis,
                "llm_insights": llm_insights,
                "analysis_summary": {
                    "files_analyzed": len(structure_analysis.get("files", [])),
                    "dependencies_found": len(dependency_analysis.get("file_dependencies", [])),
                    "complexity_score": structure_analysis.get("complexity_metrics", {}).get("complexity_score", 0),
                    "modernization_readiness": self._assess_modernization_readiness(structure_analysis, dependency_analysis)
                }
            }
            
            # Step 4: Store analysis results in state
            state["analysis_results"] = analysis_results
            
            self.log_activity("Code analysis completed with LLM insights", {
                "files_analyzed": len(structure_analysis.get("files", [])),
                "dependencies_found": len(dependency_analysis.get("file_dependencies", [])),
                "llm_insights_length": len(llm_insights),
                "modernization_readiness": analysis_results["analysis_summary"]["modernization_readiness"]
            })
            
        except Exception as e:
            self.logger.error(f"Error in analyzer agent: {e}")
            state["error"] = str(e)
        
        return state
    
    def _assess_modernization_readiness(self, structure_analysis: Dict[str, Any], dependency_analysis: Dict[str, Any]) -> str:
        """Assess how ready the codebase is for modernization."""
        complexity_score = structure_analysis.get("complexity_metrics", {}).get("cobol_complexity_score", 0)
        total_files = structure_analysis.get("total_files", 0)
        total_dependencies = len(dependency_analysis.get("dependencies", []))
        
        if complexity_score < 20 and total_files < 5 and total_dependencies < 3:
            return "HIGH - Simple codebase, easy to modernize"
        elif complexity_score < 50 and total_files < 20 and total_dependencies < 10:
            return "MEDIUM - Moderate complexity, requires careful planning"
        else:
            return "LOW - High complexity, requires extensive analysis and phased approach"
    
    def get_system_prompt(self) -> str:
        """Get the system prompt for the analyzer agent."""
        return """
        You are a specialized code analysis agent for legacy codebases.
        Your role is to:
        1. Analyze code structure and organization
        2. Identify dependencies and relationships
        3. Detect legacy patterns and anti-patterns
        4. Understand the codebase's functionality
        5. Provide insights for modernization planning
        
        Focus on understanding the codebase thoroughly before making any
        modernization recommendations.
        """
