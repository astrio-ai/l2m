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
            DirectoryScannerTool()
        ]
        super().__init__(settings, tools)
    
    async def run(self, state: AgentState) -> AgentState:
        """Run the analyzer agent."""
        self.log_activity("Starting code analysis")
        
        try:
            # Analyze the codebase structure
            structure_analysis = await self.use_tool(
                "analyze_code_structure",
                codebase_path=state["codebase_path"]
            )
            
            # Analyze dependencies
            dependency_analysis = await self.use_tool(
                "analyze_dependencies",
                codebase_path=state["codebase_path"]
            )
            
            # Scan for legacy patterns
            pattern_analysis = await self.use_tool(
                "analyze_legacy_patterns",
                codebase_path=state["codebase_path"]
            )
            
            # Update state with analysis results
            state["analysis_results"] = {
                "structure": structure_analysis,
                "dependencies": dependency_analysis,
                "patterns": pattern_analysis
            }
            
            self.log_activity("Code analysis completed", {
                "files_analyzed": len(structure_analysis.get("files", [])),
                "dependencies_found": len(dependency_analysis.get("dependencies", [])),
                "patterns_identified": len(pattern_analysis.get("patterns", []))
            })
            
        except Exception as e:
            self.logger.error(f"Error in analyzer agent: {e}")
            state["error"] = str(e)
        
        return state
    
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
