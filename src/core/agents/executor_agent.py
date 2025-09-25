"""
Code transformation execution agent.

This agent executes the modernization plan by transforming legacy code
into modern equivalents.
"""

from typing import Dict, Any, List
from src.core.agents.base_agent import BaseAgent
from src.core.state.agent_state import AgentState
from src.core.tools.code_tools import CodeTransformerTool, PatternReplacerTool
from src.core.tools.file_tools import FileWriterTool, BackupTool
from src.utils.logger import get_logger

logger = get_logger(__name__)


class ExecutorAgent(BaseAgent):
    """Agent responsible for executing code transformations."""
    
    def __init__(self, settings):
        """Initialize the executor agent."""
        tools = [
            CodeTransformerTool(),
            PatternReplacerTool(),
            FileWriterTool(),
            BackupTool()
        ]
        super().__init__(settings, tools)
    
    async def run(self, state: AgentState) -> AgentState:
        """Run the executor agent."""
        self.log_activity("Starting code transformation execution")
        
        try:
            # Create backup of original code
            backup_path = await self.use_tool(
                "create_backup",
                source_path=state["codebase_path"],
                backup_location=state["backup_location"]
            )
            
            # Execute transformations according to plan
            transformation_results = []
            for phase in state["modernization_plan"].get("phases", []):
                phase_result = await self.use_tool(
                    "transform_code_phase",
                    phase=phase,
                    source_path=state["codebase_path"],
                    target_language=state["target_language"]
                )
                transformation_results.append(phase_result)
            
            # Apply pattern replacements
            pattern_results = await self.use_tool(
                "apply_pattern_replacements",
                patterns=state["modernization_plan"].get("patterns", []),
                source_path=state["codebase_path"]
            )
            
            # Update state with execution results
            state["transformation_results"] = transformation_results
            state["pattern_results"] = pattern_results
            state["backup_path"] = backup_path
            
            self.log_activity("Code transformation execution completed", {
                "phases_executed": len(transformation_results),
                "patterns_applied": len(pattern_results),
                "backup_created": backup_path
            })
            
        except Exception as e:
            self.logger.error(f"Error in executor agent: {e}")
            state["error"] = str(e)
        
        return state
    
    def get_system_prompt(self) -> str:
        """Get the system prompt for the executor agent."""
        return """
        You are a specialized code transformation execution agent.
        Your role is to:
        1. Execute modernization plans systematically
        2. Transform legacy code patterns to modern equivalents
        3. Apply language-specific transformations
        4. Maintain code functionality during transformation
        5. Create backups and track changes
        
        Focus on accurate, safe transformations that preserve
        the original code's functionality while modernizing its structure.
        """
