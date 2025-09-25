"""
Modernization planning agent.

This agent creates detailed modernization plans based on the analysis
results from the analyzer agent.
"""

from typing import Dict, Any, List
from src.core.agents.base_agent import BaseAgent
from src.core.state.agent_state import AgentState
from src.core.tools.code_tools import ModernizationPlannerTool, RiskAssessmentTool
from src.core.tools.search_tools import PatternSearchTool, ReferenceFinderTool, CodeDiscoveryTool
from src.core.tools.file_tools import FileReaderTool, DirectoryScannerTool
from src.utils.logger import get_logger

logger = get_logger(__name__)


class PlannerAgent(BaseAgent):
    """Agent responsible for creating modernization plans."""
    
    def __init__(self, settings):
        """Initialize the planner agent."""
        tools = [
            ModernizationPlannerTool(),
            RiskAssessmentTool(),
            PatternSearchTool(),
            ReferenceFinderTool(),
            CodeDiscoveryTool(),
            FileReaderTool(),
            DirectoryScannerTool()
        ]
        super().__init__(settings, tools)
    
    async def run(self, state: AgentState) -> AgentState:
        """Run the planner agent."""
        self.log_activity("Starting modernization planning")
        
        try:
            # Create modernization plan
            modernization_plan = await self.use_tool(
                "create_modernization_plan",
                analysis_results=state["analysis_results"],
                target_language=state["target_language"],
                modernization_goals=state["modernization_goals"]
            )
            
            # Assess risks
            risk_assessment = await self.use_tool(
                "assess_modernization_risks",
                plan=modernization_plan,
                codebase_complexity=state["analysis_results"].get("structure", {})
            )
            
            # Create implementation strategy (using modernization plan as strategy)
            implementation_strategy = {
                "strategy_type": "phased_modernization",
                "phases": modernization_plan.get("phases", []),
                "risks": risk_assessment.get("risks", []),
                "mitigation_plan": risk_assessment.get("mitigation_strategies", [])
            }
            
            # Update state with planning results
            state["modernization_plan"] = modernization_plan
            state["risk_assessment"] = risk_assessment
            state["implementation_strategy"] = implementation_strategy
            
            self.log_activity("Modernization planning completed", {
                "phases": len(modernization_plan.get("phases", [])),
                "risks_identified": len(risk_assessment.get("risks", [])),
                "strategy_components": len(implementation_strategy.get("components", []))
            })
            
        except Exception as e:
            self.logger.error(f"Error in planner agent: {e}")
            state["error"] = str(e)
        
        return state
    
    def get_system_prompt(self) -> str:
        """Get the system prompt for the planner agent."""
        return """
        You are a specialized modernization planning agent for legacy codebases.
        Your role is to:
        1. Create detailed modernization plans based on analysis results
        2. Assess risks and mitigation strategies
        3. Develop implementation strategies
        4. Prioritize modernization tasks
        5. Consider dependencies and integration points
        
        Focus on creating practical, executable plans that minimize risk
        while maximizing modernization benefits.
        """
