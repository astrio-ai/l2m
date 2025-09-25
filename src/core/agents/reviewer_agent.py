"""
Code review agent.

This agent reviews the transformed code for quality, correctness,
and adherence to modern coding standards.
"""

from typing import Dict, Any, List
from src.core.agents.base_agent import BaseAgent
from src.core.state.agent_state import AgentState
from src.core.tools.code_tools import CodeReviewTool, QualityAnalyzerTool
from src.core.tools.test_tools import TestGeneratorTool
from src.utils.logger import get_logger

logger = get_logger(__name__)


class ReviewerAgent(BaseAgent):
    """Agent responsible for reviewing transformed code."""
    
    def __init__(self, settings):
        """Initialize the reviewer agent."""
        tools = [
            CodeReviewTool(),
            QualityAnalyzerTool(),
            TestGeneratorTool()
        ]
        super().__init__(settings, tools)
    
    async def run(self, state: AgentState) -> AgentState:
        """Run the reviewer agent."""
        self.log_activity("Starting code review")
        
        try:
            # Review transformed code quality
            quality_review = await self.use_tool(
                "review_code_quality",
                transformed_code=state.transformation_results,
                original_code=state.analysis_results,
                target_language=state.target_language
            )
            
            # Analyze code standards compliance
            standards_analysis = await self.use_tool(
                "analyze_standards_compliance",
                code=state.transformation_results,
                language=state.target_language
            )
            
            # Generate test cases
            test_cases = await self.use_tool(
                "generate_test_cases",
                transformed_code=state.transformation_results,
                original_functionality=state.analysis_results
            )
            
            # Update state with review results
            state.quality_review = quality_review
            state.standards_analysis = standards_analysis
            state.test_cases = test_cases
            
            self.log_activity("Code review completed", {
                "quality_score": quality_review.get("overall_score", 0),
                "standards_violations": len(standards_analysis.get("violations", [])),
                "test_cases_generated": len(test_cases.get("test_cases", []))
            })
            
        except Exception as e:
            self.logger.error(f"Error in reviewer agent: {e}")
            state.error = str(e)
        
        return state
    
    def get_system_prompt(self) -> str:
        """Get the system prompt for the reviewer agent."""
        return """
        You are a specialized code review agent for modernized code.
        Your role is to:
        1. Review code quality and correctness
        2. Analyze adherence to modern coding standards
        3. Identify potential issues and improvements
        4. Generate comprehensive test cases
        5. Ensure functionality preservation
        
        Focus on maintaining high code quality while ensuring the
        transformed code meets modern development standards.
        """
