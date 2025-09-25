"""
Test generation and execution agent.

This agent generates and executes tests to validate the transformed code
and ensure functionality preservation.
"""

from typing import Dict, Any, List
from src.core.agents.base_agent import BaseAgent
from src.core.state.agent_state import AgentState
from src.core.tools.test_tools import TestRunnerTool, CoverageAnalyzerTool
from src.core.tools.code_tools import TestValidatorTool
from src.utils.logger import get_logger

logger = get_logger(__name__)


class TesterAgent(BaseAgent):
    """Agent responsible for test generation and execution."""
    
    def __init__(self, settings):
        """Initialize the tester agent."""
        tools = [
            TestRunnerTool(),
            CoverageAnalyzerTool(),
            TestValidatorTool()
        ]
        super().__init__(settings, tools)
    
    async def run(self, state: AgentState) -> AgentState:
        """Run the tester agent."""
        self.log_activity("Starting test generation and execution")
        
        try:
            # Run generated test cases
            test_results = await self.use_tool(
                "run_test_cases",
                test_cases=state["test_cases"],
                transformed_code=state["transformation_results"],
                test_framework=state["test_framework"]
            )
            
            # Analyze test coverage
            coverage_analysis = await self.use_tool(
                "analyze_test_coverage",
                test_results=test_results,
                transformed_code=state["transformation_results"]
            )
            
            # Validate test completeness
            test_validation = await self.use_tool(
                "validate_tests",
                test_cases=state["test_cases"],
                original_functionality=state["analysis_results"]
            )
            
            # Update state with test results
            state["test_results"] = test_results
            state["coverage_analysis"] = coverage_analysis
            state["test_validation"] = test_validation
            
            self.log_activity("Test generation and execution completed", {
                "tests_passed": test_results.get("passed", 0),
                "tests_failed": test_results.get("failed", 0),
                "coverage_percentage": coverage_analysis.get("coverage_percentage", 0),
                "validation_score": test_validation.get("completeness_score", 0)
            })
            
        except Exception as e:
            self.logger.error(f"Error in tester agent: {e}")
            state["error"] = str(e)
        
        return state
    
    def get_system_prompt(self) -> str:
        """Get the system prompt for the tester agent."""
        return """
        You are a specialized test generation and execution agent.
        Your role is to:
        1. Generate comprehensive test cases for transformed code
        2. Execute tests and analyze results
        3. Measure test coverage and completeness
        4. Validate functionality preservation
        5. Identify gaps in test coverage
        
        Focus on ensuring the transformed code maintains the same
        functionality as the original through thorough testing.
        """
