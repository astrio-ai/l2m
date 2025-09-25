"""
Code validation agent.

This agent performs final validation of the modernized code,
ensuring it meets all requirements and standards.
"""

from typing import Dict, Any, List
from src.core.agents.base_agent import BaseAgent
from src.core.state.agent_state import AgentState
from src.core.tools.code_tools import FinalValidatorTool, ComplianceCheckerTool
from src.core.tools.test_tools import IntegrationTestTool
from src.utils.logger import get_logger

logger = get_logger(__name__)


class ValidatorAgent(BaseAgent):
    """Agent responsible for final code validation."""
    
    def __init__(self, settings):
        """Initialize the validator agent."""
        tools = [
            FinalValidatorTool(),
            ComplianceCheckerTool(),
            IntegrationTestTool()
        ]
        super().__init__(settings, tools)
    
    async def run(self, state: AgentState) -> AgentState:
        """Run the validator agent."""
        self.log_activity("Starting final validation")
        
        try:
            # Perform final validation
            final_validation = await self.use_tool(
                "final_validation",
                transformed_code=state["transformation_results"],
                test_results=state["test_results"],
                quality_review=state["quality_review"]
            )
            
            # Check compliance with requirements
            compliance_check = await self.use_tool(
                "check_compliance",
                code=state["transformation_results"],
                requirements=state["modernization_goals"],
                standards=state["target_language"]
            )
            
            # Run integration tests
            integration_tests = await self.use_tool(
                "run_integration_tests",
                transformed_code=state["transformation_results"],
                test_cases=state["test_cases"]
            )
            
            # Update state with validation results
            state["final_validation"] = final_validation
            state["compliance_check"] = compliance_check
            state["integration_tests"] = integration_tests
            
            # Determine overall success
            state["modernization_success"] = (
                final_validation.get("passed", False) and
                compliance_check.get("compliant", False) and
                integration_tests.get("passed", False)
            )
            
            self.log_activity("Final validation completed", {
                "validation_passed": final_validation.get("passed", False),
                "compliance_met": compliance_check.get("compliant", False),
                "integration_tests_passed": integration_tests.get("passed", False),
                "overall_success": state["modernization_success"]
            })
            
        except Exception as e:
            self.logger.error(f"Error in validator agent: {e}")
            state["error"] = str(e)
            state["modernization_success"] = False
        
        return state
    
    def get_system_prompt(self) -> str:
        """Get the system prompt for the validator agent."""
        return """
        You are a specialized code validation agent for modernized code.
        Your role is to:
        1. Perform final validation of transformed code
        2. Check compliance with requirements and standards
        3. Run integration tests to ensure system functionality
        4. Validate overall modernization success
        5. Provide final quality assessment
        
        Focus on ensuring the modernized code meets all requirements
        and maintains the same functionality as the original.
        """
