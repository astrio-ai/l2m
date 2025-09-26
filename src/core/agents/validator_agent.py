"""
Code validation agent.

This agent performs final validation of the modernized code,
ensuring it meets all requirements and standards.
"""

from typing import Dict, Any, List
from src.core.agents.base_agent import BaseAgent
from src.core.state.agent_state import AgentState
from src.core.tools.code_tools import FinalValidatorTool, ComplianceCheckerTool
from src.core.tools.test_tools import IntegrationTestTool, TestRunnerTool, CoverageAnalyzerTool, TestValidatorTool
from src.core.tools.file_tools import FileReaderTool, FileWriterTool, DirectoryScannerTool
from src.core.tools.search_tools import PatternSearchTool, ReferenceFinderTool, CodeDiscoveryTool
from src.utils.logger import get_logger

logger = get_logger(__name__)


class ValidatorAgent(BaseAgent):
    """Agent responsible for final code validation."""
    
    def __init__(self, settings):
        """Initialize the validator agent."""
        tools = [
            FinalValidatorTool(),
            ComplianceCheckerTool(),
            IntegrationTestTool(),
            TestRunnerTool(),
            CoverageAnalyzerTool(),
            TestValidatorTool(),
            FileReaderTool(),
            FileWriterTool(),
            DirectoryScannerTool(),
            PatternSearchTool(),
            ReferenceFinderTool(),
            CodeDiscoveryTool()
        ]
        super().__init__(settings, tools)
    
    async def run(self, state: AgentState) -> AgentState:
        """Run the validator agent with AI-powered final validation."""
        self.log_activity("Starting AI-powered final validation")
        
        try:
            # Step 1: Gather raw validation data using tools
            transformation_results = state.get("transformation_results", [])
            
            # Ensure transformation_results is a list
            if not isinstance(transformation_results, list):
                self.logger.warning(f"transformation_results is not a list, got {type(transformation_results)}")
                transformation_results = []
            
            # Format transformation_results for the tools
            modernized_code = {
                "files": [{"path": path, "type": "python"} for path in transformation_results],
                "transformations": transformation_results,
                "success": len(transformation_results) > 0
            }
            
            final_validation = await self.use_tool(
                "final_validation",
                modernized_code=modernized_code,
                validation_criteria=state.get("modernization_goals", [])
            )
            
            compliance_check = await self.use_tool(
                "check_compliance",
                code_path=state.get("codebase_path", ""),
                standards=[state.get("target_language", "python")]
            )
            
            integration_tests = await self.use_tool(
                "run_integration_tests",
                transformed_code=modernized_code,
                test_cases=state.get("test_cases", {})
            )
            
            # Step 2: Construct comprehensive validation prompt for LLM
            validation_prompt = f"""
            You are an expert code validator analyzing the final modernization results. Please provide intelligent insights on the following:

            **Final Validation Results:**
            {final_validation}

            **Compliance Check:**
            {compliance_check}

            **Integration Tests:**
            {integration_tests}

            **Transformation Results:**
            {state.get("transformation_results", {})}

            **Test Results:**
            {state.get("test_results", {})}

            **Coverage Analysis:**
            {state.get("coverage_analysis", {})}

            **Quality Review:**
            {state.get("quality_review", {})}

            **Target Language:** {state.get("target_language", "python")}

            Please provide:
            1. **Overall Validation Assessment**: Comprehensive evaluation of modernization success
            2. **Compliance Analysis**: Adherence to standards and requirements
            3. **Integration Readiness**: System integration and deployment readiness
            4. **Quality Assurance**: Final quality assessment and recommendations
            5. **Risk Analysis**: Potential risks and mitigation strategies
            6. **Production Readiness**: Whether the code is ready for production deployment
            7. **Success Metrics**: Key indicators of modernization success
            8. **Final Recommendations**: Actionable next steps for deployment

            Provide detailed, comprehensive insights that will help ensure successful modernization and deployment.
            """
            
            # Step 3: Get LLM insights
            from langchain_core.messages import HumanMessage
            messages = [HumanMessage(content=validation_prompt)]
            llm_response = await self.process_messages(messages)
            
            # Extract LLM insights
            llm_insights = llm_response[0].content if llm_response else "No LLM analysis available"
            
            # Step 4: Prepare comprehensive validation results
            validation_results_comprehensive = {
                "final_validation": final_validation,
                "compliance_check": compliance_check,
                "integration_tests": integration_tests,
                "llm_insights": llm_insights,
                "validation_summary": {
                    "validation_passed": final_validation.get("passed", False),
                    "compliance_met": compliance_check.get("compliant", False),
                    "integration_tests_passed": integration_tests.get("passed", False),
                    "modernization_success": self._assess_modernization_success(final_validation, compliance_check, integration_tests),
                    "production_readiness": self._assess_production_readiness(final_validation, compliance_check, integration_tests),
                    "validation_completeness": self._calculate_validation_completeness(final_validation, compliance_check, integration_tests)
                }
            }
            
            # Step 5: Store results in state
            state["final_validation"] = validation_results_comprehensive["final_validation"]
            state["compliance_check"] = validation_results_comprehensive["compliance_check"]
            state["integration_tests"] = validation_results_comprehensive["integration_tests"]
            state["llm_validation_insights"] = validation_results_comprehensive["llm_insights"]
            state["validation_summary"] = validation_results_comprehensive["validation_summary"]
            
            # Determine overall success and set workflow metadata
            import uuid
            from datetime import datetime
            
            # Get workflow timing information
            start_time = state.get("start_time")
            end_time = datetime.utcnow()
            
            # Set modernization success based on validation results
            modernization_success = validation_results_comprehensive["validation_summary"]["modernization_success"]
            state["modernization_success"] = modernization_success
            
            # Set workflow metadata
            state["workflow_id"] = state.get("workflow_id") or str(uuid.uuid4())
            state["start_time"] = start_time.isoformat() if start_time else datetime.utcnow().isoformat()
            state["end_time"] = end_time.isoformat()
            
            # Calculate total duration
            if start_time:
                duration_seconds = (end_time - start_time).total_seconds()
                state["total_duration"] = duration_seconds
            else:
                state["total_duration"] = 0.0
            
            self.log_activity("AI-powered final validation completed", {
                "validation_passed": validation_results_comprehensive["validation_summary"]["validation_passed"],
                "compliance_met": validation_results_comprehensive["validation_summary"]["compliance_met"],
                "integration_tests_passed": validation_results_comprehensive["validation_summary"]["integration_tests_passed"],
                "modernization_success": modernization_success,
                "production_readiness": validation_results_comprehensive["validation_summary"]["production_readiness"],
                "workflow_id": state["workflow_id"],
                "total_duration_seconds": state["total_duration"],
                "llm_insights_length": len(llm_insights)
            })
            
        except Exception as e:
            self.logger.error(f"Error in validator agent: {e}")
            state["error"] = str(e)
            state["modernization_success"] = False
        
        return state
    
    def _assess_modernization_success(self, final_validation: Dict[str, Any], compliance_check: Dict[str, Any], integration_tests: Dict[str, Any]) -> bool:
        """Assess overall modernization success."""
        validation_passed = final_validation.get("validation_passed", False)
        compliance_met = compliance_check.get("overall_compliance", 0.0) >= 0.8  # 80% compliance threshold
        integration_passed = integration_tests.get("passed", 0) > 0
        
        return validation_passed and compliance_met and integration_passed
    
    def _assess_production_readiness(self, final_validation: Dict[str, Any], compliance_check: Dict[str, Any], integration_tests: Dict[str, Any]) -> str:
        """Assess production readiness based on validation results."""
        validation_passed = final_validation.get("validation_passed", False)
        compliance_met = compliance_check.get("overall_compliance", 0.0) >= 0.8
        integration_passed = integration_tests.get("passed", 0) > 0
        
        if validation_passed and compliance_met and integration_passed:
            return "READY - All validations passed"
        elif validation_passed and compliance_met:
            return "MOSTLY READY - Integration tests need attention"
        elif validation_passed:
            return "PARTIALLY READY - Compliance and integration issues"
        else:
            return "NOT READY - Multiple validation failures"
    
    def _calculate_validation_completeness(self, final_validation: Dict[str, Any], compliance_check: Dict[str, Any], integration_tests: Dict[str, Any]) -> float:
        """Calculate how complete the validation process was."""
        completeness_factors = []
        
        # Final validation completeness
        if final_validation.get("passed") is not None:
            completeness_factors.append(1.0)
        else:
            completeness_factors.append(0.0)
        
        # Compliance check completeness
        if compliance_check.get("compliant") is not None:
            completeness_factors.append(1.0)
        else:
            completeness_factors.append(0.0)
        
        # Integration tests completeness
        if integration_tests.get("passed") is not None:
            completeness_factors.append(1.0)
        else:
            completeness_factors.append(0.0)
        
        return sum(completeness_factors) / len(completeness_factors) * 100
    
    def get_system_prompt(self) -> str:
        """Get the system prompt for the validator agent."""
        return """
        You are an AI-powered code validation agent for modernized legacy code.
        Your role is to:
        1. Perform comprehensive final validation using both tools and AI insights
        2. Check compliance with requirements and standards through intelligent analysis
        3. Run integration tests and evaluate system functionality with AI assessment
        4. Validate overall modernization success with intelligent evaluation
        5. Provide final quality assessment and production readiness evaluation
        6. Assess deployment readiness and risk factors
        7. Generate comprehensive validation reports and recommendations
        
        You combine automated validation tools with AI-powered analysis to ensure
        complete validation and successful modernization deployment.
        """
