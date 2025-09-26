"""
Test generation and execution agent.

This agent generates and executes tests to validate the transformed code
and ensure functionality preservation.
"""

from typing import Dict, Any, List
from src.core.agents.base_agent import BaseAgent
from src.core.state.agent_state import AgentState
from src.core.tools.test_tools import TestRunnerTool, CoverageAnalyzerTool, TestGeneratorTool, IntegrationTestTool
from src.core.tools.code_tools import TestValidatorTool
from src.core.tools.file_tools import FileReaderTool, FileWriterTool, DirectoryScannerTool
from src.core.tools.search_tools import PatternSearchTool, ReferenceFinderTool
from src.utils.logger import get_logger

logger = get_logger(__name__)


class TesterAgent(BaseAgent):
    """Agent responsible for test generation and execution."""
    
    def __init__(self, settings):
        """Initialize the tester agent."""
        tools = [
            TestRunnerTool(),
            CoverageAnalyzerTool(),
            TestValidatorTool(),
            TestGeneratorTool(),
            IntegrationTestTool(),
            FileReaderTool(),
            FileWriterTool(),
            DirectoryScannerTool(),
            PatternSearchTool(),
            ReferenceFinderTool()
        ]
        super().__init__(settings, tools)
    
    async def run(self, state: AgentState) -> AgentState:
        """Run the tester agent with AI-powered test analysis."""
        self.log_activity("Starting AI-powered test generation and execution")
        
        try:
            # Step 1: Gather raw test data using tools
            test_results = await self.use_tool(
                "run_test_cases",
                test_cases=state["test_cases"],
                transformed_code=state["transformation_results"],
                test_framework=state["test_framework"]
            )
            
            coverage_analysis = await self.use_tool(
                "analyze_test_coverage",
                test_results=test_results,
                transformed_code=state["transformation_results"]
            )
            
            test_validation = await self.use_tool(
                "validate_tests",
                test_cases=state["test_cases"],
                original_functionality=state["analysis_results"]
            )
            
            # Step 2: Construct comprehensive testing prompt for LLM
            testing_prompt = f"""
            You are an expert test analyst evaluating modernized legacy code. Please provide intelligent insights on the following:

            **Test Results:**
            {test_results}

            **Coverage Analysis:**
            {coverage_analysis}

            **Test Validation:**
            {test_validation}

            **Original Analysis:**
            {state.get("analysis_results", {})}

            **Transformation Results:**
            {state.get("transformation_results", {})}

            **Target Language:** {state.get("target_language", "python")}

            Please provide:
            1. **Test Quality Assessment**: Overall test quality and effectiveness
            2. **Coverage Analysis**: Completeness of test coverage and missing areas
            3. **Functionality Validation**: Whether tests properly validate original functionality
            4. **Test Strategy Evaluation**: Effectiveness of the testing approach
            5. **Gap Analysis**: Missing test cases or coverage areas
            6. **Risk Assessment**: Potential issues with test coverage or validation
            7. **Improvement Recommendations**: Specific suggestions for better testing
            8. **Test Readiness**: Whether the code is ready for production based on test results

            Provide detailed, actionable insights that will help ensure comprehensive testing and functionality preservation.
            """
            
            # Step 3: Get LLM insights
            from langchain_core.messages import HumanMessage
            messages = [HumanMessage(content=testing_prompt)]
            llm_response = await self.process_messages(messages)
            
            # Extract LLM insights
            llm_insights = llm_response[0].content if llm_response else "No LLM analysis available"
            
            # Step 4: Prepare comprehensive test results
            test_results_comprehensive = {
                "test_results": test_results,
                "coverage_analysis": coverage_analysis,
                "test_validation": test_validation,
                "llm_insights": llm_insights,
                "testing_summary": {
                    "tests_passed": test_results.get("passed", 0),
                    "tests_failed": test_results.get("failed", 0),
                    "coverage_percentage": coverage_analysis.get("coverage_percentage", 0),
                    "validation_score": test_validation.get("completeness_score", 0),
                    "test_readiness": self._assess_test_readiness(test_results, coverage_analysis, test_validation),
                    "testing_completeness": self._calculate_testing_completeness(test_results, coverage_analysis, test_validation)
                }
            }
            
            # Step 5: Store results in state
            state["test_results"] = test_results_comprehensive["test_results"]
            state["coverage_analysis"] = test_results_comprehensive["coverage_analysis"]
            state["test_validation"] = test_results_comprehensive["test_validation"]
            state["llm_testing_insights"] = test_results_comprehensive["llm_insights"]
            state["testing_summary"] = test_results_comprehensive["testing_summary"]
            
            self.log_activity("AI-powered test generation and execution completed", {
                "tests_passed": test_results_comprehensive["testing_summary"]["tests_passed"],
                "tests_failed": test_results_comprehensive["testing_summary"]["tests_failed"],
                "coverage_percentage": test_results_comprehensive["testing_summary"]["coverage_percentage"],
                "validation_score": test_results_comprehensive["testing_summary"]["validation_score"],
                "test_readiness": test_results_comprehensive["testing_summary"]["test_readiness"],
                "llm_insights_length": len(llm_insights)
            })
            
        except Exception as e:
            self.logger.error(f"Error in tester agent: {e}")
            state["error"] = str(e)
        
        return state
    
    def _assess_test_readiness(self, test_results: Dict[str, Any], coverage_analysis: Dict[str, Any], test_validation: Dict[str, Any]) -> str:
        """Assess test readiness based on test results and coverage."""
        tests_passed = test_results.get("passed", 0)
        tests_failed = test_results.get("failed", 0)
        coverage_percentage = coverage_analysis.get("coverage_percentage", 0)
        validation_score = test_validation.get("completeness_score", 0)
        
        if tests_failed == 0 and coverage_percentage >= 90 and validation_score >= 90:
            return "EXCELLENT - Ready for production"
        elif tests_failed <= 1 and coverage_percentage >= 80 and validation_score >= 80:
            return "GOOD - Minor test improvements needed"
        elif tests_failed <= 3 and coverage_percentage >= 70 and validation_score >= 70:
            return "FAIR - Moderate test improvements needed"
        else:
            return "POOR - Significant test improvements required"
    
    def _calculate_testing_completeness(self, test_results: Dict[str, Any], coverage_analysis: Dict[str, Any], test_validation: Dict[str, Any]) -> float:
        """Calculate how complete the testing process was."""
        completeness_factors = []
        
        # Test execution completeness
        if test_results.get("passed") is not None and test_results.get("failed") is not None:
            completeness_factors.append(1.0)
        else:
            completeness_factors.append(0.0)
        
        # Coverage analysis completeness
        if coverage_analysis.get("coverage_percentage") is not None:
            completeness_factors.append(1.0)
        else:
            completeness_factors.append(0.0)
        
        # Test validation completeness
        if test_validation.get("completeness_score") is not None:
            completeness_factors.append(1.0)
        else:
            completeness_factors.append(0.0)
        
        return sum(completeness_factors) / len(completeness_factors) * 100
    
    def get_system_prompt(self) -> str:
        """Get the system prompt for the tester agent."""
        return """
        You are an AI-powered test generation and execution agent for modernized legacy code.
        Your role is to:
        1. Generate comprehensive test cases using both tools and AI insights
        2. Execute tests and analyze results with intelligent evaluation
        3. Measure test coverage and completeness with AI-powered gap analysis
        4. Validate functionality preservation through intelligent comparison
        5. Identify gaps in test coverage and recommend improvements
        6. Assess test readiness and production readiness
        7. Provide intelligent recommendations for better testing strategies
        
        You combine automated testing tools with AI-powered analysis to ensure
        comprehensive test coverage and functionality preservation in modernized code.
        """
