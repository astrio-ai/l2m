"""
Code review agent.

This agent reviews the transformed code for quality, correctness,
and adherence to modern coding standards.
"""

from typing import Dict, Any, List
from src.core.agents.base_agent import BaseAgent
from src.core.state.agent_state import AgentState
from src.core.tools.code_tools import CodeReviewTool, QualityAnalyzerTool
from src.core.tools.test_tools import TestGeneratorTool, TestRunnerTool, CoverageAnalyzerTool
from src.core.tools.file_tools import FileReaderTool, FileWriterTool
from src.core.tools.search_tools import PatternSearchTool, ReferenceFinderTool
from src.utils.logger import get_logger

logger = get_logger(__name__)


class ReviewerAgent(BaseAgent):
    """Agent responsible for reviewing transformed code."""
    
    def __init__(self, settings):
        """Initialize the reviewer agent."""
        tools = [
            CodeReviewTool(),
            QualityAnalyzerTool(),
            TestGeneratorTool(),
            TestRunnerTool(),
            CoverageAnalyzerTool(),
            FileReaderTool(),
            FileWriterTool(),
            PatternSearchTool(),
            ReferenceFinderTool()
        ]
        super().__init__(settings, tools)
    
    async def run(self, state: AgentState) -> AgentState:
        """Run the reviewer agent with AI-powered code review."""
        self.log_activity("Starting AI-powered code review")
        
        try:
            # Step 1: Gather raw review data using tools
            # Format transformation_results for the tool
            transformed_files = state.get("transformation_results", [])
            transformed_code_data = {
                "files": [{"path": path, "type": "python"} for path in transformed_files],
                "total_files": len(transformed_files)
            }
            
            quality_review = await self.use_tool(
                "review_transformed_code",
                transformed_code=transformed_code_data,
                quality_standards=["python", "modern", "clean_code"]
            )
            
            standards_analysis = await self.use_tool(
                "analyze_code_quality",
                code_path=state["codebase_path"],
                quality_standards=["python", "modern", "clean_code"]
            )
            
            test_cases = await self.use_tool(
                "generate_test_cases",
                transformed_code=transformed_code_data,
                original_functionality=state["analysis_results"]
            )
            
            # Step 2: Construct comprehensive review prompt for LLM
            review_prompt = f"""
            You are an expert code reviewer analyzing modernized legacy code. Please provide intelligent insights on the following:

            **Transformation Results:**
            {state.get("transformation_results", {})}

            **Quality Review Data:**
            {quality_review}

            **Standards Analysis:**
            {standards_analysis}

            **Generated Test Cases:**
            {test_cases}

            **Original Analysis:**
            {state.get("analysis_results", {})}

            **Target Language:** {state.get("target_language", "python")}

            Please provide:
            1. **Code Quality Assessment**: Overall quality score and specific issues
            2. **Standards Compliance**: Adherence to modern coding standards
            3. **Functionality Preservation**: Whether the transformation maintains original behavior
            4. **Test Coverage Analysis**: Completeness of generated test cases
            5. **Improvement Recommendations**: Specific suggestions for better code
            6. **Risk Assessment**: Potential issues or areas of concern
            7. **Modernization Success**: How well the transformation achieved its goals

            Provide detailed, actionable insights that will help ensure the modernized code meets professional standards.
            """
            
            # Step 3: Get LLM insights
            from langchain_core.messages import HumanMessage
            messages = [HumanMessage(content=review_prompt)]
            llm_response = await self.process_messages(messages)
            
            # Extract LLM insights
            llm_insights = llm_response[0].content if llm_response else "No LLM analysis available"
            
            # Step 4: Prepare comprehensive review results
            review_results = {
                "quality_review": quality_review,
                "standards_analysis": standards_analysis,
                "test_cases": test_cases,
                "llm_insights": llm_insights,
                "review_summary": {
                    "quality_score": quality_review.get("overall_score", 0),
                    "standards_violations": len(standards_analysis.get("violations", [])),
                    "test_cases_generated": len(test_cases.get("test_cases", [])),
                    "modernization_readiness": self._assess_modernization_readiness(quality_review, standards_analysis, test_cases),
                    "review_completeness": self._calculate_review_completeness(quality_review, standards_analysis, test_cases)
                }
            }
            
            # Step 5: Store results in state
            state["quality_review"] = review_results["quality_review"]
            state["standards_analysis"] = review_results["standards_analysis"]
            state["test_cases"] = review_results["test_cases"]
            state["llm_review_insights"] = review_results["llm_insights"]
            state["review_summary"] = review_results["review_summary"]
            
            self.log_activity("AI-powered code review completed", {
                "quality_score": review_results["review_summary"]["quality_score"],
                "standards_violations": review_results["review_summary"]["standards_violations"],
                "test_cases_generated": review_results["review_summary"]["test_cases_generated"],
                "modernization_readiness": review_results["review_summary"]["modernization_readiness"],
                "llm_insights_length": len(llm_insights)
            })
            
        except Exception as e:
            self.logger.error(f"Error in reviewer agent: {e}")
            state["error"] = str(e)
        
        return state
    
    def _assess_modernization_readiness(self, quality_review: Dict[str, Any], standards_analysis: Dict[str, Any], test_cases: Dict[str, Any]) -> str:
        """Assess the modernization readiness based on review results."""
        quality_score = quality_review.get("overall_score", 0)
        violations_count = len(standards_analysis.get("violations", []))
        test_count = len(test_cases.get("test_cases", []))
        
        if quality_score >= 80 and violations_count == 0 and test_count >= 3:
            return "EXCELLENT - Ready for production"
        elif quality_score >= 70 and violations_count <= 2 and test_count >= 2:
            return "GOOD - Minor improvements needed"
        elif quality_score >= 60 and violations_count <= 5:
            return "FAIR - Moderate improvements needed"
        else:
            return "POOR - Significant improvements required"
    
    def _calculate_review_completeness(self, quality_review: Dict[str, Any], standards_analysis: Dict[str, Any], test_cases: Dict[str, Any]) -> float:
        """Calculate how complete the review process was."""
        completeness_factors = []
        
        # Quality review completeness
        if quality_review.get("overall_score") is not None:
            completeness_factors.append(1.0)
        else:
            completeness_factors.append(0.0)
        
        # Standards analysis completeness
        if standards_analysis.get("violations") is not None:
            completeness_factors.append(1.0)
        else:
            completeness_factors.append(0.0)
        
        # Test cases completeness
        if test_cases.get("test_cases") is not None and len(test_cases.get("test_cases", [])) > 0:
            completeness_factors.append(1.0)
        else:
            completeness_factors.append(0.0)
        
        return sum(completeness_factors) / len(completeness_factors) * 100
    
    def get_system_prompt(self) -> str:
        """Get the system prompt for the reviewer agent."""
        return """
        You are an AI-powered code review agent for modernized legacy code.
        Your role is to:
        1. Analyze code quality and correctness using both tools and AI insights
        2. Evaluate adherence to modern coding standards
        3. Identify potential issues and improvement opportunities
        4. Generate comprehensive test cases and coverage analysis
        5. Ensure functionality preservation during modernization
        6. Provide intelligent recommendations for code improvements
        7. Assess modernization success and readiness for production
        
        You combine static analysis tools with AI-powered insights to provide
        comprehensive code reviews that ensure modernized code meets professional
        standards and maintains original functionality.
        """
