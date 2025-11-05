"""
Reviewer Agent - Reviews translated code quality.

Reviews Python code generated from COBOL for quality, correctness, and best practices.
"""

from agents import Agent, Runner, function_tool, ModelConfig
from typing import Optional
from src.config import get_settings
from src.utils.logger import get_logger
from src.tools.code_quality_tool import check_code_quality, get_quality_score

logger = get_logger(__name__)


@function_tool
def review_code(python_code: str) -> str:
    """Review Python code for quality and best practices.
    
    Args:
        python_code: Python code to review
        
    Returns:
        Review report with quality score and recommendations
    """
    try:
        quality_score = get_quality_score(python_code)
        issues = check_code_quality(python_code)
        return f"Quality Score: {quality_score}/100\nIssues found: {len(issues)}"
    except Exception as e:
        return f"Error reviewing code: {str(e)}"


class ReviewerAgent:
    """Agent that reviews translated Python code."""
    
    def __init__(self):
        """Initialize the reviewer agent."""
        self.settings = get_settings()
        
        # Create model config
        model_config = ModelConfig(
            model=self.settings.openai_model,
            temperature=self.settings.openai_temperature,
        )
        
        self.agent = Agent(
            name="Code Reviewer",
            handoff_description="Specialist agent for reviewing Python code quality and correctness",
            instructions="""You are a Python code reviewer. Your task is to:
1. Review Python code generated from COBOL for correctness
2. Check adherence to PEP 8 style guidelines
3. Identify potential bugs or logic errors
4. Suggest improvements for readability and maintainability
5. Verify that business logic is preserved

Provide detailed feedback with specific line numbers and suggestions.""",
            model_config=model_config,
            tools=[review_code],
        )
    
    async def run(self, input_text: str, session=None) -> str:
        """Run the reviewer agent."""
        logger.info("Starting reviewer agent")
        result = await Runner.run(
            self.agent,
            input_text,
            session=session,
            max_turns=self.settings.max_turns,
        )
        logger.info("Reviewer agent completed")
        return result.final_output

