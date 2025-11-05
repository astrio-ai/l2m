"""
Refactor Agent - Improves code structure and readability.

Refactors Python code to improve structure, readability, and maintainability.
"""

from agents import Agent, Runner, function_tool
from typing import Optional
from src.config import get_settings
from src.utils.logger import get_logger

logger = get_logger(__name__)


@function_tool
def refactor_code(python_code: str, refactoring_goals: str) -> str:
    """Refactor Python code to improve quality.
    
    Args:
        python_code: Python code to refactor
        refactoring_goals: Goals for refactoring (e.g., "improve readability, add type hints")
        
    Returns:
        Refactored Python code
    """
    # This is a placeholder - actual implementation would use LLM
    # or automated refactoring tools
    logger.info(f"Refactoring code with goals: {refactoring_goals}")
    return python_code  # Placeholder


class RefactorAgent:
    """Agent that refactors Python code."""
    
    def __init__(self):
        """Initialize the refactor agent."""
        self.settings = get_settings()
        self.agent = Agent(
            name="Code Refactorer",
            instructions="""You are a code refactoring agent. Your task is to:
1. Improve code structure and organization
2. Enhance readability and maintainability
3. Add type hints and docstrings
4. Extract common patterns into reusable functions
5. Optimize performance where appropriate

Maintain functional equivalence while improving code quality.""",
            model=self.settings.openai_model,
            temperature=self.settings.openai_temperature,
            tools=[refactor_code],
        )
    
    async def run(self, input_text: str, session=None) -> str:
        """Run the refactor agent."""
        logger.info("Starting refactor agent")
        result = await Runner.run(
            self.agent,
            input_text,
            session=session,
            max_turns=self.settings.max_turns,
        )
        logger.info("Refactor agent completed")
        return result.final_output

