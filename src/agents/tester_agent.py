"""
Tester Agent - Creates and runs unit tests.

Generates and executes unit tests for translated Python code.
"""

from agents import Agent, Runner, function_tool
from typing import Optional
from src.config import get_settings
from src.utils.logger import get_logger
from src.tools.test_runner_tool import generate_test_code, run_tests

logger = get_logger(__name__)


@function_tool
def create_tests(python_code: str, test_requirements: str) -> str:
    """Generate unit tests for Python code.
    
    Args:
        python_code: Python code to test
        test_requirements: Requirements for test coverage
        
    Returns:
        Generated test code
    """
    try:
        test_code = generate_test_code(python_code, test_requirements)
        return test_code
    except Exception as e:
        return f"Error generating tests: {str(e)}"


@function_tool
def execute_tests(test_code: str) -> str:
    """Run test code and return results.
    
    Args:
        test_code: Test code to execute
        
    Returns:
        Test execution results
    """
    try:
        results = run_tests(test_code)
        return results
    except Exception as e:
        return f"Error running tests: {str(e)}"


class TesterAgent:
    """Agent that creates and runs unit tests."""
    
    def __init__(self):
        """Initialize the tester agent."""
        self.settings = get_settings()
        self.agent = Agent(
            name="Test Generator",
            instructions="""You are a test generation agent. Your task is to:
1. Generate comprehensive unit tests for Python code
2. Ensure test coverage for all functions and edge cases
3. Write tests that validate business logic correctness
4. Use pytest framework conventions
5. Include both positive and negative test cases

Generate well-structured, maintainable test code.""",
            model=self.settings.openai_model,
            temperature=self.settings.openai_temperature,
            tools=[create_tests, execute_tests],
        )
    
    async def run(self, input_text: str, session=None) -> str:
        """Run the tester agent."""
        logger.info("Starting tester agent")
        result = await Runner.run(
            self.agent,
            input_text,
            session=session,
            max_turns=self.settings.max_turns,
        )
        logger.info("Tester agent completed")
        return result.final_output

