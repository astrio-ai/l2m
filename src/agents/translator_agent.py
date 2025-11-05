"""
Translator Agent - Converts COBOL to Python.

Transforms analyzed COBOL code into modern Python equivalents.
"""

from agents import Agent, Runner, function_tool
from agents.model_settings import ModelSettings
from typing import Optional
from src.config import get_settings
from src.utils.logger import get_logger
from src.tools.python_synth_tool import generate_python_code, validate_python_syntax

logger = get_logger(__name__)


@function_tool
def translate_to_python(cobol_analysis: str, target_structure: str) -> str:
    """Translate COBOL logic to Python code.
    
    Args:
        cobol_analysis: Analysis results from analyzer agent
        target_structure: Desired Python code structure
        
    Returns:
        Generated Python code
    """
    try:
        python_code = generate_python_code(cobol_analysis, target_structure)
        # Validate syntax
        if validate_python_syntax(python_code):
            return python_code
        else:
            return "Error: Generated Python code has syntax errors"
    except Exception as e:
        return f"Error translating to Python: {str(e)}"


class TranslatorAgent:
    """Agent that translates COBOL to Python."""
    
    def __init__(self):
        """Initialize the translator agent."""
        self.settings = get_settings()
        
        # Create model settings
        model_settings = ModelSettings(
            temperature=self.settings.openai_temperature,
        )
        
        self.agent = Agent(
            name="COBOL Translator",
            handoff_description="Specialist agent for translating COBOL code to modern Python",
            instructions="""You are a COBOL to Python translator. Your task is to:
1. Convert COBOL procedures to Python functions
2. Map COBOL data types to Python types (PIC X → str, PIC 9 → int/float)
3. Translate control flow (PERFORM → function calls, IF-THEN-ELSE → if/else)
4. Convert COBOL I/O to Python equivalents
5. Maintain business logic equivalence

Generate clean, modern Python code that preserves the original COBOL functionality.
Use type hints, docstrings, and follow PEP 8 style guidelines.""",
            model=self.settings.openai_model,
            model_settings=model_settings,
            tools=[translate_to_python],
        )
    
    async def run(self, input_text: str, session=None) -> str:
        """Run the translator agent."""
        logger.info("Starting translator agent")
        result = await Runner.run(
            self.agent,
            input_text,
            session=session,
            max_turns=self.settings.max_turns,
        )
        logger.info("Translator agent completed")
        return result.final_output

