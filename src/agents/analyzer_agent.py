"""
Analyzer Agent - Parses COBOL and extracts logic.

Analyzes COBOL source code to understand structure, dependencies, and business logic.
"""

from agents import Agent, Runner, function_tool
from agents.model_settings import ModelSettings
from typing import Optional
from src.config import get_settings
from src.utils.logger import get_logger
from src.tools.cobol_parser_tool import parse_cobol_file, analyze_cobol_structure

logger = get_logger(__name__)


@function_tool
def analyze_cobol(code_path: str) -> str:
    """Analyze COBOL file and extract structure, variables, and procedures.
    
    Args:
        code_path: Path to the COBOL file to analyze
        
    Returns:
        Analysis report as JSON string
    """
    try:
        structure = analyze_cobol_structure(code_path)
        return f"Analysis complete: {len(structure.get('procedures', []))} procedures found"
    except Exception as e:
        return f"Error analyzing COBOL: {str(e)}"


class AnalyzerAgent:
    """Agent that analyzes COBOL code structure and logic."""
    
    def __init__(self):
        """Initialize the analyzer agent."""
        self.settings = get_settings()
        
        # Create model settings
        model_settings = ModelSettings(
            temperature=self.settings.openai_temperature,
        )
        
        self.agent = Agent(
            name="COBOL Analyzer",
            handoff_description="Specialist agent for analyzing COBOL code structure and extracting logic",
            instructions="""You are a COBOL code analyzer. Your task is to:
1. Parse COBOL source files to understand structure
2. Extract variable declarations (WORKING-STORAGE, DATA DIVISION)
3. Identify procedures and their logic flow
4. Map dependencies between procedures
5. Identify business logic patterns

Provide detailed analysis reports that will help the translator agent
understand what needs to be converted.""",
            model=self.settings.openai_model,
            model_settings=model_settings,
            tools=[analyze_cobol],
        )
    
    async def run(self, input_text: str, session=None) -> str:
        """Run the analyzer agent."""
        logger.info("Starting analyzer agent")
        result = await Runner.run(
            self.agent,
            input_text,
            session=session,
            max_turns=self.settings.max_turns,
        )
        logger.info("Analyzer agent completed")
        return result.final_output

