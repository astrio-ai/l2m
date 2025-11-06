"""
Orchestrator Agent - Manages the overall modernization pipeline.

Coordinates handoffs between specialized agents for COBOL to Python modernization.
"""

from typing import Optional, List
from agents import Agent, Runner
from agents.model_settings import ModelSettings
from src.config import get_settings
from src.utils.logger import get_logger

logger = get_logger(__name__)


class OrchestratorAgent:
    """Manages the overall modernization pipeline."""
    
    def __init__(self, handoff_agents: Optional[List[Agent]] = None):
        """Initialize the orchestrator agent.
        
        Args:
            handoff_agents: List of agents that can be handed off to
        """
        self.settings = get_settings()
        
        # Create model settings
        model_settings = ModelSettings(
            temperature=self.settings.openai_temperature,
        )
        
        self.agent = Agent(
            name="Orchestrator",
            instructions="""You are the orchestrator for a COBOL to Python modernization pipeline.
Your role is to coordinate handoffs between specialized agents:
1. Analyzer Agent - parses and analyzes COBOL code
2. Translator Agent - converts COBOL logic to Python
3. Reviewer Agent - reviews translated code quality
4. Tester Agent - creates and runs unit tests
5. Refactor Agent - improves code structure and readability

CRITICAL RULES:
- You MUST hand off to the specialized agents. Do NOT manually translate or generate code yourself.
- You MUST call the Tester Agent to generate test code - this is mandatory.
- Each agent should handle their specific task. Your job is coordination, not implementation.
- If an agent handoff fails, try again or provide clearer context, but do not do the work yourself.

Workflow:
1. Hand off to Analyzer Agent first
2. Then hand off to Translator Agent (they will generate Python code)
3. Then hand off to Reviewer Agent
4. Then hand off to Tester Agent (MANDATORY - must generate test code)
5. Finally hand off to Refactor Agent if needed

Provide clear context to each agent about what needs to be done.""",
            model=self.settings.openai_model,
            model_settings=model_settings,
            handoffs=handoff_agents or [],
        )
    
    async def run(self, input_text: str, session=None) -> str:
        """Run the orchestrator agent."""
        logger.info("Starting orchestrator agent")
        result = await Runner.run(
            self.agent,
            input_text,
            session=session,
            max_turns=self.settings.max_turns,
        )
        logger.info("Orchestrator agent completed")
        return result.final_output

