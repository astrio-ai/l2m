"""
Orchestrator Agent - Manages the overall modernization pipeline.

Coordinates handoffs between specialized agents for COBOL to Python modernization.
"""

from typing import Optional, List
from agents import Agent, Runner, ModelConfig
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
        
        # Create model config
        model_config = ModelConfig(
            model=self.settings.openai_model,
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

Decide which agent should handle the current task and hand off appropriately.
Provide clear context to each agent about what needs to be done.""",
            model_config=model_config,
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

