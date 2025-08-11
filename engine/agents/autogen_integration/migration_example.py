"""
Example of how to gradually migrate agents to AutoGen.

This shows the step-by-step process of wrapping existing agents.
"""

import asyncio
import logging
from typing import Dict, List, Optional, Any

from .autogen_wrapper import AutoGenAgentWrapper, AutoGenCoordinator, AutoGenConfig
from ..core_agents.parser_agent import ParserAgent
from ..core_agents.modernizer_agent import ModernizerAgent
from ..core_agents.refactor_agent import RefactorAgent
from ..core_agents.qa_agent import QAAgent
from ..core_agents.coordinator_agent import CoordinatorAgent
from ..utilities.ai import AI
from ..core_agents.base_memory import BaseMemory
from ..utilities.project_config import ProjectConfig

logger = logging.getLogger(__name__)

class GradualMigrationExample:
    """
    Example showing how to gradually migrate agents to AutoGen.
    """
    
    def __init__(self):
        # Initialize core components
        self.ai = AI(
            api_key="your-api-key",
            provider="anthropic",
            model="claude-3-sonnet-20240229"
        )
        self.memory = BaseMemory()
        self.config = ProjectConfig()
        
        # Create base agents (existing system)
        self.base_agents = self._create_base_agents()
        
        # Create AutoGen-wrapped agents (new system)
        self.autogen_agents = self._create_autogen_agents()
        
        # Create coordinator
        self.coordinator = self._create_coordinator()
    
    def _create_base_agents(self) -> Dict[str, Any]:
        """Create the original base agents."""
        return {
            "parser": ParserAgent(
                ai=self.ai,
                memory=self.memory,
                config=self.config
            ),
            "modernizer": ModernizerAgent(
                ai=self.ai,
                memory=self.memory,
                config=self.config
            ),
            "refactor": RefactorAgent(
                ai=self.ai,
                memory=self.memory,
                config=self.config
            ),
            "qa": QAAgent(
                ai=self.ai,
                memory=self.memory,
                config=self.config
            ),
            "coordinator": CoordinatorAgent(
                name="coordinator",
                ai=self.ai,
                memory=self.memory,
                config=self.config
            )
        }
    
    def _create_autogen_agents(self) -> Dict[str, AutoGenAgentWrapper]:
        """Create AutoGen-wrapped versions of agents."""
        wrapped_agents = {}
        
        # Phase 1: Start with just the parser agent
        wrapped_agents["parser"] = AutoGenAgentWrapper(
            self.base_agents["parser"],
            AutoGenConfig(enable_autogen=True)
        )
        
        # Phase 2: Add modernizer agent
        wrapped_agents["modernizer"] = AutoGenAgentWrapper(
            self.base_agents["modernizer"],
            AutoGenConfig(enable_autogen=True)
        )
        
        # Phase 3: Add refactor agent
        wrapped_agents["refactor"] = AutoGenAgentWrapper(
            self.base_agents["refactor"],
            AutoGenConfig(enable_autogen=True)
        )
        
        # Phase 4: Add QA agent
        wrapped_agents["qa"] = AutoGenAgentWrapper(
            self.base_agents["qa"],
            AutoGenConfig(enable_autogen=True)
        )
        
        return wrapped_agents
    
    def _create_coordinator(self) -> AutoGenCoordinator:
        """Create AutoGen coordinator."""
        return AutoGenCoordinator(
            list(self.autogen_agents.values()),
            AutoGenConfig(enable_autogen=True, use_group_chat=True)
        )
    
    async def run_migration_example(self):
        """Run an example showing the migration process."""
        logger.info("Starting gradual migration example...")
        
        # Example 1: Use original system
        logger.info("Phase 0: Using original system")
        result = await self.base_agents["parser"].execute_task({
            "type": "full_analysis",
            "project_path": "/path/to/legacy/project"
        })
        logger.info(f"Original parser result: {result}")
        
        # Example 2: Use AutoGen-wrapped parser
        logger.info("Phase 1: Using AutoGen-wrapped parser")
        result = await self.autogen_agents["parser"].execute_task({
            "type": "full_analysis",
            "project_path": "/path/to/legacy/project"
        })
        logger.info(f"AutoGen parser result: {result}")
        
        # Example 3: Use AutoGen coordinator
        logger.info("Phase 2: Using AutoGen coordinator")
        result = await self.coordinator.coordinate_task({
            "type": "full_modernization",
            "project_path": "/path/to/legacy/project",
            "target_stack": "react"
        })
        logger.info(f"AutoGen coordination result: {result}")
        
        # Example 4: Compare performance
        await self._compare_performance()
    
    async def _compare_performance(self):
        """Compare performance between original and AutoGen systems."""
        logger.info("Comparing performance...")
        
        # Test original system
        start_time = asyncio.get_event_loop().time()
        await self.base_agents["parser"].execute_task({
            "type": "file_analysis",
            "file_path": "test.html"
        })
        original_time = asyncio.get_event_loop().time() - start_time
        
        # Test AutoGen system
        start_time = asyncio.get_event_loop().time()
        await self.autogen_agents["parser"].execute_task({
            "type": "file_analysis",
            "file_path": "test.html"
        })
        autogen_time = asyncio.get_event_loop().time() - start_time
        
        logger.info(f"Original system time: {original_time:.2f}s")
        logger.info(f"AutoGen system time: {autogen_time:.2f}s")
        logger.info(f"Performance difference: {((autogen_time - original_time) / original_time * 100):.1f}%")

# Usage example
async def main():
    """Main function showing how to use the migration."""
    example = GradualMigrationExample()
    await example.run_migration_example()

if __name__ == "__main__":
    asyncio.run(main()) 