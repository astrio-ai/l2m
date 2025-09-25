"""
Main entry point for the Legacy2Modern multi-agent system.

This module provides the primary interface for running the modernization
workflows and managing the multi-agent system.
"""

import asyncio
import sys
from pathlib import Path

from src.core.graph.main_graph import MainWorkflowGraph
from src.config.settings import Settings
from src.utils.logger import get_logger

logger = get_logger(__name__)


async def main():
    """Main entry point for the application."""
    try:
        settings = Settings()
        logger.info("Starting Legacy2Modern multi-agent system")
        
        # Initialize the main workflow graph
        workflow = MainWorkflowGraph(settings)
        
        # Run the workflow
        await workflow.run()
        
    except Exception as e:
        logger.error(f"Error in main application: {e}")
        sys.exit(1)


if __name__ == "__main__":
    asyncio.run(main())
