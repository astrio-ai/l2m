"""
Test generation command implementation.

This module implements the CLI command for generating
and running tests for codebases.
"""

import asyncio
from pathlib import Path
from typing import Optional

from src.core.agents.tester_agent import TesterAgent
from src.cli.utils.output import format_output
from src.cli.utils.progress import ProgressIndicator
from src.utils.logger import get_logger

logger = get_logger(__name__)


async def test_command(ctx, codebase_path: str, output: Optional[str], framework: str):
    """Execute the test command."""
    try:
        # Initialize tester agent
        settings = ctx.obj['settings']
        tester = TesterAgent(settings)
        
        # Show progress indicator
        with ProgressIndicator("Generating and running tests...") as progress:
            # Prepare test parameters
            test_params = {
                "codebase_path": codebase_path,
                "test_framework": framework,
                "options": {}
            }
            
            # Run tests
            progress.update("Running test generation...")
            result = await tester.run(test_params)
            
            # Format and display results
            progress.update("Formatting results...")
            formatted_output = format_output(result, "json")
            
            # Write output to file or display
            if output:
                output_path = Path(output)
                output_path.parent.mkdir(parents=True, exist_ok=True)
                output_path.write_text(formatted_output)
                click.echo(f"Test results written to {output_path}")
            else:
                click.echo(formatted_output)
            
            progress.update("Test generation completed!")
        
        logger.info(f"Test generation completed for {codebase_path}")
        
    except Exception as e:
        logger.error(f"Error in test command: {e}")
        click.echo(f"Error: {e}", err=True)
        raise click.Abort()
