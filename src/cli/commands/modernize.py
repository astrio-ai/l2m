"""
Code modernization command implementation.

This module implements the CLI command for modernizing
legacy codebases.
"""

import asyncio
from pathlib import Path
from typing import Optional, List

from src.core.graph.modernization import ModernizationWorkflow
from src.cli.utils.output import format_output
from src.cli.utils.progress import ProgressIndicator
from src.utils.logger import get_logger

logger = get_logger(__name__)


async def modernize_command(ctx, codebase_path: str, target_language: str, output: Optional[str], goals: List[str], async_mode: bool):
    """Execute the modernize command."""
    try:
        # Initialize modernization workflow
        settings = ctx.obj['settings']
        workflow = ModernizationWorkflow(settings)
        
        # Show progress indicator
        with ProgressIndicator("Modernizing codebase...") as progress:
            # Prepare modernization parameters
            modernization_params = {
                "codebase_path": codebase_path,
                "target_language": target_language,
                "modernization_goals": goals,
                "options": {}
            }
            
            # Run modernization
            progress.update("Running modernization workflow...")
            result = await workflow.run(modernization_params)
            
            # Format and display results
            progress.update("Formatting results...")
            formatted_output = format_output(result, "json")
            
            # Write output to file or display
            if output:
                output_path = Path(output)
                output_path.parent.mkdir(parents=True, exist_ok=True)
                output_path.write_text(formatted_output)
                click.echo(f"Modernization results written to {output_path}")
            else:
                click.echo(formatted_output)
            
            progress.update("Modernization completed!")
        
        logger.info(f"Modernization completed for {codebase_path}")
        
    except Exception as e:
        logger.error(f"Error in modernization command: {e}")
        click.echo(f"Error: {e}", err=True)
        raise click.Abort()
