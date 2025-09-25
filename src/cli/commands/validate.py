"""
Code validation command implementation.

This module implements the CLI command for validating
modernized codebases.
"""

import asyncio
from pathlib import Path
from typing import Optional, List

from src.core.agents.validator_agent import ValidatorAgent
from src.cli.utils.output import format_output
from src.cli.utils.progress import ProgressIndicator
from src.utils.logger import get_logger

logger = get_logger(__name__)


async def validate_command(ctx, codebase_path: str, output: Optional[str], standards: List[str]):
    """Execute the validate command."""
    try:
        # Initialize validator agent
        settings = ctx.obj['settings']
        validator = ValidatorAgent(settings)
        
        # Show progress indicator
        with ProgressIndicator("Validating codebase...") as progress:
            # Prepare validation parameters
            validation_params = {
                "codebase_path": codebase_path,
                "standards": standards,
                "options": {}
            }
            
            # Run validation
            progress.update("Running validation...")
            result = await validator.run(validation_params)
            
            # Format and display results
            progress.update("Formatting results...")
            formatted_output = format_output(result, "json")
            
            # Write output to file or display
            if output:
                output_path = Path(output)
                output_path.parent.mkdir(parents=True, exist_ok=True)
                output_path.write_text(formatted_output)
                click.echo(f"Validation results written to {output_path}")
            else:
                click.echo(formatted_output)
            
            progress.update("Validation completed!")
        
        logger.info(f"Validation completed for {codebase_path}")
        
    except Exception as e:
        logger.error(f"Error in validation command: {e}")
        click.echo(f"Error: {e}", err=True)
        raise click.Abort()
