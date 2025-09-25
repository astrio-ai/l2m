"""
Code analysis command implementation.

This module implements the CLI command for analyzing
legacy codebases.
"""

import asyncio
import click
from pathlib import Path
from typing import Optional

from src.core.agents.analyzer_agent import AnalyzerAgent
from src.cli.utils.output import format_output
from src.cli.utils.progress import ProgressIndicator
from src.utils.logger import get_logger

logger = get_logger(__name__)


async def analyze_command(ctx, codebase_path: str, output: Optional[str], output_format: str):
    """Execute the analyze command."""
    try:
        # Initialize analyzer agent
        settings = ctx.obj['settings']
        analyzer = AnalyzerAgent(settings)
        
        # Show progress indicator
        with ProgressIndicator("Analyzing codebase...") as progress:
            # Prepare analysis parameters
            analysis_params = {
                "codebase_path": codebase_path,
                "analysis_type": "full",
                "options": {}
            }
            
            # Run analysis
            progress.update("Running analysis...")
            result = await analyzer.run(analysis_params)
            
            # Format and display results
            progress.update("Formatting results...")
            formatted_output = format_output(result, output_format)
            
            # Write output to file or display
            if output:
                output_path = Path(output)
                output_path.parent.mkdir(parents=True, exist_ok=True)
                output_path.write_text(formatted_output)
                click.echo(f"Analysis results written to {output_path}")
            else:
                click.echo(formatted_output)
            
            progress.update("Analysis completed!")
        
        logger.info(f"Analysis completed for {codebase_path}")
        
    except Exception as e:
        logger.error(f"Error in analysis command: {e}")
        click.echo(f"Error: {e}", err=True)
        raise click.Abort()
