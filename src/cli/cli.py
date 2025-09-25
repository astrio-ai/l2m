"""
Main CLI interface for the multi-agent system.

This module provides the primary command-line interface for
the modernization system.
"""

import click
import asyncio
from pathlib import Path
from typing import Optional

from rich.console import Console
from rich.text import Text

from src.cli.commands.analyze import analyze_command
from src.cli.commands.modernize import modernize_command
from src.cli.commands.test import test_command
from src.cli.commands.validate import validate_command
from src.config.settings import Settings
from src.utils.logger import get_logger

logger = get_logger(__name__)
console = Console()


def display_banner():
    """Display the Legacy2Modern banner with pixel-art style similar to Gemini."""
    
    # Create a minimalist banner similar to Gemini's style
    banner_art = """
██╗     ███████╗ ██████╗  █████╗  ██████╗██╗   ██╗██████╗ ███╗   ███╗ ██████╗ ██████╗ ███████╗██████╗ ███╗   ██╗
██║     ██╔════╝██╔════╝ ██╔══██╗██╔════╝╚██╗ ██╔╝╚════██╗████╗ ████║██╔═══██╗██╔══██╗██╔════╝██╔══██╗████╗  ██║
██║     █████╗  ██║  ███╗███████║██║      ╚████╔╝  █████╔╝██╔████╔██║██║   ██║██║  ██║█████╗  ██████╔╝██╔██╗ ██║
██║     ██╔══╝  ██║   ██║██╔══██║██║       ╚██╔╝  ██╔═══╝ ██║╚██╔╝██║██║   ██║██║  ██║██╔══╝  ██╔══██╗██║╚██╗██║
███████╗███████╗╚██████╔╝██║  ██║╚██████╗   ██║   ███████╗██║ ╚═╝ ██║╚██████╔╝██████╔╝███████╗██║  ██║██║ ╚████║
╚══════╝╚══════╝ ╚═════╝ ╚═╝  ╚═╝ ╚═════╝   ╚═╝   ╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝
"""
    
    console.print(banner_art)
    # Center the text using Rich's built-in centering
    centered_text = Text("Powered by ", style="white") + Text("Astrio", style="bold #0053D6")
    console.print(centered_text, justify="center")
    console.print()  # Add padding under the text


@click.group()
@click.option('--verbose', '-v', is_flag=True, help='Enable verbose output')
@click.option('--config', '-c', type=click.Path(exists=True), help='Path to configuration file')
@click.option('--no-banner', is_flag=True, help='Skip displaying the banner')
@click.pass_context
def cli(ctx, verbose, config, no_banner):
    """Legacy2Modern Multi-Agent System CLI."""
    # Display banner unless disabled
    if not no_banner:
        display_banner()
    
    # Ensure that ctx.obj exists and is a dict
    ctx.ensure_object(dict)
    
    # Set up logging
    if verbose:
        logger.setLevel("DEBUG")
    
    # Load configuration
    settings = Settings()
    if config:
        settings.load_from_file(config)
    
    ctx.obj['settings'] = settings
    ctx.obj['verbose'] = verbose


@cli.command()
@click.argument('codebase_path', type=click.Path(exists=True))
@click.option('--output', '-o', type=click.Path(), help='Output directory for analysis results')
@click.option('--format', 'output_format', type=click.Choice(['json', 'yaml', 'html']), default='json', help='Output format')
@click.pass_context
def analyze(ctx, codebase_path, output, output_format):
    """Analyze a legacy codebase."""
    asyncio.run(analyze_command(ctx, codebase_path, output, output_format))


@cli.command()
@click.argument('codebase_path', type=click.Path(exists=True))
@click.argument('target_language', type=click.Choice(['python', 'java', 'csharp', 'c']))
@click.option('--output', '-o', type=click.Path(), help='Output directory for modernized code')
@click.option('--goals', '-g', multiple=True, help='Modernization goals')
@click.option('--async', 'async_mode', is_flag=True, help='Run modernization asynchronously')
@click.pass_context
def modernize(ctx, codebase_path, target_language, output, goals, async_mode):
    """Modernize a legacy codebase."""
    asyncio.run(modernize_command(ctx, codebase_path, target_language, output, goals, async_mode))


@cli.command()
@click.argument('codebase_path', type=click.Path(exists=True))
@click.option('--output', '-o', type=click.Path(), help='Output directory for test results')
@click.option('--framework', '-f', type=click.Choice(['pytest', 'unittest', 'junit']), default='pytest', help='Test framework')
@click.pass_context
def test(ctx, codebase_path, output, framework):
    """Generate and run tests for a codebase."""
    asyncio.run(test_command(ctx, codebase_path, output, framework))


@cli.command()
@click.argument('codebase_path', type=click.Path(exists=True))
@click.option('--output', '-o', type=click.Path(), help='Output directory for validation results')
@click.option('--standards', '-s', multiple=True, help='Coding standards to validate against')
@click.pass_context
def validate(ctx, codebase_path, output, standards):
    """Validate a modernized codebase."""
    asyncio.run(validate_command(ctx, codebase_path, output, standards))


@cli.command()
@click.option('--host', default='0.0.0.0', help='Host to bind to')
@click.option('--port', default=8000, type=int, help='Port to bind to')
@click.option('--reload', is_flag=True, help='Enable auto-reload for development')
def serve(host, port, reload):
    """Start the API server."""
    from src.api.server import run_server
    run_server(host=host, port=port, reload=reload)


if __name__ == '__main__':
    cli()
