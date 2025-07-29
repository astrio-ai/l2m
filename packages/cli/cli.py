"""
Modern CLI Interface for Legacy2Modern

A beautiful, interactive command-line interface similar to Gemini CLI
that provides an intuitive way to transpile legacy code to modern languages.
"""

import os
import sys
import asyncio
from pathlib import Path
from typing import Optional, List
import json

try:
    import click
except ImportError:
    click = None

try:
    import typer
except ImportError:
    typer = None

from rich.console import Console
from rich.panel import Panel
from rich.text import Text
from rich.prompt import Prompt, Confirm
from rich.table import Table
from rich.progress import Progress, SpinnerColumn, TextColumn
from rich.syntax import Syntax
from rich.layout import Layout
from rich.live import Live
from rich.align import Align

try:
    from prompt_toolkit import PromptSession
    from prompt_toolkit.completion import WordCompleter
    from prompt_toolkit.styles import Style
except ImportError:
    PromptSession = None
    WordCompleter = None
    Style = None

# Add the project root to sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "../..")))

from packages.transpiler.engine.hybrid_transpiler import HybridTranspiler
from packages.transpiler.engine.llm_augmentor import LLMConfig
from packages.llm_agent import LLMAgent


class Legacy2ModernCLI:
    """Modern CLI interface for Legacy2Modern transpilation engine."""
    
    def __init__(self):
        self.console = Console()
        self.session = PromptSession() if PromptSession else None
        self.llm_config = None
        self.hybrid_transpiler = None
        self.llm_agent = None
        
    def display_banner(self):
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
        
        # Create "Powered by Astrio" text with styling - centered and bigger
        powered_text = Text()
        powered_text.append("Powered by ", style="white")
        powered_text.append("Astrio", style="bold #0053D6")
        
        self.console.print(banner_art)
        # Center the text using Rich's built-in centering
        centered_text = Text("Powered by ", style="white") + Text("Astrio", style="bold #0053D6")
        self.console.print(centered_text, justify="center")
        self.console.print()  # Add padding under the text
        
    def display_tips(self):
        """Display helpful tips for getting started."""
        tips = [
            "💡 Transpile COBOL files to modern Python code",
            "💡 Use natural language to describe your transformation needs", 
            "💡 Get AI-powered analysis and optimization suggestions",
            "💡 Type /help for more information"
        ]
        
        tip_text = "\n".join(tips)
        panel = Panel(
            tip_text,
            title="[bold #0053D6]Tips for getting started:[/bold #0053D6]",
            border_style="#0053D6",
            padding=(1, 2),
        )
        self.console.print(panel)
        
    def initialize_components(self):
        """Initialize LLM components."""
        try:
            self.llm_config = LLMConfig.from_env()
            self.hybrid_transpiler = HybridTranspiler(self.llm_config)
            self.llm_agent = LLMAgent(self.llm_config)
            return True
        except Exception as e:
            self.console.print(f"[red]Warning: LLM components not available: {e}[/red]")
            return False
            
    def get_status_info(self):
        """Get current status information."""
        status_items = []
        
        # Check if we're in a git repo
        try:
            import subprocess
            result = subprocess.run(['git', 'rev-parse', '--show-toplevel'], 
                                 capture_output=True, text=True)
            if result.returncode == 0:
                repo_path = Path(result.stdout.strip()).name
                status_items.append(f"📁 {repo_path}")
        except:
            pass
            
        # Check LLM availability
        if self.llm_config and (self.llm_config.api_key or self.llm_config.provider == "local"):
            status_items.append(f"🤖 {self.llm_config.provider} ({self.llm_config.model})")
        else:
            status_items.append("🤖 no LLM (see /docs)")

        return status_items
        
    def display_status(self):
        """Display status information at the bottom."""
        status_items = self.get_status_info()
        status_text = " • ".join(status_items)
        
        self.console.print(f"\n[dim]{status_text}[/dim]")
        
    def transpile_file(self, input_file: str, output_file: Optional[str] = None) -> bool:
        """Transpile a COBOL file to Python."""
        try:
            if not os.path.exists(input_file):
                self.console.print(f"[red]Error: File not found: {input_file}[/red]")
                return False
                
            # Create output file path if not provided
            if output_file is None:
                input_path = Path(input_file)
                output_file = input_path.with_suffix('.py').name
                
            with Progress(
                SpinnerColumn(),
                TextColumn("[progress.description]{task.description}"),
                console=self.console
            ) as progress:
                
                task = progress.add_task("Transpiling COBOL to Python...", total=None)
                
                # Read source code
                with open(input_file, 'r') as f:
                    source_code = f.read()
                    
                # Transpile
                target_code = self.hybrid_transpiler.transpile_source(source_code, input_file)
                
                # Write output
                with open(output_file, 'w') as f:
                    f.write(target_code)
                    
                progress.update(task, description="✅ Transpilation completed!")
                
            # Display results
            self.console.print(f"\n[#0053D6]✅ Successfully transpiled: {input_file} → {output_file}[/#0053D6]")
            
            # Show code preview
            self.show_code_preview(source_code, target_code)
            
            return True
            
        except Exception as e:
            self.console.print(f"[#FF6B6B]Error during transpilation: {e}[/#FF6B6B]")
            return False
            
    def show_code_preview(self, source_code: str, target_code: str):
        """Show a preview of the source and target code."""
        layout = Layout()
        
        # Create source code panel
        source_syntax = Syntax(source_code, "cobol", theme="monokai", line_numbers=True)
        source_panel = Panel(source_syntax, title="[bold #0053D6]Source COBOL[/bold #0053D6]", width=60)
        
        # Create target code panel  
        target_syntax = Syntax(target_code, "python", theme="monokai", line_numbers=True)
        target_panel = Panel(target_syntax, title="[bold #0053D6]Generated Python[/bold #0053D6]", width=60)
        
        # Display side by side
        self.console.print("\n[bold]Code Preview:[/bold]")
        self.console.print(Panel.fit(
            f"{source_panel}\n{target_panel}",
            title="[bold]Transpilation Result[/bold]",
            border_style="#0053D6"
        ))
        
    def analyze_code(self, source_code: str, target_code: str):
        """Analyze the transpiled code."""
        if not self.llm_agent:
            self.console.print("[#FFA500]LLM analysis not available[/#FFA500]")
            return
            
        with Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            console=self.console
        ) as progress:
            
            task = progress.add_task("Analyzing code transformation...", total=None)
            
            # Perform analysis
            analysis_result = self.llm_agent.analyze_code(source_code, target_code, "cobol-python")
            review_result = self.llm_agent.review_code(target_code, "python")
            optimization_result = self.llm_agent.optimize_code(target_code, "python")
            
            progress.update(task, description="✅ Analysis completed!")
            
        # Display analysis results
        self.display_analysis_results(analysis_result, review_result, optimization_result)
        
    def display_analysis_results(self, analysis_result, review_result, optimization_result):
        """Display analysis results in a formatted table."""
        table = Table(title="[bold]Code Analysis Results[/bold]")
        table.add_column("Metric", style="#0053D6")
        table.add_column("Value", style="#0053D6")
        
        table.add_row("Complexity Score", f"{analysis_result.complexity_score:.2f}")
        table.add_row("Maintainability Score", f"{analysis_result.maintainability_score:.2f}")
        table.add_row("Review Confidence", f"{review_result.confidence:.2f}")
        table.add_row("Optimization Confidence", f"{optimization_result.confidence:.2f}")
        
        self.console.print(table)
        
        # Show suggestions if any
        if analysis_result.suggestions:
            self.console.print("\n[bold #FFA500]Suggestions:[/bold #FFA500]")
            for suggestion in analysis_result.suggestions:
                self.console.print(f"  • {suggestion}")
                
    def interactive_mode(self):
        """Run in interactive mode with natural language commands."""
        self.console.print("\n[bold]Interactive Mode[/bold]")
        self.console.print("Type your commands or questions. Type /help for available commands.")
        
        # Command completions
        completions = WordCompleter([
            '/help', '/transpile', '/analyze', '/optimize', '/exit', '/quit',
            'transpile', 'analyze', 'optimize', 'help', 'exit', 'quit'
        ]) if WordCompleter else None
        
        while True:
            try:
                # Get user input
                if self.session:
                    user_input = self.session.prompt(
                        "> ",
                        completer=completions
                    ).strip()
                else:
                    user_input = input("> ").strip()
                
                if not user_input:
                    continue
                    
                # Handle commands
                if user_input.startswith('/'):
                    self.handle_command(user_input[1:])
                else:
                    self.handle_natural_language(user_input)
                    
            except KeyboardInterrupt:
                self.console.print("\n[#FFA500]Use /exit to quit[/#FFA500]")
            except EOFError:
                break
                
    def handle_command(self, command: str):
        """Handle slash commands."""
        parts = command.split()
        cmd = parts[0].lower()
        
        if cmd == 'help':
            self.show_help()
        elif cmd == 'transpile':
            if len(parts) < 2:
                self.console.print("[red]Usage: /transpile <filename>[/red]")
            else:
                self.transpile_file(parts[1])
        elif cmd == 'analyze':
            if len(parts) < 2:
                self.console.print("[red]Usage: /analyze <filename>[/red]")
            else:
                self.analyze_file(parts[1])
        elif cmd == 'exit':
            self.console.print("[#0053D6]Goodbye![/#0053D6]")
            sys.exit(0)
        elif cmd == 'quit':
            self.console.print("[#0053D6]Goodbye![/#0053D6]")
            sys.exit(0)
        else:
            self.console.print(f"[#FF6B6B]Unknown command: {cmd}[/#FF6B6B]")
            
    def handle_natural_language(self, query: str):
        """Handle natural language queries."""
        # Simple keyword-based parsing for now
        query_lower = query.lower()
        
        if 'transpile' in query_lower or 'convert' in query_lower:
            # Extract filename from query
            words = query.split()
            for word in words:
                if word.endswith('.cobol') or word.endswith('.cob'):
                    self.transpile_file(word)
                    return
            self.console.print("[#FFA500]Please specify a COBOL file to transpile[/#FFA500]")
            
        elif 'analyze' in query_lower or 'review' in query_lower:
            self.console.print("[#FFA500]Please use /analyze <filename> to analyze a file[/#FFA500]")
            
        elif 'help' in query_lower:
            self.show_help()
            
        else:
            self.console.print("[#FFA500]I'm not sure how to help with that. Try /help for available commands.[/#FFA500]")
            
    def show_help(self):
        """Show help information."""
        help_text = """
[bold]Available Commands:[/bold]

[bold blue]Basic Commands:[/bold blue]
  /transpile <file>    - Transpile a COBOL file to Python
  /analyze <file>      - Analyze and review transpiled code
  /help                - Show this help message
  /exit, /quit         - Exit the CLI

[bold blue]Natural Language:[/bold blue]
  "transpile HELLO.cobol"     - Transpile a specific file
  "analyze my code"           - Analyze the last transpiled code
  "help"                      - Show help

[bold blue]Examples:[/bold blue]
  > transpile examples/cobol/HELLO.cobol
  > /transpile examples/cobol/HELLO.cobol
  > analyze the generated Python code
        """
        
        self.console.print(Panel(help_text, title="[bold]Help[/bold]", border_style="#0053D6"))
        
    def analyze_file(self, filename: str):
        """Analyze a specific file."""
        if not os.path.exists(filename):
            self.console.print(f"[#FF6B6B]File not found: {filename}[/#FF6B6B]")
            return
            
        # Check if it's a COBOL file
        if filename.endswith(('.cobol', '.cob')):
            self.console.print("[#FFA500]Please transpile the COBOL file first, then analyze the generated Python file[/#FFA500]")
            return
            
        # Check if it's a Python file
        if filename.endswith('.py'):
            with open(filename, 'r') as f:
                code = f.read()
                
            if self.llm_agent:
                self.console.print(f"[#0053D6]Analyzing {filename}...[/#0053D6]")
                review_result = self.llm_agent.review_code(code, "python")
                optimization_result = self.llm_agent.optimize_code(code, "python")
                self.display_analysis_results(None, review_result, optimization_result)
            else:
                self.console.print("[#FFA500]LLM analysis not available[/#FFA500]")
        else:
            self.console.print("[#FFA500]Please specify a Python file to analyze[/#FFA500]")


def main():
    """Main CLI entry point."""
    cli = Legacy2ModernCLI()
    
    # Display banner and tips
    cli.display_banner()
    cli.display_tips()
    
    # Initialize components
    llm_available = cli.initialize_components()
    
    # Display status
    cli.display_status()
    
    # Start interactive mode
    cli.interactive_mode()


if __name__ == "__main__":
    main() 