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

from engine.modernizers.cobol_system.transpilers.hybrid_transpiler import HybridTranspiler
from engine.modernizers.static_site.transpilers.transpiler import StaticSiteTranspiler as WebsiteTranspiler
from engine.modernizers.static_site.transpilers.agent import WebsiteAgent
from engine.modernizers.cobol_system.transpilers.llm_augmentor import LLMConfig
from engine.agents.agent import LLMAgent


class Legacy2ModernCLI:
    """Modern CLI interface for Legacy2Modern transpilation engine."""
    
    def __init__(self):
        self.console = Console()
        self.session = PromptSession() if PromptSession else None
        self.llm_config = None
        self.hybrid_transpiler = None
        self.website_transpiler = None
        self.website_modernizer = None
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
            "💡 Modernize legacy websites (HTML + Bootstrap + jQuery + PHP)",
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
        """Initialize all CLI components."""
        try:
            # Initialize LLM configuration
            self.llm_config = LLMConfig.from_env()
            
            # Initialize transpilers and agents
            self.hybrid_transpiler = HybridTranspiler(self.llm_config)
            self.website_transpiler = WebsiteTranspiler()
            self.website_modernizer = WebsiteAgent()
            self.llm_agent = LLMAgent(self.llm_config)
            return True
        except Exception as e:
            self.console.print(f"[#FF6B6B]Error initializing components: {e}[/#FF6B6B]")
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
                # Create output directory
                output_dir = Path("output/modernized-python")
                output_dir.mkdir(parents=True, exist_ok=True)
                # Set output file path
                output_file = output_dir / input_path.with_suffix('.py').name
            
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

    def transpile_website(self, input_file: str, output_dir: str, framework: str = 'react') -> bool:
        """Transpile a legacy website to modern framework."""
        try:
            if not os.path.exists(input_file):
                self.console.print(f"[red]Error: File not found: {input_file}[/red]")
                return False
            
            # Validate framework
            supported_frameworks = ['react', 'astro', 'nextjs']
            if framework not in supported_frameworks:
                self.console.print(f"[red]Error: Unsupported framework '{framework}'. Supported: {', '.join(supported_frameworks)}[/red]")
                return False
            
            with Progress(
                SpinnerColumn(),
                TextColumn("[progress.description]{task.description}"),
                console=self.console
            ) as progress:
                
                task = progress.add_task(f"Modernizing website to {framework.upper()}...", total=None)
                
                # Transpile website
                result = self.website_transpiler.transpile_website(
                    input_file, 
                    output_dir, 
                    framework
                )
                
                if result['success']:
                    progress.update(task, description="✅ Website modernization completed!")
                    
                    # Display results
                    self.console.print(f"\n[#0053D6]✅ Successfully modernized: {input_file} → {output_dir}[/#0053D6]")
                    self.console.print(f"[#0053D6]Framework: {framework.upper()}[/#0053D6]")
                    self.console.print(f"[#0053D6]Files generated: {result.get('files_generated', 0)}[/#0053D6]")
                    self.console.print(f"[#0053D6]Components: {result.get('components_count', 0)}[/#0053D6]")
                    self.console.print(f"[#0053D6]Pages: {result.get('pages_count', 0)}[/#0053D6]")
                    
                    # Show next steps
                    self.show_website_next_steps(output_dir, framework)
                    
                    return True
                else:
                    progress.update(task, description="❌ Modernization failed!")
                    self.console.print(f"[#FF6B6B]Error: {result.get('error', 'Unknown error')}[/#FF6B6B]")
                    return False
                    
        except Exception as e:
            self.console.print(f"[#FF6B6B]Error during website modernization: {e}[/#FF6B6B]")
            return False

    def analyze_website(self, input_file: str) -> bool:
        """Analyze a legacy website without generating code."""
        try:
            if not os.path.exists(input_file):
                self.console.print(f"[red]Error: File not found: {input_file}[/red]")
                return False
            
            with Progress(
                SpinnerColumn(),
                TextColumn("[progress.description]{task.description}"),
                console=self.console
            ) as progress:
                
                task = progress.add_task("Analyzing legacy website...", total=None)
                
                # Analyze website
                result = self.website_transpiler.analyze_website(input_file)
                
                if result['success']:
                    progress.update(task, description="✅ Analysis completed!")
                    
                    # Display analysis results
                    self.display_website_analysis(result)
                    
                    return True
                else:
                    progress.update(task, description="❌ Analysis failed!")
                    self.console.print(f"[#FF6B6B]Error: {result.get('error', 'Unknown error')}[/#FF6B6B]")
                    return False
                    
        except Exception as e:
            self.console.print(f"[#FF6B6B]Error during website analysis: {e}[/#FF6B6B]")
            return False

    def display_website_analysis(self, result: dict):
        """Display website analysis results."""
        analysis = result.get('analysis', {})
        
        # Create analysis table
        table = Table(title="Website Analysis Results")
        table.add_column("Metric", style="cyan")
        table.add_column("Value", style="green")
        
        # Add analysis data
        table.add_row("Complexity Score", str(analysis.get('complexity_score', 0)))
        table.add_row("Modernization Effort", analysis.get('modernization_effort', 'unknown').title())
        table.add_row("Recommendations", str(len(analysis.get('recommendations', []))))
        table.add_row("Risks", str(len(analysis.get('risks', []))))
        
        # Add framework detection
        frameworks = result.get('parsed_data', {}).get('frameworks', {})
        detected_frameworks = []
        for framework, detection in frameworks.items():
            if detection.get('detected'):
                version = detection.get('version', '')
                detected_frameworks.append(f"{framework.title()}{' ' + version if version else ''}")
        
        table.add_row("Detected Frameworks", ", ".join(detected_frameworks) if detected_frameworks else "None")
        
        self.console.print(table)
        
        # Show recommendations
        recommendations = analysis.get('recommendations', [])
        if recommendations:
            self.console.print("\n[bold cyan]Recommendations:[/bold cyan]")
            for i, rec in enumerate(recommendations[:5], 1):  # Show first 5
                self.console.print(f"{i}. {rec.get('description', '')}")
        
        # Show risks
        risks = analysis.get('risks', [])
        if risks:
            self.console.print("\n[bold red]Risks:[/bold red]")
            for i, risk in enumerate(risks[:3], 1):  # Show first 3
                self.console.print(f"{i}. {risk.get('description', '')}")

    def show_website_next_steps(self, output_dir: str, framework: str):
        """Show next steps for the modernized website."""
        self.console.print(f"\n[bold #0053D6]Next Steps:[/bold #0053D6]")
        self.console.print(f"1. Navigate to the project: [cyan]cd {output_dir}[/cyan]")
        self.console.print(f"2. Install dependencies: [cyan]npm install[/cyan]")
        self.console.print(f"3. Start development server: [cyan]npm run dev[/cyan]")
        self.console.print(f"4. Open in browser: [cyan]http://localhost:3000[/cyan]")
        self.console.print(f"\n[bold]Deployment:[/bold]")
        self.console.print(f"• Deploy to Netlify: [cyan]netlify deploy[/cyan]")
        self.console.print(f"• Deploy to Vercel: [cyan]vercel[/cyan]")
        self.console.print(f"• Deploy to GitHub Pages: [cyan]npm run build && gh-pages -d dist[/cyan]")
        
        # Offer to open in IDE
        self.console.print(f"\n[bold #0053D6]Quick Actions:[/bold #0053D6]")
        self.console.print(f"💻 Type '/open-ide {output_dir}' to open in your default IDE")
        self.console.print(f"🚀 Type '/start-dev {output_dir}' to start development server")
    
    def open_project_in_ide(self, project_path: str, ide: str = 'auto') -> bool:
        """Open project in IDE."""
        try:
            if not os.path.exists(project_path):
                self.console.print(f"[red]Error: Project path does not exist: {project_path}[/red]")
                return False
            
            with Progress(
                SpinnerColumn(),
                TextColumn("[progress.description]{task.description}"),
                console=self.console
            ) as progress:
                
                task = progress.add_task("Opening project in IDE...", total=None)
                
                # Open in IDE
                success = self.website_transpiler.open_in_ide(project_path, ide)
                
                if success:
                    progress.update(task, description="✅ Project opened in IDE!")
                    self.console.print(f"[#0053D6]✅ Opened project in IDE: {project_path}[/#0053D6]")
                    return True
                else:
                    progress.update(task, description="❌ Failed to open in IDE!")
                    self.console.print(f"[#FF6B6B]Error: Could not open project in IDE[/#FF6B6B]")
                    return False
                    
        except Exception as e:
            self.console.print(f"[#FF6B6B]Error opening project in IDE: {e}[/#FF6B6B]")
            return False
    
    def start_dev_server(self, project_path: str, framework: str = 'react') -> bool:
        """Start development server for the project."""
        try:
            if not os.path.exists(project_path):
                self.console.print(f"[red]Error: Project path does not exist: {project_path}[/red]")
                return False
            
            with Progress(
                SpinnerColumn(),
                TextColumn("[progress.description]{task.description}"),
                console=self.console
            ) as progress:
                
                task = progress.add_task(f"Starting {framework.upper()} development server...", total=None)
                
                # Start dev server
                success = self.website_transpiler.start_dev_server(project_path, framework)
                
                if success:
                    progress.update(task, description="✅ Development server started!")
                    self.console.print(f"[#0053D6]✅ Started {framework.upper()} development server[/#0053D6]")
                    self.console.print(f"[#0053D6]🌐 View at: http://localhost:3000[/#0053D6]")
                    return True
                else:
                    progress.update(task, description="❌ Failed to start dev server!")
                    self.console.print(f"[#FF6B6B]Error: Could not start development server[/#FF6B6B]")
                    return False
                    
        except Exception as e:
            self.console.print(f"[#FF6B6B]Error starting development server: {e}[/#FF6B6B]")
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
        elif cmd == 'modernize':
            if len(parts) < 3:
                self.console.print("[red]Usage: /modernize <input_file> <output_dir> [framework][/red]")
                self.console.print("[red]Frameworks: react, astro, nextjs[/red]")
            else:
                framework = parts[3] if len(parts) > 3 else 'react'
                self.transpile_website(parts[1], parts[2], framework)
        elif cmd == 'modernize-llm':
            if len(parts) < 3:
                self.console.print("[red]Usage: /modernize-llm <input_file> <output_dir>[/red]")
                self.console.print("[red]Uses LLM to generate React TypeScript components[/red]")
            else:
                self.transpile_website_llm(parts[1], parts[2])
        elif cmd == 'analyze-website':
            if len(parts) < 2:
                self.console.print("[red]Usage: /analyze-website <filename>[/red]")
            else:
                self.analyze_website(parts[1])
        elif cmd == 'analyze-website-llm':
            if len(parts) < 2:
                self.console.print("[red]Usage: /analyze-website-llm <filename>[/red]")
                self.console.print("[red]Uses LLM to analyze website structure[/red]")
            else:
                self.analyze_website_llm(parts[1])
        elif cmd == 'check-llm':
            self.check_llm_status()
        elif cmd == 'open-ide':
            if len(parts) < 2:
                self.console.print("[red]Usage: /open-ide <project_path> [ide][/red]")
                self.console.print("[red]IDEs: auto, vscode, webstorm, sublime, atom[/red]")
            else:
                ide = parts[2] if len(parts) > 2 else 'auto'
                self.open_project_in_ide(parts[1], ide)
        elif cmd == 'start-dev':
            if len(parts) < 2:
                self.console.print("[red]Usage: /start-dev <project_path> [framework][/red]")
                self.console.print("[red]Frameworks: react, nextjs, astro[/red]")
            else:
                framework = parts[2] if len(parts) > 2 else 'react'
                self.start_dev_server(parts[1], framework)
        elif cmd == 'frameworks':
            self.console.print("[bold cyan]Supported Frameworks:[/bold cyan]")
            self.console.print("• [green]react[/green] - React with Vite and Tailwind CSS")
            self.console.print("• [green]astro[/green] - Astro with Tailwind CSS")
            self.console.print("• [green]nextjs[/green] - Next.js with TypeScript and Tailwind CSS")
            self.console.print("• [green]react-llm[/green] - React TypeScript with LLM-powered generation")
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
            
        elif 'modernize' in query_lower or 'website' in query_lower:
            # Check if user wants LLM-based modernization
            if 'llm' in query_lower or 'ai' in query_lower:
                # Extract filename from query
                words = query.split()
                html_files = [word for word in words if word.endswith(('.html', '.htm'))]
                if html_files:
                    input_file = html_files[0]
                    output_dir = f"output/modernized-{input_file.replace('.html', '').replace('.htm', '')}-llm"
                    self.transpile_website_llm(input_file, output_dir)
                    return
                self.console.print("[#FFA500]Please specify an HTML file to modernize with LLM[/#FFA500]")
            else:
                # Extract filename from query
                words = query.split()
                html_files = [word for word in words if word.endswith(('.html', '.htm'))]
                if html_files:
                    input_file = html_files[0]
                    output_dir = f"output/modernized-{input_file.replace('.html', '').replace('.htm', '')}"
                    self.transpile_website(input_file, output_dir)
                    return
                self.console.print("[#FFA500]Please specify an HTML file to modernize[/#FFA500]")
            
        elif 'analyze' in query_lower or 'review' in query_lower:
            if 'website' in query_lower:
                # Check if user wants LLM-based analysis
                if 'llm' in query_lower or 'ai' in query_lower:
                    # Extract filename from query
                    words = query.split()
                    html_files = [word for word in words if word.endswith(('.html', '.htm'))]
                    if html_files:
                        self.analyze_website_llm(html_files[0])
                        return
                    self.console.print("[#FFA500]Please specify an HTML file to analyze with LLM[/#FFA500]")
                else:
                    # Extract filename from query
                    words = query.split()
                    html_files = [word for word in words if word.endswith(('.html', '.htm'))]
                    if html_files:
                        self.analyze_website(html_files[0])
                        return
                    self.console.print("[#FFA500]Please specify an HTML file to analyze[/#FFA500]")
            else:
                self.console.print("[#FFA500]Please use /analyze <filename> to analyze a file[/#FFA500]")
            
        elif 'check' in query_lower and 'llm' in query_lower:
            self.check_llm_status()
            
        elif 'help' in query_lower:
            self.show_help()
            
        else:
            self.console.print("[#FFA500]I'm not sure how to help with that. Try /help for available commands.[/#FFA500]")
            
    def show_help(self):
        """Show help information."""
        help_text = """
[bold]Available Commands:[/bold]

[bold blue]COBOL Transpilation:[/bold blue]
  /transpile <file>               - Transpile a COBOL file to Python
  /analyze <file>                 - Analyze and review transpiled code

[bold blue]Website Modernization:[/bold blue]
  /modernize <file> <output> [framework]     - Modernize legacy website
  /modernize-llm <file> <output>  - Modernize legacy website using LLM
  /analyze-website <file>         - Analyze legacy website
  /analyze-website-llm <file>     - Analyze legacy website using LLM
  /open-ide <project> [ide]            - Open project in IDE
  /start-dev <project> [framework]           - Start development server
  /frameworks                     - List supported frameworks

[bold blue]General Commands:[/bold blue]
  /help                           - Show this help message
  /exit, /quit                    - Exit the CLI

[bold blue]Natural Language:[/bold blue]
  "transpile HELLO.cobol"         - Transpile a specific COBOL file
  "modernize my-website.html"     - Modernize a website
  "analyze my code"               - Analyze the last transpiled code
  "help"                          - Show help

[bold blue]Examples:[/bold blue]
  > transpile examples/cobol/HELLO.cobol
  > /transpile examples/cobol/HELLO.cobol
  > modernize legacy-site.html output/modernized-site react
  > /modernize legacy-site.html output/modernized-site astro
  > analyze-website legacy-site.html
  > /open-ide output/react vscode
  > /start-dev output/nextjs nextjs
  > analyze the generated Python code

[bold blue]Supported Frameworks:[/bold blue]
  • react   - React with Vite and Tailwind CSS
  • astro   - Astro with Tailwind CSS
  • nextjs  - Next.js with TypeScript and Tailwind CSS
  • react-llm - React TypeScript with LLM-powered generation
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

    def transpile_website_llm(self, input_file: str, output_dir: str) -> bool:
        """Transpile a legacy website to React TypeScript using LLM."""
        try:
            if not os.path.exists(input_file):
                self.console.print(f"[red]Error: File not found: {input_file}[/red]")
                return False
            
            with Progress(
                SpinnerColumn(),
                TextColumn("[progress.description]{task.description}"),
                console=self.console
            ) as progress:
                
                task = progress.add_task("Modernizing website with LLM...", total=None)
                
                # Modernize website using LLM
                result = self.website_modernizer.modernize_website(input_file, output_dir)
                
                progress.update(task, description="✅ LLM website modernization completed!")
                
                # Display results
                self.console.print(f"\n[#0053D6]✅ Successfully modernized: {input_file} → {output_dir}[/#0053D6]")
                self.console.print(f"[#0053D6]Framework: React TypeScript[/#0053D6]")
                self.console.print(f"[#0053D6]Components generated: {len(result.components)}[/#0053D6]")
                self.console.print(f"[#0053D6]Confidence: {result.confidence:.2f}[/#0053D6]")
                
                # Show next steps
                self.show_website_llm_next_steps(output_dir)
                
                return True
                    
        except Exception as e:
            self.console.print(f"[#FF6B6B]Error during LLM website modernization: {e}[/#FF6B6B]")
            return False

    def analyze_website_llm(self, input_file: str) -> bool:
        """Analyze a legacy website using LLM without generating code."""
        try:
            if not os.path.exists(input_file):
                self.console.print(f"[red]Error: File not found: {input_file}[/red]")
                return False
            
            with Progress(
                SpinnerColumn(),
                TextColumn("[progress.description]{task.description}"),
                console=self.console
            ) as progress:
                
                task = progress.add_task("Analyzing legacy website with LLM...", total=None)
                
                # Analyze website using LLM
                analysis = self.website_modernizer._analyze_website(input_file)
                
                progress.update(task, description="✅ LLM analysis completed!")
                
                # Display analysis results
                self.display_website_llm_analysis(analysis)
                
                return True
                    
        except Exception as e:
            self.console.print(f"[#FF6B6B]Error during LLM website analysis: {e}[/#FF6B6B]")
            return False

    def display_website_llm_analysis(self, analysis):
        """Display LLM website analysis results."""
        # Create analysis table
        table = Table(title="LLM Website Analysis Results")
        table.add_column("Metric", style="cyan")
        table.add_column("Value", style="green")
        
        # Add analysis data
        components = analysis.components
        styles = analysis.styles
        scripts = analysis.scripts
        
        table.add_row("Components Found", str(len(components)))
        table.add_row("Styles Found", str(len(styles)))
        table.add_row("Scripts Found", str(len(scripts)))
        
        # Show component details
        if components:
            self.console.print("\n[bold cyan]Components:[/bold cyan]")
            for i, component in enumerate(components[:5], 1):
                component_type = component.get('type', 'unknown')
                component_title = component.get('title', 'No title')
                self.console.print(f"  {i}. {component_type} - {component_title}")
            
            if len(components) > 5:
                self.console.print(f"  ... and {len(components) - 5} more components")
        
        self.console.print(table)

    def show_website_llm_next_steps(self, output_dir: str):
        """Show next steps for LLM modernized website."""
        self.console.print(f"\n[bold green]🚀 Next Steps:[/bold green]")
        self.console.print(f"1. [cyan]Navigate to project:[/cyan] cd {output_dir}")
        self.console.print(f"2. [cyan]Install dependencies:[/cyan] npm install")
        self.console.print(f"3. [cyan]Start development server:[/cyan] npm start")
        self.console.print(f"4. [cyan]Open in browser:[/cyan] http://localhost:3000")
        
        # Ask if user wants to start the dev server
        if Confirm.ask("Would you like to start the development server now?"):
            self.start_dev_server(output_dir, 'react')

    def check_llm_status(self) -> bool:
        """Check if LLM is available and working."""
        try:
            with Progress(
                SpinnerColumn(),
                TextColumn("[progress.description]{task.description}"),
                console=self.console
            ) as progress:
                
                task = progress.add_task("Checking LLM availability...", total=None)
                
                # Test LLM connection based on provider
                if self.llm_config.provider == "anthropic":
                    # Test Claude API
                    from engine.modernizers.static_site.transpilers.llm_augmentor import StaticSiteClaudeProvider
                    
                    provider = StaticSiteClaudeProvider()
                    test_messages = [
                        {"role": "system", "content": "You are a helpful assistant."},
                        {"role": "user", "content": "Say 'Hello, Claude is working!'"}
                    ]
                    
                    response = provider.generate_response(test_messages, self.llm_config)
                    
                    if response:
                        progress.update(task, description="✅ Claude API is available and working!")
                        
                        self.console.print(f"\n[#0053D6]✅ LLM Status Check[/#0053D6]")
                        self.console.print(f"[#0053D6]Status: Available and working[/#0053D6]")
                        self.console.print(f"[#0053D6]Provider: Claude API (Anthropic)[/#0053D6]")
                        self.console.print(f"[#0053D6]Model: {self.llm_config.model}[/#0053D6]")
                        self.console.print(f"[#0053D6]Test response: {response[:50]}...[/#0053D6]")
                        
                        return True
                    else:
                        progress.update(task, description="❌ Claude API is not responding!")
                        self.console.print(f"\n[#FF6B6B]❌ LLM Status Check[/#FF6B6B]")
                        self.console.print(f"[#FF6B6B]Status: Not available or not responding[/#FF6B6B]")
                        self.console.print(f"[#FF6B6B]💡 Check your Claude API key: LLM_API_KEY[/#FF6B6B]")
                        self.console.print(f"[#FF6B6B]💡 Make sure you have credits in your Anthropic account[/#FF6B6B]")
                        
                        return False
                        
                elif self.llm_config.provider == "local":
                    # Test Ollama
                    from engine.modernizers.static_site.transpilers.llm_augmentor import StaticSiteLocalProvider
                    
                    provider = StaticSiteLocalProvider()
                    test_messages = [
                        {"role": "system", "content": "You are a helpful assistant."},
                        {"role": "user", "content": "Say 'Hello, LLM is working!'"}
                    ]
                    
                    response = provider.generate_response(test_messages, self.llm_config)
                    
                    if response:
                        progress.update(task, description="✅ Ollama is available and working!")
                        
                        self.console.print(f"\n[#0053D6]✅ LLM Status Check[/#0053D6]")
                        self.console.print(f"[#0053D6]Status: Available and working[/#0053D6]")
                        self.console.print(f"[#0053D6]Provider: Local (Ollama)[/#0053D6]")
                        self.console.print(f"[#0053D6]Model: {self.llm_config.model}[/#0053D6]")
                        self.console.print(f"[#0053D6]Test response: {response[:50]}...[/#0053D6]")
                        
                        return True
                    else:
                        progress.update(task, description="❌ Ollama is not responding!")
                        self.console.print(f"\n[#FF6B6B]❌ LLM Status Check[/#FF6B6B]")
                        self.console.print(f"[#FF6B6B]Status: Not available or not responding[/#FF6B6B]")
                        self.console.print(f"[#FF6B6B]💡 Make sure Ollama is running: ollama serve[/#FF6B6B]")
                        self.console.print(f"[#FF6B6B]💡 Make sure the model is installed: ollama pull llama2[/#FF6B6B]")
                        
                        return False
                else:
                    progress.update(task, description="❌ Unsupported provider!")
                    self.console.print(f"\n[#FF6B6B]❌ LLM Status Check[/#FF6B6B]")
                    self.console.print(f"[#FF6B6B]Error: Unsupported provider '{self.llm_config.provider}'[/#FF6B6B]")
                    self.console.print(f"[#FF6B6B]Supported providers: anthropic, local[/#FF6B6B]")
                    
                    return False
                    
        except Exception as e:
            self.console.print(f"\n[#FF6B6B]❌ LLM Status Check[/#FF6B6B]")
            self.console.print(f"[#FF6B6B]Error: {e}[/#FF6B6B]")
            if self.llm_config.provider == "anthropic":
                self.console.print(f"[#FF6B6B]💡 Check your Claude API key: LLM_API_KEY[/#FF6B6B]")
                self.console.print(f"[#FF6B6B]💡 Make sure you have credits in your Anthropic account[/#FF6B6B]")
            else:
                self.console.print(f"[#FF6B6B]💡 Make sure Ollama is running: ollama serve[/#FF6B6B]")
                self.console.print(f"[#FF6B6B]💡 Make sure the model is installed: ollama pull llama2[/#FF6B6B]")
            return False


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