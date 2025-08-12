"""
Modern CLI Interface for Legacy2Modern Agents

A beautiful, interactive command-line interface for the AutoGen-based
multi-agent modernization system.
"""

# Suppress warnings early
import warnings
import sys
import os

# Temporarily redirect stderr and stdout to suppress import warnings
original_stderr = sys.stderr
original_stdout = sys.stdout
sys.stderr = open(os.devnull, 'w')
sys.stdout = open(os.devnull, 'w')

warnings.filterwarnings('ignore', category=UserWarning)
warnings.filterwarnings('ignore', message='AutoGen import failed')
warnings.filterwarnings('ignore', message='TextAnalyzerAgent not available')

import os
import sys
import asyncio
from pathlib import Path
from typing import Optional, List, Dict, Any
import json

# Load environment variables from .env file
try:
    from dotenv import load_dotenv
    # Find and load .env file from project root
    project_root = Path(__file__).parent.parent.parent
    env_path = project_root / '.env'
    if env_path.exists():
        load_dotenv(env_path)
    else:
        # Fallback to current directory
        load_dotenv()
except ImportError:
    pass

# Suppress OpenSSL warnings
try:
    import urllib3
    warnings.filterwarnings('ignore', category=urllib3.exceptions.NotOpenSSLWarning)
except ImportError:
    pass

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

# Import agents system components (with stderr redirected to suppress warnings)
sys.stderr = open(os.devnull, 'w')
from engine.agents import (
    AI, BaseAgent, BaseExecutionEnv, ChatToFiles, DiffProcessor,
    FilesDict, GitManager, LintingManager, PrepromptsHolder, ProjectConfig,
    PromptBuilder, TokenUsageTracker, VersionManager, ParserAgent,
    ModernizerAgent, RefactorAgent, QAAgent, CoordinatorAgent
)
from engine.agents.core_agents.base_memory import FileMemory
from engine.agents.core_agents.base_agent import AgentRole

# Import AutoGen integration modules (with stderr redirected to suppress warnings)
from engine.agents.autogen_integration.autogen_wrapper import AutoGenAgentWrapper, AutoGenConfig
from engine.agents.autogen_integration.group_chat_coordinator import GroupChatCoordinator
from engine.agents.autogen_integration.sandbox_executor import (
    SandboxConfig, SandboxExecutor, SandboxAgent,
    create_sandbox_executor, create_sandbox_agent, execute_in_sandbox
)


# Restore stderr and stdout after imports
sys.stderr.close()
sys.stdout.close()
sys.stderr = original_stderr
sys.stdout = original_stdout

"""
AutoGen integration layer for legacy2modern agents.

This module provides wrapper classes to integrate existing agents with AutoGen's
ConversableAgent framework while preserving domain-specific logic.
"""

import asyncio
import logging
from typing import Dict, List, Optional, Any, Union, Callable
from dataclasses import dataclass, field

# AutoGen imports
try:
    import autogen
    from autogen import ConversableAgent, AssistantAgent, UserProxyAgent
    try:
        from autogen.agentchat.contrib.text_analyzer_agent import TextAnalyzerAgent
    except ImportError:
        TextAnalyzerAgent = None
    AUTOGEN_AVAILABLE = True
except ImportError:
    AUTOGEN_AVAILABLE = False
    TextAnalyzerAgent = None
    # Fallback classes for when AutoGen is not available
    class ConversableAgent:
        def __init__(self, *args, **kwargs):
            raise ImportError("AutoGen not installed. Run: pip install -U 'autogen-agentchat' 'autogen-ext[openai]'")
    
    class AssistantAgent:
        def __init__(self, *args, **kwargs):
            raise ImportError("AutoGen not installed. Run: pip install -U 'autogen-agentchat' 'autogen-ext[openai]'")
    
    class UserProxyAgent:
        def __init__(self, *args, **kwargs):
            raise ImportError("AutoGen not installed. Run: pip install -U 'autogen-agentchat' 'autogen-ext[openai]'")

# These imports are already handled above, removing duplicates

logger = logging.getLogger(__name__)

@dataclass
class AutoGenConfig:
    """Configuration for AutoGen integration."""
    enable_autogen: bool = True
    use_group_chat: bool = False
    human_in_the_loop: bool = False
    max_consecutive_auto_reply: int = 10
    llm_config: Optional[Dict[str, Any]] = None

class AutoGenAgentWrapper:
    """
    Wrapper class that adapts existing BaseAgent to AutoGen's ConversableAgent.
    
    This allows gradual migration to AutoGen while preserving existing domain logic.
    """
    
    def __init__(
        self,
        base_agent: BaseAgent,
        autogen_config: Optional[AutoGenConfig] = None,
        **kwargs
    ):
        self.base_agent = base_agent
        self.autogen_config = autogen_config or AutoGenConfig()
        
        # Create AutoGen agent
        self.autogen_agent = self._create_autogen_agent(**kwargs)
        
        # Bridge between systems
        self.message_bridge = MessageBridge()
        
        logger.info(f"Created AutoGen wrapper for {base_agent.name} ({base_agent.role.value})")
        
    def _create_autogen_agent(self, **kwargs) -> ConversableAgent:
        """Create the underlying AutoGen agent."""
        if not AUTOGEN_AVAILABLE:
            raise ImportError("AutoGen not available. Install with: pip install -U 'autogen-agentchat' 'autogen-ext[openai]'")
        
        # Convert our AI wrapper to AutoGen LLM config
        llm_config = self._convert_ai_to_llm_config()
        
        # Create agent based on role
        if self.base_agent.role == AgentRole.COORDINATOR:
            return AssistantAgent(
                name=self.base_agent.name,
                system_message=self.base_agent.system_prompt,
                llm_config=llm_config,
                max_consecutive_auto_reply=self.autogen_config.max_consecutive_auto_reply,
                **kwargs
            )
        else:
            return ConversableAgent(
                name=self.base_agent.name,
                system_message=self.base_agent.system_prompt,
                llm_config=llm_config,
                max_consecutive_auto_reply=self.autogen_config.max_consecutive_auto_reply,
                **kwargs
            )
    
    def _convert_ai_to_llm_config(self) -> Dict[str, Any]:
        """Convert our AI wrapper to AutoGen LLM config format."""
        ai = self.base_agent.ai
        
        # Base config
        config = {
            "config_list": [{
                "model": ai.model,
                "api_key": ai.api_key,
            }],
            "temperature": ai.temperature,
            "max_tokens": ai.max_tokens,
        }
        
        # Add provider-specific settings
        if ai.provider == "openai":
            config["config_list"][0]["api_base"] = "https://api.openai.com/v1"
        elif ai.provider == "anthropic":
            config["config_list"][0]["api_base"] = "https://api.anthropic.com"
            config["config_list"][0]["api_type"] = "anthropic"
        
        return config
    
    async def process_message(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """Process message through both systems."""
        # First, process through our domain logic
        domain_response = await self.base_agent.process_message(message)
        
        # Then, if AutoGen is enabled, process through AutoGen
        if self.autogen_config.enable_autogen:
            autogen_response = await self._process_autogen_message(message)
            # Merge responses
            return self._merge_responses(domain_response, autogen_response)
        
        return domain_response
    
    async def _process_autogen_message(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """Process message through AutoGen system."""
        try:
            # Convert our message format to AutoGen format
            autogen_message = self.message_bridge.to_autogen_format(message)
            
            # Process through AutoGen
            response = await self.autogen_agent.a_generate_reply(
                messages=[autogen_message],
                sender=None
            )
            
            # Convert back to our format
            return self.message_bridge.from_autogen_format(response)
            
        except Exception as e:
            logger.error(f"AutoGen processing failed: {e}")
            return {"error": str(e)}
    
    def _merge_responses(self, domain_response: Dict[str, Any], autogen_response: Dict[str, Any]) -> Dict[str, Any]:
        """Merge responses from both systems."""
        merged = domain_response.copy()
        
        # Add AutoGen insights if available
        if "content" in autogen_response:
            merged["autogen_insights"] = autogen_response["content"]
        
        if "suggestions" in autogen_response:
            merged["autogen_suggestions"] = autogen_response["suggestions"]
        
        return merged
    
    async def execute_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Execute task through the base agent."""
        return await self.base_agent.execute_task(task)
    
    def get_status(self) -> Dict[str, Any]:
        """Get combined status from both systems."""
        base_status = self.base_agent.get_status()
        base_status["autogen_enabled"] = self.autogen_config.enable_autogen
        base_status["autogen_agent_type"] = type(self.autogen_agent).__name__
        return base_status

class MessageBridge:
    """Bridges message formats between our system and AutoGen."""
    
    def to_autogen_format(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """Convert our message format to AutoGen format."""
        return {
            "role": "user",
            "content": self._format_content_for_autogen(message),
            "name": message.get("sender", "unknown")
        }
    
    def from_autogen_format(self, response: Any) -> Dict[str, Any]:
        """Convert AutoGen response to our format."""
        if hasattr(response, 'content'):
            return {
                "type": "response",
                "content": response.content,
                "sender": getattr(response, 'name', 'autogen_agent')
            }
        elif isinstance(response, dict):
            return response
        else:
            return {
                "type": "response",
                "content": str(response),
                "sender": "autogen_agent"
            }
    
    def _format_content_for_autogen(self, message: Dict[str, Any]) -> str:
        """Format message content for AutoGen consumption."""
        message_type = message.get("type", "unknown")
        
        if message_type == "task":
            return f"Task: {message.get('task_type', 'unknown')}\nDetails: {message.get('details', {})}"
        elif message_type == "analysis":
            return f"Analysis Request: {message.get('content', '')}"
        elif message_type == "response":
            return f"Response: {message.get('content', '')}"
        else:
            return str(message)

class AutoGenCoordinator:
    """
    Coordinator that manages AutoGen group chats and agent interactions.
    """
    
    def __init__(
        self,
        agents: List[AutoGenAgentWrapper],
        config: Optional[AutoGenConfig] = None
    ):
        self.agents = agents
        self.config = config or AutoGenConfig()
        self.group_chat = None
        
        if self.config.use_group_chat:
            self._setup_group_chat()
    
    def _setup_group_chat(self):
        """Set up AutoGen group chat."""
        if not AUTOGEN_AVAILABLE:
            logger.warning("AutoGen not available, group chat disabled")
            return
        
        # Create group chat with all agents
        autogen_agents = [agent.autogen_agent for agent in self.agents]
        
        self.group_chat = autogen.GroupChat(
            agents=autogen_agents,
            messages=[],
            max_round=50
        )
        
        # Create manager
        self.manager = autogen.GroupChatManager(
            groupchat=self.group_chat,
            llm_config=self._get_manager_llm_config()
        )
    
    def _get_manager_llm_config(self) -> Dict[str, Any]:
        """Get LLM config for the group chat manager."""
        # Use the first agent's config as default
        if self.agents:
            return self.agents[0]._convert_ai_to_llm_config()
        return {}
    
    async def coordinate_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Coordinate a task across all agents using AutoGen."""
        if not self.group_chat:
            # Fallback to traditional coordination
            return await self._traditional_coordination(task)
        
        try:
            # Convert task to AutoGen message
            bridge = MessageBridge()
            autogen_message = bridge.to_autogen_format({
                "type": "task",
                "task_type": task.get("type", "unknown"),
                "details": task
            })
            
            # Run group chat
            response = await self.manager.a_run(
                messages=[autogen_message],
                sender=None
            )
            
            return bridge.from_autogen_format(response)
            
        except Exception as e:
            logger.error(f"AutoGen coordination failed: {e}")
            return await self._traditional_coordination(task)
    
    async def _traditional_coordination(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Fallback to traditional agent coordination."""
        # Find coordinator agent
        coordinator = next(
            (agent for agent in self.agents 
             if agent.base_agent.role == AgentRole.COORDINATOR),
            None
        )
        
        if coordinator:
            return await coordinator.execute_task(task)
        else:
            return {"error": "No coordinator agent found"}

# Factory function for easy agent creation
def create_autogen_wrapped_agent(
    base_agent: BaseAgent,
    enable_autogen: bool = True,
    **kwargs
) -> AutoGenAgentWrapper:
    """Create an AutoGen-wrapped agent."""
    config = AutoGenConfig(enable_autogen=enable_autogen)
    return AutoGenAgentWrapper(base_agent, config, **kwargs)


class Legacy2ModernCLI:
    """Modern CLI interface for Legacy2Modern agents system."""
    
    def __init__(self):
        self.console = Console()
        
        # Agent system components
        self.ai = None
        self.memory = None
        self.config = None
        self.coordinator = None
        self.parser_agent = None
        self.modernizer_agent = None
        self.refactor_agent = None
        self.qa_agent = None
        
        # Comment out modernizer components - not using them
        # self.llm_config = None
        # self.hybrid_transpiler = None
        # self.website_transpiler = None
        # self.website_modernizer = None
        # self.llm_agent = None
        
        # Add AutoGen support
        self.use_autogen = os.getenv('USE_AUTOGEN', 'false').lower() == 'true'
        self.autogen_agents = {}
        
        # Add Sandbox support
        self.use_sandbox = os.getenv('USE_SANDBOX', 'true').lower() == 'true'
        self.sandbox_executor = None
        self.sandbox_agent = None
        
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
            "🤖 Multi-agent modernization system powered by AutoGen",
            "💡 Use natural language to describe your transformation needs", 
            "💡 Get AI-powered analysis and optimization suggestions",
            "💡 Coordinate multiple specialized agents for complex tasks",
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
        
    async def initialize_components(self):
        """Initialize all CLI components."""
        try:
            # Initialize AI configuration
            api_key = os.getenv('LLM_API_KEY') or os.getenv('OPENAI_API_KEY') or os.getenv('ANTHROPIC_API_KEY')
            if not api_key:
                self.console.print("[#FF6B6B]Warning: No LLM API key found. Set LLM_API_KEY, OPENAI_API_KEY, or ANTHROPIC_API_KEY[/#FF6B6B]")
                return False
            
            # Determine provider and model
            provider = "anthropic" if os.getenv('ANTHROPIC_API_KEY') else "openai"
            model = os.getenv('LLM_MODEL', 'claude-3-sonnet-20240229' if provider == "anthropic" else 'gpt-4')
            
            # Initialize AI
            self.ai = AI(api_key=api_key, provider=provider, model=model)
            
            # Initialize memory and configuration
            self.memory = FileMemory()
            self.config = ProjectConfig()
            
            # Initialize agents based on configuration
            if self.use_autogen:
                success = await self._initialize_autogen_agents()
            else:
                success = await self._initialize_traditional_agents()
            
            # Initialize sandbox if enabled
            if self.use_sandbox:
                sandbox_success = await self._initialize_sandbox()
                if not sandbox_success:
                    self.console.print("[#FF6B6B]Warning: Sandbox initialization failed[/#FF6B6B]")
            
            return success
                
        except Exception as e:
            self.console.print(f"[#FF6B6B]Error initializing components: {e}[/#FF6B6B]")
            return False
            
    async def _initialize_traditional_agents(self):
        """Initialize traditional agents (existing system)."""
        # Initialize AI configuration
        api_key = os.getenv('LLM_API_KEY') or os.getenv('OPENAI_API_KEY') or os.getenv('ANTHROPIC_API_KEY')
        if not api_key:
            self.console.print("[#FF6B6B]Warning: No LLM API key found. Set LLM_API_KEY, OPENAI_API_KEY, or ANTHROPIC_API_KEY[/#FF6B6B]")
            return False
        
        # Determine provider and model
        provider = "anthropic" if os.getenv('ANTHROPIC_API_KEY') else "openai"
        model = os.getenv('LLM_MODEL', 'claude-3-sonnet-20240229' if provider == "anthropic" else 'gpt-4')
        
        # Initialize AI
        self.ai = AI(api_key=api_key, provider=provider, model=model)
        
        # Initialize memory and configuration
        self.memory = FileMemory()
        self.config = ProjectConfig()
        
        # Initialize agents
        self.coordinator = CoordinatorAgent(
            name="coordinator",
            ai=self.ai,
            memory=self.memory,
            config=self.config
        )
        
        self.parser_agent = ParserAgent(
            ai=self.ai,
            memory=self.memory,
            config=self.config
        )
        
        self.modernizer_agent = ModernizerAgent(
            ai=self.ai,
            memory=self.memory,
            config=self.config
        )
        
        self.refactor_agent = RefactorAgent(
            ai=self.ai,
            memory=self.memory,
            config=self.config
        )
        
        self.qa_agent = QAAgent(
            ai=self.ai,
            memory=self.memory,
            config=self.config
        )
        
        # Register agents with coordinator
        await self.coordinator.register_agent(self.parser_agent)
        await self.coordinator.register_agent(self.modernizer_agent)
        await self.coordinator.register_agent(self.refactor_agent)
        await self.coordinator.register_agent(self.qa_agent)
        
        return True
    
    async def _initialize_autogen_agents(self):
        """Initialize AutoGen-wrapped agents."""
        from .agents.autogen_wrapper import AutoGenAgentWrapper, AutoGenCoordinator, AutoGenConfig
        
        # Create base agents
        base_agents = {
            "parser": ParserAgent(ai=self.ai, memory=self.memory, config=self.config),
            "modernizer": ModernizerAgent(ai=self.ai, memory=self.memory, config=self.config),
            "refactor": RefactorAgent(ai=self.ai, memory=self.memory, config=self.config),
            "qa": QAAgent(ai=self.ai, memory=self.memory, config=self.config),
            "coordinator": CoordinatorAgent(name="coordinator", ai=self.ai, memory=self.memory, config=self.config)
        }
        
        # Wrap with AutoGen
        for name, agent in base_agents.items():
            self.autogen_agents[name] = AutoGenAgentWrapper(
                agent,
                AutoGenConfig(enable_autogen=True)
            )
        
        # Create coordinator
        self.coordinator = AutoGenCoordinator(
            list(self.autogen_agents.values()),
            AutoGenConfig(enable_autogen=True, use_group_chat=True)
        )
        
        self.console.print("[green]AutoGen agents initialized successfully[/green]")
    
    async def _initialize_sandbox(self):
        """Initialize sandbox environment."""
        try:
            # Create sandbox configuration
            config = SandboxConfig(
                docker_image="sandbox:latest",
                work_dir="/workspace",
                command_timeout=300,
                env_vars={
                    "NODE_ENV": "development",
                    "CI": "false"
                }
            )
            
            # Initialize sandbox executor
            self.sandbox_executor = SandboxExecutor(config)
            
            # Initialize sandbox agent
            self.sandbox_agent = SandboxAgent("cli-sandbox-agent", config)
            
            self.console.print("[#00D4AA]✅ Sandbox environment initialized successfully[/#00D4AA]")
            return True
            
        except Exception as e:
            self.console.print(f"[#FF6B6B]Error initializing sandbox: {e}[/#FF6B6B]")
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
            
        # Check AI availability
        if self.ai:
            status_items.append(f"🤖 {self.ai.provider} ({self.ai.model})")
        else:
            status_items.append("🤖 no AI (see /docs)")

        return status_items
        
    def display_status(self):
        """Display status information at the bottom."""
        status_items = self.get_status_info()
        status_text = " • ".join(status_items)
        
        self.console.print(f"\n[dim]{status_text}[/dim]")
        
    async def start_modernization_workflow(self, input_path: str, target_stack: str = "react") -> bool:
        """Start a modernization workflow using the agents system and sandbox."""
        try:
            if not os.path.exists(input_path):
                self.console.print(f"[red]Error: Input path not found: {input_path}[/red]")
                return False
            
            # Output to output directory - no user choice
            project_name = Path(input_path).stem
            output_dir = f"output/modernized-{project_name}"
            
            self.console.print(f"[#00D4AA]Modernizing: {input_path}[/#00D4AA]")
            self.console.print(f"[#00D4AA]Target Framework: {target_stack}[/#00D4AA]")
            self.console.print(f"[#00D4AA]Output Location: {output_dir}[/#00D4AA]")
            self.console.print()
            
            with Progress(
                SpinnerColumn(),
                TextColumn("[progress.description]{task.description}"),
                console=self.console
            ) as progress:
                
                task = progress.add_task("Starting modernization workflow...", total=None)
                
                # Configure project
                self.config.set_target_stack(target_stack)
                self.config.set_project_name(project_name)
                
                progress.update(task, description="Analyzing project structure...")
                
                # Step 1: Analyze with ParserAgent
                analysis_task = {
                    "type": "full_analysis",
                    "project_path": input_path,
                    "description": "Analyze the project structure and create modernization plan"
                }
                
                analysis_result = await self.parser_agent.execute_task(analysis_task)
                
                if not analysis_result.get('success', False):
                    self.console.print(f"\n[#FF6B6B]❌ Analysis failed: {analysis_result.get('error', 'Unknown error')}[/#FF6B6B]")
                    return False
                
                progress.update(task, description="Generating modern code...")
                
                # Step 2: Modernize with ModernizerAgent
                modernization_task = {
                    "type": "full_modernization",
                    "input_path": input_path,
                    "output_path": output_dir,
                    "target_stack": target_stack,
                    "analysis_data": analysis_result.get('project_map', {}),
                    "description": f"Modernize the project to {target_stack}"
                }
                
                modernization_result = await self.modernizer_agent.execute_task(modernization_task)
                
                progress.update(task, description="✅ Modernization completed!")
                
            if modernization_result.get('success', False):
                self.console.print(f"\n[#0053D6]✅ Successfully modernized: {input_path} → {output_dir}[/#0053D6]")
                self.console.print(f"[#0053D6]Framework: {target_stack.upper()}[/#0053D6]")
                
                # Write generated files to output directory
                await self._write_modernization_files(modernization_result, Path(output_dir))
                
                # Show modernization results
                self._display_modernization_results(modernization_result)
                
                # Deploy to sandbox and start development server (optional)
                await self._deploy_to_sandbox(output_dir, target_stack, project_name, modernization_result)
                
                return True
            else:
                error_msg = modernization_result.get('error', 'Unknown error occurred')
                self.console.print(f"\n[#FF6B6B]❌ Modernization failed: {error_msg}[/#FF6B6B]")
                return False
                
        except Exception as e:
            self.console.print(f"\n[#FF6B6B]❌ Error during modernization: {e}[/#FF6B6B]")
            return False
    
    async def _deploy_to_sandbox(self, output_dir: str, target_stack: str, project_name: str, modernization_result: Dict[str, Any]):
        """Deploy the modernized project to the sandbox and start development server."""
        if not self.sandbox_executor:
            self.console.print("[#FF6B6B]Sandbox not available - skipping deployment[/#FF6B6B]")
            return
        
        self.console.print(f"\n[#00D4AA]🚀 Deploying to sandbox and starting development server...[/#00D4AA]")
        
        try:
            # Use the sandbox executor to run the modernized project
            self.console.print(f"[#00D4AA]📁 Using generated files from: {output_dir}[/#00D4AA]")
            
            # Check if the output directory exists and has files
            output_path = Path(output_dir)
            if not output_path.exists():
                self.console.print(f"[#FF6B6B]❌ Output directory not found: {output_dir}[/#FF6B6B]")
                return
            
            # Get the absolute path for Docker mounting
            abs_output_path = output_path.absolute()
            
            # Configure sandbox to mount the output directory
            from engine.agents.autogen_integration.sandbox_executor import SandboxConfig
            sandbox_config = SandboxConfig(
                mount_host_path=str(abs_output_path),
                mount_container_path="/workspace/modernized-project",
                work_dir="/workspace/modernized-project"
            )
            
            self.console.print(f"[#00D4AA]🔗 Mounting: {abs_output_path} → /workspace/modernized-project[/#00D4AA]")
            
            # Use direct Docker commands for deployment
            self.console.print(f"[#00D4AA]📦 Installing dependencies...[/#00D4AA]")
            
            # Install dependencies using Docker
            install_cmd = [
                "docker", "run", "--rm",
                "-v", f"{abs_output_path}:/workspace/modernized-project",
                "-w", "/workspace/modernized-project",
                "sandbox:latest",
                "npm", "install"
            ]
            
            import subprocess
            install_result = subprocess.run(install_cmd, capture_output=True, text=True, timeout=300)
            
            if install_result.returncode != 0:
                self.console.print(f"[#FF6B6B]❌ Failed to install dependencies: {install_result.stderr}[/#FF6B6B]")
                # Try with legacy peer deps
                install_cmd = [
                    "docker", "run", "--rm",
                    "-v", f"{abs_output_path}:/workspace/modernized-project",
                    "-w", "/workspace/modernized-project",
                    "sandbox:latest",
                    "npm", "install", "--legacy-peer-deps"
                ]
                install_result = subprocess.run(install_cmd, capture_output=True, text=True, timeout=300)
                
                if install_result.returncode != 0:
                    self.console.print(f"[#FF6B6B]❌ Failed to install dependencies with legacy peer deps[/#FF6B6B]")
                    self.console.print(f"[#FFA500]💡 You can still access the generated files at: {output_dir}[/#FFA500]")
                    self.console.print(f"[#FFA500]💡 Try running manually: cd {output_dir} && npm install && npm start[/#FFA500]")
                    return
            
            self.console.print(f"[#00D4AA]✅ Dependencies installed successfully[/#00D4AA]")
            
            # Start the development server
            self.console.print(f"[#00D4AA]🚀 Starting development server...[/#00D4AA]")
            
            # Start the server in the background
            server_cmd = [
                "docker", "run", "--rm", "-d",
                "-v", f"{abs_output_path}:/workspace/modernized-project",
                "-w", "/workspace/modernized-project",
                "-p", "3000:3000",
                "sandbox:latest",
                "npm", "start", "--", "--port", "3000", "--host", "0.0.0.0"
            ]
            
            server_result = subprocess.run(server_cmd, capture_output=True, text=True, timeout=30)
            
            if server_result.returncode == 0:
                self.console.print(f"[#00D4AA]✅ Development server started successfully![/#00D4AA]")
                self.console.print(f"[#00D4AA]🌐 Access your modernized website at: http://localhost:3000[/#00D4AA]")
                self.console.print(f"[#00D4AA]📁 Project location: {output_dir}[/#00D4AA]")
                self.console.print(f"[#00D4AA]🔄 Container ID: {server_result.stdout.strip()}[/#00D4AA]")
            else:
                self.console.print(f"[#FF6B6B]❌ Failed to start development server: {server_result.stderr}[/#FF6B6B]")
                self.console.print(f"[#FFA500]💡 You can still access the generated files at: {output_dir}[/#FFA500]")
                self.console.print(f"[#FFA500]💡 Try running manually: cd {output_dir} && npm install && npm start[/#FFA500]")
                
        except Exception as e:
            self.console.print(f"[#FF6B6B]❌ Sandbox deployment error: {e}[/#FF6B6B]")
            self.console.print(f"[#FFA500]💡 You can still access the generated files at: {output_dir}[/#FFA500]")
            self.console.print(f"[#FFA500]💡 Try running manually: cd {output_dir} && npm install && npm start[/#FFA500]")

    def _display_workflow_results(self, result: Dict[str, Any]):
        """Display workflow results."""
        if 'stages' in result:
            stages = result['stages']
            
            # Create a table for workflow summary
            table = Table(title="Workflow Summary", show_header=True, header_style="bold magenta")
            table.add_column("Stage", style="cyan", no_wrap=True)
            table.add_column("Status", style="green")
            table.add_column("Duration", style="yellow")
            
            for stage in stages:
                status = stage.get('status', 'unknown')
                duration = stage.get('duration', 'N/A')
                table.add_row(stage.get('name', 'Unknown'), status, str(duration))
            
            self.console.print(table)
        
        if 'agents' in result:
            agents = result['agents']
            self.console.print("\n🤖 Agent Activity:", style="bold blue")
            
            agent_table = Table(show_header=True, header_style="bold magenta")
            agent_table.add_column("Agent", style="cyan")
            agent_table.add_column("Tasks Completed", style="green")
            agent_table.add_column("Status", style="yellow")
            
            for agent_name, agent_data in agents.items():
                tasks = agent_data.get('tasks_completed', 0)
                status = agent_data.get('status', 'unknown')
                agent_table.add_row(agent_name, str(tasks), status)
            
            self.console.print(agent_table)
    
    def _display_modernization_results(self, result: Dict[str, Any]):
        """Display modernization results."""
        modernization_result = result.get('modernization_result', {})
        
        if 'converted_files' in modernization_result:
            files = modernization_result['converted_files']
            self.console.print("\n📁 Generated Files:", style="bold blue")
            
            file_table = Table(show_header=True, header_style="bold magenta")
            file_table.add_column("File", style="cyan")
            file_table.add_column("Type", style="green")
            file_table.add_column("Status", style="yellow")
            
            for file_path, file_data in files.items():
                status = file_data.get('status', 'Unknown')
                file_type = file_data.get('target_stack', 'Unknown')
                file_table.add_row(file_path, file_type, status)
            
            self.console.print(file_table)
        
        if 'config_files' in modernization_result:
            config_files = modernization_result['config_files']
            self.console.print("\n⚙️  Configuration Files:", style="bold blue")
            
            config_table = Table(show_header=True, header_style="bold magenta")
            config_table.add_column("File", style="cyan")
            config_table.add_column("Status", style="green")
            
            for config_name, config_data in config_files.items():
                status = config_data.get('status', 'Unknown')
                config_table.add_row(config_name, status)
            
            self.console.print(config_table)
        
        if 'project_structure' in modernization_result:
            structure = modernization_result['project_structure']
            self.console.print("\n🏗️  Project Structure:", style="bold blue")
            self.console.print(f"Status: {structure.get('status', 'Unknown')}", style="dim")
    
    async def _write_modernization_files(self, result: Dict[str, Any], output_path: Path):
        """Write generated modernization files to the output directory."""
        try:
            modernization_result = result.get('modernization_result', {})
            
            # Ensure output directory exists
            output_path.mkdir(parents=True, exist_ok=True)
            
            # Write converted files (if any)
            if 'converted_files' in modernization_result:
                converted_files = modernization_result['converted_files']
                for file_path, file_data in converted_files.items():
                    if file_data.get('status') == 'success' and 'converted_content' in file_data:
                        # Determine new file path
                        new_file_path = file_data.get('new_file_path', file_path)
                        if not new_file_path.startswith('/'):
                            new_file_path = output_path / new_file_path
                        else:
                            new_file_path = output_path / Path(new_file_path).name
                        
                        # Create directory if needed
                        new_file_path.parent.mkdir(parents=True, exist_ok=True)
                        
                        # Write file content
                        with open(new_file_path, 'w', encoding='utf-8') as f:
                            f.write(file_data['converted_content'])
                        
                        self.console.print(f"📝 Wrote: {new_file_path}", style="dim")
            
            # Write configuration files
            if 'config_files' in modernization_result:
                config_files = modernization_result['config_files']
                for config_name, config_data in config_files.items():
                    if config_data.get('status') == 'success' and 'content' in config_data:
                        config_path = output_path / config_name
                        
                        # Ensure parent directories exist
                        config_path.parent.mkdir(parents=True, exist_ok=True)
                        
                        with open(config_path, 'w', encoding='utf-8') as f:
                            f.write(config_data['content'])
                        
                        self.console.print(f"⚙️  Wrote: {config_path}", style="dim")
            
            # Write components
            if 'components' in modernization_result:
                components = modernization_result['components']
                if components.get('status') == 'success' and 'component_files' in components:
                    # Write each component to its proper path
                    component_files = components['component_files']
                    for component_path, component_content in component_files.items():
                        # Create full path relative to output directory
                        full_path = output_path / component_path
                        
                        # Ensure parent directories exist
                        full_path.parent.mkdir(parents=True, exist_ok=True)
                        
                        # Write the file
                        with open(full_path, 'w', encoding='utf-8') as f:
                            f.write(component_content)
                        
                        self.console.print(f"🧩 Wrote: {full_path}", style="dim")
            
            self.console.print(f"\n✅ All files written to: {output_path}", style="bold green")
            
        except Exception as e:
            self.console.print(f"❌ Error writing files: {e}", style="bold red")

    async def analyze_with_agents(self, input_path: str) -> bool:
        """Analyze a project using the agents system."""
        try:
            if not os.path.exists(input_path):
                self.console.print(f"[red]Error: Input path not found: {input_path}[/red]")
                return False
            
            with Progress(
                SpinnerColumn(),
                TextColumn("[progress.description]{task.description}"),
                console=self.console
            ) as progress:
                
                task = progress.add_task("Analyzing project with agents...", total=None)
                
                # Create analysis task
                analysis_task = {
                    "type": "full_analysis",
                    "project_path": input_path,
                    "agents": ["parser", "qa"],
                    "analysis_type": "comprehensive"
                }
                
                # Execute analysis using parser agent directly
                result = await self.parser_agent.execute_task(analysis_task)
                
                progress.update(task, description="✅ Analysis completed!")
                
            if result.get('success', False):
                self.console.print(f"✅ Project analysis completed!", style="bold green")
                
                # Show analysis results
                self.display_analysis_results(result)
                
                return True
            else:
                error_msg = result.get('error', 'Unknown error occurred')
                self.console.print(f"❌ Analysis failed: {error_msg}", style="bold red")
                return False
                
        except Exception as e:
            self.console.print(f"❌ Error during analysis: {e}", style="bold red")
            return False

    def display_analysis_results(self, result: dict):
        """Display analysis results."""
        if not result.get('success', False):
            self.console.print(f"❌ Analysis failed: {result.get('error', 'Unknown error')}", style="bold red")
            return
        
        project_map = result.get('project_map', {})
        if not project_map:
            self.console.print("❌ No analysis data available", style="bold red")
            return
        
        # Create main analysis table
        table = Table(title="Project Analysis Results", show_header=True, header_style="bold magenta")
        table.add_column("Category", style="cyan", no_wrap=True)
        table.add_column("Details", style="green")
        
        # Project structure
        structure = project_map.get('structure', {})
        if structure:
            table.add_row("Total Files", str(structure.get('total_files', 0)))
            
            categorized_files = structure.get('categorized_files', {})
            if categorized_files:
                for file_type, files in categorized_files.items():
                    if files:
                        table.add_row(f"{file_type.title()} Files", str(len(files)))
        
        # File analysis statistics
        file_analyses = project_map.get('file_analyses', {})
        if file_analyses:
            successful_analyses = len([f for f in file_analyses.values() if f.get('status') == 'success'])
            failed_analyses = len([f for f in file_analyses.values() if f.get('status') == 'failed'])
            table.add_row("Successfully Analyzed", str(successful_analyses))
            if failed_analyses > 0:
                table.add_row("Failed Analyses", str(failed_analyses))
        
        # Dependencies
        dependencies = project_map.get('dependencies', {})
        if dependencies:
            total_deps = sum(len(deps) for deps in dependencies.get('imports', {}).values())
            table.add_row("Dependencies Found", str(total_deps))
        
        # Patterns
        patterns = project_map.get('patterns', [])
        if patterns:
            table.add_row("Architecture Patterns", str(len(patterns)))
        
        # Summary statistics
        summary = project_map.get('summary', {})
        if summary and summary.get('status') == 'success':
            stats = summary.get('statistics', {})
            if stats:
                table.add_row("Patterns Found", str(stats.get('patterns_found', 0)))
                table.add_row("Dependencies Mapped", str(stats.get('dependencies_mapped', 0)))
        
        self.console.print(table)
        
        # Show project summary
        if summary and summary.get('status') == 'success' and summary.get('summary'):
            self.console.print("\n📋 Project Summary:", style="bold blue")
            summary_text = summary['summary']
            # Truncate if too long
            if len(summary_text) > 500:
                summary_text = summary_text[:500] + "..."
            self.console.print(summary_text, style="dim")
        
        # Show file analysis highlights
        if file_analyses:
            self.console.print("\n📁 File Analysis Highlights:", style="bold blue")
            for file_path, analysis in list(file_analyses.items())[:3]:  # Show first 3 files
                if analysis.get('status') == 'success':
                    file_name = file_path.split('/')[-1]
                    content_length = analysis.get('content_length', 0)
                    self.console.print(f"   • {file_name} ({content_length} chars)", style="dim")
        
        # Show patterns found
        if patterns:
            self.console.print("\n🏗️  Architecture Patterns:", style="bold blue")
            for pattern in patterns[:3]:  # Show first 3 patterns
                pattern_name = pattern.get('name', 'Unknown Pattern')
                pattern_type = pattern.get('type', 'Unknown Type')
                self.console.print(f"   • {pattern_name} ({pattern_type})", style="dim")
        
        # Show next steps
        self.console.print("\n🚀 Next Steps:", style="bold green")
        self.console.print("   • Use /modernize to start modernization process", style="dim")
        self.console.print("   • Use /status to check agent status", style="dim")
        self.console.print("   • Use /help for more commands", style="dim")

    async def get_agent_status(self) -> Dict[str, Any]:
        """Get status of all agents."""
        try:
            status = await self.coordinator.get_status()
            return status
        except Exception as e:
            self.console.print(f"[#FF6B6B]Error getting agent status: {e}[/#FF6B6B]")
            return {}

    def display_agent_status(self, status: Dict[str, Any]):
        """Display agent status information."""
        if not status:
            self.console.print("[#FFA500]No agent status available[/#FFA500]")
            return
        
        # Create agent status table
        table = Table(title="Agent Status", show_header=True, header_style="bold magenta")
        table.add_column("Agent", style="cyan", no_wrap=True)
        table.add_column("Status", style="green")
        table.add_column("Current Task", style="yellow")
        table.add_column("Progress", style="blue")
        
        if 'agents' in status:
            for agent_name, agent_data in status['agents'].items():
                agent_status = agent_data.get('status', 'unknown')
                current_task = agent_data.get('current_task', 'idle')
                progress = f"{agent_data.get('progress', 0):.1f}%"
                
                table.add_row(agent_name, agent_status, current_task, progress)
        
        self.console.print(table)
        
        # Show workflow status
        if 'workflow' in status:
            workflow = status['workflow']
            self.console.print(f"\n📋 Workflow Status: {workflow.get('status', 'unknown')}")
            self.console.print(f"📊 Overall Progress: {workflow.get('progress', 0):.1f}%")

    async def interactive_mode(self):
        """Run in interactive mode with natural language commands."""
        self.console.print("\n[bold]Interactive Mode[/bold]")
        self.console.print("Type your commands or questions. Type /help for available commands.")
        
        while True:
            try:
                # Get user input using regular input to avoid event loop conflicts
                user_input = input("> ").strip()
                
                if not user_input:
                    continue
                    
                # Handle commands
                if user_input.startswith('/'):
                    await self.handle_command(user_input[1:])
                else:
                    await self.handle_natural_language(user_input)
                    
            except KeyboardInterrupt:
                self.console.print("\n[#FFA500]Use /exit to quit[/#FFA500]")
            except EOFError:
                break
                
    async def handle_command(self, command: str):
        """Handle slash commands."""
        parts = command.split()
        cmd = parts[0].lower()
        
        if cmd == 'help':
            self.show_help()
        elif cmd == 'modernize':
            if len(parts) < 2:
                self.console.print("[red]Usage: /modernize <input_path> [framework][/red]")
                self.console.print("[red]Frameworks: react, nextjs, astro, vue, svelte, angular[/red]")
                self.console.print("[red]Output will be automatically placed in sandbox[/red]")
            else:
                framework = parts[2] if len(parts) > 2 else 'react'
                await self.start_modernization_workflow(parts[1], framework)
        elif cmd == 'analyze':
            if len(parts) < 2:
                self.console.print("[red]Usage: /analyze <input_path>[/red]")
            else:
                await self.analyze_with_agents(parts[1])
        elif cmd == 'status':
            status = await self.get_agent_status()
            self.display_agent_status(status)
        elif cmd == 'agents':
            status = await self.get_agent_status()
            self.display_agent_status(status)
        elif cmd == 'exit':
            self.console.print("[#0053D6]Goodbye![/#0053D6]")
            sys.exit(0)
        elif cmd == 'quit':
            self.console.print("[#0053D6]Goodbye![/#0053D6]")
            sys.exit(0)
        else:
            self.console.print(f"[#FF6B6B]Unknown command: {cmd}[/#FF6B6B]")
            
    async def handle_natural_language(self, query: str):
        """Handle natural language queries."""
        # Simple keyword-based parsing for now
        query_lower = query.lower()
        
        if 'modernize' in query_lower or 'transform' in query_lower:
            # Extract input and output from query
            words = query.split()
            
            # Find input path
            input_path = None
            framework = 'react'
            
            for i, word in enumerate(words):
                # Check for file paths
                if os.path.exists(word) or word.endswith(('.html', '.htm', '.js', '.css')):
                    input_path = word
                    break
                # Check for directories
                elif os.path.isdir(word):
                    input_path = word
                    break
            
            # Check for framework specification
            for word in words:
                if word.lower() in ['react', 'nextjs', 'astro', 'vue', 'svelte', 'angular']:
                    framework = word.lower()
                    break
            
            if input_path:
                await self.start_modernization_workflow(input_path, framework)
                return
            else:
                self.console.print("[#FFA500]Please specify an input path to modernize[/#FFA500]")
                self.console.print("[#FFA500]Examples:[/#FFA500]")
                self.console.print("[#FFA500]  - modernize my-website.html react[/#FFA500]")
                self.console.print("[#FFA500]  - modernize legacy-project nextjs[/#FFA500]")
                self.console.print("[#FFA500]Output will be automatically placed in sandbox[/#FFA500]")
            
        elif 'analyze' in query_lower or 'review' in query_lower:
            # Extract input from query
            words = query.split()
            input_path = None
            
            for word in words:
                if os.path.exists(word) or os.path.isdir(word):
                    input_path = word
                    break
            
            if input_path:
                await self.analyze_with_agents(input_path)
                return
            else:
                self.console.print("[#FFA500]Please specify an input path to analyze[/#FFA500]")
                self.console.print("[#FFA500]Examples:[/#FFA500]")
                self.console.print("[#FFA500]  - analyze my-website.html[/#FFA500]")
                self.console.print("[#FFA500]  - analyze legacy-project[/#FFA500]")
        
    def show_help(self):
        """Show help information."""
        help_text = """
[bold]Available Commands:[/bold]

[bold blue]Modernization:[/bold blue]
  /modernize <input> [framework]                     - Modernize a project
  /analyze <input>                        - Analyze a project
  /status                                 - Show agent and workflow status
  /agents                                 - Show detailed agent status

[bold blue]General Commands:[/bold blue]
  /help                                   - Show this help message
  /exit, /quit                            - Exit the CLI

[bold blue]Natural Language:[/bold blue]
  "modernize my-website.html"             - Modernize a specific project
  "analyze legacy-project"                - Analyze a project
  "show agent status"                     - Show agent status
  "help"                                  - Show help

[bold blue]Examples:[/bold blue]
  > modernize examples/website/legacy-site.html react
  > /modernize examples/website/legacy-site.html nextjs
  > analyze examples/website/legacy-site.html
  > /analyze examples/website/legacy-site.html
  > status
  > /status

[bold blue]Supported Frameworks:[/bold blue]
  • react   - React with modern tooling
  • nextjs  - Next.js with TypeScript
  • astro   - Astro with static generation
  • vue     - Vue.js with Composition API
  • svelte  - Svelte with SvelteKit
  • angular - Angular with modern features
        """
        
        self.console.print(Panel(help_text, title="[bold]Help[/bold]", border_style="#0053D6"))

    @click.command()
    def test_autogen():
        """Test AutoGen integration with ParserAgent."""
        asyncio.run(cli._test_autogen_integration())
    
    async def _test_autogen_integration(self):
        """Test AutoGen integration."""
        try:
            # Initialize components
            if not await self.initialize_components():
                self.console.print("[red]Failed to initialize components[/red]")
                return
            
            # Create AutoGen-wrapped ParserAgent
            from .agents.autogen_wrapper import AutoGenAgentWrapper, AutoGenConfig
            
            autogen_parser = AutoGenAgentWrapper(
                self.parser_agent,
                AutoGenConfig(enable_autogen=True)
            )
            
            self.console.print("[green]✓ AutoGen ParserAgent created successfully[/green]")
            
            # Test basic functionality
            test_task = {
                "type": "file_analysis",
                "file_path": "test.html",
                "content": """
       <!DOCTYPE html>
       <html>
       <head>
           <title>Test Page</title>
       </head>
       <body>
           <h1>Hello, World!</h1>
       </body>
       </html>
                """
            }
            
            self.console.print("[blue]Testing AutoGen integration...[/blue]")
            result = await autogen_parser.execute_task(test_task)
            
            self.console.print(f"[green]✓ Test completed: {result}[/green]")
            self.console.print("[green]🎉 AutoGen integration test passed![/green]")
            
        except ImportError as e:
            self.console.print(f"[red]❌ AutoGen not installed: {e}[/red]")
            self.console.print("[yellow]Please run: pip install -U 'autogen-agentchat' 'autogen-ext[openai]'[/yellow]")
        except Exception as e:
            self.console.print(f"[red]❌ Test failed: {e}[/red]")

async def main():
    """Main CLI entry point."""
    cli = Legacy2ModernCLI()
    
    # Display banner and tips
    cli.display_banner()
    cli.display_tips()
    
    # Initialize components with progress indication
    cli.console.print("\n[blue]Initializing agents...[/blue]")
    agents_available = await cli.initialize_components()
    
    if not agents_available:
        cli.console.print("[#FF6B6B]Failed to initialize agents. Please check your configuration.[/#FF6B6B]")
        return
    
    # Display status
    cli.display_status()
    
    # Start interactive mode
    await cli.interactive_mode()

def run_cli():
    """Run the CLI with proper async handling."""
    asyncio.run(main())

if __name__ == "__main__":
    run_cli() 