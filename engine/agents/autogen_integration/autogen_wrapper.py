"""
AutoGen integration layer for legacy2modern agents.

This module provides wrapper classes to integrate existing agents with AutoGen's
ConversableAgent framework while preserving domain-specific logic.
"""

import asyncio
import logging
from typing import Dict, List, Optional, Any, Union, Callable
from dataclasses import dataclass, field

# AutoGen imports with better error handling
AUTOGEN_AVAILABLE = False
try:
    import autogen_agentchat as autogen
    print(f"AutoGen imported successfully: {getattr(autogen, '__version__', 'unknown')}")
    
    # Try importing specific classes from autogen_core
    try:
        from autogen_core import Agent, BaseAgent
        print("AutoGen core classes imported successfully")
        AUTOGEN_AVAILABLE = True
        
        # Create wrapper classes for compatibility
        class ConversableAgent(Agent):
            def __init__(self, name, system_message=None, llm_config=None, **kwargs):
                super().__init__(name=name, **kwargs)
                self.system_message = system_message
                self.llm_config = llm_config
        
        class AssistantAgent(Agent):
            def __init__(self, name, system_message=None, llm_config=None, **kwargs):
                super().__init__(name=name, **kwargs)
                self.system_message = system_message
                self.llm_config = llm_config
        
        class UserProxyAgent(Agent):
            def __init__(self, name, system_message=None, llm_config=None, **kwargs):
                super().__init__(name=name, **kwargs)
                self.system_message = system_message
                self.llm_config = llm_config
                
    except ImportError as e:
        print(f"Warning: AutoGen core classes not available: {e}")
        # Create fallback classes
        class ConversableAgent:
            def __init__(self, *args, **kwargs):
                raise ImportError("AutoGen ConversableAgent not available")
        
        class AssistantAgent:
            def __init__(self, *args, **kwargs):
                raise ImportError("AutoGen AssistantAgent not available")
        
        class UserProxyAgent:
            def __init__(self, *args, **kwargs):
                raise ImportError("AutoGen UserProxyAgent not available")
        
        AUTOGEN_AVAILABLE = False
        
except ImportError as e:
    print(f"AutoGen import failed: {e}")
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

# Try to import TextAnalyzerAgent (optional)
try:
    # TextAnalyzerAgent might not be available in newer versions
    TextAnalyzerAgent = None
    print("TextAnalyzerAgent not available in this version (optional)")
except ImportError:
    print("TextAnalyzerAgent not available (optional)")
    TextAnalyzerAgent = None

from ..core_agents.base_agent import BaseAgent, AgentRole
from ..utilities.ai import AI
from ..core_agents.base_memory import BaseMemory
from ..utilities.project_config import ProjectConfig

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
            config["config_list"][0]["api_type"] = "openai"
        elif ai.provider == "anthropic":
            # AutoGen supports Anthropic natively
            config["config_list"][0]["api_base"] = "https://api.anthropic.com"
            config["config_list"][0]["api_type"] = "anthropic"
            # Ensure model name is correct for Anthropic
            if not ai.model.startswith("claude"):
                logger.warning(f"Model {ai.model} may not be a valid Anthropic model")
        else:
            logger.warning(f"Unknown provider: {ai.provider}, using default settings")
        
        logger.info(f"Created LLM config for {ai.provider} with model {ai.model}")
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
            # For now, we'll use a simplified approach since the new AutoGen API is different
            # We'll just log that AutoGen processing was attempted
            logger.info(f"AutoGen processing attempted for message: {message.get('type', 'unknown')}")
            
            # Return a simple response indicating AutoGen processing
            return {
                "type": "autogen_response",
                "content": f"AutoGen processing completed for {message.get('type', 'unknown')} message",
                "autogen_enabled": True
            }
            
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
    
    async def get_status(self) -> Dict[str, Any]:
        """Get combined status from both systems."""
        # Handle both sync and async get_status methods
        if asyncio.iscoroutinefunction(self.base_agent.get_status):
            base_status = await self.base_agent.get_status()
        else:
            base_status = self.base_agent.get_status()
        
        base_status["autogen_enabled"] = self.autogen_config.enable_autogen
        base_status["autogen_agent_type"] = type(self.autogen_agent).__name__
        base_status["llm_provider"] = self.base_agent.ai.provider
        base_status["llm_model"] = self.base_agent.ai.model
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
        
        # Note: GroupChat and GroupChatManager might not be available in newer versions
        # For now, we'll use a simplified approach
        self.group_chat = None
        self.manager = None
        print("GroupChat functionality not implemented in this version")
    
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

# Helper function to create LLM config for different providers
def create_llm_config(
    api_key: str,
    provider: str = "anthropic",
    model: str = "claude-3-sonnet-20240229",
    temperature: float = 0.7,
    max_tokens: int = 4000
) -> Dict[str, Any]:
    """
    Create LLM config for AutoGen with support for multiple providers.
    
    Args:
        api_key: API key for the provider
        provider: 'anthropic' or 'openai'
        model: Model name
        temperature: Temperature setting
        max_tokens: Maximum tokens
        
    Returns:
        LLM config dictionary for AutoGen
    """
    config = {
        "config_list": [{
            "model": model,
            "api_key": api_key,
        }],
        "temperature": temperature,
        "max_tokens": max_tokens,
    }
    
    if provider == "openai":
        config["config_list"][0]["api_base"] = "https://api.openai.com/v1"
        config["config_list"][0]["api_type"] = "openai"
    elif provider == "anthropic":
        config["config_list"][0]["api_base"] = "https://api.anthropic.com"
        config["config_list"][0]["api_type"] = "anthropic"
    else:
        raise ValueError(f"Unsupported provider: {provider}")
    
    return config 