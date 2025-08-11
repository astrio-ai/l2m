"""
Abstract base class for all agents in the AutoGen modernization system.
"""

import asyncio
import logging
from abc import ABC, abstractmethod
from typing import Dict, List, Optional, Any, Union
from dataclasses import dataclass, field
from enum import Enum

from ..utilities.ai import AI
from .base_memory import BaseMemory
from ..utilities.project_config import ProjectConfig

logger = logging.getLogger(__name__)

class AgentRole(Enum):
    """Defines the role of each agent in the system."""
    PARSER = "parser"
    MODERNIZER = "modernizer"
    REFACTOR = "refactor"
    QA = "qa"
    COORDINATOR = "coordinator"

@dataclass
class AgentState:
    """Tracks the current state of an agent."""
    is_active: bool = True
    current_task: Optional[str] = None
    task_progress: float = 0.0
    error_count: int = 0
    last_activity: float = 0.0
    messages_sent: int = 0
    messages_received: int = 0

class BaseAgent(ABC):
    """
    Abstract base class for all agents in the AutoGen modernization system.
    
    Each agent has a specific role and can communicate with other agents
    through the AutoGen framework.
    """
    
    def __init__(
        self,
        name: str,
        role: AgentRole,
        ai: AI,
        memory: BaseMemory,
        config: ProjectConfig,
        system_prompt: Optional[str] = None
    ):
        self.name = name
        self.role = role
        self.ai = ai
        self.memory = memory
        self.config = config
        self.system_prompt = system_prompt or self._get_default_system_prompt()
        
        # Agent state
        self.state = AgentState()
        
        # Communication channels
        self.message_queue: List[Dict[str, Any]] = []
        self.other_agents: Dict[str, 'BaseAgent'] = {}
        
        # Performance tracking
        self.start_time = None
        self.task_history: List[Dict[str, Any]] = []
        
    @abstractmethod
    def _get_default_system_prompt(self) -> str:
        """Return the default system prompt for this agent."""
        pass
    
    @abstractmethod
    async def process_message(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """
        Process an incoming message and return a response.
        
        Args:
            message: Dictionary containing message data
            
        Returns:
            Response dictionary
        """
        pass
    
    @abstractmethod
    async def execute_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """
        Execute a specific task assigned to this agent.
        
        Args:
            task: Dictionary containing task details
            
        Returns:
            Task result dictionary
        """
        pass
    
    async def start(self):
        """Start the agent and begin processing."""
        self.start_time = asyncio.get_event_loop().time()
        self.state.is_active = True
        logger.info(f"Agent {self.name} ({self.role.value}) started")
        
        # Load any saved state from memory
        await self._load_state()
    
    async def stop(self):
        """Stop the agent and save state."""
        self.state.is_active = False
        logger.info(f"Agent {self.name} ({self.role.value}) stopped")
        
        # Save current state to memory
        await self._save_state()
    
    async def send_message(self, target_agent: str, message: Dict[str, Any]):
        """Send a message to another agent."""
        if target_agent in self.other_agents:
            await self.other_agents[target_agent].receive_message(self.name, message)
            self.state.messages_sent += 1
        else:
            logger.warning(f"Target agent {target_agent} not found")
    
    async def receive_message(self, sender: str, message: Dict[str, Any]):
        """Receive a message from another agent."""
        self.state.messages_received += 1
        self.message_queue.append({
            "sender": sender,
            "timestamp": asyncio.get_event_loop().time(),
            "message": message
        })
        
        # Process message if agent is active
        if self.state.is_active:
            await self._process_message_queue()
    
    async def _process_message_queue(self):
        """Process all pending messages in the queue."""
        while self.message_queue:
            msg_data = self.message_queue.pop(0)
            try:
                response = await self.process_message(msg_data["message"])
                if response:
                    await self.send_message(msg_data["sender"], response)
            except Exception as e:
                logger.error(f"Error processing message in {self.name}: {e}")
                self.state.error_count += 1
    
    async def _load_state(self):
        """Load agent state from memory."""
        try:
            saved_state = await self.memory.get(f"agent_state_{self.name}")
            if saved_state:
                # Update state with saved data
                for key, value in saved_state.items():
                    if hasattr(self.state, key):
                        setattr(self.state, key, value)
        except Exception as e:
            logger.warning(f"Failed to load state for {self.name}: {e}")
    
    async def _save_state(self):
        """Save agent state to memory."""
        try:
            state_data = {
                "is_active": self.state.is_active,
                "current_task": self.state.current_task,
                "task_progress": self.state.task_progress,
                "error_count": self.state.error_count,
                "last_activity": self.state.last_activity,
                "messages_sent": self.state.messages_sent,
                "messages_received": self.state.messages_received
            }
            await self.memory.set(f"agent_state_{self.name}", state_data)
        except Exception as e:
            logger.warning(f"Failed to save state for {self.name}: {e}")
    
    def get_status(self) -> Dict[str, Any]:
        """Get current agent status."""
        return {
            "name": self.name,
            "role": self.role.value,
            "is_active": self.state.is_active,
            "current_task": self.state.current_task,
            "task_progress": self.state.task_progress,
            "error_count": self.state.error_count,
            "messages_sent": self.state.messages_sent,
            "messages_received": self.state.messages_received,
            "queue_length": len(self.message_queue)
        }
    
    def update_progress(self, progress: float):
        """Update task progress (0.0 to 1.0)."""
        self.state.task_progress = max(0.0, min(1.0, progress))
        self.state.last_activity = asyncio.get_event_loop().time()
    
    def log_task(self, task_name: str, result: Dict[str, Any]):
        """Log a completed task."""
        self.task_history.append({
            "task": task_name,
            "timestamp": asyncio.get_event_loop().time(),
            "result": result
        })
    
    async def get_ai_response(self, prompt: str, context: Optional[str] = None) -> str:
        """Get a response from the AI with optional context."""
        full_prompt = prompt
        if context:
            full_prompt = f"Context: {context}\n\n{prompt}"
        
        return await self.ai.chat_simple(full_prompt, self.system_prompt) 