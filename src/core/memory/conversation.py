"""
Conversation memory management.

This module handles conversation history and context for the
multi-agent system.
"""

from typing import Dict, Any, List, Optional
from datetime import datetime
import json

from src.utils.logger import get_logger

logger = get_logger(__name__)


class ConversationMemory:
    """Manages conversation memory for the multi-agent system."""
    
    def __init__(self, max_history: int = 100):
        """Initialize conversation memory."""
        self.max_history = max_history
        self.conversations: Dict[str, List[Dict[str, Any]]] = {}
        self.logger = get_logger(__name__)
    
    def add_message(self, conversation_id: str, message: Dict[str, Any]):
        """Add a message to conversation history."""
        if conversation_id not in self.conversations:
            self.conversations[conversation_id] = []
        
        message["timestamp"] = datetime.now().isoformat()
        self.conversations[conversation_id].append(message)
        
        # Trim history if it exceeds max_history
        if len(self.conversations[conversation_id]) > self.max_history:
            self.conversations[conversation_id] = self.conversations[conversation_id][-self.max_history:]
    
    def get_conversation(self, conversation_id: str) -> List[Dict[str, Any]]:
        """Get conversation history."""
        return self.conversations.get(conversation_id, [])
    
    def get_recent_messages(self, conversation_id: str, count: int = 10) -> List[Dict[str, Any]]:
        """Get recent messages from conversation."""
        conversation = self.get_conversation(conversation_id)
        return conversation[-count:] if conversation else []
    
    def clear_conversation(self, conversation_id: str):
        """Clear conversation history."""
        if conversation_id in self.conversations:
            del self.conversations[conversation_id]
    
    def get_conversation_summary(self, conversation_id: str) -> Dict[str, Any]:
        """Get summary of conversation."""
        conversation = self.get_conversation(conversation_id)
        if not conversation:
            return {}
        
        return {
            "message_count": len(conversation),
            "start_time": conversation[0]["timestamp"] if conversation else None,
            "end_time": conversation[-1]["timestamp"] if conversation else None,
            "participants": list(set(msg.get("sender", "unknown") for msg in conversation))
        }
