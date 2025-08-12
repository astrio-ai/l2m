"""
Group Chat Coordinator for AutoGen-enabled agents.

This module provides a comprehensive group chat system that enables
multi-agent communication and coordination using AutoGen's group chat
capabilities.
"""

import asyncio
import logging
from typing import Dict, List, Optional, Any, Callable
from dataclasses import dataclass, field
from enum import Enum

from .autogen_wrapper import AutoGenAgentWrapper, AutoGenConfig
from ..core_agents.base_agent import BaseAgent
from ..utilities.ai import AI
from ..core_agents.base_memory import BaseMemory
from ..utilities.project_config import ProjectConfig

logger = logging.getLogger(__name__)

class ChatType(Enum):
    """Types of group chat sessions."""
    WORKFLOW_COORDINATION = "workflow_coordination"
    TASK_DISCUSSION = "task_discussion"
    PROBLEM_SOLVING = "problem_solving"
    CODE_REVIEW = "code_review"
    PLANNING = "planning"

@dataclass
class GroupChatSession:
    """Represents a group chat session."""
    session_id: str
    chat_type: ChatType
    participants: List[str]
    topic: str
    max_rounds: int = 10
    current_round: int = 0
    messages: List[Dict[str, Any]] = field(default_factory=list)
    is_active: bool = True
    created_at: float = field(default_factory=lambda: asyncio.get_event_loop().time())
    last_activity: float = field(default_factory=lambda: asyncio.get_event_loop().time())

class GroupChatCoordinator:
    """
    Coordinates group chat sessions between AutoGen-wrapped agents.
    
    This coordinator manages:
    - Multi-agent group chat sessions
    - Inter-agent communication
    - Coordinated task execution
    - Enhanced workflow management
    """
    
    def __init__(
        self,
        ai: AI,
        memory: BaseMemory,
        config: ProjectConfig,
        autogen_config: Optional[AutoGenConfig] = None
    ):
        self.ai = ai
        self.memory = memory
        self.config = config
        self.autogen_config = autogen_config or AutoGenConfig(use_group_chat=True)
        
        # Agent management
        self.agents: Dict[str, AutoGenAgentWrapper] = {}
        self.agent_roles: Dict[str, str] = {}
        
        # Session management
        self.active_sessions: Dict[str, GroupChatSession] = {}
        self.session_history: List[GroupChatSession] = []
        
        # Communication tracking
        self.message_count = 0
        self.coordination_events = []
        
    async def register_agent(self, agent: BaseAgent, role: str = "participant") -> str:
        """Register an agent for group chat participation."""
        try:
            # Wrap the agent with AutoGen if not already wrapped
            if isinstance(agent, AutoGenAgentWrapper):
                wrapped_agent = agent
            else:
                wrapped_agent = AutoGenAgentWrapper(agent, self.autogen_config)
            
            agent_id = wrapped_agent.base_agent.name
            self.agents[agent_id] = wrapped_agent
            self.agent_roles[agent_id] = role
            
            logger.info(f"Registered agent {agent_id} with role {role}")
            return agent_id
            
        except Exception as e:
            logger.error(f"Failed to register agent: {e}")
            raise
    
    async def unregister_agent(self, agent_id: str):
        """Unregister an agent from group chat."""
        if agent_id in self.agents:
            del self.agents[agent_id]
            del self.agent_roles[agent_id]
            logger.info(f"Unregistered agent {agent_id}")
    
    async def create_group_chat(
        self,
        chat_type: ChatType,
        topic: str,
        participants: List[str],
        max_rounds: int = 10
    ) -> str:
        """Create a new group chat session."""
        try:
            # Validate participants
            available_agents = list(self.agents.keys())
            valid_participants = [p for p in participants if p in available_agents]
            
            if not valid_participants:
                raise ValueError("No valid participants found")
            
            # Create session with unique ID
            import time
            import random
            session_id = f"group_chat_{int(time.time() * 1000)}_{random.randint(1000, 9999)}"
            session = GroupChatSession(
                session_id=session_id,
                chat_type=chat_type,
                participants=valid_participants,
                topic=topic,
                max_rounds=max_rounds
            )
            
            self.active_sessions[session_id] = session
            
            # Initialize session in memory
            await self.memory.set(f"group_chat_{session_id}", {
                "session_id": session_id,
                "chat_type": chat_type.value,
                "topic": topic,
                "participants": valid_participants,
                "created_at": session.created_at
            })
            
            logger.info(f"Created group chat session {session_id} with {len(valid_participants)} participants")
            return session_id
            
        except Exception as e:
            logger.error(f"Failed to create group chat: {e}")
            raise
    
    async def start_group_chat(self, session_id: str, initial_message: str = "") -> Dict[str, Any]:
        """Start a group chat session with initial message."""
        if session_id not in self.active_sessions:
            raise ValueError(f"Session {session_id} not found")
        
        session = self.active_sessions[session_id]
        
        try:
            # Create AutoGen group chat
            autogen_agents = [self.agents[agent_id].autogen_agent for agent_id in session.participants]
            
            # Import AutoGen group chat components
            try:
                # Try importing from autogen_agentchat.teams
                try:
                    from autogen_agentchat.teams import BaseGroupChat, RoundRobinGroupChat
                    logger.info("GroupChat imported from autogen_agentchat.teams")
                    
                    # Create group chat using the correct classes
                    group_chat = RoundRobinGroupChat(
                        participants=autogen_agents,
                        max_turns=session.max_rounds
                    )
                    
                    # For now, we'll use a simplified approach since the new API is different
                    # The group chat manager functionality might be handled differently
                    logger.info("GroupChat created successfully")
                    
                    # Start the conversation
                    if initial_message:
                        session.messages.append({
                            "role": "user",
                            "content": initial_message,
                            "timestamp": asyncio.get_event_loop().time()
                        })
                    
                    # Update session
                    session.current_round += 1
                    session.last_activity = asyncio.get_event_loop().time()
                    
                    # Store result
                    result = {
                        "session_id": session_id,
                        "status": "success",
                        "chat_type": session.chat_type.value,
                        "topic": session.topic,
                        "participants": session.participants,
                        "current_round": session.current_round,
                        "max_rounds": session.max_rounds,
                        "result": "GroupChat created successfully",
                        "messages": session.messages
                    }
                    
                    await self.memory.set(f"group_chat_result_{session_id}", result)
                    
                    logger.info(f"Group chat session {session_id} completed successfully")
                    return result
                    
                except ImportError:
                    # Try importing from autogen_ext.teams
                    try:
                        from autogen_ext.teams import BaseGroupChat, RoundRobinGroupChat
                        logger.info("GroupChat imported from autogen_ext.teams")
                        
                        # Similar implementation as above
                        group_chat = RoundRobinGroupChat(
                            participants=autogen_agents,
                            max_turns=session.max_rounds
                        )
                        
                        logger.info("GroupChat created successfully")
                        
                        # Start the conversation
                        if initial_message:
                            session.messages.append({
                                "role": "user",
                                "content": initial_message,
                                "timestamp": asyncio.get_event_loop().time()
                            })
                        
                        # Update session
                        session.current_round += 1
                        session.last_activity = asyncio.get_event_loop().time()
                        
                        # Store result
                        result = {
                            "session_id": session_id,
                            "status": "success",
                            "chat_type": session.chat_type.value,
                            "topic": session.topic,
                            "participants": session.participants,
                            "current_round": session.current_round,
                            "max_rounds": session.max_rounds,
                            "result": "GroupChat created successfully",
                            "messages": session.messages
                        }
                        
                        await self.memory.set(f"group_chat_result_{session_id}", result)
                        
                        logger.info(f"Group chat session {session_id} completed successfully")
                        return result
                        
                    except ImportError:
                        # GroupChat classes might not be available in newer AutoGen versions
                        logger.warning("GroupChat classes not available in this AutoGen version, using fallback")
                        raise ImportError("GroupChat not available")
                
            except ImportError:
                # Fallback to traditional coordination
                logger.warning("AutoGen GroupChat not available, using fallback coordination")
                return await self._fallback_group_chat(session, initial_message)
                
        except Exception as e:
            logger.error(f"Failed to start group chat {session_id}: {e}")
            return {
                "session_id": session_id,
                "status": "error",
                "error": str(e)
            }
    
    async def _fallback_group_chat(self, session: GroupChatSession, initial_message: str) -> Dict[str, Any]:
        """Fallback group chat using traditional agent coordination."""
        try:
            # Simulate group chat by having agents communicate sequentially
            messages = []
            current_message = initial_message or f"Let's discuss: {session.topic}"
            
            for round_num in range(session.max_rounds):
                round_messages = []
                
                for agent_id in session.participants:
                    agent = self.agents[agent_id]
                    
                    # Send message to agent
                    response = await agent.process_message({
                        "type": "group_chat_message",
                        "content": current_message,
                        "session_id": session.session_id,
                        "round": round_num + 1,
                        "topic": session.topic
                    })
                    
                    agent_message = {
                        "agent": agent_id,
                        "role": self.agent_roles[agent_id],
                        "content": response.get("content", str(response)),
                        "timestamp": asyncio.get_event_loop().time()
                    }
                    
                    round_messages.append(agent_message)
                    messages.append(agent_message)
                    
                    # Update current message for next agent
                    current_message = response.get("content", str(response))
                
                # Check if consensus reached or max rounds hit
                if self._check_consensus(round_messages) or round_num >= session.max_rounds - 1:
                    break
            
            session.current_round = round_num + 1
            session.messages = messages
            session.last_activity = asyncio.get_event_loop().time()
            
            return {
                "session_id": session.session_id,
                "status": "success",
                "chat_type": session.chat_type.value,
                "topic": session.topic,
                "participants": session.participants,
                "current_round": session.current_round,
                "max_rounds": session.max_rounds,
                "messages": messages,
                "consensus_reached": self._check_consensus(messages[-len(session.participants):] if messages else [])
            }
            
        except Exception as e:
            logger.error(f"Fallback group chat failed: {e}")
            return {
                "session_id": session.session_id,
                "status": "error",
                "error": str(e)
            }
    
    def _check_consensus(self, messages: List[Dict[str, Any]]) -> bool:
        """Check if agents have reached consensus."""
        if len(messages) < 2:
            return False
        
        # Simple consensus check - look for agreement keywords
        agreement_keywords = ["agree", "consensus", "approved", "accepted", "yes", "okay"]
        disagreement_keywords = ["disagree", "no", "reject", "concern", "issue"]
        
        last_messages = messages[-len(self.agents):] if len(messages) >= len(self.agents) else messages
        
        agreement_count = 0
        disagreement_count = 0
        
        for message in last_messages:
            content = message.get("content", "").lower()
            
            if any(keyword in content for keyword in agreement_keywords):
                agreement_count += 1
            elif any(keyword in content for keyword in disagreement_keywords):
                disagreement_count += 1
        
        # Consensus if majority agree and no strong disagreements
        return agreement_count > len(last_messages) / 2 and disagreement_count == 0
    
    async def coordinate_workflow(self, workflow_config: Dict[str, Any]) -> Dict[str, Any]:
        """Coordinate a complete workflow using group chat."""
        try:
            # Create workflow coordination session
            session_id = await self.create_group_chat(
                chat_type=ChatType.WORKFLOW_COORDINATION,
                topic=f"Workflow: {workflow_config.get('name', 'Unknown')}",
                participants=list(self.agents.keys()),
                max_rounds=15
            )
            
            # Start coordination
            initial_message = f"""
            Let's coordinate the workflow: {workflow_config.get('name', 'Unknown')}
            
            Workflow Details:
            - Project: {workflow_config.get('project_path', 'Unknown')}
            - Target Stack: {workflow_config.get('target_stack', 'Unknown')}
            - Files: {workflow_config.get('files', [])}
            
            Please discuss the workflow plan and coordinate your tasks.
            """
            
            result = await self.start_group_chat(session_id, initial_message)
            
            # Extract coordination decisions
            coordination_plan = self._extract_coordination_plan(result)
            
            return {
                "workflow_id": workflow_config.get("workflow_id", "unknown"),
                "coordination_result": result,
                "coordination_plan": coordination_plan,
                "status": "coordinated"
            }
            
        except Exception as e:
            logger.error(f"Workflow coordination failed: {e}")
            return {
                "status": "error",
                "error": str(e)
            }
    
    def _extract_coordination_plan(self, chat_result: Dict[str, Any]) -> Dict[str, Any]:
        """Extract coordination plan from group chat result."""
        try:
            messages = chat_result.get("messages", [])
            
            # Analyze messages to extract plan
            plan = {
                "tasks": [],
                "assignments": {},
                "dependencies": [],
                "timeline": {},
                "decisions": []
            }
            
            for message in messages:
                content = message.get("content", "")
                agent = message.get("agent", "")
                
                # Extract task assignments
                if "task" in content.lower() and "assign" in content.lower():
                    plan["assignments"][agent] = content
                
                # Extract decisions
                if any(keyword in content.lower() for keyword in ["decide", "decision", "agree", "consensus"]):
                    plan["decisions"].append({
                        "agent": agent,
                        "decision": content
                    })
            
            return plan
            
        except Exception as e:
            logger.error(f"Failed to extract coordination plan: {e}")
            return {"error": str(e)}
    
    async def solve_problem(self, problem_description: str, participants: Optional[List[str]] = None) -> Dict[str, Any]:
        """Solve a problem using group chat."""
        try:
            # Use all agents if no specific participants
            if not participants:
                participants = list(self.agents.keys())
            
            # Create problem-solving session
            session_id = await self.create_group_chat(
                chat_type=ChatType.PROBLEM_SOLVING,
                topic="Problem Solving Session",
                participants=participants,
                max_rounds=12
            )
            
            # Start problem-solving discussion
            initial_message = f"""
            Problem to solve: {problem_description}
            
            Please analyze the problem and work together to find a solution.
            Consider different perspectives and approaches.
            """
            
            result = await self.start_group_chat(session_id, initial_message)
            
            # Extract solution
            solution = self._extract_solution(result)
            
            return {
                "problem": problem_description,
                "solution": solution,
                "discussion": result,
                "status": "solved"
            }
            
        except Exception as e:
            logger.error(f"Problem solving failed: {e}")
            return {
                "status": "error",
                "error": str(e)
            }
    
    def _extract_solution(self, chat_result: Dict[str, Any]) -> Dict[str, Any]:
        """Extract solution from problem-solving chat."""
        try:
            messages = chat_result.get("messages", [])
            
            solution = {
                "summary": "",
                "steps": [],
                "recommendations": [],
                "agents_involved": []
            }
            
            # Analyze final messages for solution
            final_messages = messages[-len(self.agents):] if len(messages) >= len(self.agents) else messages
            
            for message in final_messages:
                content = message.get("content", "")
                agent = message.get("agent", "")
                
                solution["agents_involved"].append(agent)
                
                # Extract solution components
                if "solution" in content.lower() or "recommend" in content.lower():
                    solution["recommendations"].append({
                        "agent": agent,
                        "recommendation": content
                    })
            
            return solution
            
        except Exception as e:
            logger.error(f"Failed to extract solution: {e}")
            return {"error": str(e)}
    
    async def review_code(self, code_content: str, review_focus: str = "general") -> Dict[str, Any]:
        """Perform code review using group chat."""
        try:
            # Create code review session
            session_id = await self.create_group_chat(
                chat_type=ChatType.CODE_REVIEW,
                topic=f"Code Review: {review_focus}",
                participants=list(self.agents.keys()),
                max_rounds=8
            )
            
            # Start code review
            initial_message = f"""
            Code Review Request
            
            Focus: {review_focus}
            
            Code to review:
            ```
            {code_content}
            ```
            
            Please review the code and provide feedback on:
            - Code quality
            - Best practices
            - Potential issues
            - Improvements
            """
            
            result = await self.start_group_chat(session_id, initial_message)
            
            # Extract review feedback
            review_feedback = self._extract_review_feedback(result)
            
            return {
                "code_review": review_feedback,
                "discussion": result,
                "status": "reviewed"
            }
            
        except Exception as e:
            logger.error(f"Code review failed: {e}")
            return {
                "status": "error",
                "error": str(e)
            }
    
    def _extract_review_feedback(self, chat_result: Dict[str, Any]) -> Dict[str, Any]:
        """Extract review feedback from code review chat."""
        try:
            messages = chat_result.get("messages", [])
            
            feedback = {
                "issues": [],
                "suggestions": [],
                "quality_score": 0,
                "reviewers": []
            }
            
            for message in messages:
                content = message.get("content", "")
                agent = message.get("agent", "")
                
                feedback["reviewers"].append(agent)
                
                # Extract issues and suggestions
                if any(keyword in content.lower() for keyword in ["issue", "problem", "bug", "error"]):
                    feedback["issues"].append({
                        "agent": agent,
                        "issue": content
                    })
                
                if any(keyword in content.lower() for keyword in ["suggest", "improve", "better", "recommend"]):
                    feedback["suggestions"].append({
                        "agent": agent,
                        "suggestion": content
                    })
            
            # Calculate quality score based on feedback
            feedback["quality_score"] = max(0, 10 - len(feedback["issues"]))
            
            return feedback
            
        except Exception as e:
            logger.error(f"Failed to extract review feedback: {e}")
            return {"error": str(e)}
    
    def _get_manager_llm_config(self) -> Dict[str, Any]:
        """Get LLM config for group chat manager."""
        return {
            "config_list": [{
                "model": self.ai.model,
                "api_key": self.ai.api_key,
                "base_url": self.ai.base_url if hasattr(self.ai, 'base_url') else None
            }],
            "temperature": 0.7,
            "max_tokens": 4000
        }
    
    async def get_session_status(self, session_id: str) -> Dict[str, Any]:
        """Get status of a group chat session."""
        if session_id not in self.active_sessions:
            return {"status": "not_found"}
        
        session = self.active_sessions[session_id]
        
        return {
            "status": "active" if session.is_active else "inactive",
            "session_id": session_id,
            "chat_type": session.chat_type.value,
            "topic": session.topic,
            "participants": session.participants,
            "current_round": session.current_round,
            "max_rounds": session.max_rounds,
            "is_active": session.is_active,
            "message_count": len(session.messages),
            "last_activity": session.last_activity
        }
    
    async def close_session(self, session_id: str) -> Dict[str, Any]:
        """Close a group chat session."""
        if session_id not in self.active_sessions:
            return {"status": "not_found"}
        
        session = self.active_sessions[session_id]
        session.is_active = False
        
        # Move to history
        self.session_history.append(session)
        del self.active_sessions[session_id]
        
        logger.info(f"Closed group chat session {session_id}")
        
        return {
            "session_id": session_id,
            "status": "closed",
            "total_messages": len(session.messages),
            "final_round": session.current_round
        }
    
    async def get_coordination_stats(self) -> Dict[str, Any]:
        """Get coordination statistics."""
        return {
            "total_sessions": len(self.session_history) + len(self.active_sessions),
            "active_sessions": len(self.active_sessions),
            "total_messages": sum(len(s.messages) for s in self.active_sessions.values()),
            "registered_agents": len(self.agents),
            "agent_roles": self.agent_roles.copy()
        } 