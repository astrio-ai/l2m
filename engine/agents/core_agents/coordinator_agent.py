"""
Coordinator Agent for the Legacy2Modern system.

This agent coordinates the activities of all other agents in the system,
managing the overall modernization workflow and ensuring proper
communication between specialized agents.
"""

import asyncio
import logging
from typing import Dict, List, Optional, Any, Set
from dataclasses import dataclass, field
from enum import Enum

from .base_agent import BaseAgent, AgentRole, AgentState
from ..utilities.ai import AI
from .base_memory import BaseMemory
from ..utilities.project_config import ProjectConfig
from ..utilities.preprompts_holder import PrepromptsHolder

logger = logging.getLogger(__name__)

class TaskStatus(Enum):
    """Status of a modernization task."""
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"

class TaskPriority(Enum):
    """Priority levels for tasks."""
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"

@dataclass
class ModernizationTask:
    """Represents a modernization task."""
    id: str
    name: str
    description: str
    assigned_agent: Optional[str] = None
    status: TaskStatus = TaskStatus.PENDING
    priority: TaskPriority = TaskPriority.MEDIUM
    dependencies: List[str] = field(default_factory=list)
    result: Optional[Dict[str, Any]] = None
    error_message: Optional[str] = None
    created_at: float = field(default_factory=lambda: asyncio.get_event_loop().time())
    started_at: Optional[float] = None
    completed_at: Optional[float] = None

@dataclass
class WorkflowStage:
    """Represents a stage in the modernization workflow."""
    name: str
    description: str
    required_agents: List[str]
    tasks: List[ModernizationTask] = field(default_factory=list)
    is_completed: bool = False
    can_parallelize: bool = True

class CoordinatorAgent(BaseAgent):
    """
    Coordinator agent that manages the modernization workflow.
    
    This agent is responsible for:
    - Orchestrating the work of other agents
    - Managing task dependencies and priorities
    - Monitoring progress and ensuring quality
    - Handling communication between agents
    - Providing status updates and reporting
    """
    
    def __init__(
        self,
        name: str,
        ai: AI,
        memory: BaseMemory,
        config: ProjectConfig,
        system_prompt: Optional[str] = None
    ):
        super().__init__(name, AgentRole.COORDINATOR, ai, memory, config, system_prompt)
        
        # Workflow management
        self.workflow_stages: List[WorkflowStage] = []
        self.tasks: Dict[str, ModernizationTask] = {}
        self.completed_tasks: Set[str] = set()
        self.failed_tasks: Set[str] = set()
        
        # Agent management
        self.available_agents: Dict[str, BaseAgent] = {}
        self.agent_status: Dict[str, Dict[str, Any]] = {}
        
        # Progress tracking
        self.overall_progress: float = 0.0
        self.current_stage: Optional[str] = None
        self.workflow_start_time: Optional[float] = None
        
        # Load preprompts
        self.prompts = PrepromptsHolder()
    
    def _get_default_system_prompt(self) -> str:
        """Return the default system prompt for the coordinator agent."""
        return """You are the Coordinator Agent for the Legacy2Modern system. Your role is to:

1. Orchestrate the modernization workflow
2. Manage task dependencies and priorities
3. Coordinate communication between specialized agents
4. Monitor progress and ensure quality standards
5. Handle errors and recovery
6. Provide status updates and reporting

You should be proactive, organized, and ensure the modernization process runs smoothly."""
    
    async def process_message(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """Process incoming messages from other agents."""
        message_type = message.get("type", "unknown")
        
        if message_type == "task_completed":
            return await self._handle_task_completed(message)
        elif message_type == "task_failed":
            return await self._handle_task_failed(message)
        elif message_type == "status_update":
            return await self._handle_status_update(message)
        elif message_type == "error_report":
            return await self._handle_error_report(message)
        elif message_type == "request_assistance":
            return await self._handle_assistance_request(message)
        elif message_type == "group_chat_message":
            return await self._handle_group_chat_message(message)
        else:
            logger.warning(f"Unknown message type: {message_type}")
            return {"type": "error", "message": f"Unknown message type: {message_type}"}
    
    async def execute_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Execute a coordination task."""
        task_type = task.get("type", "unknown")
        
        if task_type == "initialize_workflow":
            return await self._initialize_workflow(task)
        elif task_type == "assign_task":
            return await self._assign_task(task)
        elif task_type == "monitor_progress":
            return await self._monitor_progress(task)
        elif task_type == "handle_error":
            return await self._handle_error(task)
        elif task_type == "generate_report":
            return await self._generate_report(task)
        elif task_type == "manage_workflow":
            return await self._manage_workflow(task)
        elif task_type == "schedule_tasks":
            return await self._schedule_tasks(task)
        elif task_type == "track_progress":
            return await self._track_progress(task)
        else:
            logger.warning(f"Unknown task type: {task_type}")
            return {"status": "error", "message": f"Unknown task type: {task_type}"}
    
    async def register_agent(self, agent: BaseAgent):
        """Register an agent with the coordinator."""
        self.available_agents[agent.name] = agent
        self.agent_status[agent.name] = {
            "status": "available",
            "current_task": None,
            "task_count": 0,
            "success_count": 0,
            "error_count": 0,
            "last_activity": asyncio.get_event_loop().time()
        }
        # Also register the coordinator with the agent so they can communicate back
        agent.other_agents[self.name] = self
        logger.info(f"Registered agent: {agent.name}")
    
    async def unregister_agent(self, agent_name: str):
        """Unregister an agent from the coordinator."""
        if agent_name in self.available_agents:
            del self.available_agents[agent_name]
            del self.agent_status[agent_name]
            logger.info(f"Unregistered agent: {agent_name}")
    
    async def send_message(self, target_agent: str, message: Dict[str, Any]):
        """Send a message to another agent using the coordinator's available agents."""
        if target_agent in self.available_agents:
            await self.available_agents[target_agent].receive_message(self.name, message)
            self.state.messages_sent += 1
        else:
            logger.warning(f"Target agent {target_agent} not found in available agents: {list(self.available_agents.keys())}")
    
    async def create_workflow(self, project_config: Dict[str, Any]) -> str:
        """Create a modernization workflow based on project configuration."""
        workflow_id = f"workflow_{int(asyncio.get_event_loop().time())}"
        
        # Define workflow stages
        self.workflow_stages = [
            WorkflowStage(
                name="analysis",
                description="Analyze legacy code and create modernization plan",
                required_agents=["ParserAgent"],
                can_parallelize=False
            ),
            WorkflowStage(
                name="modernization",
                description="Generate modern code using target framework",
                required_agents=["ModernizerAgent"],
                can_parallelize=True
            ),
            WorkflowStage(
                name="refactoring",
                description="Optimize and improve generated code",
                required_agents=["RefactorAgent"],
                can_parallelize=True
            ),
            WorkflowStage(
                name="quality_assurance",
                description="Test and validate modernized code",
                required_agents=["QAAgent"],
                can_parallelize=False
            )
        ]
        
        # Create tasks for each stage
        for stage in self.workflow_stages:
            stage.tasks = await self._create_stage_tasks(stage, project_config)
        
        logger.info(f"Created workflow: {workflow_id}")
        return workflow_id
    
    async def _create_stage_tasks(self, stage: WorkflowStage, project_config: Dict[str, Any]) -> List[ModernizationTask]:
        """Create tasks for a specific workflow stage."""
        tasks = []
        
        if stage.name == "analysis":
            tasks.append(ModernizationTask(
                id=f"task_{stage.name}_parse",
                name="Parse Legacy Code",
                description="Parse and analyze the legacy code structure",
                priority=TaskPriority.HIGH
            ))
            tasks.append(ModernizationTask(
                id=f"task_{stage.name}_plan",
                name="Create Modernization Plan",
                description="Create a detailed plan for modernization",
                priority=TaskPriority.HIGH,
                dependencies=[f"task_{stage.name}_parse"]
            ))
        
        elif stage.name == "modernization":
            # Create tasks for each file to modernize
            source_files = project_config.get("source_files", [])
            for i, file_path in enumerate(source_files):
                tasks.append(ModernizationTask(
                    id=f"task_{stage.name}_file_{i}",
                    name=f"Modernize {file_path}",
                    description=f"Modernize the file {file_path}",
                    priority=TaskPriority.MEDIUM
                ))
        
        elif stage.name == "refactoring":
            tasks.append(ModernizationTask(
                id=f"task_{stage.name}_optimize",
                name="Optimize Code",
                description="Optimize the modernized code for performance and maintainability",
                priority=TaskPriority.MEDIUM
            ))
        
        elif stage.name == "quality_assurance":
            tasks.append(ModernizationTask(
                id=f"task_{stage.name}_test",
                name="Run Tests",
                description="Run comprehensive tests on the modernized code",
                priority=TaskPriority.HIGH
            ))
            tasks.append(ModernizationTask(
                id=f"task_{stage.name}_validate",
                name="Validate Quality",
                description="Validate code quality and standards",
                priority=TaskPriority.HIGH,
                dependencies=[f"task_{stage.name}_test"]
            ))
        
        return tasks
    
    async def start_workflow(self) -> Dict[str, Any]:
        """Start the modernization workflow and wait for completion."""
        self.workflow_start_time = asyncio.get_event_loop().time()
        self.current_stage = self.workflow_stages[0].name if self.workflow_stages else None
        
        # Add all tasks to the task dictionary
        for stage in self.workflow_stages:
            for task in stage.tasks:
                self.tasks[task.id] = task
        
        # Start the first stage
        if self.workflow_stages:
            await self._start_stage(self.workflow_stages[0])
        
        logger.info("Started modernization workflow")
        
        # Wait for workflow completion
        max_wait_time = 300  # 5 minutes timeout
        start_time = asyncio.get_event_loop().time()
        
        while not self._is_workflow_completed() and (asyncio.get_event_loop().time() - start_time) < max_wait_time:
            await asyncio.sleep(1)  # Wait 1 second before checking again
        
        if self._is_workflow_completed():
            return {
                "success": True,
                "status": "completed",
                "workflow_id": f"workflow_{int(self.workflow_start_time)}",
                "total_stages": len(self.workflow_stages),
                "total_tasks": len(self.tasks),
                "completed_tasks": len(self.completed_tasks),
                "failed_tasks": len(self.failed_tasks)
            }
        else:
            return {
                "success": False,
                "status": "timeout",
                "error": "Workflow timed out",
                "workflow_id": f"workflow_{int(self.workflow_start_time)}"
            }
    
    def _is_workflow_completed(self) -> bool:
        """Check if the workflow is completed."""
        return len(self.completed_tasks) + len(self.failed_tasks) >= len(self.tasks)
    
    async def _start_stage(self, stage: WorkflowStage):
        """Start a specific workflow stage."""
        logger.info(f"Starting stage: {stage.name}")
        
        # Debug: Log available agents
        logger.info(f"Available agents: {list(self.available_agents.keys())}")
        logger.info(f"Required agents for stage {stage.name}: {stage.required_agents}")
        
        # Check if required agents are available
        for agent_name in stage.required_agents:
            if agent_name not in self.available_agents:
                logger.error(f"Agent {agent_name} not found in available agents: {list(self.available_agents.keys())}")
                raise ValueError(f"Required agent {agent_name} not available for stage {stage.name}")
        
        # Assign tasks to agents
        for task in stage.tasks:
            if task.dependencies:
                # Check if dependencies are completed
                if not all(dep in self.completed_tasks for dep in task.dependencies):
                    continue
            
            await self._assign_task_to_agent(task, stage)
    
    async def _assign_task_to_agent(self, task: ModernizationTask, stage: WorkflowStage):
        """Assign a task to an appropriate agent."""
        # Find available agent for this task
        available_agent = None
        for agent_name in stage.required_agents:
            agent = self.available_agents.get(agent_name)
            if agent and self.agent_status[agent_name]["status"] == "available":
                available_agent = agent
                break
        
        if not available_agent:
            logger.warning(f"No available agent for task: {task.id}")
            return
        
        # Assign task
        task.assigned_agent = available_agent.name
        task.status = TaskStatus.IN_PROGRESS
        task.started_at = asyncio.get_event_loop().time()
        
        # Update agent status
        self.agent_status[available_agent.name]["status"] = "busy"
        self.agent_status[available_agent.name]["current_task"] = task.id
        self.agent_status[available_agent.name]["task_count"] += 1
        
        # Send task to agent
        await self.send_message(available_agent.name, {
            "type": "execute_task",
            "task_id": task.id,
            "task_data": {
                "type": task.name.lower().replace(" ", "_"),
                "description": task.description,
                "priority": task.priority.value
            }
        })
        
        logger.info(f"Assigned task {task.id} to agent {available_agent.name}")
    
    async def _handle_task_completed(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """Handle task completion message from an agent."""
        task_id = message.get("task_id")
        result = message.get("result", {})
        agent_name = message.get("agent_name")
        
        if task_id in self.tasks:
            task = self.tasks[task_id]
            task.status = TaskStatus.COMPLETED
            task.result = result
            task.completed_at = asyncio.get_event_loop().time()
            
            self.completed_tasks.add(task_id)
            
            # Update agent status
            if agent_name and agent_name in self.agent_status:
                self.agent_status[agent_name]["status"] = "available"
                self.agent_status[agent_name]["current_task"] = None
                self.agent_status[agent_name]["success_count"] += 1
                self.agent_status[agent_name]["last_activity"] = asyncio.get_event_loop().time()
            
            # Check if stage is completed
            await self._check_stage_completion()
            
            # Update overall progress
            await self._update_progress()
            
            logger.info(f"Task completed: {task_id}")
        
        return {"type": "acknowledgment", "status": "received"}
    
    async def _handle_task_failed(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """Handle task failure message from an agent."""
        task_id = message.get("task_id")
        error_message = message.get("error_message", "Unknown error")
        agent_name = message.get("agent_name")
        
        if task_id in self.tasks:
            task = self.tasks[task_id]
            task.status = TaskStatus.FAILED
            task.error_message = error_message
            
            self.failed_tasks.add(task_id)
            
            # Update agent status
            if agent_name and agent_name in self.agent_status:
                self.agent_status[agent_name]["status"] = "available"
                self.agent_status[agent_name]["current_task"] = None
                self.agent_status[agent_name]["error_count"] += 1
                self.agent_status[agent_name]["last_activity"] = asyncio.get_event_loop().time()
            
            logger.error(f"Task failed: {task_id} - {error_message}")
        
        return {"type": "acknowledgment", "status": "received"}
    
    async def _check_stage_completion(self):
        """Check if the current stage is completed and move to next stage."""
        if not self.current_stage:
            return
        
        current_stage = next((s for s in self.workflow_stages if s.name == self.current_stage), None)
        if not current_stage:
            return
        
        # Check if all tasks in current stage are completed
        stage_tasks = [task.id for task in current_stage.tasks]
        completed_stage_tasks = [task_id for task_id in stage_tasks if task_id in self.completed_tasks]
        
        if len(completed_stage_tasks) == len(stage_tasks):
            current_stage.is_completed = True
            logger.info(f"Stage completed: {self.current_stage}")
            
            # Move to next stage
            current_index = next((i for i, s in enumerate(self.workflow_stages) if s.name == self.current_stage), -1)
            if current_index >= 0 and current_index + 1 < len(self.workflow_stages):
                next_stage = self.workflow_stages[current_index + 1]
                self.current_stage = next_stage.name
                await self._start_stage(next_stage)
            else:
                # Workflow completed
                await self._workflow_completed()
    
    async def _workflow_completed(self):
        """Handle workflow completion."""
        logger.info("Modernization workflow completed")
        
        # Generate final report
        report = await self._generate_final_report()
        
        # Save to memory
        await self.memory.set("workflow_completion_report", report)
        
        # Notify all agents
        for agent_name in self.available_agents:
            await self.send_message(agent_name, {
                "type": "workflow_completed",
                "report": report
            })
    
    async def _update_progress(self):
        """Update overall progress."""
        total_tasks = len(self.tasks)
        completed_tasks = len(self.completed_tasks)
        
        if total_tasks > 0:
            self.overall_progress = completed_tasks / total_tasks
            self.update_progress(self.overall_progress)
    
    async def _generate_final_report(self) -> Dict[str, Any]:
        """Generate a final report of the modernization workflow."""
        return {
            "workflow_id": f"workflow_{int(self.workflow_start_time)}",
            "start_time": self.workflow_start_time,
            "end_time": asyncio.get_event_loop().time(),
            "duration": asyncio.get_event_loop().time() - self.workflow_start_time,
            "total_tasks": len(self.tasks),
            "completed_tasks": len(self.completed_tasks),
            "failed_tasks": len(self.failed_tasks),
            "success_rate": len(self.completed_tasks) / len(self.tasks) if self.tasks else 0,
            "stages": [
                {
                    "name": stage.name,
                    "description": stage.description,
                    "is_completed": stage.is_completed,
                    "task_count": len(stage.tasks),
                    "completed_task_count": len([t for t in stage.tasks if t.id in self.completed_tasks])
                }
                for stage in self.workflow_stages
            ],
            "agent_performance": self.agent_status,
            "overall_progress": self.overall_progress
        }
    
    async def get_status(self) -> Dict[str, Any]:
        """Get detailed status of the coordinator and workflow."""
        base_status = super().get_status()
        
        return {
            **base_status,
            "workflow_status": {
                "current_stage": self.current_stage,
                "overall_progress": self.overall_progress,
                "total_tasks": len(self.tasks),
                "completed_tasks": len(self.completed_tasks),
                "failed_tasks": len(self.failed_tasks),
                "available_agents": len([a for a in self.agent_status.values() if a["status"] == "available"]),
                "busy_agents": len([a for a in self.agent_status.values() if a["status"] == "busy"])
            },
            "stages": [
                {
                    "name": stage.name,
                    "is_completed": stage.is_completed,
                    "task_count": len(stage.tasks),
                    "completed_count": len([t for t in stage.tasks if t.id in self.completed_tasks])
                }
                for stage in self.workflow_stages
            ],
            "agent_status": self.agent_status
        }
    
    async def _handle_status_update(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """Handle status update from an agent."""
        agent_name = message.get("agent_name")
        status_data = message.get("status", {})
        
        if agent_name in self.agent_status:
            self.agent_status[agent_name].update(status_data)
            self.agent_status[agent_name]["last_activity"] = asyncio.get_event_loop().time()
        
        return {"type": "acknowledgment", "status": "received"}
    
    async def _handle_error_report(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """Handle error report from an agent."""
        agent_name = message.get("agent_name")
        error_data = message.get("error", {})
        
        logger.error(f"Error from agent {agent_name}: {error_data}")
        
        # Update agent error count
        if agent_name in self.agent_status:
            self.agent_status[agent_name]["error_count"] += 1
        
        return {"type": "acknowledgment", "status": "received"}
    
    async def _handle_assistance_request(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """Handle assistance request from an agent."""
        agent_name = message.get("agent_name")
        request_data = message.get("request", {})
        
        logger.info(f"Assistance request from {agent_name}: {request_data}")
        
        # Provide assistance based on request type
        assistance_type = request_data.get("type")
        if assistance_type == "task_clarification":
            return await self._provide_task_clarification(request_data)
        elif assistance_type == "resource_request":
            return await self._handle_resource_request(request_data)
        else:
            return {"type": "assistance_response", "message": "Request received, processing..."}
    
    async def _provide_task_clarification(self, request_data: Dict[str, Any]) -> Dict[str, Any]:
        """Provide clarification for a task."""
        task_id = request_data.get("task_id")
        clarification_needed = request_data.get("clarification_needed")
        
        # Use AI to generate clarification
        prompt = f"Task {task_id} needs clarification: {clarification_needed}. Provide clear, actionable guidance."
        clarification = await self.get_ai_response(prompt)
        
        return {
            "type": "task_clarification",
            "task_id": task_id,
            "clarification": clarification
        }
    
    async def _handle_resource_request(self, request_data: Dict[str, Any]) -> Dict[str, Any]:
        """Handle resource request from an agent."""
        resource_type = request_data.get("resource_type")
        resource_details = request_data.get("resource_details", {})
        
        # Provide appropriate resources based on type
        if resource_type == "template":
            template_name = resource_details.get("template_name")
            variables = resource_details.get("variables", {})
            
            template_content = self.prompts.render_template(template_name, variables)
            return {
                "type": "resource_response",
                "resource_type": "template",
                "content": template_content
            }
        
        return {
            "type": "resource_response",
            "message": f"Resource request for {resource_type} received"
        }
    
    async def _initialize_workflow(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Initialize the modernization workflow."""
        project_config = task.get("project_config", {})
        
        try:
            workflow_id = await self.create_workflow(project_config)
            result = await self.start_workflow()
            
            return {
                "status": "success",
                "workflow_id": workflow_id,
                "result": result
            }
        except Exception as e:
            logger.error(f"Failed to initialize workflow: {e}")
            return {
                "status": "error",
                "error": str(e)
            }
    
    async def _assign_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Assign a specific task to an agent."""
        task_id = task.get("task_id")
        agent_name = task.get("agent_name")
        
        if task_id not in self.tasks:
            return {"status": "error", "message": f"Task {task_id} not found"}
        
        if agent_name not in self.available_agents:
            return {"status": "error", "message": f"Agent {agent_name} not available"}
        
        task_obj = self.tasks[task_id]
        stage = next((s for s in self.workflow_stages if any(t.id == task_id for t in s.tasks)), None)
        
        if stage:
            await self._assign_task_to_agent(task_obj, stage)
            return {"status": "success", "message": f"Task {task_id} assigned to {agent_name}"}
        else:
            return {"status": "error", "message": f"Could not find stage for task {task_id}"}
    
    async def _monitor_progress(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Monitor the progress of the modernization workflow."""
        return await self.get_status()
    
    async def _handle_error(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Handle errors in the workflow."""
        error_type = task.get("error_type")
        error_data = task.get("error_data", {})
        
        logger.error(f"Workflow error - Type: {error_type}, Data: {error_data}")
        
        # Implement error recovery strategies
        if error_type == "agent_failure":
            return await self._handle_agent_failure(error_data)
        elif error_type == "task_failure":
            return await self._handle_task_failure_recovery(error_data)
        else:
            return {"status": "error_handled", "message": "Error logged and handled"}
    
    async def _handle_agent_failure(self, error_data: Dict[str, Any]) -> Dict[str, Any]:
        """Handle agent failure and recovery."""
        failed_agent = error_data.get("agent_name")
        failed_task = error_data.get("task_id")
        
        if failed_agent in self.agent_status:
            self.agent_status[failed_agent]["status"] = "failed"
            self.agent_status[failed_agent]["error_count"] += 1
        
        # Reassign task if possible
        if failed_task and failed_task in self.tasks:
            task_obj = self.tasks[failed_task]
            task_obj.status = TaskStatus.PENDING
            task_obj.assigned_agent = None
            
            # Try to find another available agent
            for agent_name, agent in self.available_agents.items():
                if self.agent_status[agent_name]["status"] == "available":
                    await self._assign_task_to_agent(task_obj, self.workflow_stages[0])  # Simplified
                    break
        
        return {"status": "recovery_initiated", "message": f"Handled failure of agent {failed_agent}"}
    
    async def _handle_task_failure_recovery(self, error_data: Dict[str, Any]) -> Dict[str, Any]:
        """Handle task failure and recovery."""
        failed_task = error_data.get("task_id")
        error_message = error_data.get("error_message", "Unknown error")
        
        if failed_task in self.tasks:
            task_obj = self.tasks[failed_task]
            
            # Implement retry logic
            max_retries = 3
            current_retries = task_obj.result.get("retry_count", 0) if task_obj.result else 0
            
            if current_retries < max_retries:
                task_obj.status = TaskStatus.PENDING
                task_obj.assigned_agent = None
                task_obj.result = {"retry_count": current_retries + 1}
                
                # Retry the task
                stage = next((s for s in self.workflow_stages if any(t.id == failed_task for t in s.tasks)), None)
                if stage:
                    await self._assign_task_to_agent(task_obj, stage)
                    return {"status": "retry_initiated", "message": f"Retrying task {failed_task}"}
            
            # Mark as permanently failed
            task_obj.status = TaskStatus.FAILED
            task_obj.error_message = error_message
        
        return {"status": "failure_handled", "message": f"Handled failure of task {failed_task}"}
    
    async def _generate_report(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Generate a report of the modernization progress."""
        report_type = task.get("report_type", "progress")
        
        if report_type == "progress":
            return await self.get_status()
        elif report_type == "final":
            return await self._generate_final_report()
        else:
            return {"status": "error", "message": f"Unknown report type: {report_type}"}
    
    async def _manage_workflow(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Manage workflow operations."""
        operation = task.get("operation", "status")
        project_path = task.get("project_path", "")
        target_stack = task.get("target_stack", "")
        
        if operation == "create":
            workflow_id = await self.create_workflow({
                "project_path": project_path,
                "target_stack": target_stack
            })
            return {
                "status": "success",
                "workflow_id": workflow_id,
                "message": "Workflow created successfully"
            }
        elif operation == "start":
            return await self.start_workflow()
        else:
            return {
                "status": "success",
                "workflow": {
                    "current_stage": self.current_stage,
                    "progress": self.overall_progress,
                    "total_tasks": len(self.tasks),
                    "completed_tasks": len(self.completed_tasks)
                }
            }
    
    async def _schedule_tasks(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Schedule tasks for execution."""
        tasks = task.get("tasks", [])
        
        scheduled_tasks = []
        for task_data in tasks:
            task_id = f"task_{len(self.tasks) + 1}"
            modernization_task = ModernizationTask(
                id=task_id,
                name=task_data.get("type", "unknown"),
                description=f"Task: {task_data.get('file', 'unknown')}",
                priority=TaskPriority.MEDIUM
            )
            self.tasks[task_id] = modernization_task
            scheduled_tasks.append(task_id)
        
        return {
            "status": "success",
            "scheduled_tasks": scheduled_tasks,
            "total_tasks": len(self.tasks)
        }
    
    async def _track_progress(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Track progress of a specific workflow."""
        workflow_id = task.get("workflow_id", "")
        
        return {
            "status": "success",
            "workflow_id": workflow_id,
            "progress": {
                "overall_progress": self.overall_progress,
                "current_stage": self.current_stage,
                "total_tasks": len(self.tasks),
                "completed_tasks": len(self.completed_tasks),
                "failed_tasks": len(self.failed_tasks),
                "available_agents": len([a for a in self.agent_status.values() if a["status"] == "available"])
            }
        }
    
    async def _handle_group_chat_message(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """Handle group chat messages."""
        content = message.get("content", "")
        session_id = message.get("session_id", "")
        round_num = message.get("round", 1)
        topic = message.get("topic", "")
        
        # Generate a response based on the coordinator's role
        response = f"""
        As the Coordinator Agent, I'm participating in group chat session {session_id} (Round {round_num}).
        
        Topic: {topic}
        Message: {content}
        
        My role is to coordinate and facilitate the discussion. I'll help ensure all agents are working together effectively and that we reach consensus on important decisions.
        
        Let's continue the discussion and work towards our common goal.
        """
        
        return {
            "type": "group_chat_response",
            "content": response,
            "session_id": session_id,
            "round": round_num,
            "agent": self.name
        } 