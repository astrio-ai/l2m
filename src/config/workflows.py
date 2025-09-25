"""
Workflow configuration settings.

This module provides configuration settings for LangGraph workflows
in the multi-agent system.
"""

from typing import Dict, Any, List, Optional
from pydantic import BaseModel, Field
from enum import Enum

from src.utils.logger import get_logger

logger = get_logger(__name__)


class WorkflowType(str, Enum):
    """Workflow type enumeration."""
    MAIN = "main"
    LEGACY_ANALYSIS = "legacy_analysis"
    MODERNIZATION = "modernization"
    VALIDATION = "validation"


class WorkflowConfig(BaseModel):
    """Base configuration for workflows."""
    
    name: str = Field(..., description="Workflow name")
    description: str = Field(..., description="Workflow description")
    workflow_type: WorkflowType = Field(..., description="Workflow type")
    timeout: int = Field(default=3600, description="Workflow timeout in seconds")
    retry_count: int = Field(default=2, description="Workflow retry count")
    parallel_limit: int = Field(default=3, description="Workflow parallel limit")
    agents: List[str] = Field(default_factory=list, description="Workflow agents")
    options: Dict[str, Any] = Field(default_factory=dict, description="Workflow options")


class MainWorkflowConfig(WorkflowConfig):
    """Configuration for the main workflow."""
    
    name: str = "main"
    description: str = "Complete modernization workflow with all agents"
    workflow_type: WorkflowType = WorkflowType.MAIN
    agents: List[str] = ["analyzer", "planner", "executor", "reviewer", "tester", "validator"]
    options: Dict[str, Any] = {
        "sequential_execution": True,
        "error_handling": "strict",
        "progress_tracking": True
    }


class LegacyAnalysisWorkflowConfig(WorkflowConfig):
    """Configuration for the legacy analysis workflow."""
    
    name: str = "legacy_analysis"
    description: str = "Analyze legacy codebases"
    workflow_type: WorkflowType = WorkflowType.LEGACY_ANALYSIS
    agents: List[str] = ["analyzer"]
    options: Dict[str, Any] = {
        "analysis_depth": "comprehensive",
        "include_metrics": True,
        "generate_report": True
    }


class ModernizationWorkflowConfig(WorkflowConfig):
    """Configuration for the modernization workflow."""
    
    name: str = "modernization"
    description: str = "Transform legacy code to modern equivalents"
    workflow_type: WorkflowType = WorkflowType.MODERNIZATION
    agents: List[str] = ["planner", "executor", "validator"]
    options: Dict[str, Any] = {
        "transformation_mode": "safe",
        "backup_enabled": True,
        "validation_enabled": True
    }


class ValidationWorkflowConfig(WorkflowConfig):
    """Configuration for the validation workflow."""
    
    name: str = "validation"
    description: str = "Validate modernized code"
    workflow_type: WorkflowType = WorkflowType.VALIDATION
    agents: List[str] = ["reviewer", "tester", "validator"]
    options: Dict[str, Any] = {
        "validation_level": "strict",
        "test_coverage_target": 80.0,
        "quality_threshold": 0.8
    }


class WorkflowConfigManager:
    """Manager for workflow configurations."""
    
    def __init__(self):
        """Initialize the workflow configuration manager."""
        self.configs = {
            "main": MainWorkflowConfig(),
            "legacy_analysis": LegacyAnalysisWorkflowConfig(),
            "modernization": ModernizationWorkflowConfig(),
            "validation": ValidationWorkflowConfig()
        }
        self.logger = get_logger(__name__)
    
    def get_config(self, workflow_name: str) -> Optional[WorkflowConfig]:
        """Get configuration for a specific workflow."""
        return self.configs.get(workflow_name)
    
    def get_all_configs(self) -> Dict[str, WorkflowConfig]:
        """Get all workflow configurations."""
        return self.configs
    
    def update_config(self, workflow_name: str, config: WorkflowConfig):
        """Update configuration for a specific workflow."""
        self.configs[workflow_name] = config
        self.logger.info(f"Updated configuration for workflow: {workflow_name}")
    
    def get_workflow_agents(self, workflow_name: str) -> List[str]:
        """Get agents for a specific workflow."""
        config = self.get_config(workflow_name)
        return config.agents if config else []
    
    def get_workflow_options(self, workflow_name: str) -> Dict[str, Any]:
        """Get options for a specific workflow."""
        config = self.get_config(workflow_name)
        return config.options if config else {}
    
    def get_workflows_by_type(self, workflow_type: WorkflowType) -> Dict[str, WorkflowConfig]:
        """Get workflows by type."""
        return {
            name: config for name, config in self.configs.items()
            if config.workflow_type == workflow_type
        }
