"""
Agent configuration settings.

This module provides configuration settings for all agents
in the multi-agent system.
"""

from typing import Dict, Any, List, Optional
from pydantic import BaseModel, Field

from src.utils.logger import get_logger

logger = get_logger(__name__)


class AgentConfig(BaseModel):
    """Base configuration for all agents."""
    
    name: str = Field(..., description="Agent name")
    description: str = Field(..., description="Agent description")
    timeout: int = Field(default=300, description="Agent timeout in seconds")
    retry_count: int = Field(default=3, description="Agent retry count")
    parallel_limit: int = Field(default=5, description="Agent parallel limit")
    tools: List[str] = Field(default_factory=list, description="Available tools")
    options: Dict[str, Any] = Field(default_factory=dict, description="Agent options")


class AnalyzerAgentConfig(AgentConfig):
    """Configuration for the analyzer agent."""
    
    name: str = "analyzer"
    description: str = "Analyzes legacy codebases"
    tools: List[str] = ["code_analyzer", "dependency_analyzer", "file_reader", "directory_scanner"]
    options: Dict[str, Any] = {
        "analysis_depth": "full",
        "include_comments": True,
        "detect_patterns": True
    }


class PlannerAgentConfig(AgentConfig):
    """Configuration for the planner agent."""
    
    name: str = "planner"
    description: str = "Creates modernization plans"
    tools: List[str] = ["modernization_planner", "risk_assessor", "pattern_searcher"]
    options: Dict[str, Any] = {
        "planning_horizon": "long_term",
        "risk_tolerance": "medium",
        "optimization_goal": "maintainability"
    }


class ExecutorAgentConfig(AgentConfig):
    """Configuration for the executor agent."""
    
    name: str = "executor"
    description: str = "Executes code transformations"
    tools: List[str] = ["code_transformer", "pattern_replacer", "file_writer", "backup_tool"]
    options: Dict[str, Any] = {
        "transformation_mode": "safe",
        "backup_enabled": True,
        "validation_enabled": True
    }


class ReviewerAgentConfig(AgentConfig):
    """Configuration for the reviewer agent."""
    
    name: str = "reviewer"
    description: str = "Reviews transformed code"
    tools: List[str] = ["code_reviewer", "quality_analyzer", "test_generator"]
    options: Dict[str, Any] = {
        "review_depth": "comprehensive",
        "quality_threshold": 0.8,
        "generate_tests": True
    }


class TesterAgentConfig(AgentConfig):
    """Configuration for the tester agent."""
    
    name: str = "tester"
    description: str = "Generates and runs tests"
    tools: List[str] = ["test_runner", "coverage_analyzer", "test_validator"]
    options: Dict[str, Any] = {
        "test_framework": "pytest",
        "coverage_target": 80.0,
        "test_types": ["unit", "integration"]
    }


class ValidatorAgentConfig(AgentConfig):
    """Configuration for the validator agent."""
    
    name: str = "validator"
    description: str = "Validates final code"
    tools: List[str] = ["final_validator", "compliance_checker", "integration_tester"]
    options: Dict[str, Any] = {
        "validation_level": "strict",
        "compliance_standards": ["pep8", "pylint"],
        "integration_tests": True
    }


class AgentConfigManager:
    """Manager for agent configurations."""
    
    def __init__(self):
        """Initialize the agent configuration manager."""
        self.configs = {
            "analyzer": AnalyzerAgentConfig(),
            "planner": PlannerAgentConfig(),
            "executor": ExecutorAgentConfig(),
            "reviewer": ReviewerAgentConfig(),
            "tester": TesterAgentConfig(),
            "validator": ValidatorAgentConfig()
        }
        self.logger = get_logger(__name__)
    
    def get_config(self, agent_name: str) -> Optional[AgentConfig]:
        """Get configuration for a specific agent."""
        return self.configs.get(agent_name)
    
    def get_all_configs(self) -> Dict[str, AgentConfig]:
        """Get all agent configurations."""
        return self.configs
    
    def update_config(self, agent_name: str, config: AgentConfig):
        """Update configuration for a specific agent."""
        self.configs[agent_name] = config
        self.logger.info(f"Updated configuration for agent: {agent_name}")
    
    def get_agent_tools(self, agent_name: str) -> List[str]:
        """Get tools for a specific agent."""
        config = self.get_config(agent_name)
        return config.tools if config else []
    
    def get_agent_options(self, agent_name: str) -> Dict[str, Any]:
        """Get options for a specific agent."""
        config = self.get_config(agent_name)
        return config.options if config else {}
