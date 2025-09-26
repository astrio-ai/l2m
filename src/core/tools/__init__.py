"""
Agent tools and utilities.

This package contains all the tools that agents can use to perform
their specialized tasks in the modernization process.
"""

# Import handoff tools for agent-to-agent communication
from .handoff_tools import (
    create_handoff_tool,
    create_task_description_handoff_tool,
    create_transformation_handoff_tool,
    create_analysis_handoff_tool,
    transfer_to_planner,
    transfer_to_executor,
    transfer_to_reviewer,
    transfer_to_tester,
    transfer_to_validator
)

__all__ = [
    "create_handoff_tool",
    "create_task_description_handoff_tool", 
    "create_transformation_handoff_tool",
    "create_analysis_handoff_tool",
    "transfer_to_planner",
    "transfer_to_executor",
    "transfer_to_reviewer",
    "transfer_to_tester",
    "transfer_to_validator"
]
