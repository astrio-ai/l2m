"""
Agent-specific state definitions.

This module defines the state structure for individual agents,
including their specific data and context.
"""

from typing import Dict, Any, List, Optional, TypedDict
from langchain_core.messages import BaseMessage


class AgentState(TypedDict):
    """State for individual agents."""
    
    # Agent identification
    agent_id: str
    agent_type: str
    
    # Input data
    codebase_path: str
    target_language: str
    modernization_goals: List[str]
    
    # Analysis results
    analysis_results: Optional[Dict[str, Any]]
    
    # Planning results
    modernization_plan: Optional[Dict[str, Any]]
    risk_assessment: Optional[Dict[str, Any]]
    implementation_strategy: Optional[Dict[str, Any]]
    
    # Execution results
    transformation_results: Optional[List[Dict[str, Any]]]
    pattern_results: Optional[Dict[str, Any]]
    backup_path: Optional[str]
    
    # Review results
    quality_review: Optional[Dict[str, Any]]
    standards_analysis: Optional[Dict[str, Any]]
    test_cases: Optional[Dict[str, Any]]
    
    # Test results
    test_results: Optional[Dict[str, Any]]
    coverage_analysis: Optional[Dict[str, Any]]
    test_validation: Optional[Dict[str, Any]]
    
    # Validation results
    final_validation: Optional[Dict[str, Any]]
    compliance_check: Optional[Dict[str, Any]]
    integration_tests: Optional[Dict[str, Any]]
    
    # Agent-specific data
    agent_data: Optional[Dict[str, Any]]
    
    # Error handling
    error: Optional[str]
    
    # Message history
    messages: List[BaseMessage]
    
    # Agent metadata
    start_time: Optional[str]
    end_time: Optional[str]
    duration: Optional[float]
