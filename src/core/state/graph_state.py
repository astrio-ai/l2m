"""
LangGraph state definitions.

This module defines the state structure for LangGraph workflows,
including all the data that flows between agents.
"""

from typing import Dict, Any, List, Optional, TypedDict, Annotated
from langchain_core.messages import BaseMessage
from operator import add


class GraphState(TypedDict):
    """Main state for the LangGraph workflow."""
    
    # Input parameters
    codebase_path: str
    target_language: str
    modernization_goals: List[str]
    backup_location: Optional[str]
    test_framework: Optional[str]
    
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
    
    # Final results
    modernization_success: Optional[bool]
    error: Optional[str]
    
    # Message history - using Annotated for LangGraph compatibility
    messages: Annotated[List[BaseMessage], add]
    
    # Workflow metadata
    workflow_id: Optional[str]
    start_time: Optional[str]
    end_time: Optional[str]
    total_duration: Optional[float]
