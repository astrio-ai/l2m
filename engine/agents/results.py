"""
Result classes for LLM Agent

This module contains the dataclass definitions for analysis, optimization,
and review results to avoid circular imports.
"""

from dataclasses import dataclass
from typing import Dict, Any, List


@dataclass
class AnalysisResult:
    """Result of code analysis."""
    complexity_score: float
    maintainability_score: float
    performance_issues: List[str]
    security_concerns: List[str]
    suggestions: List[str]
    confidence: float


@dataclass
class OptimizationResult:
    """Result of code optimization."""
    original_code: str
    optimized_code: str
    improvements: List[str]
    performance_gains: Dict[str, float]
    confidence: float


@dataclass
class ReviewResult:
    """Result of code review."""
    issues: List[str]
    suggestions: List[str]
    severity: str  # low, medium, high, critical
    confidence: float
    automated_fixes: List[str] 