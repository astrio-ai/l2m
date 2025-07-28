"""
Main LLM Agent for Advanced AI Capabilities

This module provides a comprehensive AI agent that can analyze, optimize,
and review code transformations.
"""

import logging
from typing import Dict, Any, List, Optional
from dataclasses import dataclass
from ..transpiler.engine.llm_augmentor import LLMConfig, LLMAugmentor


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


class LLMAgent:
    """
    Advanced AI agent for code analysis, optimization, and review.
    """
    
    def __init__(self, config: Optional[LLMConfig] = None):
        self.config = config or LLMConfig.from_env()
        self.llm_augmentor = LLMAugmentor(config)
        self.logger = logging.getLogger(__name__)
        
        # Initialize specialized components
        self.analyzer = CodeAnalyzer(self.llm_augmentor)
        self.optimizer = CodeOptimizer(self.llm_augmentor)
        self.reviewer = CodeReviewer(self.llm_augmentor)
    
    def analyze_code(self, source_code: str, target_code: str, 
                    language_pair: str = "cobol-python") -> AnalysisResult:
        """
        Analyze the quality and characteristics of code transformation.
        
        Args:
            source_code: Original source code
            target_code: Transformed target code
            language_pair: Source-target language pair
            
        Returns:
            Analysis result with scores and suggestions
        """
        return self.analyzer.analyze(source_code, target_code, language_pair)
    
    def optimize_code(self, code: str, language: str = "python",
                     optimization_level: str = "balanced") -> OptimizationResult:
        """
        Optimize code for performance, readability, or maintainability.
        
        Args:
            code: Code to optimize
            language: Programming language
            optimization_level: Level of optimization (conservative, balanced, aggressive)
            
        Returns:
            Optimization result with improved code
        """
        return self.optimizer.optimize(code, language, optimization_level)
    
    def review_code(self, code: str, language: str = "python",
                   review_type: str = "comprehensive") -> ReviewResult:
        """
        Perform automated code review.
        
        Args:
            code: Code to review
            language: Programming language
            review_type: Type of review (basic, comprehensive, security-focused)
            
        Returns:
            Review result with issues and suggestions
        """
        return self.reviewer.review(code, language, review_type)
    
    def generate_documentation(self, code: str, language: str = "python") -> str:
        """
        Generate documentation for code.
        
        Args:
            code: Code to document
            language: Programming language
            
        Returns:
            Generated documentation
        """
        return self.analyzer.generate_documentation(code, language)
    
    def suggest_improvements(self, source_code: str, target_code: str,
                           language_pair: str = "cobol-python") -> List[str]:
        """
        Suggest improvements for code transformation.
        
        Args:
            source_code: Original source code
            target_code: Transformed target code
            language_pair: Source-target language pair
            
        Returns:
            List of improvement suggestions
        """
        analysis = self.analyze_code(source_code, target_code, language_pair)
        return analysis.suggestions
    
    def validate_transformation(self, source_code: str, target_code: str,
                              language_pair: str = "cobol-python") -> Dict[str, Any]:
        """
        Validate that a code transformation is correct and complete.
        
        Args:
            source_code: Original source code
            target_code: Transformed target code
            language_pair: Source-target language pair
            
        Returns:
            Validation result with confidence and issues
        """
        return self.analyzer.validate_transformation(source_code, target_code, language_pair)
    
    def get_agent_capabilities(self) -> Dict[str, Any]:
        """
        Get information about agent capabilities.
        
        Returns:
            Dictionary with capability information
        """
        return {
            "llm_available": self.llm_augmentor.can_augment(),
            "provider": self.config.provider,
            "model": self.config.model,
            "cache_enabled": self.config.cache_enabled,
            "capabilities": [
                "code_analysis",
                "code_optimization", 
                "code_review",
                "documentation_generation",
                "transformation_validation"
            ]
        }


# Import these after the main class to avoid circular imports
from .code_analyzer import CodeAnalyzer
from .optimizer import CodeOptimizer
from .reviewer import CodeReviewer 