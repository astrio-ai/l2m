"""
Main LLM Agent for Advanced AI Capabilities

This module provides a comprehensive AI agent that can analyze, optimize,
and review code transformations for any language pair.
"""

import logging
from typing import Dict, Any, List, Optional
from dataclasses import dataclass
from abc import ABC, abstractmethod
import os


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


class LLMProvider(ABC):
    """Abstract base class for LLM providers."""
    
    @abstractmethod
    def generate_response(self, messages: List[Dict[str, str]], config: 'LLMConfig') -> str:
        """Generate response from LLM."""
        pass


@dataclass
class LLMConfig:
    """Configuration for LLM API calls."""
    api_key: str
    model: str
    temperature: float
    max_tokens: int = 2000
    provider: str = "anthropic"  # openai, anthropic, local
    cache_enabled: bool = True
    cache_ttl: int = 3600  # 1 hour
    retry_attempts: int = 3
    retry_delay: float = 1.0
    
    @classmethod
    def from_env(cls):
        """Create config from environment variables."""
        # Handle invalid temperature gracefully
        try:
            temperature = float(os.getenv('DEFAULT_LLM_TEMPERATURE', '0.1'))
        except ValueError:
            temperature = 0.1  # Default fallback
        
        # Get provider and set appropriate defaults
        provider = os.getenv('LLM_PROVIDER', 'anthropic')  # Default to Claude
        api_key = os.getenv('LLM_API_KEY', '')
        
        # Set model based on provider
        if provider == 'anthropic':
            default_model = 'claude-3-5-sonnet-20241022'  # Latest Claude model
        elif provider == 'local':
            default_model = 'llama2'
        else:
            default_model = 'claude-3-5-sonnet-20241022'
        
        return cls(
            api_key=api_key,
            model=os.getenv('LLM_MODEL', default_model),
            temperature=temperature,
            provider=provider,
            cache_enabled=os.getenv('LLM_CACHE_ENABLED', 'true').lower() == 'true',
            cache_ttl=int(os.getenv('LLM_CACHE_TTL', '3600')),
            retry_attempts=int(os.getenv('LLM_RETRY_ATTEMPTS', '3')),
            retry_delay=float(os.getenv('LLM_RETRY_DELAY', '1.0'))
        )


class LLMAgent:
    """
    Advanced AI agent for code analysis, optimization, and review.
    Language-agnostic and extensible for any code transformation.
    """
    
    def __init__(self, config: Optional[LLMConfig] = None, provider: Optional[LLMProvider] = None):
        self.config = config or LLMConfig(
            api_key="",
            model="llama2",
            temperature=0.1,
            provider="local"
        )
        self.provider = provider
        self.logger = logging.getLogger(__name__)
        
        # Initialize specialized components
        self.analyzer = CodeAnalyzer(self)
        self.optimizer = CodeOptimizer(self)
        self.reviewer = CodeReviewer(self)
    
    def analyze_code(self, source_code: str, target_code: str, 
                    language_pair: str = "legacy-modern") -> AnalysisResult:
        """
        Analyze the quality and characteristics of code transformation.
        
        Args:
            source_code: Original source code
            target_code: Transformed target code
            language_pair: Source-target language pair (e.g., "cobol-python", "html-react")
            
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
                           language_pair: str = "legacy-modern") -> List[str]:
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
                              language_pair: str = "legacy-modern") -> Dict[str, Any]:
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
    
    def call_llm(self, messages: List[Dict[str, str]]) -> str:
        """
        Call the LLM with messages.
        
        Args:
            messages: List of message dictionaries with 'role' and 'content'
            
        Returns:
            LLM response
        """
        if not self.provider:
            raise Exception("No LLM provider configured")
        
        return self.provider.generate_response(messages, self.config)
    
    def get_agent_capabilities(self) -> Dict[str, Any]:
        """
        Get information about agent capabilities.
        
        Returns:
            Dictionary with capability information
        """
        return {
            "llm_available": self.provider is not None,
            "provider": self.config.provider if self.provider else None,
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