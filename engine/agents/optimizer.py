"""
Code Optimizer for LLM Agent

This module provides language-agnostic code optimization capabilities
for the LLM agent system.
"""

import logging
from typing import Dict, Any, List, Optional
from .agent import OptimizationResult


class CodeOptimizer:
    """
    Language-agnostic code optimizer for the LLM agent.
    """
    
    def __init__(self, llm_agent):
        self.llm_agent = llm_agent
        self.logger = logging.getLogger(__name__)
    
    def optimize(self, code: str, language: str = "python",
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
        try:
            # Create optimization prompt
            prompt = self._create_optimization_prompt(code, language, optimization_level)
            
            # Call LLM for optimization
            messages = [
                {"role": "system", "content": "You are an expert code optimizer specializing in performance and maintainability improvements."},
                {"role": "user", "content": prompt}
            ]
            
            response = self.llm_agent.call_llm(messages)
            
            # Parse optimization results
            optimization = self._parse_optimization_response(response)
            
            return OptimizationResult(
                original_code=code,
                optimized_code=optimization.get('optimized_code', code),
                improvements=optimization.get('improvements', []),
                performance_gains=optimization.get('performance_gains', {}),
                confidence=optimization.get('confidence', 0.7)
            )
            
        except Exception as e:
            self.logger.error(f"Error optimizing code: {e}")
            # Return original code with error message
            return OptimizationResult(
                original_code=code,
                optimized_code=code,
                improvements=[f"Optimization failed due to error: {e}"],
                performance_gains={},
                confidence=0.3
            )
    
    def _create_optimization_prompt(self, code: str, language: str, 
                                   optimization_level: str) -> str:
        """Create prompt for code optimization."""
        level_descriptions = {
            "conservative": "Make minimal changes focusing on readability and maintainability",
            "balanced": "Balance performance improvements with code clarity",
            "aggressive": "Prioritize performance optimizations while maintaining functionality"
        }
        
        level_desc = level_descriptions.get(optimization_level, "balanced")
        
        return f"""Optimize this {language} code with {optimization_level} optimization level.

Original Code:
{code}

Optimization Level: {level_desc}

Please provide optimization in the following JSON format:
{{
    "optimized_code": "the optimized code here",
    "improvements": ["improvement1", "improvement2"],
    "performance_gains": {{
        "time_complexity": "improvement description",
        "space_complexity": "improvement description",
        "readability": "improvement description"
    }},
    "confidence": 0.0-1.0
}}

Focus on:
1. Performance improvements
2. Code readability
3. Maintainability
4. Best practices for {language}
5. Memory efficiency
6. Algorithm optimization

Provide only the JSON response with the optimized code and explanations."""
    
    def _parse_optimization_response(self, response: str) -> Dict[str, Any]:
        """Parse LLM optimization response."""
        try:
            import json
            # Try to extract JSON from response
            import re
            json_match = re.search(r'\{.*\}', response, re.DOTALL)
            if json_match:
                return json.loads(json_match.group())
            else:
                # Fallback parsing
                return self._fallback_optimization_parsing(response)
        except Exception as e:
            self.logger.warning(f"Failed to parse optimization response: {e}")
            return self._fallback_optimization_parsing(response)
    
    def _fallback_optimization_parsing(self, response: str) -> Dict[str, Any]:
        """Fallback parsing for optimization response."""
        return {
            'optimized_code': response,
            'improvements': ["Applied general optimizations"],
            'performance_gains': {
                'time_complexity': 'Unknown',
                'space_complexity': 'Unknown',
                'readability': 'Improved'
            },
            'confidence': 0.5
        } 