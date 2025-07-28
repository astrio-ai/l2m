"""
Code Optimizer for LLM Agent

This module provides intelligent code optimization capabilities,
including performance improvements, readability enhancements, and maintainability optimizations.
"""

import logging
import json
from typing import Dict, Any, List, Optional
from ..transpiler.engine.llm_augmentor import LLMAugmentor
from .agent import OptimizationResult


class CodeOptimizer:
    """
    Optimizes code for performance, readability, and maintainability.
    """
    
    def __init__(self, llm_augmentor: LLMAugmentor):
        self.llm_augmentor = llm_augmentor
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
        if not self.llm_augmentor.can_augment():
            return self._fallback_optimization(code, language)
        
        try:
            # Create optimization prompt
            prompt = self._create_optimization_prompt(code, language, optimization_level)
            
            # Get AI optimization
            optimization_text = self._get_ai_optimization(prompt)
            
            # Parse optimization result
            return self._parse_optimization_result(optimization_text, code)
            
        except Exception as e:
            self.logger.error(f"AI optimization failed: {e}")
            return self._fallback_optimization(code, language)
    
    def _create_optimization_prompt(self, code: str, language: str, 
                                   optimization_level: str) -> str:
        """Create a prompt for code optimization."""
        
        level_descriptions = {
            "conservative": "Make minimal changes focused on readability and maintainability",
            "balanced": "Balance performance improvements with code clarity",
            "aggressive": "Maximize performance improvements while maintaining functionality"
        }
        
        level_desc = level_descriptions.get(optimization_level, level_descriptions["balanced"])
        
        prompt = f"""You are an expert {language} code optimizer. Optimize the following code with a {optimization_level} approach.

ORIGINAL CODE:
```{language}
{code}
```

OPTIMIZATION LEVEL: {optimization_level}
APPROACH: {level_desc}

Please provide the optimized code and analysis in the following JSON format:

{{
    "optimized_code": "the optimized code here",
    "improvements": ["improvement1", "improvement2"],
    "performance_gains": {{
        "time_complexity": "O(n) -> O(log n)",
        "space_complexity": "O(n) -> O(1)",
        "readability": "improved/unchanged/degraded"
    }},
    "confidence": 0.0-1.0
}}

Optimization guidelines:
1. Maintain functionality and correctness
2. Improve performance where possible
3. Enhance readability and maintainability
4. Follow {language} best practices
5. Add appropriate comments for complex optimizations

Provide only the JSON response without any additional text."""

        return prompt
    
    def _get_ai_optimization(self, prompt: str) -> str:
        """Get AI optimization of the code."""
        messages = [
            {
                "role": "system", 
                "content": "You are an expert code optimizer. Provide optimization in JSON format only."
            },
            {"role": "user", "content": prompt}
        ]
        
        # Use the LLM augmentor to get response
        response = self.llm_augmentor.provider.generate_response(messages, self.llm_augmentor.config)
        return response
    
    def _parse_optimization_result(self, optimization_text: str, original_code: str) -> OptimizationResult:
        """Parse AI optimization result into structured format."""
        try:
            # Extract JSON from response
            json_start = optimization_text.find('{')
            json_end = optimization_text.rfind('}') + 1
            if json_start != -1 and json_end > json_start:
                json_str = optimization_text[json_start:json_end]
                optimization_data = json.loads(json_str)
            else:
                raise ValueError("No JSON found in response")
            
            return OptimizationResult(
                original_code=original_code,
                optimized_code=optimization_data.get('optimized_code', original_code),
                improvements=optimization_data.get('improvements', []),
                performance_gains=optimization_data.get('performance_gains', {}),
                confidence=float(optimization_data.get('confidence', 0.5))
            )
            
        except Exception as e:
            self.logger.error(f"Failed to parse optimization result: {e}")
            return self._fallback_optimization(original_code, "python")
    
    def _fallback_optimization(self, code: str, language: str) -> OptimizationResult:
        """Provide fallback optimization when AI is not available."""
        return OptimizationResult(
            original_code=code,
            optimized_code=code,  # No changes in fallback
            improvements=["Enable AI optimization for performance improvements"],
            performance_gains={},
            confidence=0.3
        )
    
    def optimize_for_performance(self, code: str, language: str = "python") -> OptimizationResult:
        """
        Optimize code specifically for performance.
        
        Args:
            code: Code to optimize
            language: Programming language
            
        Returns:
            Optimization result focused on performance
        """
        return self.optimize(code, language, "aggressive")
    
    def optimize_for_readability(self, code: str, language: str = "python") -> OptimizationResult:
        """
        Optimize code specifically for readability.
        
        Args:
            code: Code to optimize
            language: Programming language
            
        Returns:
            Optimization result focused on readability
        """
        return self.optimize(code, language, "conservative")
    
    def suggest_optimizations(self, code: str, language: str = "python") -> List[str]:
        """
        Suggest optimizations without applying them.
        
        Args:
            code: Code to analyze
            language: Programming language
            
        Returns:
            List of optimization suggestions
        """
        if not self.llm_augmentor.can_augment():
            return ["Enable AI optimization for detailed suggestions"]
        
        try:
            prompt = f"""Analyze the following {language} code and suggest optimizations:

```{language}
{code}
```

Please provide optimization suggestions in the following JSON format:

{{
    "suggestions": [
        {{
            "type": "performance/readability/maintainability",
            "description": "description of the optimization",
            "impact": "high/medium/low",
            "effort": "high/medium/low"
        }}
    ]
}}

Focus on:
1. Performance improvements
2. Readability enhancements
3. Maintainability improvements
4. Best practices
5. Code quality

Provide only the JSON response without any additional text."""

            messages = [
                {
                    "role": "system",
                    "content": "You are an expert code optimization analyst. Provide suggestions in JSON format only."
                },
                {"role": "user", "content": prompt}
            ]
            
            response = self.llm_augmentor.provider.generate_response(messages, self.llm_augmentor.config)
            
            # Parse JSON response
            json_start = response.find('{')
            json_end = response.rfind('}') + 1
            if json_start != -1 and json_end > json_start:
                json_str = response[json_start:json_end]
                suggestions_data = json.loads(json_str)
                
                suggestions = []
                for suggestion in suggestions_data.get('suggestions', []):
                    desc = suggestion.get('description', '')
                    impact = suggestion.get('impact', 'medium')
                    effort = suggestion.get('effort', 'medium')
                    suggestions.append(f"[{impact.upper()}/{effort.upper()}] {desc}")
                
                return suggestions
            else:
                raise ValueError("No JSON found in response")
                
        except Exception as e:
            self.logger.error(f"Optimization suggestions failed: {e}")
            return ["Enable AI optimization for detailed suggestions"]
    
    def benchmark_optimization(self, original_code: str, optimized_code: str,
                             language: str = "python") -> Dict[str, Any]:
        """
        Benchmark the performance difference between original and optimized code.
        
        Args:
            original_code: Original code
            optimized_code: Optimized code
            language: Programming language
            
        Returns:
            Benchmark results
        """
        if not self.llm_augmentor.can_augment():
            return self._fallback_benchmark(original_code, optimized_code)
        
        try:
            prompt = f"""Analyze the performance characteristics of these two {language} code versions:

ORIGINAL CODE:
```{language}
{original_code}
```

OPTIMIZED CODE:
```{language}
{optimized_code}
```

Please provide a performance analysis in the following JSON format:

{{
    "time_complexity_original": "O(n)",
    "time_complexity_optimized": "O(log n)",
    "space_complexity_original": "O(n)",
    "space_complexity_optimized": "O(1)",
    "estimated_performance_improvement": "50%",
    "bottlenecks_removed": ["bottleneck1", "bottleneck2"],
    "trade_offs": ["trade_off1", "trade_off2"]
}}

Provide only the JSON response without any additional text."""

            messages = [
                {
                    "role": "system",
                    "content": "You are an expert performance analyst. Provide analysis in JSON format only."
                },
                {"role": "user", "content": prompt}
            ]
            
            response = self.llm_augmentor.provider.generate_response(messages, self.llm_augmentor.config)
            
            # Parse JSON response
            json_start = response.find('{')
            json_end = response.rfind('}') + 1
            if json_start != -1 and json_end > json_start:
                json_str = response[json_start:json_end]
                return json.loads(json_str)
            else:
                raise ValueError("No JSON found in response")
                
        except Exception as e:
            self.logger.error(f"Benchmark analysis failed: {e}")
            return self._fallback_benchmark(original_code, optimized_code)
    
    def _fallback_benchmark(self, original_code: str, optimized_code: str) -> Dict[str, Any]:
        """Provide fallback benchmark when AI is not available."""
        return {
            "time_complexity_original": "Unknown",
            "time_complexity_optimized": "Unknown",
            "space_complexity_original": "Unknown",
            "space_complexity_optimized": "Unknown",
            "estimated_performance_improvement": "Unknown",
            "bottlenecks_removed": [],
            "trade_offs": ["Enable AI analysis for detailed performance metrics"]
        } 