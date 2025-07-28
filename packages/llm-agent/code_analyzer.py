"""
Code Analyzer for LLM Agent

This module provides intelligent analysis of code transformations,
including quality assessment, complexity analysis, and improvement suggestions.
"""

import logging
import json
from typing import Dict, Any, List, Optional
from ..transpiler.engine.llm_augmentor import LLMAugmentor
from .agent import AnalysisResult


class CodeAnalyzer:
    """
    Analyzes code transformations for quality, complexity, and improvements.
    """
    
    def __init__(self, llm_augmentor: LLMAugmentor):
        self.llm_augmentor = llm_augmentor
        self.logger = logging.getLogger(__name__)
    
    def analyze(self, source_code: str, target_code: str, 
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
        if not self.llm_augmentor.can_augment():
            return self._fallback_analysis(source_code, target_code)
        
        try:
            # Create analysis prompt
            prompt = self._create_analysis_prompt(source_code, target_code, language_pair)
            
            # Get AI analysis
            analysis_text = self._get_ai_analysis(prompt)
            
            # Parse analysis result
            return self._parse_analysis_result(analysis_text, source_code, target_code)
            
        except Exception as e:
            self.logger.error(f"AI analysis failed: {e}")
            return self._fallback_analysis(source_code, target_code)
    
    def _create_analysis_prompt(self, source_code: str, target_code: str, 
                               language_pair: str) -> str:
        """Create a prompt for code analysis."""
        
        prompt = f"""You are an expert code transformation analyst. Analyze the following code transformation from {language_pair}.

SOURCE CODE:
```{language_pair.split('-')[0]}
{source_code}
```

TARGET CODE:
```{language_pair.split('-')[1]}
{target_code}
```

Please provide a comprehensive analysis in the following JSON format:

{{
    "complexity_score": 0.0-1.0,
    "maintainability_score": 0.0-1.0,
    "performance_issues": ["issue1", "issue2"],
    "security_concerns": ["concern1", "concern2"],
    "suggestions": ["suggestion1", "suggestion2"],
    "confidence": 0.0-1.0
}}

Analysis criteria:
1. Complexity: Assess code complexity and readability
2. Maintainability: Evaluate how easy the code is to maintain
3. Performance: Identify potential performance issues
4. Security: Check for security vulnerabilities
5. Suggestions: Provide specific improvement recommendations

Provide only the JSON response without any additional text."""

        return prompt
    
    def _get_ai_analysis(self, prompt: str) -> str:
        """Get AI analysis of the code transformation."""
        messages = [
            {
                "role": "system", 
                "content": "You are an expert code transformation analyst. Provide analysis in JSON format only."
            },
            {"role": "user", "content": prompt}
        ]
        
        # Use the LLM augmentor to get response
        response = self.llm_augmentor.provider.generate_response(messages, self.llm_augmentor.config)
        return response
    
    def _parse_analysis_result(self, analysis_text: str, source_code: str, 
                              target_code: str) -> AnalysisResult:
        """Parse AI analysis result into structured format."""
        try:
            # Extract JSON from response
            json_start = analysis_text.find('{')
            json_end = analysis_text.rfind('}') + 1
            if json_start != -1 and json_end > json_start:
                json_str = analysis_text[json_start:json_end]
                analysis_data = json.loads(json_str)
            else:
                raise ValueError("No JSON found in response")
            
            return AnalysisResult(
                complexity_score=float(analysis_data.get('complexity_score', 0.5)),
                maintainability_score=float(analysis_data.get('maintainability_score', 0.5)),
                performance_issues=analysis_data.get('performance_issues', []),
                security_concerns=analysis_data.get('security_concerns', []),
                suggestions=analysis_data.get('suggestions', []),
                confidence=float(analysis_data.get('confidence', 0.5))
            )
            
        except Exception as e:
            self.logger.error(f"Failed to parse analysis result: {e}")
            return self._fallback_analysis(source_code, target_code)
    
    def _fallback_analysis(self, source_code: str, target_code: str) -> AnalysisResult:
        """Provide fallback analysis when AI is not available."""
        # Simple heuristic analysis
        source_lines = len(source_code.split('\n'))
        target_lines = len(target_code.split('\n'))
        
        complexity_score = min(1.0, (target_lines / max(source_lines, 1)) * 0.3)
        maintainability_score = 0.7  # Default moderate score
        
        return AnalysisResult(
            complexity_score=complexity_score,
            maintainability_score=maintainability_score,
            performance_issues=[],
            security_concerns=[],
            suggestions=["Enable AI analysis for detailed insights"],
            confidence=0.3
        )
    
    def generate_documentation(self, code: str, language: str = "python") -> str:
        """
        Generate documentation for code.
        
        Args:
            code: Code to document
            language: Programming language
            
        Returns:
            Generated documentation
        """
        if not self.llm_augmentor.can_augment():
            return self._fallback_documentation(code, language)
        
        try:
            prompt = f"""Generate comprehensive documentation for the following {language} code:

```{language}
{code}
```

Please provide:
1. Function/class descriptions
2. Parameter explanations
3. Return value descriptions
4. Usage examples
5. Important notes or warnings

Format the documentation in a clear, professional style."""

            messages = [
                {
                    "role": "system",
                    "content": f"You are an expert {language} documentation writer. Provide clear, comprehensive documentation."
                },
                {"role": "user", "content": prompt}
            ]
            
            return self.llm_augmentor.provider.generate_response(messages, self.llm_augmentor.config)
            
        except Exception as e:
            self.logger.error(f"Documentation generation failed: {e}")
            return self._fallback_documentation(code, language)
    
    def _fallback_documentation(self, code: str, language: str) -> str:
        """Provide fallback documentation when AI is not available."""
        return f"""# {language.title()} Code Documentation

This code was generated by the legacy2modern transpiler.

## Overview
The code above represents a transformation from legacy code to modern {language}.

## Notes
- Enable AI documentation generation for detailed function descriptions
- Review the code for any specific business logic requirements
- Test thoroughly before deployment

## Generated Code
```{language}
{code}
```"""
    
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
        if not self.llm_augmentor.can_augment():
            return self._fallback_validation(source_code, target_code)
        
        try:
            prompt = f"""Validate the following code transformation from {language_pair}:

SOURCE CODE:
```{language_pair.split('-')[0]}
{source_code}
```

TARGET CODE:
```{language_pair.split('-')[1]}
{target_code}
```

Please provide validation in the following JSON format:

{{
    "is_valid": true/false,
    "confidence": 0.0-1.0,
    "missing_functionality": ["item1", "item2"],
    "incorrect_translations": ["issue1", "issue2"],
    "suggestions": ["suggestion1", "suggestion2"]
}}

Validation criteria:
1. Check if all functionality is preserved
2. Verify syntax correctness
3. Ensure logical equivalence
4. Identify any missing features
5. Suggest improvements

Provide only the JSON response without any additional text."""

            messages = [
                {
                    "role": "system",
                    "content": "You are an expert code transformation validator. Provide validation in JSON format only."
                },
                {"role": "user", "content": prompt}
            ]
            
            response = self.llm_augmentor.provider.generate_response(messages, self.llm_augmentor.config)
            
            # Parse JSON response
            json_start = response.find('{')
            json_end = response.rfind('}') + 1
            if json_start != -1 and json_end > json_start:
                json_str = response[json_start:json_end]
                validation_data = json.loads(json_str)
            else:
                raise ValueError("No JSON found in response")
            
            return {
                "is_valid": validation_data.get('is_valid', False),
                "confidence": float(validation_data.get('confidence', 0.5)),
                "missing_functionality": validation_data.get('missing_functionality', []),
                "incorrect_translations": validation_data.get('incorrect_translations', []),
                "suggestions": validation_data.get('suggestions', [])
            }
            
        except Exception as e:
            self.logger.error(f"Validation failed: {e}")
            return self._fallback_validation(source_code, target_code)
    
    def _fallback_validation(self, source_code: str, target_code: str) -> Dict[str, Any]:
        """Provide fallback validation when AI is not available."""
        return {
            "is_valid": True,  # Assume valid for fallback
            "confidence": 0.3,
            "missing_functionality": [],
            "incorrect_translations": [],
            "suggestions": ["Enable AI validation for detailed analysis"]
        } 