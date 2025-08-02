"""
Code Analyzer for LLM Agent

This module provides language-agnostic code analysis capabilities
for the LLM agent system.
"""

import logging
from typing import Dict, Any, List, Optional
from .agent import AnalysisResult


class CodeAnalyzer:
    """
    Language-agnostic code analyzer for the LLM agent.
    """
    
    def __init__(self, llm_agent):
        self.llm_agent = llm_agent
        self.logger = logging.getLogger(__name__)
    
    def analyze(self, source_code: str, target_code: str, 
                language_pair: str = "legacy-modern") -> AnalysisResult:
        """
        Analyze the quality and characteristics of code transformation.
        
        Args:
            source_code: Original source code
            target_code: Transformed target code
            language_pair: Source-target language pair
            
        Returns:
            Analysis result with scores and suggestions
        """
        try:
            # Create analysis prompt
            prompt = self._create_analysis_prompt(source_code, target_code, language_pair)
            
            # Call LLM for analysis
            messages = [
                {"role": "system", "content": "You are an expert code analyst specializing in code transformation quality assessment."},
                {"role": "user", "content": prompt}
            ]
            
            response = self.llm_agent.call_llm(messages)
            
            # Parse analysis results
            analysis = self._parse_analysis_response(response)
            
            return AnalysisResult(
                complexity_score=analysis.get('complexity_score', 0.5),
                maintainability_score=analysis.get('maintainability_score', 0.5),
                performance_issues=analysis.get('performance_issues', []),
                security_concerns=analysis.get('security_concerns', []),
                suggestions=analysis.get('suggestions', []),
                confidence=analysis.get('confidence', 0.7)
            )
            
        except Exception as e:
            self.logger.error(f"Error analyzing code: {e}")
            # Return default analysis result
            return AnalysisResult(
                complexity_score=0.5,
                maintainability_score=0.5,
                performance_issues=[],
                security_concerns=[],
                suggestions=["Analysis failed due to error"],
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
        try:
            prompt = self._create_documentation_prompt(code, language)
            
            messages = [
                {"role": "system", "content": "You are an expert technical writer specializing in code documentation."},
                {"role": "user", "content": prompt}
            ]
            
            return self.llm_agent.call_llm(messages)
            
        except Exception as e:
            self.logger.error(f"Error generating documentation: {e}")
            return f"# Documentation for {language} code\n\nDocumentation generation failed due to error: {e}"
    
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
        try:
            prompt = self._create_validation_prompt(source_code, target_code, language_pair)
            
            messages = [
                {"role": "system", "content": "You are an expert code validator specializing in transformation correctness."},
                {"role": "user", "content": prompt}
            ]
            
            response = self.llm_agent.call_llm(messages)
            
            # Parse validation results
            validation = self._parse_validation_response(response)
            
            return {
                'valid': validation.get('valid', False),
                'confidence': validation.get('confidence', 0.5),
                'issues': validation.get('issues', []),
                'missing_features': validation.get('missing_features', []),
                'suggestions': validation.get('suggestions', [])
            }
            
        except Exception as e:
            self.logger.error(f"Error validating transformation: {e}")
            return {
                'valid': False,
                'confidence': 0.3,
                'issues': [f"Validation failed due to error: {e}"],
                'missing_features': [],
                'suggestions': []
            }
    
    def _create_analysis_prompt(self, source_code: str, target_code: str, 
                               language_pair: str) -> str:
        """Create prompt for code analysis."""
        return f"""Analyze the quality of this code transformation from {language_pair}.

Source Code:
{source_code}

Target Code:
{target_code}

Please provide analysis in the following JSON format:
{{
    "complexity_score": 0.0-1.0,
    "maintainability_score": 0.0-1.0,
    "performance_issues": ["issue1", "issue2"],
    "security_concerns": ["concern1", "concern2"],
    "suggestions": ["suggestion1", "suggestion2"],
    "confidence": 0.0-1.0
}}

Focus on:
1. Code quality and readability
2. Performance implications
3. Security considerations
4. Maintainability aspects
5. Completeness of transformation"""
    
    def _create_documentation_prompt(self, code: str, language: str) -> str:
        """Create prompt for documentation generation."""
        return f"""Generate comprehensive documentation for this {language} code:

{code}

Please provide:
1. Function/class descriptions
2. Parameter explanations
3. Usage examples
4. Important notes or warnings
5. Dependencies and requirements

Format the documentation clearly and professionally."""
    
    def _create_validation_prompt(self, source_code: str, target_code: str, 
                                 language_pair: str) -> str:
        """Create prompt for transformation validation."""
        return f"""Validate this code transformation from {language_pair}.

Source Code:
{source_code}

Target Code:
{target_code}

Please provide validation in the following JSON format:
{{
    "valid": true/false,
    "confidence": 0.0-1.0,
    "issues": ["issue1", "issue2"],
    "missing_features": ["feature1", "feature2"],
    "suggestions": ["suggestion1", "suggestion2"]
}}

Check for:
1. Completeness of transformation
2. Functional equivalence
3. Missing features or logic
4. Syntax correctness
5. Best practices adherence"""
    
    def _parse_analysis_response(self, response: str) -> Dict[str, Any]:
        """Parse LLM analysis response."""
        try:
            import json
            # Try to extract JSON from response
            import re
            json_match = re.search(r'\{.*\}', response, re.DOTALL)
            if json_match:
                return json.loads(json_match.group())
            else:
                # Fallback parsing
                return self._fallback_analysis_parsing(response)
        except Exception as e:
            self.logger.warning(f"Failed to parse analysis response: {e}")
            return self._fallback_analysis_parsing(response)
    
    def _parse_validation_response(self, response: str) -> Dict[str, Any]:
        """Parse LLM validation response."""
        try:
            import json
            # Try to extract JSON from response
            import re
            json_match = re.search(r'\{.*\}', response, re.DOTALL)
            if json_match:
                return json.loads(json_match.group())
            else:
                # Fallback parsing
                return self._fallback_validation_parsing(response)
        except Exception as e:
            self.logger.warning(f"Failed to parse validation response: {e}")
            return self._fallback_validation_parsing(response)
    
    def _fallback_analysis_parsing(self, response: str) -> Dict[str, Any]:
        """Fallback parsing for analysis response."""
        return {
            'complexity_score': 0.5,
            'maintainability_score': 0.5,
            'performance_issues': [],
            'security_concerns': [],
            'suggestions': [response[:200] + "..." if len(response) > 200 else response],
            'confidence': 0.5
        }
    
    def _fallback_validation_parsing(self, response: str) -> Dict[str, Any]:
        """Fallback parsing for validation response."""
        return {
            'valid': True,
            'confidence': 0.5,
            'issues': [],
            'missing_features': [],
            'suggestions': [response[:200] + "..." if len(response) > 200 else response]
        } 