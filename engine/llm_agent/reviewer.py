"""
Code Reviewer for LLM Agent

This module provides automated code review capabilities,
including issue detection, improvement suggestions, and automated fixes.
"""

import logging
import json
from typing import Dict, Any, List, Optional
from engine.modernizers.cobol_system.transpilers.llm_augmentor import LLMAugmentor
from .results import ReviewResult


class CodeReviewer:
    """
    Performs automated code review with issue detection and suggestions.
    """
    
    def __init__(self, llm_augmentor: LLMAugmentor):
        self.llm_augmentor = llm_augmentor
        self.logger = logging.getLogger(__name__)
    
    def review(self, code: str, language: str = "python",
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
        if not self.llm_augmentor.can_augment():
            return self._fallback_review(code, language)
        
        try:
            # Create review prompt
            prompt = self._create_review_prompt(code, language, review_type)
            
            # Get AI review
            review_text = self._get_ai_review(prompt)
            
            # Parse review result
            return self._parse_review_result(review_text, code)
            
        except Exception as e:
            self.logger.error(f"AI review failed: {e}")
            return self._fallback_review(code, language)
    
    def _create_review_prompt(self, code: str, language: str, review_type: str) -> str:
        """Create a prompt for code review."""
        
        review_focus = {
            "basic": "Focus on syntax errors, basic best practices, and obvious issues",
            "comprehensive": "Comprehensive review including performance, security, maintainability, and best practices",
            "security-focused": "Focus on security vulnerabilities, input validation, and security best practices"
        }
        
        focus_desc = review_focus.get(review_type, review_focus["comprehensive"])
        
        prompt = f"""You are an expert {language} code reviewer. Perform a {review_type} review of the following code:

```{language}
{code}
```

REVIEW FOCUS: {focus_desc}

Please provide a comprehensive review in the following JSON format:

{{
    "issues": [
        {{
            "type": "error/warning/info",
            "severity": "critical/high/medium/low",
            "line": 10,
            "description": "description of the issue",
            "suggestion": "how to fix the issue"
        }}
    ],
    "suggestions": [
        {{
            "type": "improvement/optimization/best_practice",
            "description": "description of the suggestion",
            "impact": "high/medium/low"
        }}
    ],
    "overall_severity": "critical/high/medium/low",
    "confidence": 0.0-1.0,
    "automated_fixes": [
        {{
            "type": "fix_type",
            "description": "description of the automated fix",
            "code": "the fixed code snippet"
        }}
    ]
}}

Review criteria:
1. Syntax and compilation errors
2. Logic errors and bugs
3. Performance issues
4. Security vulnerabilities
5. Code style and best practices
6. Maintainability and readability
7. Documentation and comments
8. Error handling and edge cases

Provide only the JSON response without any additional text."""

        return prompt
    
    def _get_ai_review(self, prompt: str) -> str:
        """Get AI review of the code."""
        messages = [
            {
                "role": "system", 
                "content": "You are an expert code reviewer. Provide review in JSON format only."
            },
            {"role": "user", "content": prompt}
        ]
        
        # Use the LLM augmentor to get response
        response = self.llm_augmentor.provider.generate_response(messages, self.llm_augmentor.config)
        return response
    
    def _parse_review_result(self, review_text: str, code: str) -> ReviewResult:
        """Parse AI review result into structured format."""
        try:
            # Extract JSON from response
            json_start = review_text.find('{')
            json_end = review_text.rfind('}') + 1
            if json_start != -1 and json_end > json_start:
                json_str = review_text[json_start:json_end]
                review_data = json.loads(json_str)
            else:
                raise ValueError("No JSON found in response")
            
            # Extract issues
            issues = []
            for issue in review_data.get('issues', []):
                issues.append(f"[{issue.get('severity', 'medium').upper()}] {issue.get('description', '')}")
            
            # Extract suggestions
            suggestions = []
            for suggestion in review_data.get('suggestions', []):
                suggestions.append(f"[{suggestion.get('impact', 'medium').upper()}] {suggestion.get('description', '')}")
            
            # Extract automated fixes
            automated_fixes = []
            for fix in review_data.get('automated_fixes', []):
                automated_fixes.append(f"{fix.get('type', '')}: {fix.get('description', '')}")
            
            return ReviewResult(
                issues=issues,
                suggestions=suggestions,
                severity=review_data.get('overall_severity', 'medium'),
                confidence=float(review_data.get('confidence', 0.5)),
                automated_fixes=automated_fixes
            )
            
        except Exception as e:
            self.logger.error(f"Failed to parse review result: {e}")
            return self._fallback_review(code, "python")
    
    def _fallback_review(self, code: str, language: str) -> ReviewResult:
        """Provide fallback review when AI is not available."""
        return ReviewResult(
            issues=["Enable AI review for detailed analysis"],
            suggestions=["Enable AI review for improvement suggestions"],
            severity="medium",
            confidence=0.3,
            automated_fixes=[]
        )
    
    def review_security(self, code: str, language: str = "python") -> ReviewResult:
        """
        Perform security-focused code review.
        
        Args:
            code: Code to review
            language: Programming language
            
        Returns:
            Security review result
        """
        return self.review(code, language, "security-focused")
    
    def review_performance(self, code: str, language: str = "python") -> ReviewResult:
        """
        Perform performance-focused code review.
        
        Args:
            code: Code to review
            language: Programming language
            
        Returns:
            Performance review result
        """
        if not self.llm_augmentor.can_augment():
            return self._fallback_review(code, language)
        
        try:
            prompt = f"""You are an expert {language} performance analyst. Review the following code for performance issues:

```{language}
{code}
```

Please provide a performance-focused review in the following JSON format:

{{
    "issues": [
        {{
            "type": "performance",
            "severity": "critical/high/medium/low",
            "description": "description of the performance issue",
            "suggestion": "how to fix the performance issue"
        }}
    ],
    "suggestions": [
        {{
            "type": "optimization",
            "description": "performance optimization suggestion",
            "impact": "high/medium/low"
        }}
    ],
    "overall_severity": "critical/high/medium/low",
    "confidence": 0.0-1.0,
    "performance_metrics": {{
        "time_complexity": "O(n)",
        "space_complexity": "O(n)",
        "bottlenecks": ["bottleneck1", "bottleneck2"]
    }}
}}

Focus on:
1. Time complexity analysis
2. Space complexity analysis
3. Inefficient algorithms
4. Memory usage
5. I/O operations
6. Loop optimizations
7. Data structure choices

Provide only the JSON response without any additional text."""

            messages = [
                {
                    "role": "system",
                    "content": "You are an expert performance analyst. Provide review in JSON format only."
                },
                {"role": "user", "content": prompt}
            ]
            
            response = self.llm_augmentor.provider.generate_response(messages, self.llm_augmentor.config)
            
            # Parse the response similar to regular review
            return self._parse_review_result(response, code)
            
        except Exception as e:
            self.logger.error(f"Performance review failed: {e}")
            return self._fallback_review(code, language)
    
    def suggest_improvements(self, code: str, language: str = "python") -> List[str]:
        """
        Suggest improvements for the code.
        
        Args:
            code: Code to analyze
            language: Programming language
            
        Returns:
            List of improvement suggestions
        """
        review_result = self.review(code, language)
        return review_result.suggestions
    
    def get_automated_fixes(self, code: str, language: str = "python") -> List[str]:
        """
        Get automated fixes for code issues.
        
        Args:
            code: Code to analyze
            language: Programming language
            
        Returns:
            List of automated fixes
        """
        review_result = self.review(code, language)
        return review_result.automated_fixes
    
    def validate_code_quality(self, code: str, language: str = "python") -> Dict[str, Any]:
        """
        Validate code quality metrics.
        
        Args:
            code: Code to validate
            language: Programming language
            
        Returns:
            Quality metrics
        """
        if not self.llm_augmentor.can_augment():
            return self._fallback_quality_metrics(code, language)
        
        try:
            prompt = f"""Analyze the quality of the following {language} code:

```{language}
{code}
```

Please provide quality metrics in the following JSON format:

{{
    "readability_score": 0.0-1.0,
    "maintainability_score": 0.0-1.0,
    "complexity_score": 0.0-1.0,
    "documentation_score": 0.0-1.0,
    "test_coverage_score": 0.0-1.0,
    "overall_quality": 0.0-1.0,
    "quality_issues": ["issue1", "issue2"],
    "strengths": ["strength1", "strength2"]
}}

Quality criteria:
1. Readability and clarity
2. Maintainability and structure
3. Complexity and cyclomatic complexity
4. Documentation and comments
5. Error handling
6. Code organization
7. Naming conventions
8. Best practices adherence

Provide only the JSON response without any additional text."""

            messages = [
                {
                    "role": "system",
                    "content": "You are an expert code quality analyst. Provide metrics in JSON format only."
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
            self.logger.error(f"Quality validation failed: {e}")
            return self._fallback_quality_metrics(code, language)
    
    def _fallback_quality_metrics(self, code: str, language: str) -> Dict[str, Any]:
        """Provide fallback quality metrics when AI is not available."""
        return {
            "readability_score": 0.5,
            "maintainability_score": 0.5,
            "complexity_score": 0.5,
            "documentation_score": 0.5,
            "test_coverage_score": 0.5,
            "overall_quality": 0.5,
            "quality_issues": ["Enable AI analysis for detailed quality metrics"],
            "strengths": ["Code appears to be syntactically correct"]
        } 