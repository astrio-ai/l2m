"""
Code Reviewer for LLM Agent

This module provides language-agnostic code review capabilities
for the LLM agent system.
"""

import logging
from typing import Dict, Any, List, Optional
from .agent import ReviewResult


class CodeReviewer:
    """
    Language-agnostic code reviewer for the LLM agent.
    """
    
    def __init__(self, llm_agent):
        self.llm_agent = llm_agent
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
        try:
            # Create review prompt
            prompt = self._create_review_prompt(code, language, review_type)
            
            # Call LLM for review
            messages = [
                {"role": "system", "content": "You are an expert code reviewer specializing in code quality and best practices."},
                {"role": "user", "content": prompt}
            ]
            
            response = self.llm_agent.call_llm(messages)
            
            # Parse review results
            review = self._parse_review_response(response)
            
            return ReviewResult(
                issues=review.get('issues', []),
                suggestions=review.get('suggestions', []),
                severity=review.get('severity', 'medium'),
                confidence=review.get('confidence', 0.7),
                automated_fixes=review.get('automated_fixes', [])
            )
            
        except Exception as e:
            self.logger.error(f"Error reviewing code: {e}")
            # Return default review result
            return ReviewResult(
                issues=[f"Review failed due to error: {e}"],
                suggestions=[],
                severity='low',
                confidence=0.3,
                automated_fixes=[]
            )
    
    def _create_review_prompt(self, code: str, language: str, review_type: str) -> str:
        """Create prompt for code review."""
        review_focus = {
            "basic": "Focus on syntax errors, basic best practices, and obvious issues",
            "comprehensive": "Comprehensive review including performance, security, maintainability, and best practices",
            "security-focused": "Focus on security vulnerabilities, input validation, and security best practices"
        }
        
        focus_desc = review_focus.get(review_type, "comprehensive")
        
        return f"""Review this {language} code with {review_type} review type.

Code:
{code}

Review Focus: {focus_desc}

Please provide review in the following JSON format:
{{
    "issues": [
        {{
            "type": "error/warning/info",
            "severity": "critical/high/medium/low",
            "description": "description of the issue",
            "line": "line number or section"
        }}
    ],
    "suggestions": [
        {{
            "type": "improvement/optimization/best_practice",
            "description": "description of the suggestion",
            "priority": "high/medium/low"
        }}
    ],
    "severity": "critical/high/medium/low",
    "confidence": 0.0-1.0,
    "automated_fixes": [
        {{
            "description": "description of the fix",
            "code": "the fixed code snippet"
        }}
    ]
}}

Review criteria:
1. Code quality and readability
2. Performance considerations
3. Security vulnerabilities
4. Best practices adherence
5. Maintainability
6. Error handling
7. Documentation quality"""
    
    def _parse_review_response(self, response: str) -> Dict[str, Any]:
        """Parse LLM review response."""
        try:
            import json
            # Try to extract JSON from response
            import re
            json_match = re.search(r'\{.*\}', response, re.DOTALL)
            if json_match:
                return json.loads(json_match.group())
            else:
                # Fallback parsing
                return self._fallback_review_parsing(response)
        except Exception as e:
            self.logger.warning(f"Failed to parse review response: {e}")
            return self._fallback_review_parsing(response)
    
    def _fallback_review_parsing(self, response: str) -> Dict[str, Any]:
        """Fallback parsing for review response."""
        return {
            'issues': [response[:200] + "..." if len(response) > 200 else response],
            'suggestions': [],
            'severity': 'medium',
            'confidence': 0.5,
            'automated_fixes': []
        } 