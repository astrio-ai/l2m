"""
Code Analysis Agent

This module provides advanced code analysis capabilities including
quality assessment, transformation validation, and documentation generation.
"""

import logging
import json
import re
from typing import Dict, Any, List, Optional
from dataclasses import dataclass
from datetime import datetime

from .agent import LLMAgent
from ..functionality_mapper import ValidationStrategy, TestType, TestCase, ValidationResult


@dataclass
class AnalysisResult:
    """Comprehensive code analysis result."""
    complexity_score: float
    maintainability_score: float
    performance_score: float
    security_score: float
    quality_score: float
    issues: List[str]
    warnings: List[str]
    suggestions: List[str]
    confidence: float
    timestamp: datetime
    metrics: Dict[str, Any]


class CodeAnalyzer:
    """
    Advanced code analyzer with comprehensive validation capabilities.
    
    Provides detailed analysis of code transformations, quality assessment,
    and integration with the enhanced validation system.
    """
    
    def __init__(self, llm_agent: LLMAgent):
        self.llm_agent = llm_agent
        self.logger = logging.getLogger(__name__)
    
    def analyze(self, source_code: str, target_code: str, 
                language_pair: str = "legacy-modern") -> AnalysisResult:
        """
        Perform comprehensive code analysis.
        
        Args:
            source_code: Original source code
            target_code: Transformed target code
            language_pair: Source-target language pair
            
        Returns:
            Comprehensive analysis result
        """
        try:
            prompt = self._create_analysis_prompt(source_code, target_code, language_pair)
            
            messages = [
                {"role": "system", "content": "You are an expert code analyst specializing in transformation quality assessment."},
                {"role": "user", "content": prompt}
            ]
            
            response = self.llm_agent.call_llm(messages)
            
            # Parse analysis results
            analysis = self._parse_analysis_response(response)
            
            # Calculate overall quality score
            quality_score = self._calculate_quality_score(analysis)
            
            return AnalysisResult(
                complexity_score=analysis.get('complexity_score', 0.5),
                maintainability_score=analysis.get('maintainability_score', 0.5),
                performance_score=analysis.get('performance_score', 0.5),
                security_score=analysis.get('security_score', 0.5),
                quality_score=quality_score,
                issues=analysis.get('issues', []),
                warnings=analysis.get('warnings', []),
                suggestions=analysis.get('suggestions', []),
                confidence=analysis.get('confidence', 0.5),
                timestamp=datetime.now(),
                metrics=analysis.get('metrics', {})
            )
            
        except Exception as e:
            self.logger.error(f"Error analyzing code: {e}")
            return AnalysisResult(
                complexity_score=0.3,
                maintainability_score=0.3,
                performance_score=0.3,
                security_score=0.3,
                quality_score=0.3,
                issues=[f"Analysis failed due to error: {e}"],
                warnings=[],
                suggestions=[],
                confidence=0.3,
                timestamp=datetime.now(),
                metrics={}
            )
    
    def generate_documentation(self, code: str, language: str = "python") -> str:
        """
        Generate comprehensive documentation for code.
        
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
            
            response = self.llm_agent.call_llm(messages)
            return response
            
        except Exception as e:
            self.logger.error(f"Error generating documentation: {e}")
            return f"Documentation generation failed: {e}"
    
    def validate_transformation(self, source_code: str, target_code: str,
                              language_pair: str = "legacy-modern") -> Dict[str, Any]:
        """
        Validate code transformation using advanced validation strategies.
        
        Args:
            source_code: Original source code
            target_code: Transformed target code
            language_pair: Source-target language pair
            
        Returns:
            Validation result with confidence and issues
        """
        try:
            # Perform comprehensive validation using multiple strategies
            validation_results = {}
            
            # Syntax validation
            syntax_result = self._validate_syntax(source_code, target_code, language_pair)
            validation_results['syntax_check'] = syntax_result
            
            # Semantic validation
            semantic_result = self._validate_semantics(source_code, target_code, language_pair)
            validation_results['semantic_analysis'] = semantic_result
            
            # Security validation
            security_result = self._validate_security(source_code, target_code, language_pair)
            validation_results['security_audit'] = security_result
            
            # Code quality validation
            quality_result = self._validate_code_quality(source_code, target_code, language_pair)
            validation_results['code_quality'] = quality_result
            
            # Calculate overall confidence
            confidence_score = self._calculate_validation_confidence(validation_results)
            
            return {
                'valid': confidence_score >= 0.7,
                'confidence': confidence_score,
                'validation_results': validation_results,
                'issues': self._collect_all_issues(validation_results),
                'warnings': self._collect_all_warnings(validation_results),
                'suggestions': self._collect_all_suggestions(validation_results)
            }
            
        except Exception as e:
            self.logger.error(f"Error validating transformation: {e}")
            return {
                'valid': False,
                'confidence': 0.3,
                'validation_results': {},
                'issues': [f"Validation failed due to error: {e}"],
                'warnings': [],
                'suggestions': []
            }
    
    def generate_test_cases(self, source_code: str, target_code: str,
                           language_pair: str = "legacy-modern") -> List[TestCase]:
        """
        Generate comprehensive test cases for code transformation.
        
        Args:
            source_code: Original source code
            target_code: Transformed target code
            language_pair: Source-target language pair
            
        Returns:
            List of test cases
        """
        try:
            prompt = self._create_test_generation_prompt(source_code, target_code, language_pair)
            
            messages = [
                {"role": "system", "content": "You are an expert test engineer specializing in transformation testing."},
                {"role": "user", "content": prompt}
            ]
            
            response = self.llm_agent.call_llm(messages)
            test_cases = self._parse_test_cases_response(response)
            
            return test_cases
            
        except Exception as e:
            self.logger.error(f"Error generating test cases: {e}")
            return []
    
    def _validate_syntax(self, source_code: str, target_code: str, language_pair: str) -> ValidationResult:
        """Validate syntax correctness."""
        try:
            prompt = f"""Validate the syntax of this {language_pair} transformation.

Source Code:
{source_code}

Target Code:
{target_code}

Focus on syntax correctness, proper structure, and language-specific requirements."""

            messages = [
                {"role": "system", "content": "You are a syntax validation expert."},
                {"role": "user", "content": prompt}
            ]
            
            response = self.llm_agent.call_llm(messages)
            result = self._parse_validation_response(response)
            
            return ValidationResult(
                strategy=ValidationStrategy.SYNTAX_CHECK,
                passed=result.get('valid', False),
                score=result.get('confidence', 0.5),
                issues=result.get('issues', []),
                warnings=result.get('warnings', []),
                metrics=result.get('metrics', {})
            )
            
        except Exception as e:
            self.logger.error(f"Syntax validation failed: {e}")
            return ValidationResult(
                strategy=ValidationStrategy.SYNTAX_CHECK,
                passed=False,
                score=0.0,
                issues=[f"Syntax validation failed: {e}"],
                warnings=[]
            )
    
    def _validate_semantics(self, source_code: str, target_code: str, language_pair: str) -> ValidationResult:
        """Validate semantic equivalence."""
        try:
            prompt = f"""Validate the semantic equivalence of this {language_pair} transformation.

Source Code:
{source_code}

Target Code:
{target_code}

Focus on preserving business logic, data flow, and functional behavior."""

            messages = [
                {"role": "system", "content": "You are a semantic analysis expert."},
                {"role": "user", "content": prompt}
            ]
            
            response = self.llm_agent.call_llm(messages)
            result = self._parse_validation_response(response)
            
            return ValidationResult(
                strategy=ValidationStrategy.SEMANTIC_ANALYSIS,
                passed=result.get('valid', False),
                score=result.get('confidence', 0.5),
                issues=result.get('issues', []),
                warnings=result.get('warnings', []),
                metrics=result.get('metrics', {})
            )
            
        except Exception as e:
            self.logger.error(f"Semantic validation failed: {e}")
            return ValidationResult(
                strategy=ValidationStrategy.SEMANTIC_ANALYSIS,
                passed=False,
                score=0.0,
                issues=[f"Semantic validation failed: {e}"],
                warnings=[]
            )
    
    def _validate_security(self, source_code: str, target_code: str, language_pair: str) -> ValidationResult:
        """Validate security aspects."""
        try:
            prompt = f"""Analyze the security aspects of this {language_pair} transformation.

Source Code:
{source_code}

Target Code:
{target_code}

Focus on security vulnerabilities, input validation, and security best practices."""

            messages = [
                {"role": "system", "content": "You are a security analysis expert."},
                {"role": "user", "content": prompt}
            ]
            
            response = self.llm_agent.call_llm(messages)
            result = self._parse_validation_response(response)
            
            return ValidationResult(
                strategy=ValidationStrategy.SECURITY_AUDIT,
                passed=result.get('valid', False),
                score=result.get('confidence', 0.5),
                issues=result.get('issues', []),
                warnings=result.get('warnings', []),
                metrics=result.get('metrics', {})
            )
            
        except Exception as e:
            self.logger.error(f"Security validation failed: {e}")
            return ValidationResult(
                strategy=ValidationStrategy.SECURITY_AUDIT,
                passed=False,
                score=0.0,
                issues=[f"Security validation failed: {e}"],
                warnings=[]
            )
    
    def _validate_code_quality(self, source_code: str, target_code: str, language_pair: str) -> ValidationResult:
        """Validate code quality metrics."""
        try:
            prompt = f"""Analyze the code quality of this {language_pair} transformation.

Source Code:
{source_code}

Target Code:
{target_code}

Focus on readability, maintainability, complexity, and best practices."""

            messages = [
                {"role": "system", "content": "You are a code quality expert."},
                {"role": "user", "content": prompt}
            ]
            
            response = self.llm_agent.call_llm(messages)
            result = self._parse_validation_response(response)
            
            return ValidationResult(
                strategy=ValidationStrategy.CODE_QUALITY,
                passed=result.get('valid', False),
                score=result.get('confidence', 0.5),
                issues=result.get('issues', []),
                warnings=result.get('warnings', []),
                metrics=result.get('metrics', {})
            )
            
        except Exception as e:
            self.logger.error(f"Code quality validation failed: {e}")
            return ValidationResult(
                strategy=ValidationStrategy.CODE_QUALITY,
                passed=False,
                score=0.0,
                issues=[f"Code quality validation failed: {e}"],
                warnings=[]
            )
    
    def _create_analysis_prompt(self, source_code: str, target_code: str, 
                               language_pair: str) -> str:
        """Create prompt for comprehensive code analysis."""
        return f"""Analyze the quality of this code transformation from {language_pair}.

Source Code:
{source_code}

Target Code:
{target_code}

Please provide analysis in the following JSON format:
{{
    "complexity_score": 0.0-1.0,
    "maintainability_score": 0.0-1.0,
    "performance_score": 0.0-1.0,
    "security_score": 0.0-1.0,
    "issues": ["issue1", "issue2"],
    "warnings": ["warning1", "warning2"],
    "suggestions": ["suggestion1", "suggestion2"],
    "confidence": 0.0-1.0,
    "metrics": {{
        "cyclomatic_complexity": 1-10,
        "lines_of_code": 100,
        "function_count": 5
    }}
}}

Focus on:
1. Code quality and readability
2. Performance implications
3. Security considerations
4. Maintainability aspects
5. Completeness of transformation
6. Best practices adherence"""
    
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
6. Performance considerations
7. Security considerations

Format the documentation clearly and professionally."""
    
    def _create_test_generation_prompt(self, source_code: str, target_code: str, 
                                      language_pair: str) -> str:
        """Create prompt for test case generation."""
        return f"""Generate comprehensive test cases for this {language_pair} transformation.

Source Code:
{source_code}

Target Code:
{target_code}

Please provide test cases in the following JSON format:
{{
    "test_cases": [
        {{
            "test_id": "test_1",
            "test_type": "unit_test",
            "name": "Basic Functionality Test",
            "description": "Test basic functionality",
            "inputs": {{"param1": "value1"}},
            "expected_outputs": {{"result": "expected_value"}},
            "priority": "high"
        }},
        {{
            "test_id": "test_2",
            "test_type": "edge_case_test",
            "name": "Edge Case Test",
            "description": "Test boundary conditions",
            "inputs": {{"param1": "edge_value"}},
            "expected_outputs": {{"result": "edge_expected"}},
            "priority": "medium"
        }}
    ]
}}

Include:
1. Unit tests for basic functionality
2. Edge case tests for boundary conditions
3. Error handling tests
4. Performance tests
5. Security tests"""
    
    def _parse_analysis_response(self, response: str) -> Dict[str, Any]:
        """Parse LLM analysis response."""
        try:
            # Try to extract JSON from response
            json_match = re.search(r'\{.*\}', response, re.DOTALL)
            if json_match:
                return json.loads(json_match.group())
            else:
                return self._fallback_analysis_parsing(response)
        except json.JSONDecodeError as e:
            self.logger.warning(f"Failed to parse analysis response: {e}")
            return self._fallback_analysis_parsing(response)
    
    def _parse_validation_response(self, response: str) -> Dict[str, Any]:
        """Parse LLM validation response."""
        try:
            # Try to extract JSON from response
            json_match = re.search(r'\{.*\}', response, re.DOTALL)
            if json_match:
                return json.loads(json_match.group())
            else:
                return self._fallback_validation_parsing(response)
        except json.JSONDecodeError as e:
            self.logger.warning(f"Failed to parse validation response: {e}")
            return self._fallback_validation_parsing(response)
    
    def _parse_test_cases_response(self, response: str) -> List[TestCase]:
        """Parse test cases from LLM response."""
        try:
            # Try to extract JSON from response
            json_match = re.search(r'\{.*\}', response, re.DOTALL)
            if json_match:
                data = json.loads(json_match.group())
                test_cases = []
                
                for test_data in data.get('test_cases', []):
                    test_case = TestCase(
                        test_id=test_data.get('test_id', f"test_{len(test_cases) + 1}"),
                        test_type=TestType(test_data.get('test_type', 'unit_test')),
                        name=test_data.get('name', 'Test'),
                        description=test_data.get('description', ''),
                        inputs=test_data.get('inputs', {}),
                        expected_outputs=test_data.get('expected_outputs', {}),
                        priority=test_data.get('priority', 'medium'),
                        tags=test_data.get('tags', [])
                    )
                    test_cases.append(test_case)
                
                return test_cases
            else:
                return []
        except Exception as e:
            self.logger.warning(f"Failed to parse test cases response: {e}")
            return []
    
    def _fallback_analysis_parsing(self, response: str) -> Dict[str, Any]:
        """Fallback parsing for analysis response."""
        return {
            "complexity_score": 0.5,
            "maintainability_score": 0.5,
            "performance_score": 0.5,
            "security_score": 0.5,
            "issues": ["Analysis parsing failed"],
            "warnings": [],
            "suggestions": [],
            "confidence": 0.3,
            "metrics": {}
        }
    
    def _fallback_validation_parsing(self, response: str) -> Dict[str, Any]:
        """Fallback parsing for validation response."""
        return {
            "valid": False,
            "confidence": 0.3,
            "issues": ["Validation parsing failed"],
            "warnings": [],
            "suggestions": [],
            "metrics": {}
        }
    
    def _calculate_quality_score(self, analysis: Dict[str, Any]) -> float:
        """Calculate overall quality score from analysis."""
        scores = [
            analysis.get('complexity_score', 0.5),
            analysis.get('maintainability_score', 0.5),
            analysis.get('performance_score', 0.5),
            analysis.get('security_score', 0.5)
        ]
        
        # Weight the scores (maintainability and security are more important)
        weights = [0.2, 0.3, 0.2, 0.3]
        weighted_score = sum(score * weight for score, weight in zip(scores, weights))
        
        return max(0.0, min(1.0, weighted_score))
    
    def _calculate_validation_confidence(self, validation_results: Dict[str, ValidationResult]) -> float:
        """Calculate overall validation confidence."""
        if not validation_results:
            return 0.0
        
        scores = [result.score for result in validation_results.values()]
        return sum(scores) / len(scores)
    
    def _collect_all_issues(self, validation_results: Dict[str, ValidationResult]) -> List[str]:
        """Collect all issues from validation results."""
        issues = []
        for result in validation_results.values():
            issues.extend(result.issues)
        return issues
    
    def _collect_all_warnings(self, validation_results: Dict[str, ValidationResult]) -> List[str]:
        """Collect all warnings from validation results."""
        warnings = []
        for result in validation_results.values():
            warnings.extend(result.warnings)
        return warnings
    
    def _collect_all_suggestions(self, validation_results: Dict[str, ValidationResult]) -> List[str]:
        """Collect all suggestions from validation results."""
        suggestions = []
        for result in validation_results.values():
            if hasattr(result, 'suggestions'):
                suggestions.extend(result.suggestions)
        return suggestions 