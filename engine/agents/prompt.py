"""
Builds system + user prompts for LLM calls.
"""

import logging
from typing import Dict, List, Optional, Any, Union
from dataclasses import dataclass, field
from .preprompts_holder import PrepromptsHolder

logger = logging.getLogger(__name__)

@dataclass
class PromptContext:
    """Context information for building prompts."""
    project_name: str = ""
    target_stack: str = ""
    current_task: str = ""
    file_content: str = ""
    file_path: str = ""
    user_requirements: str = ""
    previous_output: str = ""
    constraints: List[str] = field(default_factory=list)
    examples: List[str] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)

@dataclass
class PromptResult:
    """Result of prompt building."""
    system_prompt: str
    user_prompt: str
    context: PromptContext
    variables_used: List[str]
    template_name: Optional[str] = None

class PromptBuilder:
    """
    Builds system and user prompts for LLM calls.
    
    Provides methods for constructing prompts with context, templates,
    and dynamic content for the modernization process.
    """
    
    def __init__(self, preprompts_holder: Optional[PrepromptsHolder] = None):
        self.preprompts = preprompts_holder or PrepromptsHolder()
        
    def build_prompt(
        self,
        template_name: str,
        context: PromptContext,
        additional_variables: Optional[Dict[str, Any]] = None
    ) -> PromptResult:
        """
        Build a complete prompt using a template and context.
        
        Args:
            template_name: Name of the template to use
            context: Context information for the prompt
            additional_variables: Additional variables to include
            
        Returns:
            PromptResult with system and user prompts
        """
        # Get template content
        template_content = self.preprompts.get_template_content(template_name)
        if not template_content:
            raise ValueError(f"Template not found: {template_name}")
        
        # Prepare variables
        variables = self._prepare_variables(context, additional_variables or {})
        
        # Render template
        rendered_content = self.preprompts.render_template(template_name, variables)
        if not rendered_content:
            raise ValueError(f"Failed to render template: {template_name}")
        
        # Split into system and user prompts
        system_prompt, user_prompt = self._split_prompt(rendered_content)
        
        return PromptResult(
            system_prompt=system_prompt,
            user_prompt=user_prompt,
            context=context,
            variables_used=list(variables.keys()),
            template_name=template_name
        )
    
    def build_simple_prompt(
        self,
        system_prompt: str,
        user_prompt: str,
        context: Optional[PromptContext] = None
    ) -> PromptResult:
        """
        Build a simple prompt without using templates.
        
        Args:
            system_prompt: System prompt content
            user_prompt: User prompt content
            context: Optional context information
            
        Returns:
            PromptResult with the provided prompts
        """
        if context is None:
            context = PromptContext()
        
        return PromptResult(
            system_prompt=system_prompt,
            user_prompt=user_prompt,
            context=context,
            variables_used=[],
            template_name=None
        )
    
    def build_modernization_prompt(
        self,
        context: PromptContext,
        task_type: str = "general"
    ) -> PromptResult:
        """
        Build a prompt specifically for modernization tasks.
        
        Args:
            context: Context information
            task_type: Type of modernization task
            
        Returns:
            PromptResult with modernization-specific prompts
        """
        # Choose appropriate template based on task type
        template_mapping = {
            "parsing": "entrypoint",
            "generation": "generate",
            "refactoring": "improve",
            "clarification": "clarify",
            "file_format": "file_format",
            "general": "entrypoint"
        }
        
        template_name = template_mapping.get(task_type, "entrypoint")
        
        # Add task-specific context
        enhanced_context = self._enhance_context_for_task(context, task_type)
        
        return self.build_prompt(template_name, enhanced_context)
    
    def build_code_generation_prompt(
        self,
        context: PromptContext,
        target_language: str,
        framework: Optional[str] = None
    ) -> PromptResult:
        """
        Build a prompt for code generation tasks.
        
        Args:
            context: Context information
            target_language: Target programming language
            framework: Optional framework specification
            
        Returns:
            PromptResult with code generation prompts
        """
        # Enhance context with language-specific information
        enhanced_context = context
        enhanced_context.metadata.update({
            'target_language': target_language,
            'framework': framework,
            'code_generation': True
        })
        
        # Add language-specific constraints
        language_constraints = self._get_language_constraints(target_language, framework)
        enhanced_context.constraints.extend(language_constraints)
        
        return self.build_prompt("generate", enhanced_context)
    
    def build_refactoring_prompt(
        self,
        context: PromptContext,
        refactoring_type: str = "general"
    ) -> PromptResult:
        """
        Build a prompt for code refactoring tasks.
        
        Args:
            context: Context information
            refactoring_type: Type of refactoring (performance, readability, etc.)
            
        Returns:
            PromptResult with refactoring prompts
        """
        # Enhance context with refactoring information
        enhanced_context = context
        enhanced_context.metadata.update({
            'refactoring_type': refactoring_type,
            'refactoring': True
        })
        
        # Add refactoring-specific constraints
        refactoring_constraints = self._get_refactoring_constraints(refactoring_type)
        enhanced_context.constraints.extend(refactoring_constraints)
        
        return self.build_prompt("improve", enhanced_context)
    
    def build_qa_prompt(
        self,
        context: PromptContext,
        qa_type: str = "general"
    ) -> PromptResult:
        """
        Build a prompt for QA/testing tasks.
        
        Args:
            context: Context information
            qa_type: Type of QA task (testing, validation, etc.)
            
        Returns:
            PromptResult with QA prompts
        """
        # Enhance context with QA information
        enhanced_context = context
        enhanced_context.metadata.update({
            'qa_type': qa_type,
            'qa_task': True
        })
        
        # Add QA-specific constraints
        qa_constraints = self._get_qa_constraints(qa_type)
        enhanced_context.constraints.extend(qa_constraints)
        
        return self.build_prompt("generate", enhanced_context)
    
    def _prepare_variables(self, context: PromptContext, additional_variables: Dict[str, Any]) -> Dict[str, Any]:
        """Prepare variables for template rendering."""
        variables = {
            'project_name': context.project_name,
            'target_stack': context.target_stack,
            'current_task': context.current_task,
            'file_content': context.file_content,
            'file_path': context.file_path,
            'user_requirements': context.user_requirements,
            'previous_output': context.previous_output,
            'constraints': '\n'.join(context.constraints),
            'examples': '\n'.join(context.examples),
            'metadata': str(context.metadata)
        }
        
        # Add additional variables
        variables.update(additional_variables)
        
        # Add computed variables
        variables.update(self._compute_derived_variables(context))
        
        return variables
    
    def _compute_derived_variables(self, context: PromptContext) -> Dict[str, Any]:
        """Compute derived variables from context."""
        derived = {}
        
        # File extension
        if context.file_path:
            import os
            derived['file_extension'] = os.path.splitext(context.file_path)[1]
            derived['file_name'] = os.path.basename(context.file_path)
            derived['file_directory'] = os.path.dirname(context.file_path)
        
        # Content statistics
        if context.file_content:
            derived['content_length'] = len(context.file_content)
            derived['line_count'] = context.file_content.count('\n') + 1
            derived['word_count'] = len(context.file_content.split())
        
        # Task complexity estimation
        derived['task_complexity'] = self._estimate_task_complexity(context)
        
        return derived
    
    def _estimate_task_complexity(self, context: PromptContext) -> str:
        """Estimate the complexity of the current task."""
        complexity_score = 0
        
        # Content length factor
        if context.file_content:
            if len(context.file_content) > 10000:
                complexity_score += 3
            elif len(context.file_content) > 5000:
                complexity_score += 2
            elif len(context.file_content) > 1000:
                complexity_score += 1
        
        # Constraints factor
        complexity_score += len(context.constraints)
        
        # Requirements factor
        if context.user_requirements:
            complexity_score += context.user_requirements.count('\n') // 5
        
        if complexity_score >= 5:
            return "high"
        elif complexity_score >= 3:
            return "medium"
        else:
            return "low"
    
    def _split_prompt(self, content: str) -> tuple[str, str]:
        """Split content into system and user prompts."""
        # Look for common separators
        separators = [
            "\n\n---\n\n",
            "\n\nUSER:\n",
            "\n\nASSISTANT:\n",
            "\n\nSYSTEM:\n",
            "\n\n###\n\n"
        ]
        
        for separator in separators:
            if separator in content:
                parts = content.split(separator, 1)
                if len(parts) == 2:
                    return parts[0].strip(), parts[1].strip()
        
        # If no separator found, treat first paragraph as system prompt
        paragraphs = content.split('\n\n')
        if len(paragraphs) >= 2:
            system_prompt = paragraphs[0].strip()
            user_prompt = '\n\n'.join(paragraphs[1:]).strip()
            return system_prompt, user_prompt
        
        # If only one paragraph, treat as user prompt
        return "", content.strip()
    
    def _enhance_context_for_task(self, context: PromptContext, task_type: str) -> PromptContext:
        """Enhance context with task-specific information."""
        enhanced = context
        
        if task_type == "parsing":
            enhanced.metadata.update({
                'focus': 'analysis',
                'output_format': 'structured'
            })
        elif task_type == "generation":
            enhanced.metadata.update({
                'focus': 'creation',
                'output_format': 'code'
            })
        elif task_type == "refactoring":
            enhanced.metadata.update({
                'focus': 'improvement',
                'output_format': 'code'
            })
        elif task_type == "clarification":
            enhanced.metadata.update({
                'focus': 'understanding',
                'output_format': 'text'
            })
        
        return enhanced
    
    def _get_language_constraints(self, language: str, framework: Optional[str]) -> List[str]:
        """Get language-specific constraints."""
        constraints = []
        
        if language.lower() in ['javascript', 'js']:
            constraints.extend([
                "Use modern JavaScript (ES6+) features",
                "Follow JavaScript best practices",
                "Use meaningful variable and function names"
            ])
            
            if framework == 'react':
                constraints.extend([
                    "Use functional components with hooks",
                    "Follow React best practices",
                    "Use JSX syntax"
                ])
            elif framework == 'vue':
                constraints.extend([
                    "Use Vue 3 Composition API",
                    "Follow Vue best practices",
                    "Use Vue SFC format"
                ])
        
        elif language.lower() in ['typescript', 'ts']:
            constraints.extend([
                "Use TypeScript with strict typing",
                "Define proper interfaces and types",
                "Avoid 'any' type usage"
            ])
        
        elif language.lower() in ['python', 'py']:
            constraints.extend([
                "Use Python 3.8+ features",
                "Follow PEP 8 style guidelines",
                "Use type hints where appropriate"
            ])
        
        return constraints
    
    def _get_refactoring_constraints(self, refactoring_type: str) -> List[str]:
        """Get refactoring-specific constraints."""
        constraints = []
        
        if refactoring_type == "performance":
            constraints.extend([
                "Optimize for performance",
                "Reduce time complexity where possible",
                "Minimize memory usage"
            ])
        elif refactoring_type == "readability":
            constraints.extend([
                "Improve code readability",
                "Use clear and descriptive names",
                "Add appropriate comments"
            ])
        elif refactoring_type == "maintainability":
            constraints.extend([
                "Improve code maintainability",
                "Reduce code duplication",
                "Follow SOLID principles"
            ])
        
        return constraints
    
    def _get_qa_constraints(self, qa_type: str) -> List[str]:
        """Get QA-specific constraints."""
        constraints = []
        
        if qa_type == "testing":
            constraints.extend([
                "Create comprehensive tests",
                "Cover edge cases",
                "Use appropriate testing frameworks"
            ])
        elif qa_type == "validation":
            constraints.extend([
                "Validate code correctness",
                "Check for potential issues",
                "Ensure best practices are followed"
            ])
        elif qa_type == "documentation":
            constraints.extend([
                "Create clear documentation",
                "Include usage examples",
                "Document API interfaces"
            ])
        
        return constraints
    
    def validate_prompt(self, prompt_result: PromptResult) -> Dict[str, Any]:
        """Validate a built prompt."""
        validation = {
            'valid': True,
            'warnings': [],
            'errors': []
        }
        
        # Check system prompt
        if not prompt_result.system_prompt.strip():
            validation['warnings'].append("System prompt is empty")
        
        # Check user prompt
        if not prompt_result.user_prompt.strip():
            validation['errors'].append("User prompt is empty")
            validation['valid'] = False
        
        # Check prompt length
        total_length = len(prompt_result.system_prompt) + len(prompt_result.user_prompt)
        if total_length > 8000:
            validation['warnings'].append(f"Prompt is very long ({total_length} characters)")
        
        # Check for missing variables
        if prompt_result.template_name:
            template = self.preprompts.get_template(prompt_result.template_name)
            if template:
                missing_vars = set(template.variables) - set(prompt_result.variables_used)
                if missing_vars:
                    validation['warnings'].append(f"Template variables not used: {missing_vars}")
        
        return validation
    
    def get_prompt_summary(self, prompt_result: PromptResult) -> Dict[str, Any]:
        """Get a summary of the built prompt."""
        return {
            'template_name': prompt_result.template_name,
            'system_prompt_length': len(prompt_result.system_prompt),
            'user_prompt_length': len(prompt_result.user_prompt),
            'total_length': len(prompt_result.system_prompt) + len(prompt_result.user_prompt),
            'variables_used': prompt_result.variables_used,
            'context_info': {
                'project_name': prompt_result.context.project_name,
                'target_stack': prompt_result.context.target_stack,
                'current_task': prompt_result.context.current_task,
                'file_path': prompt_result.context.file_path
            }
        } 