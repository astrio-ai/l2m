"""
Loads prompt templates from agents/preprompts/ directory.
"""

import os
import logging
from typing import Dict, List, Optional, Any
from pathlib import Path
from dataclasses import dataclass

logger = logging.getLogger(__name__)

@dataclass
class PromptTemplate:
    """A prompt template with metadata."""
    name: str
    content: str
    description: Optional[str] = None
    variables: List[str] = None
    category: Optional[str] = None

class PrepromptsHolder:
    """
    Manages prompt templates loaded from the preprompts directory.
    
    Provides methods for loading, caching, and rendering prompt templates
    used by the AI agents in the modernization system.
    """
    
    def __init__(self, preprompts_dir: Optional[str] = None):
        if preprompts_dir is None:
            # Default to the preprompts directory relative to this file
            current_dir = Path(__file__).parent
            preprompts_dir = current_dir / "preprompts"
        
        self.prompts_dir = Path(preprompts_dir)
        self.templates: Dict[str, PromptTemplate] = {}
        self.categories: Dict[str, List[str]] = {}
        
        self._load_templates()
    
    def _load_templates(self):
        """Load all prompt templates from the preprompts directory."""
        if not self.prompts_dir.exists():
            logger.warning(f"Preprompts directory not found: {self.prompts_dir}")
            return
        
        # Load templates from .txt files
        for file_path in self.prompts_dir.glob("*.txt"):
            try:
                template = self._load_template_from_file(file_path)
                if template:
                    self.templates[template.name] = template
                    
                    # Add to category
                    if template.category:
                        if template.category not in self.categories:
                            self.categories[template.category] = []
                        self.categories[template.category].append(template.name)
                    
                    logger.debug(f"Loaded template: {template.name}")
            except Exception as e:
                logger.error(f"Failed to load template from {file_path}: {e}")
        
        logger.info(f"Loaded {len(self.templates)} prompt templates")
    
    def _load_template_from_file(self, file_path: Path) -> Optional[PromptTemplate]:
        """Load a single template from a file."""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Extract template name from filename
            name = file_path.stem
            
            # Parse template metadata from content
            description = None
            variables = []
            category = None
            
            # Look for metadata in comments at the top
            lines = content.split('\n')
            metadata_lines = []
            
            for line in lines:
                line = line.strip()
                if line.startswith('#'):
                    metadata_lines.append(line[1:].strip())
                elif line.startswith('<!--') and line.endswith('-->'):
                    metadata_lines.append(line[4:-3].strip())
                elif line.startswith('//'):
                    metadata_lines.append(line[2:].strip())
                else:
                    break
            
            # Parse metadata
            for line in metadata_lines:
                if line.startswith('description:'):
                    description = line.split(':', 1)[1].strip()
                elif line.startswith('variables:'):
                    vars_str = line.split(':', 1)[1].strip()
                    variables = [v.strip() for v in vars_str.split(',') if v.strip()]
                elif line.startswith('category:'):
                    category = line.split(':', 1)[1].strip()
            
            # Extract variables from content if not specified in metadata
            if not variables:
                variables = self._extract_variables_from_content(content)
            
            return PromptTemplate(
                name=name,
                content=content,
                description=description,
                variables=variables,
                category=category
            )
            
        except Exception as e:
            logger.error(f"Error loading template from {file_path}: {e}")
            return None
    
    def _extract_variables_from_content(self, content: str) -> List[str]:
        """Extract variable placeholders from template content."""
        import re
        
        # Look for common variable patterns
        patterns = [
            r'\{\{(\w+)\}\}',  # {{variable}}
            r'\{(\w+)\}',      # {variable}
            r'\$(\w+)',        # $variable
            r'%(\w+)%',        # %variable%
        ]
        
        variables = set()
        for pattern in patterns:
            matches = re.findall(pattern, content)
            variables.update(matches)
        
        return sorted(list(variables))
    
    def get_template(self, name: str) -> Optional[PromptTemplate]:
        """Get a template by name."""
        return self.templates.get(name)
    
    def get_template_content(self, name: str) -> Optional[str]:
        """Get the content of a template by name."""
        template = self.get_template(name)
        return template.content if template else None
    
    def list_templates(self, category: Optional[str] = None) -> List[str]:
        """List available template names, optionally filtered by category."""
        if category:
            return self.categories.get(category, [])
        else:
            return list(self.templates.keys())
    
    def list_categories(self) -> List[str]:
        """List available template categories."""
        return list(self.categories.keys())
    
    def render_template(self, name: str, variables: Dict[str, Any]) -> Optional[str]:
        """
        Render a template with the given variables.
        
        Args:
            name: Template name
            variables: Dictionary of variables to substitute
            
        Returns:
            Rendered template content or None if template not found
        """
        template = self.get_template(name)
        if not template:
            logger.warning(f"Template not found: {name}")
            return None
        
        content = template.content
        
        # Replace variables in order of specificity (longer names first)
        sorted_vars = sorted(variables.keys(), key=len, reverse=True)
        
        for var_name in sorted_vars:
            var_value = str(variables[var_name])
            
            # Try different variable patterns
            patterns = [
                f'{{{{{var_name}}}}}',  # {{variable}}
                f'{{{var_name}}}',      # {variable}
                f'${var_name}',         # $variable
                f'%{var_name}%',        # %variable%
            ]
            
            for pattern in patterns:
                content = content.replace(pattern, var_value)
        
        return content
    
    def render_template_safe(self, name: str, variables: Dict[str, Any], 
                           default_value: str = "") -> str:
        """
        Render a template safely, returning a default value if rendering fails.
        
        Args:
            name: Template name
            variables: Dictionary of variables to substitute
            default_value: Default value to return if rendering fails
            
        Returns:
            Rendered template content or default value
        """
        try:
            result = self.render_template(name, variables)
            return result if result is not None else default_value
        except Exception as e:
            logger.error(f"Error rendering template {name}: {e}")
            return default_value
    
    def get_template_info(self, name: str) -> Optional[Dict[str, Any]]:
        """Get detailed information about a template."""
        template = self.get_template(name)
        if not template:
            return None
        
        return {
            'name': template.name,
            'description': template.description,
            'variables': template.variables,
            'category': template.category,
            'content_length': len(template.content),
            'variable_count': len(template.variables)
        }
    
    def search_templates(self, query: str, search_content: bool = True) -> List[str]:
        """
        Search for templates by name or content.
        
        Args:
            query: Search query
            search_content: Whether to search in template content as well
            
        Returns:
            List of matching template names
        """
        query_lower = query.lower()
        matches = []
        
        for name, template in self.templates.items():
            # Search in name
            if query_lower in name.lower():
                matches.append(name)
                continue
            
            # Search in description
            if template.description and query_lower in template.description.lower():
                matches.append(name)
                continue
            
            # Search in content
            if search_content and query_lower in template.content.lower():
                matches.append(name)
                continue
        
        return matches
    
    def validate_template(self, name: str, variables: Dict[str, Any]) -> Dict[str, Any]:
        """
        Validate that all required variables are provided for a template.
        
        Args:
            name: Template name
            variables: Variables to validate
            
        Returns:
            Dictionary with validation results
        """
        template = self.get_template(name)
        if not template:
            return {
                'valid': False,
                'error': f'Template not found: {name}',
                'missing_variables': [],
                'extra_variables': []
            }
        
        required_vars = set(template.variables)
        provided_vars = set(variables.keys())
        
        missing_vars = required_vars - provided_vars
        extra_vars = provided_vars - required_vars
        
        return {
            'valid': len(missing_vars) == 0,
            'missing_variables': sorted(list(missing_vars)),
            'extra_variables': sorted(list(extra_vars)),
            'required_variables': template.variables,
            'provided_variables': list(variables.keys())
        }
    
    def reload_templates(self):
        """Reload all templates from disk."""
        self.templates.clear()
        self.categories.clear()
        self._load_templates()
    
    def add_template(self, name: str, content: str, description: Optional[str] = None,
                    variables: Optional[List[str]] = None, category: Optional[str] = None):
        """Add a template programmatically."""
        if variables is None:
            variables = self._extract_variables_from_content(content)
        
        template = PromptTemplate(
            name=name,
            content=content,
            description=description,
            variables=variables,
            category=category
        )
        
        self.templates[name] = template
        
        if category:
            if category not in self.categories:
                self.categories[category] = []
            self.categories[category].append(name)
    
    def remove_template(self, name: str) -> bool:
        """Remove a template."""
        if name in self.templates:
            template = self.templates[name]
            
            # Remove from categories
            if template.category and template.category in self.categories:
                self.categories[template.category].remove(name)
                if not self.categories[template.category]:
                    del self.categories[template.category]
            
            del self.templates[name]
            return True
        
        return False
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get statistics about loaded templates."""
        total_templates = len(self.templates)
        total_categories = len(self.categories)
        
        # Count templates by category
        category_counts = {}
        for category, templates in self.categories.items():
            category_counts[category] = len(templates)
        
        # Count templates without category
        uncategorized = sum(1 for t in self.templates.values() if not t.category)
        
        # Average variables per template
        total_variables = sum(len(t.variables) for t in self.templates.values())
        avg_variables = total_variables / total_templates if total_templates > 0 else 0
        
        return {
            'total_templates': total_templates,
            'total_categories': total_categories,
            'category_counts': category_counts,
            'uncategorized_templates': uncategorized,
            'average_variables_per_template': avg_variables,
            'total_variables': total_variables
        } 