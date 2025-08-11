"""
Template system for modern framework code generation.

This package contains ready-to-use templates for modern frameworks
that are injected by agents when generating new code.
"""

from pathlib import Path
from typing import Dict, List, Optional, Any

class TemplateManager:
    """Manages framework templates for code generation."""
    
    def __init__(self, templates_dir: Optional[str] = None):
        if templates_dir is None:
            current_dir = Path(__file__).parent
            templates_dir = current_dir
        
        self.templates_dir = Path(templates_dir)
        self.templates: Dict[str, str] = {}
        self._load_templates()
    
    def _load_templates(self):
        """Load all template files."""
        for file_path in self.templates_dir.glob("*.*"):
            if file_path.name != "__init__.py":
                try:
                    with open(file_path, 'r', encoding='utf-8') as f:
                        self.templates[file_path.name] = f.read()
                except Exception as e:
                    print(f"Failed to load template {file_path}: {e}")
    
    def get_template(self, name: str) -> Optional[str]:
        """Get a template by name."""
        return self.templates.get(name)
    
    def list_templates(self) -> List[str]:
        """List all available template names."""
        return list(self.templates.keys())
    
    def render_template(self, name: str, variables: Dict[str, Any]) -> Optional[str]:
        """Render a template with variables."""
        template = self.get_template(name)
        if not template:
            return None
        
        # Simple variable substitution
        for key, value in variables.items():
            template = template.replace(f"{{{{{key}}}}}", str(value))
            template = template.replace(f"{{{key}}}", str(value))
            template = template.replace(f"${key}", str(value))
        
        return template

# Create a global template manager instance
template_manager = TemplateManager()

__all__ = ['TemplateManager', 'template_manager'] 