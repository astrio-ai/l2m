"""
Pattern-based transformation engine.

This module implements pattern-based code transformations,
including legacy pattern replacement and modernization.
"""

from typing import Dict, Any, List, Optional
import re

from src.transformers.base_transformer import BaseTransformer
from src.utils.logger import get_logger

logger = get_logger(__name__)


class PatternTransformer(BaseTransformer):
    """Transformer for pattern-based code transformations."""
    
    def __init__(self):
        """Initialize the pattern transformer."""
        super().__init__("pattern")
        self.transformation_rules = [
            {
                "name": "modernize_loops",
                "description": "Modernize loop constructs",
                "pattern": r"for\s+(\w+)\s+in\s+range\s*\(\s*(\d+)\s*\):",
                "replacement": "for \\1 in range(\\2):"
            },
            {
                "name": "modernize_string_formatting",
                "description": "Modernize string formatting",
                "pattern": r"print\s*\(\s*\"%s\"\s*%\s*(\w+)\s*\)",
                "replacement": "print(f\"{\\1}\")"
            },
            {
                "name": "modernize_exceptions",
                "description": "Modernize exception handling",
                "pattern": r"except\s+(\w+)\s*,\s*(\w+):",
                "replacement": "except \\1 as \\2:"
            }
        ]
    
    async def transform(self, code: str, context: Dict[str, Any] = None) -> str:
        """Transform code patterns."""
        if not self.validate_input(code):
            raise ValueError("Invalid input code")
        
        self.log_transformation_activity("Starting pattern transformation")
        
        try:
            transformed_code = code
            
            # Apply pattern transformation rules
            for rule in self.get_transformation_rules():
                transformed_code = await self._apply_rule(transformed_code, rule, context)
            
            self.log_transformation_activity("Pattern transformation completed", {
                "original_length": len(code),
                "transformed_length": len(transformed_code)
            })
            
            return transformed_code
            
        except Exception as e:
            self.logger.error(f"Error in pattern transformation: {e}")
            raise
    
    def get_transformation_rules(self) -> List[Dict[str, Any]]:
        """Get pattern transformation rules."""
        return self.transformation_rules
    
    async def _apply_rule(self, code: str, rule: Dict[str, Any], context: Dict[str, Any] = None) -> str:
        """Apply a specific pattern transformation rule."""
        try:
            pattern = rule["pattern"]
            replacement = rule["replacement"]
            
            # Apply regex transformation
            transformed = re.sub(pattern, replacement, code, flags=re.MULTILINE)
            
            self.logger.debug(f"Applied pattern rule '{rule['name']}': {rule['description']}")
            return transformed
            
        except Exception as e:
            self.logger.error(f"Error applying pattern rule '{rule['name']}': {e}")
            return code
