"""
Code optimization transformation engine.

This module implements code optimization transformations,
including performance improvements and best practices.
"""

from typing import Dict, Any, List, Optional
import re

from src.transformers.base_transformer import BaseTransformer
from src.utils.logger import get_logger

logger = get_logger(__name__)


class OptimizationTransformer(BaseTransformer):
    """Transformer for code optimization."""
    
    def __init__(self):
        """Initialize the optimization transformer."""
        super().__init__("optimization")
        self.transformation_rules = [
            {
                "name": "optimize_imports",
                "description": "Optimize import statements",
                "pattern": r"import\s+(\w+)\s*,\s*(\w+)",
                "replacement": "import \\1, \\2"
            },
            {
                "name": "optimize_string_operations",
                "description": "Optimize string operations",
                "pattern": r"str\s*\(\s*(\w+)\s*\)",
                "replacement": "\\1"
            },
            {
                "name": "optimize_list_comprehensions",
                "description": "Optimize list comprehensions",
                "pattern": r"list\s*\(\s*map\s*\(\s*(\w+)\s*,\s*(\w+)\s*\)\s*\)",
                "replacement": "[\\1(x) for x in \\2]"
            }
        ]
    
    async def transform(self, code: str, context: Dict[str, Any] = None) -> str:
        """Transform code for optimization."""
        if not self.validate_input(code):
            raise ValueError("Invalid input code")
        
        self.log_transformation_activity("Starting optimization transformation")
        
        try:
            transformed_code = code
            
            # Apply optimization transformation rules
            for rule in self.get_transformation_rules():
                transformed_code = await self._apply_rule(transformed_code, rule, context)
            
            self.log_transformation_activity("Optimization transformation completed", {
                "original_length": len(code),
                "transformed_length": len(transformed_code)
            })
            
            return transformed_code
            
        except Exception as e:
            self.logger.error(f"Error in optimization transformation: {e}")
            raise
    
    def get_transformation_rules(self) -> List[Dict[str, Any]]:
        """Get optimization transformation rules."""
        return self.transformation_rules
    
    async def _apply_rule(self, code: str, rule: Dict[str, Any], context: Dict[str, Any] = None) -> str:
        """Apply a specific optimization transformation rule."""
        try:
            pattern = rule["pattern"]
            replacement = rule["replacement"]
            
            # Apply regex transformation
            transformed = re.sub(pattern, replacement, code, flags=re.MULTILINE)
            
            self.logger.debug(f"Applied optimization rule '{rule['name']}': {rule['description']}")
            return transformed
            
        except Exception as e:
            self.logger.error(f"Error applying optimization rule '{rule['name']}': {e}")
            return code
