"""
Base Rule Class for COBOL to Python Transformations

This module provides the base class for all transformation rules.
"""

from abc import ABC, abstractmethod
from typing import Dict, Any, List, Optional
from ..parsers.cobol_lst import LosslessNode


class BaseRule(ABC):
    """
    Base class for all COBOL to Python transformation rules.
    
    Each rule should implement:
    - can_apply(): Check if this rule can be applied to the given node
    - apply(): Transform the COBOL construct to Python
    - get_priority(): Return priority for rule ordering
    """
    
    def __init__(self):
        self.variables: Dict[str, Dict[str, Any]] = {}
        self.indent_level: int = 0
        self.generated_code: List[str] = []
    
    @abstractmethod
    def can_apply(self, node: LosslessNode) -> bool:
        """
        Check if this rule can be applied to the given node.
        
        Args:
            node: The COBOL AST node to check
            
        Returns:
            True if this rule can handle the node, False otherwise
        """
        pass
    
    @abstractmethod
    def apply(self, node: LosslessNode) -> str:
        """
        Apply the transformation rule to convert COBOL to Python.
        
        Args:
            node: The COBOL AST node to transform
            
        Returns:
            Generated Python code as a string
        """
        pass
    
    @abstractmethod
    def get_priority(self) -> int:
        """
        Get the priority of this rule for ordering.
        
        Higher priority rules are applied first.
        
        Returns:
            Priority value (higher = applied first)
        """
        pass
    
    def sanitize_python_name(self, cobol_name: str) -> str:
        """
        Convert COBOL names to valid Python names.
        
        Args:
            cobol_name: The COBOL identifier name
            
        Returns:
            Valid Python identifier name
        """
        # Replace hyphens with underscores
        python_name = cobol_name.replace('-', '_')
        
        # If name starts with digit, add prefix
        if python_name[0].isdigit():
            python_name = f"para_{python_name}"
        
        # Convert to lowercase
        python_name = python_name.lower()
        
        return python_name
    
    def convert_cobol_condition(self, condition: str) -> str:
        """
        Convert COBOL condition to Python condition.
        
        Args:
            condition: The COBOL condition string
            
        Returns:
            Python condition string
        """
        # Simple conversions
        condition = condition.replace(' = ', ' == ')
        condition = condition.replace(' NOT = ', ' != ')
        condition = condition.replace(' GREATER THAN ', ' > ')
        condition = condition.replace(' LESS THAN ', ' < ')
        condition = condition.replace(' GREATER THAN OR EQUAL TO ', ' >= ')
        condition = condition.replace(' LESS THAN OR EQUAL TO ', ' <= ')
        
        # Handle string literals with spaces
        if "'" in condition:
            # Find and fix string literals
            parts = condition.split("'")
            for i in range(1, len(parts), 2):
                if i < len(parts):
                    parts[i] = f"'{parts[i]}'"
            condition = "".join(parts)
        
        # Normalize string comparisons - remove trailing spaces
        condition = condition.replace("'NO '", "'NO'")
        condition = condition.replace("'YES '", "'YES'")
        
        # Sanitize variable names in condition
        for var_name in self.variables:
            if var_name in condition:
                python_var_name = self.sanitize_python_name(var_name)
                condition = condition.replace(var_name, python_var_name)
        
        return condition
    
    def add_line(self, line: str):
        """
        Add a line to the generated Python code with proper indentation.
        
        Args:
            line: The line to add
        """
        indent = "    " * self.indent_level
        self.generated_code.append(indent + line)
    
    def set_variables(self, variables: Dict[str, Dict[str, Any]]):
        """
        Set the variables dictionary for this rule.
        
        Args:
            variables: Dictionary of variable information
        """
        self.variables = variables 