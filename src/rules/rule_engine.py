"""
Rule Engine for COBOL to Python Transformations

This module provides a rule engine that manages and applies transformation rules
to convert COBOL constructs to Python equivalents.
"""

from typing import Dict, Any, List, Optional
from .base_rule import BaseRule
from .control_flow_rules import (
    IfStatementRule,
    PerformUntilRule,
    PerformTimesRule,
    PerformParagraphRule,
    EvaluateRule,
    GoBackRule
)
from .file_io_rules import (
    FileSelectRule,
    FileOpenRule,
    FileReadRule,
    FileWriteRule,
    FileCloseRule
)
from .data_definition_rules import (
    DataDescriptionRule,
    WorkingStorageRule
)
from .arithmetic_rules import (
    AddRule,
    SubtractRule,
    ComputeRule,
    ComparisonRule
)
from ..parsers.cobol_lst import LosslessNode


class RuleEngine:
    """
    Engine for applying transformation rules to COBOL AST nodes.
    
    The rule engine manages a collection of rules and applies them
    to transform COBOL constructs to Python equivalents.
    """
    
    def __init__(self):
        self.rules: List[BaseRule] = []
        self.variables: Dict[str, Dict[str, Any]] = {}
        self.indent_level: int = 0
        self.generated_code: List[str] = []
        
        # Register default rules
        self._register_default_rules()
    
    def _register_default_rules(self):
        """Register the default transformation rules."""
        # Control flow rules
        self.register_rule(IfStatementRule())
        self.register_rule(PerformUntilRule())
        self.register_rule(PerformTimesRule())
        self.register_rule(PerformParagraphRule())
        self.register_rule(EvaluateRule())
        self.register_rule(GoBackRule())
        
        # File I/O rules
        self.register_rule(FileSelectRule())
        self.register_rule(FileOpenRule())
        self.register_rule(FileReadRule())
        self.register_rule(FileWriteRule())
        self.register_rule(FileCloseRule())
        
        # Data definition rules
        self.register_rule(DataDescriptionRule())
        self.register_rule(WorkingStorageRule())
        
        # Arithmetic rules
        self.register_rule(AddRule())
        self.register_rule(SubtractRule())
        self.register_rule(ComputeRule())
        self.register_rule(ComparisonRule())
    
    def register_rule(self, rule: BaseRule):
        """
        Register a transformation rule.
        
        Args:
            rule: The rule to register
        """
        rule.set_variables(self.variables)
        self.rules.append(rule)
        
        # Sort rules by priority (highest first)
        self.rules.sort(key=lambda r: r.get_priority(), reverse=True)
    
    def set_variables(self, variables: Dict[str, Dict[str, Any]]):
        """
        Set the variables dictionary for all rules.
        
        Args:
            variables: Dictionary of variable information
        """
        self.variables = variables
        for rule in self.rules:
            rule.set_variables(variables)
    
    def apply_rules(self, node: LosslessNode) -> str:
        """
        Apply transformation rules to a COBOL AST node.
        
        Args:
            node: The COBOL AST node to transform
            
        Returns:
            Generated Python code as a string
        """
        # Find the first rule that can handle this node
        for rule in self.rules:
            if rule.can_apply(node):
                return rule.apply(node)
        
        # No rule found, return empty string
        return ""
    
    def apply_rules_to_children(self, node: LosslessNode) -> List[str]:
        """
        Apply transformation rules to all children of a node.
        
        Args:
            node: The parent COBOL AST node
            
        Returns:
            List of generated Python code strings
        """
        results = []
        
        for child in node.children:
            result = self.apply_rules(child)
            if result:
                results.append(result)
        
        return results
    
    def add_line(self, line: str):
        """
        Add a line to the generated Python code with proper indentation.
        
        Args:
            line: The line to add
        """
        indent = "    " * self.indent_level
        self.generated_code.append(indent + line)
    
    def clear_generated_code(self):
        """Clear the generated code buffer."""
        self.generated_code = []
    
    def get_generated_code(self) -> str:
        """
        Get the generated Python code.
        
        Returns:
            Generated Python code as a string
        """
        return '\n'.join(self.generated_code) 