"""
Data Definition Transformation Rules

This module contains rules for transforming COBOL data definitions
to Python variable declarations.
"""

from typing import Dict, Any, List, Optional
from .base_rule import BaseRule
from ..parsers.cobol_lst import LosslessNode


class DataDescriptionRule(BaseRule):
    """
    Rule for transforming COBOL data descriptions to Python variable declarations.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is a data description entry."""
        return (node.rule_name == "DataDescriptionEntryContext" or 
                node.rule_name == "DataDescriptionEntryFormat1Context" or
                any(token.text == "01" for token in node.get_tokens() if hasattr(token, 'text')))
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL data description to Python variable declaration.
        
        COBOL:
            01  COUNTER    PIC 9(3).
            01  NAME       PIC X(10) VALUE 'HELLO'.
            
        Python:
            counter = 0  # int
            name = 'HELLO'  # str
        """
        tokens = node.get_tokens()
        var_name = None
        pic_clause = None
        value_clause = None
        
        # Extract variable name (level 01)
        for i, token in enumerate(tokens):
            if hasattr(token, 'text') and token.text:
                if token.text == "01" and i + 1 < len(tokens):
                    var_name = tokens[i + 1].text
                    break
        
        # Extract PIC clause
        for i, token in enumerate(tokens):
            if hasattr(token, 'text') and token.text == "PIC" and i + 1 < len(tokens):
                pic_clause = tokens[i + 1].text
                break
        
        # Extract VALUE clause
        for i, token in enumerate(tokens):
            if hasattr(token, 'text') and token.text == "VALUE" and i + 1 < len(tokens):
                value_clause = tokens[i + 1].text
                break
        
        if var_name:
            python_name = self.sanitize_python_name(var_name)
            python_type, default_value = self.get_python_type_and_default(pic_clause, value_clause)
            
            if default_value:
                self.add_line(f"{python_name} = {default_value}  # {python_type}")
            else:
                self.add_line(f"{python_name} = {default_value}  # {python_type}")
    
    def get_python_type_and_default(self, pic_clause: str, value_clause: str) -> tuple[str, str]:
        """Convert PIC clause to Python type and default value."""
        if not pic_clause:
            return "str", "''"
        
        # Handle PIC 9(n) - numeric
        if pic_clause.startswith("9(") and pic_clause.endswith(")"):
            return "int", "0"
        
        # Handle PIC X(n) - alphanumeric
        if pic_clause.startswith("X(") and pic_clause.endswith(")"):
            if value_clause:
                return "str", value_clause
            return "str", "''"
        
        # Handle PIC 9 - single digit
        if pic_clause == "9":
            return "int", "0"
        
        # Handle PIC X - single character
        if pic_clause == "X":
            if value_clause:
                return "str", value_clause
            return "str", "''"
        
        # Default to string
        if value_clause:
            return "str", value_clause
        return "str", "''"
    
    def sanitize_python_name(self, cobol_name: str) -> str:
        """Convert COBOL variable name to Python naming convention."""
        # Convert to lowercase and replace hyphens with underscores
        return cobol_name.lower().replace('-', '_')
    
    def get_priority(self) -> int:
        return 10


class WorkingStorageRule(BaseRule):
    """
    Rule for handling WORKING-STORAGE SECTION.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is a WORKING-STORAGE SECTION."""
        return (node.rule_name == "WorkingStorageSectionContext" or
                any(token.text == "WORKING-STORAGE" for token in node.get_tokens() if hasattr(token, 'text')))
    
    def apply(self, node: LosslessNode) -> str:
        """
        Handle WORKING-STORAGE SECTION - just add a comment.
        """
        self.add_line("# Working Storage Variables")
        self.add_line("")
    
    def get_priority(self) -> int:
        return 5 