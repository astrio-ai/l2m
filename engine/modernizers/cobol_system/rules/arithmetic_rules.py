"""
Arithmetic Transformation Rules

This module contains rules for transforming COBOL arithmetic operations
to Python arithmetic expressions.
"""

from typing import Dict, Any, List, Optional
from .base_rule import BaseRule
from ..parsers.cobol_lst import LosslessNode


class AddRule(BaseRule):
    """
    Rule for transforming COBOL ADD statements to Python addition.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is an ADD statement."""
        return (node.rule_name == "AddStatementContext" or
                any(token.text == "ADD" for token in node.get_tokens() if hasattr(token, 'text')))
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL ADD statement to Python addition.
        
        COBOL:
            ADD 1 TO COUNTER
            ADD A B TO C
            
        Python:
            counter += 1
            c = a + b
        """
        tokens = node.get_tokens()
        operands = []
        result_var = None
        
        # Parse ADD statement
        i = 0
        while i < len(tokens):
            token = tokens[i]
            if hasattr(token, 'text') and token.text:
                if token.text == "ADD":
                    # Collect operands until TO
                    i += 1
                    while i < len(tokens) and tokens[i].text != "TO":
                        if hasattr(tokens[i], 'text') and tokens[i].text:
                            operands.append(tokens[i].text)
                        i += 1
                elif token.text == "TO":
                    # Get result variable
                    if i + 1 < len(tokens):
                        result_var = tokens[i + 1].text
                    break
            i += 1
        
        if operands and result_var:
            python_result = self.sanitize_python_name(result_var)
            
            if len(operands) == 1:
                # Simple addition: ADD 1 TO COUNTER
                operand = self.sanitize_python_name(operands[0])
                self.add_line(f"{python_result} += {operand}")
            else:
                # Multiple operands: ADD A B TO C
                python_operands = [self.sanitize_python_name(op) for op in operands]
                expression = " + ".join(python_operands)
                self.add_line(f"{python_result} = {expression}")
    
    def sanitize_python_name(self, cobol_name: str) -> str:
        """Convert COBOL variable name to Python naming convention."""
        return cobol_name.lower().replace('-', '_')
    
    def get_priority(self) -> int:
        return 20


class SubtractRule(BaseRule):
    """
    Rule for transforming COBOL SUBTRACT statements to Python subtraction.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is a SUBTRACT statement."""
        return (node.rule_name == "SubtractStatementContext" or
                any(token.text == "SUBTRACT" for token in node.get_tokens() if hasattr(token, 'text')))
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL SUBTRACT statement to Python subtraction.
        
        COBOL:
            SUBTRACT 1 FROM COUNTER
            SUBTRACT A FROM B
            
        Python:
            counter -= 1
            b -= a
        """
        tokens = node.get_tokens()
        operand = None
        result_var = None
        
        # Parse SUBTRACT statement
        i = 0
        while i < len(tokens):
            token = tokens[i]
            if hasattr(token, 'text') and token.text:
                if token.text == "SUBTRACT":
                    # Get operand
                    if i + 1 < len(tokens):
                        operand = tokens[i + 1].text
                    i += 2
                elif token.text == "FROM":
                    # Get result variable
                    if i + 1 < len(tokens):
                        result_var = tokens[i + 1].text
                    break
            i += 1
        
        if operand and result_var:
            python_result = self.sanitize_python_name(result_var)
            python_operand = self.sanitize_python_name(operand)
            self.add_line(f"{python_result} -= {python_operand}")
    
    def sanitize_python_name(self, cobol_name: str) -> str:
        """Convert COBOL variable name to Python naming convention."""
        return cobol_name.lower().replace('-', '_')
    
    def get_priority(self) -> int:
        return 20


class ComputeRule(BaseRule):
    """
    Rule for transforming COBOL COMPUTE statements to Python expressions.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is a COMPUTE statement."""
        return (node.rule_name == "ComputeStatementContext" or
                any(token.text == "COMPUTE" for token in node.get_tokens() if hasattr(token, 'text')))
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL COMPUTE statement to Python expression.
        
        COBOL:
            COMPUTE RESULT = A + B * C
            
        Python:
            result = a + b * c
        """
        tokens = node.get_tokens()
        expression = []
        result_var = None
        
        # Parse COMPUTE statement
        i = 0
        in_expression = False
        while i < len(tokens):
            token = tokens[i]
            if hasattr(token, 'text') and token.text:
                if token.text == "COMPUTE":
                    # Get result variable
                    if i + 1 < len(tokens):
                        result_var = tokens[i + 1].text
                    i += 2
                elif token.text == "=":
                    in_expression = True
                    i += 1
                elif in_expression:
                    # Convert COBOL operators to Python
                    if token.text == "*":
                        expression.append("*")
                    elif token.text == "/":
                        expression.append("/")
                    elif token.text == "+":
                        expression.append("+")
                    elif token.text == "-":
                        expression.append("-")
                    elif token.text == "**":
                        expression.append("**")
                    else:
                        # Variable or literal
                        expression.append(self.sanitize_python_name(token.text))
                    i += 1
                else:
                    i += 1
            else:
                i += 1
        
        if result_var and expression:
            python_result = self.sanitize_python_name(result_var)
            python_expression = " ".join(expression)
            self.add_line(f"{python_result} = {python_expression}")
    
    def sanitize_python_name(self, cobol_name: str) -> str:
        """Convert COBOL variable name to Python naming convention."""
        return cobol_name.lower().replace('-', '_')
    
    def get_priority(self) -> int:
        return 25


class ComparisonRule(BaseRule):
    """
    Rule for transforming COBOL comparison operators to Python.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this contains comparison operators."""
        tokens = node.get_tokens()
        comparison_ops = ["=", ">", "<", ">=", "<=", "NOT", "EQUAL", "GREATER", "LESS"]
        return any(token.text in comparison_ops for token in tokens if hasattr(token, 'text'))
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL comparison to Python comparison.
        
        COBOL:
            A = B
            A > B
            A NOT EQUAL B
            
        Python:
            a == b
            a > b
            a != b
        """
        tokens = node.get_tokens()
        python_tokens = []
        
        for token in tokens:
            if hasattr(token, 'text') and token.text:
                if token.text == "=":
                    python_tokens.append("==")
                elif token.text == "NOT":
                    python_tokens.append("!")
                elif token.text == "EQUAL":
                    python_tokens.append("=")
                elif token.text == "GREATER":
                    python_tokens.append(">")
                elif token.text == "LESS":
                    python_tokens.append("<")
                else:
                    python_tokens.append(self.sanitize_python_name(token.text))
        
        return " ".join(python_tokens)
    
    def sanitize_python_name(self, cobol_name: str) -> str:
        """Convert COBOL variable name to Python naming convention."""
        return cobol_name.lower().replace('-', '_')
    
    def get_priority(self) -> int:
        return 15 