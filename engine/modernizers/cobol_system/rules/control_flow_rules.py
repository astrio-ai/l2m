"""
Control Flow Transformation Rules

This module contains rules for transforming COBOL control flow structures
to Python equivalents.
"""

from typing import Dict, Any, List, Optional
from .base_rule import BaseRule
from ..parsers.cobol_lst import LosslessNode


class IfStatementRule(BaseRule):
    """
    Rule for transforming COBOL IF/ELSE/END-IF statements to Python if/else.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is an IF statement node."""
        return (node.rule_name == "IfStatementContext" or 
                "IF" in [token.text for token in node.get_tokens() if hasattr(token, 'text')])
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL IF statement to Python if/else.
        
        COBOL:
            IF A = B THEN
                MOVE X TO Y
            ELSE
                MOVE Z TO Y
            END-IF
            
        Python:
            if a == b:
                y = x
            else:
                y = z
        """
        self.generated_code = []
        tokens = node.get_tokens()
        
        # Parse IF-THEN-ELSE structure
        condition = None
        then_statements = []
        else_statements = []
        
        in_then = False
        in_else = False
        i = 0
        
        while i < len(tokens):
            token = tokens[i]
            if hasattr(token, 'text') and token.text:
                if token.text == 'IF':
                    # Extract condition
                    condition_parts = []
                    i += 1
                    while i < len(tokens) and tokens[i].text != 'THEN':
                        if hasattr(tokens[i], 'text') and tokens[i].text:
                            condition_parts.append(tokens[i].text)
                        i += 1
                    condition = " ".join(condition_parts)
                elif token.text == 'THEN':
                    in_then = True
                    in_else = False
                elif token.text == 'ELSE':
                    in_then = False
                    in_else = True
                elif token.text == 'END-IF':
                    break
                elif in_then and token.text not in ['IF', 'THEN', 'ELSE', 'END-IF']:
                    then_statements.append(token.text)
                elif in_else and token.text not in ['IF', 'THEN', 'ELSE', 'END-IF']:
                    else_statements.append(token.text)
            i += 1
        
        if condition:
            python_condition = self.convert_cobol_condition(condition)
            self.add_line(f"if {python_condition}:")
            self.indent_level += 1
            
            # Add THEN statements
            for stmt in then_statements:
                if stmt.strip():
                    # TODO: Apply other rules to translate individual statements
                    self.add_line(f"# {stmt}")
            
            if else_statements:
                self.indent_level -= 1
                self.add_line("else:")
                self.indent_level += 1
                
                # Add ELSE statements
                for stmt in else_statements:
                    if stmt.strip():
                        # TODO: Apply other rules to translate individual statements
                        self.add_line(f"# {stmt}")
            
            self.indent_level -= 1
    
    def convert_cobol_condition(self, condition: str) -> str:
        """Convert COBOL condition to Python condition."""
        # Replace COBOL operators with Python operators
        condition = condition.replace(" = ", " == ")
        condition = condition.replace(" NOT EQUAL ", " != ")
        condition = condition.replace(" GREATER THAN ", " > ")
        condition = condition.replace(" LESS THAN ", " < ")
        condition = condition.replace(" GREATER THAN OR EQUAL ", " >= ")
        condition = condition.replace(" LESS THAN OR EQUAL ", " <= ")
        
        # Convert variable names to Python naming
        parts = condition.split()
        python_parts = []
        for part in parts:
            if part not in ["==", "!=", ">", "<", ">=", "<=", "AND", "OR", "NOT"]:
                python_parts.append(part.lower().replace('-', '_'))
            else:
                python_parts.append(part)
        
        return " ".join(python_parts)
    
    def get_priority(self) -> int:
        return 30


class PerformUntilRule(BaseRule):
    """
    Rule for transforming COBOL PERFORM UNTIL statements to Python while loops.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is a PERFORM UNTIL statement."""
        tokens = node.get_tokens()
        return any(token.text == "PERFORM" for token in tokens if hasattr(token, 'text')) and \
               any(token.text == "UNTIL" for token in tokens if hasattr(token, 'text'))
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL PERFORM UNTIL to Python while loop.
        
        COBOL:
            PERFORM UNTIL COUNTER > 5
                DISPLAY COUNTER
                ADD 1 TO COUNTER
            END-PERFORM
            
        Python:
            while counter <= 5:
                print(counter)
                counter += 1
        """
        tokens = node.get_tokens()
        condition = None
        loop_body = []
        
        # Parse PERFORM UNTIL structure
        i = 0
        in_condition = False
        in_body = False
        
        while i < len(tokens):
            token = tokens[i]
            if hasattr(token, 'text') and token.text:
                if token.text == "PERFORM":
                    i += 1
                elif token.text == "UNTIL":
                    in_condition = True
                    i += 1
                    condition_parts = []
                    while i < len(tokens) and tokens[i].text != "END-PERFORM":
                        if hasattr(tokens[i], 'text') and tokens[i].text:
                            condition_parts.append(tokens[i].text)
                        i += 1
                    condition = " ".join(condition_parts)
                    break
            i += 1
        
        if condition:
            # Invert condition for while loop
            python_condition = self.invert_condition(condition)
            self.add_line(f"while {python_condition}:")
            self.indent_level += 1
            # TODO: Add loop body translation
            self.indent_level -= 1
    
    def invert_condition(self, condition: str) -> str:
        """Invert COBOL condition for while loop."""
        # Simple inversion for common cases
        if ">" in condition:
            return condition.replace(">", "<=")
        elif "<" in condition:
            return condition.replace("<", ">=")
        elif "=" in condition:
            return condition.replace("=", "!=")
        else:
            return f"not ({condition})"
    
    def get_priority(self) -> int:
        return 25


class PerformTimesRule(BaseRule):
    """
    Rule for transforming COBOL PERFORM TIMES statements to Python for loops.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is a PERFORM TIMES statement."""
        tokens = node.get_tokens()
        return any(token.text == "PERFORM" for token in tokens if hasattr(token, 'text')) and \
               any(token.text == "TIMES" for token in tokens if hasattr(token, 'text'))
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL PERFORM TIMES to Python for loop.
        
        COBOL:
            PERFORM A000-COUNT 3 TIMES
            
        Python:
            for _ in range(3):
                a000_count()
        """
        tokens = node.get_tokens()
        paragraph_name = None
        times_count = None
        
        # Parse PERFORM TIMES structure
        i = 0
        while i < len(tokens):
            token = tokens[i]
            if hasattr(token, 'text') and token.text:
                if token.text == "PERFORM":
                    # Get paragraph name
                    if i + 1 < len(tokens):
                        paragraph_name = tokens[i + 1].text
                    i += 2
                elif token.text == "TIMES":
                    # Get count
                    if i - 1 >= 0:
                        times_count = tokens[i - 1].text
                    break
            i += 1
        
        if paragraph_name and times_count:
            python_paragraph = self.sanitize_python_name(paragraph_name)
            self.add_line(f"for _ in range({times_count}):")
            self.indent_level += 1
            self.add_line(f"{python_paragraph}()")
            self.indent_level -= 1
    
    def sanitize_python_name(self, cobol_name: str) -> str:
        """Convert COBOL paragraph name to Python function name."""
        return cobol_name.lower().replace('-', '_')
    
    def get_priority(self) -> int:
        return 25


class PerformParagraphRule(BaseRule):
    """
    Rule for transforming COBOL PERFORM paragraph calls to Python function calls.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is a PERFORM paragraph call."""
        tokens = node.get_tokens()
        return any(token.text == "PERFORM" for token in tokens if hasattr(token, 'text')) and \
               not any(token.text in ["UNTIL", "TIMES"] for token in tokens if hasattr(token, 'text'))
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL PERFORM paragraph to Python function call.
        
        COBOL:
            PERFORM A000-COUNT
            
        Python:
            a000_count()
        """
        tokens = node.get_tokens()
        paragraph_name = None
        
        # Find paragraph name after PERFORM
        for i, token in enumerate(tokens):
            if hasattr(token, 'text') and token.text == "PERFORM" and i + 1 < len(tokens):
                paragraph_name = tokens[i + 1].text
                break
        
        if paragraph_name:
            python_name = self.sanitize_python_name(paragraph_name)
            self.add_line(f"{python_name}()")
    
    def sanitize_python_name(self, cobol_name: str) -> str:
        """Convert COBOL paragraph name to Python function name."""
        return cobol_name.lower().replace('-', '_')
    
    def get_priority(self) -> int:
        return 20


class EvaluateRule(BaseRule):
    """
    Rule for transforming COBOL EVALUATE statements to Python if/elif/else.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is an EVALUATE statement."""
        return (node.rule_name == "EvaluateStatementContext" or
                any(token.text == "EVALUATE" for token in node.get_tokens() if hasattr(token, 'text')))
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL EVALUATE to Python if/elif/else.
        
        COBOL:
            EVALUATE CHOICE
                WHEN 1
                    DISPLAY 'ONE'
                WHEN 2
                    DISPLAY 'TWO'
                WHEN OTHER
                    DISPLAY 'OTHER'
            END-EVALUATE
            
        Python:
            if choice == 1:
                print('ONE')
            elif choice == 2:
                print('TWO')
            else:
                print('OTHER')
        """
        # TODO: Implement EVALUATE translation
        self.add_line("# TODO: Implement EVALUATE translation")
    
    def get_priority(self) -> int:
        return 15


class GoBackRule(BaseRule):
    """
    Rule for transforming COBOL GOBACK to Python return.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is a GOBACK statement."""
        return (node.rule_name == "GoBackStatementContext" or
                any(token.text == "GOBACK" for token in node.get_tokens() if hasattr(token, 'text')))
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL GOBACK to Python return.
        
        COBOL:
            GOBACK
            
        Python:
            return
        """
        self.add_line("return")
    
    def get_priority(self) -> int:
        return 10 