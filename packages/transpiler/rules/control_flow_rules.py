"""
Control Flow Transformation Rules

This module contains rules for transforming COBOL control flow structures
to Python equivalents.
"""

from typing import Dict, Any, List, Optional
from .base_rule import BaseRule
from ..engine.parser.cobol_lst import LosslessNode


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
        
        return '\n'.join(self.generated_code)
    
    def get_priority(self) -> int:
        """High priority for control flow rules."""
        return 100


class PerformUntilRule(BaseRule):
    """
    Rule for transforming COBOL PERFORM UNTIL loops to Python while loops.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is a PERFORM UNTIL statement."""
        tokens = node.get_tokens()
        token_texts = [t.text for t in tokens if hasattr(t, 'text') and t.text]
        return 'PERFORM' in token_texts and 'UNTIL' in token_texts
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL PERFORM UNTIL to Python while loop.
        
        COBOL:
            PERFORM UNTIL MORE-DATA = 'NO'
                DISPLAY 'ENTER NAME'
                ACCEPT CUST-NO-IN
            END-PERFORM
            
        Python:
            while not (more_data == 'NO'):
                print('ENTER NAME')
                cust_no_in = input()
        """
        self.generated_code = []
        tokens = node.get_tokens()
        
        # Extract condition
        condition = None
        i = 0
        
        while i < len(tokens):
            token = tokens[i]
            if hasattr(token, 'text') and token.text == 'UNTIL' and i + 1 < len(tokens):
                # Build condition from next tokens
                condition_parts = []
                for j in range(i + 1, len(tokens)):
                    t = tokens[j]
                    if hasattr(t, 'text') and t.text:
                        if t.text in ['END-PERFORM', 'PERFORM']:
                            break
                        condition_parts.append(t.text)
                condition = " ".join(condition_parts)
                break
            i += 1
        
        if condition:
            # For PERFORM UNTIL, we need to negate the condition
            python_condition = self.convert_cobol_condition(condition)
            self.add_line(f"while not ({python_condition}):")
            self.indent_level += 1
            
            # TODO: Parse and translate statements inside the loop
            self.add_line("# TODO: Generate loop body statements")
            
            self.indent_level -= 1
        
        return '\n'.join(self.generated_code)
    
    def get_priority(self) -> int:
        """High priority for control flow rules."""
        return 100


class PerformTimesRule(BaseRule):
    """
    Rule for transforming COBOL PERFORM TIMES loops to Python for loops.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is a PERFORM TIMES statement."""
        tokens = node.get_tokens()
        token_texts = [t.text for t in tokens if hasattr(t, 'text') and t.text]
        return 'PERFORM' in token_texts and 'TIMES' in token_texts
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL PERFORM TIMES to Python for loop.
        
        COBOL:
            PERFORM A000-COUNT 10 TIMES
                ADD 1 TO COUNTER
            END-PERFORM
            
        Python:
            for _ in range(10):
                counter += 1
        """
        self.generated_code = []
        tokens = node.get_tokens()
        
        # Extract loop count and paragraph name
        paragraph_name = None
        loop_count = None
        
        i = 0
        while i < len(tokens):
            token = tokens[i]
            if hasattr(token, 'text') and token.text:
                if token.text == 'PERFORM':
                    # Next token should be paragraph name
                    if i + 1 < len(tokens):
                        paragraph_name = tokens[i + 1].text
                elif token.text.isdigit():
                    loop_count = token.text
                elif token.text == 'TIMES':
                    break
            i += 1
        
        if paragraph_name and loop_count:
            python_paragraph = self.sanitize_python_name(paragraph_name)
            self.add_line(f"for _ in range({loop_count}):")
            self.indent_level += 1
            self.add_line(f"{python_paragraph}()")
            self.indent_level -= 1
        elif loop_count:
            self.add_line(f"for _ in range({loop_count}):")
            self.indent_level += 1
            self.add_line("# TODO: Generate loop body statements")
            self.indent_level -= 1
        
        return '\n'.join(self.generated_code)
    
    def get_priority(self) -> int:
        """High priority for control flow rules."""
        return 100


class EvaluateRule(BaseRule):
    """
    Rule for transforming COBOL EVALUATE statements to Python if/elif/else.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is an EVALUATE statement."""
        tokens = node.get_tokens()
        token_texts = [t.text for t in tokens if hasattr(t, 'text') and t.text]
        return 'EVALUATE' in token_texts
    
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
        self.generated_code = []
        tokens = node.get_tokens()
        
        # TODO: Implement full EVALUATE parsing
        self.add_line("# EVALUATE statement - convert to if/elif/else")
        self.add_line("# TODO: Implement full EVALUATE translation")
        
        return '\n'.join(self.generated_code)
    
    def get_priority(self) -> int:
        """Medium priority for control flow rules."""
        return 80 