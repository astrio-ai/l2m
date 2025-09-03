"""
File I/O Transformation Rules

This module contains rules for transforming COBOL file I/O operations
to Python equivalents.
"""

from typing import Dict, Any, List, Optional
from .base_rule import BaseRule
from ..parsers.cobol_lst import LosslessNode


class FileSelectRule(BaseRule):
    """
    Rule for transforming COBOL SELECT statements to Python file handling.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is a SELECT statement node."""
        tokens = node.get_tokens()
        token_texts = [t.text for t in tokens if hasattr(t, 'text') and t.text]
        return 'SELECT' in token_texts
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL SELECT statement to Python file handling.
        
        COBOL:
            SELECT INPUT-FILE ASSIGN TO 'INPUT.TXT'
            SELECT OUTPUT-FILE ASSIGN TO 'OUTPUT.TXT'
            
        Python:
            input_file = open('INPUT.TXT', 'r')
            output_file = open('OUTPUT.TXT', 'w')
        """
        self.generated_code = []
        tokens = node.get_tokens()
        
        # Extract file name and assignment
        file_name = None
        file_path = None
        
        i = 0
        while i < len(tokens):
            token = tokens[i]
            if hasattr(token, 'text') and token.text:
                if token.text == 'SELECT':
                    # Next token should be file name
                    if i + 1 < len(tokens):
                        file_name = tokens[i + 1].text
                elif token.text == 'ASSIGN' and i + 2 < len(tokens):
                    # Look for file path after ASSIGN TO
                    if tokens[i + 1].text == 'TO':
                        file_path = tokens[i + 2].text.strip("'")
                elif token.text == 'TO' and i + 1 < len(tokens):
                    file_path = tokens[i + 1].text.strip("'")
            i += 1
        
        if file_name and file_path:
            python_file_name = self.sanitize_python_name(file_name)
            # Determine file mode based on context (simplified)
            mode = 'r'  # Default to read mode
            self.add_line(f"{python_file_name} = open('{file_path}', '{mode}')")
        
        return '\n'.join(self.generated_code)
    
    def get_priority(self) -> int:
        """High priority for file operations."""
        return 90


class FileOpenRule(BaseRule):
    """
    Rule for transforming COBOL OPEN statements to Python file operations.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is an OPEN statement node."""
        tokens = node.get_tokens()
        token_texts = [t.text for t in tokens if hasattr(t, 'text') and t.text]
        return 'OPEN' in token_texts
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL OPEN statement to Python file operations.
        
        COBOL:
            OPEN INPUT INPUT-FILE
            OPEN OUTPUT OUTPUT-FILE
            
        Python:
            # Files are already opened in SELECT statements
            pass
        """
        self.generated_code = []
        tokens = node.get_tokens()
        
        # Extract file name and mode
        file_name = None
        mode = None
        
        i = 0
        while i < len(tokens):
            token = tokens[i]
            if hasattr(token, 'text') and token.text:
                if token.text == 'OPEN':
                    # Next token should be mode (INPUT/OUTPUT)
                    if i + 1 < len(tokens):
                        mode = tokens[i + 1].text
                    # Next token should be file name
                    if i + 2 < len(tokens):
                        file_name = tokens[i + 2].text
            i += 1
        
        if file_name:
            python_file_name = self.sanitize_python_name(file_name)
            if mode == 'INPUT':
                self.add_line(f"# {python_file_name} opened for reading")
            elif mode == 'OUTPUT':
                self.add_line(f"# {python_file_name} opened for writing")
            else:
                self.add_line(f"# {python_file_name} opened")
        
        return '\n'.join(self.generated_code)
    
    def get_priority(self) -> int:
        """High priority for file operations."""
        return 90


class FileReadRule(BaseRule):
    """
    Rule for transforming COBOL READ statements to Python file reading.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is a READ statement node."""
        tokens = node.get_tokens()
        token_texts = [t.text for t in tokens if hasattr(t, 'text') and t.text]
        return 'READ' in token_texts
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL READ statement to Python file reading.
        
        COBOL:
            READ INPUT-FILE
                AT END MOVE 'Y' TO EOF-FLAG
            END-READ
            
        Python:
            line = input_file.readline()
            if not line:
                eof_flag = 'Y'
        """
        self.generated_code = []
        tokens = node.get_tokens()
        
        # Extract file name and AT END clause
        file_name = None
        at_end_variable = None
        
        i = 0
        while i < len(tokens):
            token = tokens[i]
            if hasattr(token, 'text') and token.text:
                if token.text == 'READ':
                    # Next token should be file name
                    if i + 1 < len(tokens):
                        file_name = tokens[i + 1].text
                elif token.text == 'AT' and i + 1 < len(tokens) and tokens[i + 1].text == 'END':
                    # Look for the variable to set
                    if i + 3 < len(tokens) and tokens[i + 2].text == 'MOVE':
                        at_end_variable = tokens[i + 4].text
                    elif i + 2 < len(tokens):
                        # Handle case where AT END is followed by a value and variable
                        at_end_value = tokens[i + 2].text.strip("'")
                        if i + 4 < len(tokens) and tokens[i + 3].text == 'TO':
                            at_end_variable = tokens[i + 4].text
            i += 1
        
        if file_name:
            python_file_name = self.sanitize_python_name(file_name)
            self.add_line(f"line = {python_file_name}.readline()")
            
            if at_end_variable:
                python_var = self.sanitize_python_name(at_end_variable)
                self.add_line(f"if not line:")
                self.indent_level += 1
                self.add_line(f"{python_var} = 'Y'")
                self.indent_level -= 1
        
        return '\n'.join(self.generated_code)
    
    def get_priority(self) -> int:
        """High priority for file operations."""
        return 90


class FileWriteRule(BaseRule):
    """
    Rule for transforming COBOL WRITE statements to Python file writing.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is a WRITE statement node."""
        tokens = node.get_tokens()
        token_texts = [t.text for t in tokens if hasattr(t, 'text') and t.text]
        return 'WRITE' in token_texts
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL WRITE statement to Python file writing.
        
        COBOL:
            WRITE OUTPUT-RECORD FROM INPUT-RECORD
            
        Python:
            output_file.write(input_record)
        """
        self.generated_code = []
        tokens = node.get_tokens()
        
        # Extract record name and FROM clause
        record_name = None
        from_record = None
        
        i = 0
        while i < len(tokens):
            token = tokens[i]
            if hasattr(token, 'text') and token.text:
                if token.text == 'WRITE':
                    # Next token should be record name
                    if i + 1 < len(tokens):
                        record_name = tokens[i + 1].text
                elif token.text == 'FROM' and i + 1 < len(tokens):
                    from_record = tokens[i + 1].text
            i += 1
        
        if record_name:
            python_record = self.sanitize_python_name(record_name)
            if from_record:
                python_from_record = self.sanitize_python_name(from_record)
                self.add_line(f"# Write {python_record} from {python_from_record}")
                self.add_line(f"output_file.write({python_from_record})")
            else:
                self.add_line(f"output_file.write({python_record})")
        
        return '\n'.join(self.generated_code)
    
    def get_priority(self) -> int:
        """High priority for file operations."""
        return 90


class FileCloseRule(BaseRule):
    """
    Rule for transforming COBOL CLOSE statements to Python file closing.
    """
    
    def can_apply(self, node: LosslessNode) -> bool:
        """Check if this is a CLOSE statement node."""
        tokens = node.get_tokens()
        token_texts = [t.text for t in tokens if hasattr(t, 'text') and t.text]
        return 'CLOSE' in token_texts
    
    def apply(self, node: LosslessNode) -> str:
        """
        Transform COBOL CLOSE statement to Python file closing.
        
        COBOL:
            CLOSE INPUT-FILE
            CLOSE OUTPUT-FILE
            
        Python:
            input_file.close()
            output_file.close()
        """
        self.generated_code = []
        tokens = node.get_tokens()
        
        # Extract file names
        file_names = []
        
        i = 0
        while i < len(tokens):
            token = tokens[i]
            if hasattr(token, 'text') and token.text:
                if token.text == 'CLOSE':
                    # Next token should be file name
                    if i + 1 < len(tokens):
                        file_names.append(tokens[i + 1].text)
                    # Check for additional file names
                    j = i + 2
                    while j < len(tokens) and hasattr(tokens[j], 'text') and tokens[j].text:
                        if tokens[j].text not in ['CLOSE', 'INPUT', 'OUTPUT']:
                            file_names.append(tokens[j].text)
                        j += 1
            i += 1
        
        for file_name in file_names:
            python_file_name = self.sanitize_python_name(file_name)
            self.add_line(f"{python_file_name}.close()")
        
        return '\n'.join(self.generated_code)
    
    def get_priority(self) -> int:
        """High priority for file operations."""
        return 90 