"""
COBOL to Python Rule-Based Transpiler

This module provides functionality to translate COBOL source code into Python,
handling data definitions and basic procedural statements.
"""

import os
import sys
from typing import Dict, List, Optional, Any
from .cobol_lst import parse_cobol_source, CobolSemanticAnalyzer, LosslessNode

class CobolTranspiler:
    """
    Rule-based transpiler that converts COBOL to Python.
    """
    
    def __init__(self):
        self.variables: Dict[str, Dict[str, Any]] = {}
        self.generated_code: List[str] = []
        self.indent_level = 0
        self.loop_counter = 0
        
    def transpile_file(self, cobol_file_path: str) -> str:
        """
        Transpile a COBOL file to Python.
        """
        with open(cobol_file_path, 'r') as f:
            cobol_source = f.read()
        
        return self.transpile_source(cobol_source)
    
    def transpile_source(self, cobol_source: str) -> str:
        """
        Transpile COBOL source code to Python.
        """
        # Parse COBOL into LST
        lst, tokens = parse_cobol_source(cobol_source)
        analyzer = CobolSemanticAnalyzer(lst, tokens)
        analyzer.analyze()
        
        # Generate Python code
        self.generate_python_code(analyzer)
        
        return '\n'.join(self.generated_code)
    
    def generate_python_code(self, analyzer: CobolSemanticAnalyzer):
        """
        Generate Python code from the analyzed COBOL LST.
        """
        self.generated_code = []
        self.variables = {}
        self.loop_counter = 0
        
        # Add header
        self.add_line("# Generated Python code from COBOL")
        self.add_line("")
        
        # Extract variables from LST (more reliable than symbol table for complex structures)
        self.extract_variables_from_lst(analyzer.lst_root)
        
        # Also try symbol table as backup
        self.extract_variables(analyzer.symbol_table_root)
        
        # Generate variable declarations
        self.generate_variable_declarations()
        
        # Generate main function
        self.generate_main_function(analyzer.lst_root)
        
        # Add main function call
        self.add_line("")
        self.add_line("if __name__ == '__main__':")
        self.indent_level += 1
        self.add_line("main()")
        self.indent_level -= 1
    
    def extract_variables(self, symbol_node):
        """
        Extract variable information from symbol table.
        """
        if symbol_node.kind in ('variable', 'constant'):
            self.variables[symbol_node.name] = {
                'kind': symbol_node.kind,
                'metadata': symbol_node.metadata,
                'python_type': self.get_python_type(symbol_node.metadata)
            }
        
        for child in symbol_node.children:
            self.extract_variables(child)
    
    def extract_variables_from_lst(self, lst_root: LosslessNode):
        """
        Extract variables directly from the LST by looking for data description entries.
        """
        def find_data_entries(node: LosslessNode):
            if node.rule_name == "DataDescriptionEntryContext":
                # Extract variable information from data description entry
                tokens = node.get_tokens()
                token_texts = [t.text for t in tokens if hasattr(t, 'text') and t.text.strip()]
                
                # Parse tokens to extract level, name, and PIC
                level = None
                name = None
                pic = None
                
                # Find level number (first numeric token)
                for i, token in enumerate(token_texts):
                    if token.isdigit() and i == 0:
                        level = token
                        break
                
                # Find variable name (first non-numeric, non-keyword token after level)
                for i, token in enumerate(token_texts):
                    if i > 0 and token and not token.isdigit() and not token.startswith('PIC') and token not in ['(', ')', '.', 'TO', 'FROM', 'GIVING']:
                        # This should be the variable name
                        name = token
                        break
                
                # Find PIC clause
                for i, token in enumerate(token_texts):
                    if token == 'PIC' and i + 1 < len(token_texts):
                        # Extract PIC clause
                        pic_parts = []
                        for j in range(i + 1, len(token_texts)):
                            if token_texts[j] == '.' or token_texts[j] == 'VALUE':
                                break
                            pic_parts.append(token_texts[j])
                        pic = ''.join(pic_parts).strip().rstrip('.')
                        break
                
                if name and level and name != level and not name.isdigit():
                    self.variables[name] = {
                        'kind': 'variable',
                        'level': level,
                        'pic': pic,
                        'python_type': self.get_python_type_from_pic(pic) if pic else 'str'
                    }
            
            for child in node.children:
                find_data_entries(child)
        
        find_data_entries(lst_root)
    
    def get_python_type_from_pic(self, pic: str) -> str:
        """
        Convert COBOL PIC clause to Python type.
        """
        if not pic:
            return 'str'
        
        pic = pic.upper()
        
        if 'X' in pic:
            return 'str'
        elif '9' in pic:
            if 'V' in pic or '.' in pic:
                return 'float'
            else:
                return 'int'  # PIC 9(n) should be int, not float
        elif 'S' in pic:
            return 'int'
        else:
            return 'str'
    
    def get_python_type(self, metadata: Dict[str, Any]) -> str:
        """
        Convert COBOL PIC clause to Python type.
        """
        pic = metadata.get('pic', '').upper()
        level = metadata.get('level', '')
        
        if level == '88':
            return 'bool'  # Condition names are typically boolean
        
        if 'X' in pic:
            return 'str'
        elif '9' in pic:
            if 'V' in pic or '.' in pic:
                return 'float'
            else:
                return 'int'
        elif 'S' in pic:
            return 'int'
        else:
            return 'str'  # Default to string
    
    def generate_variable_declarations(self):
        """
        Generate Python variable declarations.
        """
        if self.variables:
            self.add_line("# Variable declarations")
            for var_name, var_info in self.variables.items():
                # Skip level numbers and invalid variable names
                if var_name.isdigit() or var_name in ['01', '05', '77']:
                    continue
                    
                python_type = var_info.get('python_type', 'str')
                if python_type == 'str':
                    self.add_line(f"{var_name} = ''")
                elif python_type == 'int':
                    self.add_line(f"{var_name} = 0")
                elif python_type == 'float':
                    self.add_line(f"{var_name} = 0.0")
                elif python_type == 'bool':
                    self.add_line(f"{var_name} = False")
            self.add_line("")
    
    def generate_main_function(self, lst_root: LosslessNode):
        """
        Generate the main Python function from COBOL procedure division.
        """
        self.add_line("def main():")
        self.indent_level += 1
        
        # Find procedure division and translate statements
        self.translate_procedure_division(lst_root)
        
        self.indent_level -= 1
    
    def translate_procedure_division(self, node: LosslessNode):
        """
        Translate COBOL procedure division statements to Python.
        """
        if node.rule_name == "ProcedureDivisionContext":
            for child in node.children:
                self.translate_procedure_division(child)
        elif node.rule_name == "ProcedureDivisionBodyContext":
            for child in node.children:
                self.translate_procedure_division(child)
        elif node.rule_name == "ParagraphsContext":
            for child in node.children:
                self.translate_procedure_division(child)
        elif node.rule_name == "StatementContext":
            self.translate_statement(node)
        elif node.rule_name == "ParagraphContext":
            self.translate_paragraph(node)
        elif node.rule_name == "SentenceContext":
            self.translate_sentence(node)
        elif node.rule_name in ["DisplayStatementContext", "AcceptStatementContext", "MoveStatementContext", 
                               "AddStatementContext", "SubtractStatementContext", "GobackStatementContext",
                               "PerformStatementContext", "InspectStatementContext"]:
            # Direct statement translation
            self.translate_statement(node)
        else:
            for child in node.children:
                self.translate_procedure_division(child)
    
    def translate_sentence(self, node: LosslessNode):
        """
        Translate COBOL sentence (group of statements) to Python.
        """
        # Get all tokens from the sentence
        tokens = node.get_tokens()
        token_texts = [t.text for t in tokens if hasattr(t, 'text') and t.text.strip()]
        
        # Check if we have a PERFORM UNTIL structure in the sentence
        has_perform_until = any(token_texts[i] == 'PERFORM' and i + 1 < len(token_texts) and token_texts[i + 1] == 'UNTIL' 
                               for i in range(len(token_texts)))
        
        if has_perform_until:
            # Use sentence text parsing for PERFORM UNTIL structures
            self.parse_sentence_text(token_texts)
        else:
            # Try to find individual statements in children first
            for child in node.children:
                if child.rule_name == "StatementContext":
                    self.translate_statement(child)
                elif child.rule_name in ["DisplayStatementContext", "AcceptStatementContext", "MoveStatementContext", 
                                       "AddStatementContext", "SubtractStatementContext", "GobackStatementContext",
                                       "PerformStatementContext", "InspectStatementContext"]:
                    self.translate_statement(child)
            
            # If no statements found in children, try to parse the sentence text
            if not any(child.rule_name in ["StatementContext", "DisplayStatementContext", "AcceptStatementContext", 
                                          "MoveStatementContext", "AddStatementContext", "SubtractStatementContext", 
                                          "GobackStatementContext", "PerformStatementContext", "InspectStatementContext"] 
                      for child in node.children):
                self.parse_sentence_text(token_texts)
    
    def parse_sentence_text(self, token_texts: List[str]):
        """
        Parse individual statements from sentence text.
        """
        i = 0
        while i < len(token_texts):
            token = token_texts[i]
            
            if token == 'PERFORM' and i + 1 < len(token_texts) and token_texts[i + 1] == 'UNTIL':
                # Parse PERFORM UNTIL loop
                i += 2  # Skip PERFORM UNTIL
                
                # Extract condition
                condition_parts = []
                while i < len(token_texts) and token_texts[i] not in ['DISPLAY', 'ACCEPT', 'MOVE', 'ADD', 'SUBTRACT', 'GOBACK', 'PERFORM', 'INSPECT']:
                    condition_parts.append(token_texts[i])
                    i += 1
                
                condition = " ".join(condition_parts)
                python_condition = self.convert_cobol_condition(condition)
                self.add_line(f"while {python_condition}:")
                self.indent_level += 1
                
                # Parse statements inside the loop
                while i < len(token_texts) and token_texts[i] != 'END-PERFORM':
                    if token_texts[i] == 'DISPLAY':
                        # Parse DISPLAY statement
                        display_parts = []
                        i += 1
                        while i < len(token_texts) and token_texts[i] not in ['ACCEPT', 'MOVE', 'ADD', 'SUBTRACT', 'GOBACK', 'PERFORM', 'INSPECT', 'END-PERFORM']:
                            if token_texts[i].startswith("'") and token_texts[i].endswith("'"):
                                display_parts.append(token_texts[i])
                            elif token_texts[i] in self.variables:
                                display_parts.append(token_texts[i])
                            i += 1
                        
                        if display_parts:
                            if len(display_parts) > 1:
                                # Handle multiple display items with proper concatenation
                                display_str = " + ".join(display_parts)
                            else:
                                display_str = display_parts[0]
                            self.add_line(f"print({display_str})")
                    
                    elif token_texts[i] == 'ACCEPT':
                        # Parse ACCEPT statement
                        i += 1
                        if i < len(token_texts) and token_texts[i] in self.variables:
                            var_name = token_texts[i]
                            var_info = self.variables.get(var_name, {})
                            python_type = var_info.get('python_type', 'str')
                            
                            if python_type == 'int':
                                self.add_line(f"{var_name} = int(input())")
                            elif python_type == 'float':
                                self.add_line(f"{var_name} = float(input())")
                            else:
                                self.add_line(f"{var_name} = input()")
                            i += 1
                    
                    elif token_texts[i] == 'MOVE':
                        # Parse MOVE statement
                        i += 1
                        source = None
                        destination = None
                        
                        while i < len(token_texts) and token_texts[i] != 'TO':
                            if not source:
                                source = token_texts[i]
                            i += 1
                        
                        if i < len(token_texts) and token_texts[i] == 'TO':
                            i += 1
                            if i < len(token_texts):
                                destination = token_texts[i]
                                i += 1
                        
                        if source and destination:
                            if source.startswith("'") and source.endswith("'"):
                                self.add_line(f"{destination} = {source}")
                            elif source.isdigit():
                                self.add_line(f"{destination} = {source}")
                            else:
                                self.add_line(f"{destination} = {source}")
                    
                    elif token_texts[i] == 'ADD':
                        # Parse ADD statement
                        i += 1
                        operands = []
                        result_var = None
                        
                        while i < len(token_texts) and token_texts[i] != 'GIVING':
                            if token_texts[i] not in ['ADD', 'TO']:
                                operands.append(token_texts[i])
                            i += 1
                        
                        if i < len(token_texts) and token_texts[i] == 'GIVING':
                            i += 1
                            if i < len(token_texts):
                                result_var = token_texts[i]
                                i += 1
                        
                        if result_var and len(operands) >= 2:
                            expression = " + ".join(operands)
                            self.add_line(f"{result_var} = {expression}")
                        elif len(operands) >= 2:
                            destination = operands[-1]
                            sources = operands[:-1]
                            for source in sources:
                                self.add_line(f"{destination} += {source}")
                    
                    elif token_texts[i] == 'INSPECT':
                        # Parse INSPECT statement
                        i += 1
                        variable = None
                        
                        while i < len(token_texts) and token_texts[i] != 'CONVERTING':
                            if token_texts[i] in self.variables:
                                variable = token_texts[i]
                                break
                            i += 1
                        
                        if variable:
                            self.add_line(f"{variable} = {variable}.upper()")
                            i += 1
                    
                    else:
                        i += 1
                
                # End of PERFORM UNTIL loop
                if i < len(token_texts) and token_texts[i] == 'END-PERFORM':
                    i += 1
                    self.indent_level -= 1
            
            elif token == 'DISPLAY':
                # Parse DISPLAY statement
                display_parts = []
                i += 1
                while i < len(token_texts) and token_texts[i] not in ['ACCEPT', 'MOVE', 'ADD', 'SUBTRACT', 'GOBACK', 'PERFORM', 'INSPECT']:
                    if token_texts[i].startswith("'") and token_texts[i].endswith("'"):
                        display_parts.append(token_texts[i])
                    elif token_texts[i] in self.variables:
                        display_parts.append(token_texts[i])
                    i += 1
                
                if display_parts:
                    if len(display_parts) > 1:
                        # Handle multiple display items with proper concatenation
                        display_str = " + ".join(display_parts)
                    else:
                        display_str = display_parts[0]
                    self.add_line(f"print({display_str})")
            
            elif token == 'ACCEPT':
                # Parse ACCEPT statement
                i += 1
                if i < len(token_texts) and token_texts[i] in self.variables:
                    var_name = token_texts[i]
                    var_info = self.variables.get(var_name, {})
                    python_type = var_info.get('python_type', 'str')
                    
                    if python_type == 'int':
                        self.add_line(f"{var_name} = int(input())")
                    elif python_type == 'float':
                        self.add_line(f"{var_name} = float(input())")
                    else:
                        self.add_line(f"{var_name} = input()")
                    i += 1
            
            elif token == 'MOVE':
                # Parse MOVE statement
                i += 1
                source = None
                destination = None
                
                while i < len(token_texts) and token_texts[i] != 'TO':
                    if not source:
                        source = token_texts[i]
                    i += 1
                
                if i < len(token_texts) and token_texts[i] == 'TO':
                    i += 1
                    if i < len(token_texts):
                        destination = token_texts[i]
                        i += 1
                
                if source and destination:
                    if source.startswith("'") and source.endswith("'"):
                        self.add_line(f"{destination} = {source}")
                    elif source.isdigit():
                        self.add_line(f"{destination} = {source}")
                    else:
                        self.add_line(f"{destination} = {source}")
            
            elif token == 'ADD':
                # Parse ADD statement
                i += 1
                operands = []
                result_var = None
                
                while i < len(token_texts) and token_texts[i] != 'GIVING':
                    if token_texts[i] not in ['ADD', 'TO']:
                        operands.append(token_texts[i])
                    i += 1
                
                if i < len(token_texts) and token_texts[i] == 'GIVING':
                    i += 1
                    if i < len(token_texts):
                        result_var = token_texts[i]
                        i += 1
                
                if result_var and len(operands) >= 2:
                    expression = " + ".join(operands)
                    self.add_line(f"{result_var} = {expression}")
                elif len(operands) >= 2:
                    destination = operands[-1]
                    sources = operands[:-1]
                    for source in sources:
                        self.add_line(f"{destination} += {source}")
            
            elif token == 'INSPECT':
                # Parse INSPECT statement
                i += 1
                variable = None
                
                while i < len(token_texts) and token_texts[i] != 'CONVERTING':
                    if token_texts[i] in self.variables:
                        variable = token_texts[i]
                        break
                    i += 1
                
                if variable:
                    self.add_line(f"{variable} = {variable}.upper()")
                    i += 1
            
            elif token == 'GOBACK':
                self.add_line("return")
                i += 1
            
            else:
                i += 1
    
    def translate_paragraph(self, node: LosslessNode):
        """
        Translate COBOL paragraph to Python function.
        """
        # Extract paragraph name
        paragraph_name = None
        for child in node.children:
            if child.rule_name == "ParagraphNameContext":
                tokens = child.get_tokens()
                if tokens:
                    paragraph_name = tokens[0].text
                    break
        
        if paragraph_name:
            self.add_line(f"def {paragraph_name.lower().replace('-', '_')}():")
            self.indent_level += 1
            
            # Translate statements in paragraph
            for child in node.children:
                if child.rule_name == "StatementContext":
                    self.translate_statement(child)
                elif child.rule_name in ["DisplayStatementContext", "AcceptStatementContext", "MoveStatementContext", 
                                       "AddStatementContext", "SubtractStatementContext", "GobackStatementContext",
                                       "PerformStatementContext", "InspectStatementContext"]:
                    self.translate_statement(child)
                elif child.rule_name == "SentenceContext":
                    self.translate_sentence(child)
            
            self.indent_level -= 1
            self.add_line("")
    
    def translate_statement(self, node: LosslessNode):
        """
        Translate individual COBOL statements to Python.
        """
        # Check if this is a StatementContext and look for statement types in children
        if node.rule_name == "StatementContext":
            for child in node.children:
                if child.rule_name == "DisplayStatementContext":
                    self.translate_display(child)
                elif child.rule_name == "AcceptStatementContext":
                    self.translate_accept(child)
                elif child.rule_name == "MoveStatementContext":
                    self.translate_move(child)
                elif child.rule_name == "AddStatementContext":
                    self.translate_add(child)
                elif child.rule_name == "SubtractStatementContext":
                    self.translate_subtract(child)
                elif child.rule_name == "GobackStatementContext":
                    self.translate_goback(child)
                elif child.rule_name == "PerformStatementContext":
                    self.translate_perform(child)
                elif child.rule_name == "InspectStatementContext":
                    self.translate_inspect(child)
        elif node.rule_name == "DisplayStatementContext":
            self.translate_display(node)
        elif node.rule_name == "AcceptStatementContext":
            self.translate_accept(node)
        elif node.rule_name == "MoveStatementContext":
            self.translate_move(node)
        elif node.rule_name == "AddStatementContext":
            self.translate_add(node)
        elif node.rule_name == "SubtractStatementContext":
            self.translate_subtract(node)
        elif node.rule_name == "GobackStatementContext":
            self.translate_goback(node)
        elif node.rule_name == "PerformStatementContext":
            self.translate_perform(node)
        elif node.rule_name == "InspectStatementContext":
            self.translate_inspect(node)
        else:
            # Try to find statement types in children
            for child in node.children:
                if child.rule_name == "DisplayStatementContext":
                    self.translate_display(child)
                elif child.rule_name == "AcceptStatementContext":
                    self.translate_accept(child)
                elif child.rule_name == "MoveStatementContext":
                    self.translate_move(child)
                elif child.rule_name == "AddStatementContext":
                    self.translate_add(child)
                elif child.rule_name == "SubtractStatementContext":
                    self.translate_subtract(child)
                elif child.rule_name == "GobackStatementContext":
                    self.translate_goback(child)
                elif child.rule_name == "PerformStatementContext":
                    self.translate_perform(child)
                elif child.rule_name == "InspectStatementContext":
                    self.translate_inspect(child)
    
    def translate_display(self, node: LosslessNode):
        """
        Translate DISPLAY statement to Python print().
        """
        tokens = node.get_tokens()
        display_parts = []
        
        for token in tokens:
            if hasattr(token, 'text') and token.text:
                if token.text.startswith("'") and token.text.endswith("'"):
                    # String literal
                    display_parts.append(token.text)
                elif token.text in self.variables:
                    # Variable
                    display_parts.append(token.text)
                elif token.text == 'DISPLAY':
                    continue  # Skip the DISPLAY keyword
        
        if display_parts:
            # Join multiple display items
            if len(display_parts) > 1:
                display_str = " + ' ' + ".join(display_parts)
            else:
                display_str = display_parts[0]
            self.add_line(f"print({display_str})")
    
    def translate_accept(self, node: LosslessNode):
        """
        Translate ACCEPT statement to Python input().
        """
        tokens = node.get_tokens()
        variable_name = None
        
        for token in tokens:
            if hasattr(token, 'text') and token.text and token.text != 'ACCEPT':
                if token.text in self.variables:
                    variable_name = token.text
                    break
        
        if variable_name:
            var_info = self.variables.get(variable_name, {})
            python_type = var_info.get('python_type', 'str')
            
            if python_type == 'int':
                self.add_line(f"{variable_name} = int(input())")
            elif python_type == 'float':
                self.add_line(f"{variable_name} = float(input())")
            else:
                self.add_line(f"{variable_name} = input()")
    
    def translate_move(self, node: LosslessNode):
        """
        Translate MOVE statement to Python assignment.
        """
        tokens = node.get_tokens()
        source = None
        destination = None
        
        # Find source and destination
        for i, token in enumerate(tokens):
            if hasattr(token, 'text') and token.text:
                if token.text == 'TO' and i > 0 and i < len(tokens) - 1:
                    source = tokens[i-1].text if hasattr(tokens[i-1], 'text') else None
                    destination = tokens[i+1].text if hasattr(tokens[i+1], 'text') else None
                    break
        
        if source and destination:
            if source.startswith("'") and source.endswith("'"):
                # String literal
                self.add_line(f"{destination} = {source}")
            elif source.isdigit():
                # Numeric literal
                self.add_line(f"{destination} = {source}")
            else:
                # Variable
                self.add_line(f"{destination} = {source}")
    
    def translate_add(self, node: LosslessNode):
        """
        Translate ADD statement to Python addition.
        """
        tokens = node.get_tokens()
        operands = []
        result_var = None
        
        # Parse ADD statement
        for token in tokens:
            if hasattr(token, 'text') and token.text:
                if token.text == 'GIVING':
                    # Find the result variable
                    for t in tokens:
                        if hasattr(t, 'text') and t.text and t.text not in ['ADD', 'GIVING']:
                            if t.text not in operands:
                                result_var = t.text
                                break
                    break
                elif token.text not in ['ADD', 'TO']:
                    operands.append(token.text)
        
        if result_var and len(operands) >= 2:
            # ADD with GIVING: result = op1 + op2 + op3
            expression = " + ".join(operands)
            self.add_line(f"{result_var} = {expression}")
        elif len(operands) >= 2:
            # Simple ADD: destination += source
            destination = operands[-1]
            sources = operands[:-1]
            for source in sources:
                self.add_line(f"{destination} += {source}")
    
    def translate_subtract(self, node: LosslessNode):
        """
        Translate SUBTRACT statement to Python subtraction.
        """
        tokens = node.get_tokens()
        operands = []
        
        for token in tokens:
            if hasattr(token, 'text') and token.text and token.text not in ['SUBTRACT', 'FROM']:
                operands.append(token.text)
        
        if len(operands) >= 2:
            destination = operands[-1]
            sources = operands[:-1]
            for source in sources:
                self.add_line(f"{destination} -= {source}")
    
    def translate_perform(self, node: LosslessNode):
        """
        Translate PERFORM statement to Python loop or function call.
        """
        tokens = node.get_tokens()
        token_texts = [t.text for t in tokens if hasattr(t, 'text') and t.text]
        
        if 'UNTIL' in token_texts:
            # PERFORM UNTIL loop
            self.translate_perform_until(node)
        else:
            # Simple PERFORM (function call)
            for token in tokens:
                if hasattr(token, 'text') and token.text and token.text not in ['PERFORM']:
                    self.add_line(f"{token.text.lower().replace('-', '_')}()")
    
    def translate_perform_until(self, node: LosslessNode):
        """
        Translate PERFORM UNTIL to Python while loop.
        """
        tokens = node.get_tokens()
        condition = None
        
        # Extract condition
        for i, token in enumerate(tokens):
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
        
        if condition:
            # Convert COBOL condition to Python
            python_condition = self.convert_cobol_condition(condition)
            self.add_line(f"while {python_condition}:")
            self.indent_level += 1
    
    def translate_inspect(self, node: LosslessNode):
        """
        Translate INSPECT statement to Python string manipulation.
        """
        tokens = node.get_tokens()
        variable = None
        
        for token in tokens:
            if hasattr(token, 'text') and token.text and token.text not in ['INSPECT', 'CONVERTING']:
                if token.text in self.variables:
                    variable = token.text
                    break
        
        if variable:
            # Simple INSPECT CONVERTING (case conversion)
            self.add_line(f"{variable} = {variable}.upper()")
    
    def convert_cobol_condition(self, condition: str) -> str:
        """
        Convert COBOL condition to Python condition.
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
        
        return condition
    
    def translate_goback(self, node: LosslessNode):
        """
        Translate GOBACK statement to Python return.
        """
        self.add_line("return")
    
    def add_line(self, line: str):
        """
        Add a line to the generated Python code with proper indentation.
        """
        indent = "    " * self.indent_level
        self.generated_code.append(indent + line)

def transpile_cobol_file(input_file: str, output_file: str = None):
    """
    Convenience function to transpile a COBOL file to Python.
    """
    transpiler = CobolTranspiler()
    python_code = transpiler.transpile_file(input_file)
    
    if output_file:
        with open(output_file, 'w') as f:
            f.write(python_code)
        print(f"Transpiled {input_file} to {output_file}")
    else:
        print(python_code)
    
    return python_code

# Example usage
if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python -m packages.transpiler.engine.cobol_transpiler <cobol_file> [output_file]")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else None
    
    transpile_cobol_file(input_file, output_file) 