"""
COBOL to Python Rule-Based Transpiler

This module provides functionality to translate COBOL source code into Python,
handling data definitions and basic procedural statements.
"""

import os
import sys
from typing import Dict, List, Optional, Any
from .parser.cobol_lst import parse_cobol_source, CobolSemanticAnalyzer, LosslessNode

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
                    
                python_var_name = self.sanitize_python_name(var_name)
                python_type = var_info.get('python_type', 'str')
                if python_type == 'str':
                    self.add_line(f"{python_var_name} = ''")
                elif python_type == 'int':
                    self.add_line(f"{python_var_name} = 0")
                elif python_type == 'float':
                    self.add_line(f"{python_var_name} = 0.0")
                elif python_type == 'bool':
                    self.add_line(f"{python_var_name} = False")
            self.add_line("")
    
    def generate_main_function(self, lst_root: LosslessNode):
        """
        Generate the main Python function from COBOL procedure division.
        """
        # First, generate paragraph functions
        self.generate_paragraph_functions(lst_root)
        
        # Then generate the main function that calls the first paragraph
        self.add_line("def main():")
        self.indent_level += 1
        
        # Find the first paragraph and call it
        first_paragraph = self.find_first_paragraph(lst_root)
        if first_paragraph:
            python_name = self.sanitize_python_name(first_paragraph)
            self.add_line(f"{python_name}()")
        else:
            # Fallback: translate procedure division directly
            self.translate_procedure_division(lst_root)
        
        self.indent_level -= 1
    
    def generate_paragraph_functions(self, lst_root: LosslessNode):
        """
        Generate separate functions for each COBOL paragraph.
        """
        # Find and translate all paragraphs
        def translate_paragraphs(node: LosslessNode):
            if node.rule_name == "ParagraphContext":
                self.translate_paragraph(node)
            else:
                for child in node.children:
                    translate_paragraphs(child)
        
        translate_paragraphs(lst_root)
    
    def find_first_paragraph(self, lst_root: LosslessNode) -> str:
        """
        Find the first paragraph name in the procedure division.
        """
        def find_paragraphs(node: LosslessNode):
            if node.rule_name == "ParagraphContext":
                for child in node.children:
                    if child.rule_name == "ParagraphNameContext":
                        tokens = child.get_tokens()
                        if tokens:
                            return tokens[0].text
            
            for child in node.children:
                result = find_paragraphs(child)
                if result:
                    return result
            return None
        
        return find_paragraphs(lst_root)
    
    def find_all_paragraphs(self, lst_root: LosslessNode) -> List[str]:
        """
        Find all paragraph names in the procedure division.
        """
        paragraphs = []
        
        def find_paragraphs(node: LosslessNode):
            if node.rule_name == "ParagraphContext":
                for child in node.children:
                    if child.rule_name == "ParagraphNameContext":
                        tokens = child.get_tokens()
                        if tokens:
                            paragraphs.append(tokens[0].text)
            
            for child in node.children:
                find_paragraphs(child)
        
        find_paragraphs(lst_root)
        return paragraphs
    
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
                               "PerformStatementContext", "InspectStatementContext", "IfStatementContext",
                               "EvaluateStatementContext"]:
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
                # For PERFORM UNTIL, we need to negate the condition
                # UNTIL A = B becomes while not (A == B)
                python_condition = self.convert_cobol_condition(condition)
                # Also sanitize variable names in the condition
                for var_name in self.variables:
                    if var_name in python_condition:
                        python_var_name = self.sanitize_python_name(var_name)
                        python_condition = python_condition.replace(var_name, python_var_name)
                
                # Normalize string comparisons - remove trailing spaces
                python_condition = python_condition.replace("'NO '", "'NO'")
                python_condition = python_condition.replace("'YES '", "'YES'")
                
                self.add_line(f"while not ({python_condition}):")
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
                                converted_parts = []
                                for part in display_parts:
                                    if part.startswith("'") and part.endswith("'"):
                                        # String literal
                                        converted_parts.append(part)
                                    else:
                                        # Variable - convert to string and sanitize
                                        python_var = self.sanitize_python_name(part)
                                        converted_parts.append(f"str({python_var})")
                                display_str = " + ' ' + ".join(converted_parts)
                            else:
                                display_str = display_parts[0]
                            self.add_line(f"print({display_str})")
                    
                    elif token_texts[i] == 'ACCEPT':
                        # Parse ACCEPT statement
                        i += 1
                        if i < len(token_texts) and token_texts[i] in self.variables:
                            var_name = token_texts[i]
                            python_var_name = self.sanitize_python_name(var_name)
                            var_info = self.variables.get(var_name, {})
                            python_type = var_info.get('python_type', 'str')
                            
                            if python_type == 'int':
                                self.add_line(f"{python_var_name} = int(input())")
                            elif python_type == 'float':
                                self.add_line(f"{python_var_name} = float(input())")
                            else:
                                self.add_line(f"{python_var_name} = input()")
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
                            python_dest = self.sanitize_python_name(destination)
                            if source.startswith("'") and source.endswith("'"):
                                self.add_line(f"{python_dest} = {source}")
                            elif source.isdigit():
                                self.add_line(f"{python_dest} = {source}")
                            else:
                                python_source = self.sanitize_python_name(source)
                                self.add_line(f"{python_dest} = {python_source}")
                    
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
                            python_result = self.sanitize_python_name(result_var)
                            python_operands = [self.sanitize_python_name(op) if op not in ['ADD', 'GIVING'] else op for op in operands]
                            expression = " + ".join(python_operands)
                            self.add_line(f"{python_result} = {expression}")
                        elif len(operands) >= 2:
                            destination = operands[-1]
                            python_dest = self.sanitize_python_name(destination)
                            sources = operands[:-1]
                            for source in sources:
                                python_source = self.sanitize_python_name(source)
                                self.add_line(f"{python_dest} += {python_source}")
                    
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
                            python_var = self.sanitize_python_name(variable)
                            self.add_line(f"{python_var} = {python_var}.upper()")
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
                    python_dest = self.sanitize_python_name(destination)
                    if source.startswith("'") and source.endswith("'"):
                        self.add_line(f"{python_dest} = {source}")
                    elif source.isdigit():
                        self.add_line(f"{python_dest} = {source}")
                    else:
                        python_source = self.sanitize_python_name(source)
                        self.add_line(f"{python_dest} = {python_source}")
            
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
                    python_result = self.sanitize_python_name(result_var)
                    python_operands = [self.sanitize_python_name(op) if op not in ['ADD', 'GIVING'] else op for op in operands]
                    expression = " + ".join(python_operands)
                    self.add_line(f"{python_result} = {expression}")
                elif len(operands) >= 2:
                    destination = operands[-1]
                    python_dest = self.sanitize_python_name(destination)
                    sources = operands[:-1]
                    for source in sources:
                        python_source = self.sanitize_python_name(source)
                        self.add_line(f"{python_dest} += {python_source}")
            
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
                    python_var = self.sanitize_python_name(variable)
                    self.add_line(f"{python_var} = {python_var}.upper()")
                    i += 1
            
            elif token == 'GOBACK':
                self.add_line("return")
                i += 1
            elif token == 'IF':
                # Parse IF statement
                i += 1
                condition_parts = []
                while i < len(token_texts) and token_texts[i] != 'THEN':
                    condition_parts.append(token_texts[i])
                    i += 1
                if condition_parts:
                    condition = " ".join(condition_parts)
                    python_condition = self.convert_cobol_condition(condition)
                    self.add_line(f"if {python_condition}:")
                    self.indent_level += 1
                    # TODO: Parse THEN and ELSE blocks
                    self.add_line("# TODO: Implement IF-THEN-ELSE blocks")
                    self.indent_level -= 1
            
            else:
                i += 1
    
    def sanitize_python_name(self, cobol_name: str) -> str:
        """
        Convert COBOL names to valid Python names.
        - Replace hyphens with underscores
        - Ensure names don't start with digits
        - Convert to lowercase
        """
        # Replace hyphens with underscores
        python_name = cobol_name.replace('-', '_')
        
        # If name starts with digit, add prefix
        if python_name[0].isdigit():
            python_name = f"para_{python_name}"
        
        # Convert to lowercase
        python_name = python_name.lower()
        
        return python_name
    
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
            python_name = self.sanitize_python_name(paragraph_name)
            self.add_line(f"def {python_name}():")
            self.indent_level += 1
            
            # Add global declarations for all variables
            if self.variables:
                global_vars = []
                for var_name in self.variables:
                    if not var_name.isdigit() and var_name not in ['01', '05', '77']:
                        python_var_name = self.sanitize_python_name(var_name)
                        global_vars.append(python_var_name)
                
                if global_vars:
                    self.add_line(f"global {', '.join(global_vars)}")
                    self.add_line("")
            
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
        elif node.rule_name == "IfStatementContext":
            self.translate_if_statement(node)
        elif node.rule_name == "EvaluateStatementContext":
            self.translate_evaluate_statement(node)
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
                    # Variable - sanitize the name
                    python_var_name = self.sanitize_python_name(token.text)
                    display_parts.append(python_var_name)
                elif token.text == 'DISPLAY':
                    continue  # Skip the DISPLAY keyword
        
        if display_parts:
            # Join multiple display items with proper type conversion
            if len(display_parts) > 1:
                # Convert each part to string and join
                converted_parts = []
                for part in display_parts:
                    if part.startswith("'") and part.endswith("'"):
                        # String literal
                        converted_parts.append(part)
                    else:
                        # Variable - convert to string
                        converted_parts.append(f"str({part})")
                display_str = " + ' ' + ".join(converted_parts)
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
            python_var_name = self.sanitize_python_name(variable_name)
            var_info = self.variables.get(variable_name, {})
            python_type = var_info.get('python_type', 'str')
            
            if python_type == 'int':
                self.add_line(f"{python_var_name} = int(input())")
            elif python_type == 'float':
                self.add_line(f"{python_var_name} = float(input())")
            else:
                self.add_line(f"{python_var_name} = input()")
    
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
            python_dest = self.sanitize_python_name(destination)
            if source.startswith("'") and source.endswith("'"):
                # String literal
                self.add_line(f"{python_dest} = {source}")
            elif source.isdigit():
                # Numeric literal
                self.add_line(f"{python_dest} = {source}")
            else:
                # Variable - sanitize source too
                python_source = self.sanitize_python_name(source)
                self.add_line(f"{python_dest} = {python_source}")
    
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
            python_result = self.sanitize_python_name(result_var)
            python_operands = [self.sanitize_python_name(op) if op not in ['ADD', 'GIVING'] else op for op in operands]
            expression = " + ".join(python_operands)
            self.add_line(f"{python_result} = {expression}")
        elif len(operands) >= 2:
            # Simple ADD: destination += source
            destination = operands[-1]
            python_dest = self.sanitize_python_name(destination)
            sources = operands[:-1]
            for source in sources:
                python_source = self.sanitize_python_name(source)
                self.add_line(f"{python_dest} += {python_source}")
    
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
            python_dest = self.sanitize_python_name(destination)
            sources = operands[:-1]
            for source in sources:
                python_source = self.sanitize_python_name(source)
                self.add_line(f"{python_dest} -= {python_source}")
    
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
                    python_func_name = self.sanitize_python_name(token.text)
                    self.add_line(f"{python_func_name}()")
    
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
        converting_from = None
        converting_to = None
        
        # Parse INSPECT CONVERTING
        for i, token in enumerate(tokens):
            if hasattr(token, 'text') and token.text:
                if token.text in self.variables:
                    variable = token.text
                elif token.text == 'CONVERTING' and i + 1 < len(tokens):
                    # Look for the FROM and TO parts
                    for j in range(i + 1, len(tokens)):
                        t = tokens[j]
                        if hasattr(t, 'text') and t.text == 'TO':
                            # Found TO, now look for the strings
                            if j > 0 and j + 1 < len(tokens):
                                converting_from = tokens[j-1].text if hasattr(tokens[j-1], 'text') else None
                                converting_to = tokens[j+1].text if hasattr(tokens[j+1], 'text') else None
                            break
        
        if variable:
            python_var = self.sanitize_python_name(variable)
            if converting_from and converting_to:
                # INSPECT CONVERTING with specific character mapping
                self.add_line(f"{python_var} = {python_var}.replace('{converting_from}', '{converting_to}')")
            else:
                # Simple INSPECT CONVERTING (case conversion)
                self.add_line(f"{python_var} = {python_var}.upper()")
    
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
        
        # Fix PERFORM UNTIL logic - invert the condition
        if 'UNTIL' in condition:
            # For PERFORM UNTIL, we need to negate the condition
            # UNTIL A = B becomes while not (A == B)
            condition = f"not ({condition})"
        
        return condition
    
    def translate_if_statement(self, node: LosslessNode):
        """
        Translate IF statement to Python if-elif-else.
        """
        tokens = node.get_tokens()
        condition = None
        then_statements = []
        else_statements = []
        
        # Parse IF-THEN-ELSE structure
        in_then = False
        in_else = False
        
        for i, token in enumerate(tokens):
            if hasattr(token, 'text') and token.text:
                if token.text == 'IF':
                    # Extract condition
                    condition_parts = []
                    j = i + 1
                    while j < len(tokens) and tokens[j].text != 'THEN':
                        if hasattr(tokens[j], 'text') and tokens[j].text:
                            condition_parts.append(tokens[j].text)
                        j += 1
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
        
        if condition:
            python_condition = self.convert_cobol_condition(condition)
            self.add_line(f"if {python_condition}:")
            self.indent_level += 1
            
            # Add THEN statements
            for stmt in then_statements:
                if stmt.strip():
                    self.add_line(f"# {stmt}")
            
            if else_statements:
                self.indent_level -= 1
                self.add_line("else:")
                self.indent_level += 1
                
                # Add ELSE statements
                for stmt in else_statements:
                    if stmt.strip():
                        self.add_line(f"# {stmt}")
            
            self.indent_level -= 1
    
    def translate_evaluate_statement(self, node: LosslessNode):
        """
        Translate EVALUATE statement to Python if-elif-else.
        """
        tokens = node.get_tokens()
        self.add_line("# EVALUATE statement - convert to if-elif-else")
        self.add_line("# TODO: Implement full EVALUATE translation")
    
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