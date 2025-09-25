"""
COBOL to IR Translator

This module translates COBOL LST nodes into a language-agnostic IR representation.
"""

from typing import List, Dict, Any, Optional
from .ir import (
    IRBuilder, IRNode, IRProgram, IRFunction, IRVariable, IRAssignment,
    IROperation, IRLiteral, IRIdentifier, IRLoop, IRInput, IROutput,
    IRReturn, IROperator
)
from ..parsers.cobol_lst import LosslessNode, CobolSemanticAnalyzer

class CobolToIRTranslator:
    """
    Translates COBOL LST nodes into language-agnostic IR.
    """
    
    def __init__(self):
        self.builder = IRBuilder()
        self.variables: Dict[str, Dict[str, Any]] = {}
        self.current_function: Optional[IRFunction] = None
    
    def translate_program(self, analyzer: CobolSemanticAnalyzer) -> IRProgram:
        """
        Translate a COBOL program into IR.
        """
        # Create the main program
        program = self.builder.program("main")
        
        # Extract variables from LST
        self.extract_variables_from_lst(analyzer.lst_root)
        
        # Add variables to program
        for var_name, var_info in self.variables.items():
            if not var_name.isdigit() and var_name not in ['01', '05', '77']:
                var_type = var_info.get('python_type', 'str')
                initial_value = self.get_initial_value(var_type)
                variable = self.builder.variable(var_name, var_type, initial_value)
                program.add_variable(variable)
        
        # Create main function
        main_function = self.builder.function("main")
        program.add_function(main_function)
        
        # Translate procedure division
        self.current_function = main_function
        self.translate_procedure_division(analyzer.lst_root, main_function)
        
        return program
    
    def extract_variables_from_lst(self, lst_root: LosslessNode):
        """
        Extract variables from COBOL LST using text-based parsing.
        """
        def find_data_entries(node: LosslessNode):
            if node.rule_name == "DataDescriptionEntryContext":
                text = node.get_text()
                
                # Parse the text to extract level, name, and PIC
                level = None
                name = None
                pic = None
                
                # Handle concatenated text like "01TEST-VARPICX(10)."
                # Look for level number (01, 05, 77, etc.)
                for i in range(len(text) - 1):
                    if text[i:i+2].isdigit():
                        level = text[i:i+2]
                        # Look for variable name after level
                        remaining = text[i+2:]
                        # Find the start of the variable name (after level)
                        name_start = 0
                        for j, char in enumerate(remaining):
                            if char.isalpha() or char == '-':
                                name_start = j
                                break
                        
                        # Extract variable name
                        name_end = name_start
                        for j in range(name_start, len(remaining)):
                            if remaining[j] in [' ', 'P', 'I', 'C', '(', ')', '.']:
                                break
                            name_end = j + 1
                        
                        if name_start < name_end:
                            name = remaining[name_start:name_end]
                        
                        # Find PIC clause
                        pic_start = remaining.find('PIC')
                        if pic_start != -1:
                            pic_text = remaining[pic_start:]
                            # Extract PIC content
                            pic_end = pic_text.find('.')
                            if pic_end != -1:
                                pic = pic_text[:pic_end]
                            else:
                                pic = pic_text
                        break
                
                if name and level and name != level and not name.isdigit():
                    python_name = self._convert_to_python_name(name)
                    self.variables[python_name] = {
                        'kind': 'variable',
                        'level': level,
                        'pic': pic,
                        'python_type': self.get_python_type_from_pic(pic) if pic else 'str',
                        'original_name': name
                    }
            
            for child in node.children:
                find_data_entries(child)
        
        find_data_entries(lst_root)
    
    def get_python_type_from_pic(self, pic: str) -> str:
        """Convert COBOL PIC clause to Python type."""
        if not pic:
            return 'str'
        
        pic = pic.upper()
        
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
            return 'str'
    
    def get_initial_value(self, var_type: str) -> Any:
        """Get initial value for a variable type."""
        if var_type == 'str':
            return ''
        elif var_type == 'int':
            return 0
        elif var_type == 'float':
            return 0.0
        elif var_type == 'bool':
            return False
        else:
            return ''
    
    def translate_procedure_division(self, node: LosslessNode, function: IRFunction):
        """
        Translate COBOL procedure division into IR statements.
        """
        if node.rule_name == "ProcedureDivisionContext":
            for child in node.children:
                self.translate_procedure_division(child, function)
        elif node.rule_name == "ProcedureDivisionBodyContext":
            for child in node.children:
                self.translate_procedure_division(child, function)
        elif node.rule_name == "ParagraphsContext":
            for child in node.children:
                self.translate_procedure_division(child, function)
        elif node.rule_name == "ParagraphContext":
            self.translate_paragraph(node, function)
        elif node.rule_name == "SentenceContext":
            self.translate_sentence(node, function)
        elif node.rule_name == "StatementContext":
            self.translate_statement(node, function)
        elif node.rule_name in ["DisplayStatementContext", "GobackStatementContext", "MoveStatementContext"]:
            # Direct statement translation
            self.translate_statement(node, function)
        else:
            # Recursively process children
            for child in node.children:
                self.translate_procedure_division(child, function)
    
    def translate_paragraph(self, node: LosslessNode, function: IRFunction):
        """
        Translate COBOL paragraph into IR function.
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
            # Create a new function for the paragraph
            paragraph_func = self.builder.function(paragraph_name.lower().replace('-', '_'))
            function.body.append(paragraph_func)
            
            # Translate statements in paragraph
            for child in node.children:
                if child.rule_name == "StatementContext":
                    self.translate_statement(child, paragraph_func)
                elif child.rule_name == "SentenceContext":
                    self.translate_sentence(child, paragraph_func)
        else:
            # If no paragraph name, translate directly to main function
            for child in node.children:
                if child.rule_name == "StatementContext":
                    self.translate_statement(child, function)
                elif child.rule_name == "SentenceContext":
                    self.translate_sentence(child, function)
    
    def translate_sentence(self, node: LosslessNode, function: IRFunction):
        """
        Translate COBOL sentence into IR statements.
        """
        tokens = node.get_tokens()
        token_texts = [t.text for t in tokens if hasattr(t, 'text') and t.text.strip()]
        
        # Check if we have a PERFORM UNTIL structure
        has_perform_until = any(token_texts[i] == 'PERFORM' and i + 1 < len(token_texts) and token_texts[i + 1] == 'UNTIL' 
                               for i in range(len(token_texts)))
        
        if has_perform_until:
            self.translate_perform_until_sentence(token_texts, function)
        else:
            # Try to find individual statements in children
            for child in node.children:
                if child.rule_name == "StatementContext":
                    self.translate_statement(child, function)
                elif child.rule_name in ["DisplayStatementContext", "GobackStatementContext", "MoveStatementContext"]:
                    self.translate_statement(child, function)
    
    def translate_perform_until_sentence(self, token_texts: List[str], function: IRFunction):
        """
        Translate PERFORM UNTIL sentence into IR loop.
        """
        i = 0
        while i < len(token_texts):
            if token_texts[i] == 'PERFORM' and i + 1 < len(token_texts) and token_texts[i + 1] == 'UNTIL':
                i += 2  # Skip PERFORM UNTIL
                
                # Extract condition
                condition_parts = []
                while i < len(token_texts) and token_texts[i] not in ['DISPLAY', 'ACCEPT', 'MOVE', 'ADD', 'SUBTRACT', 'GOBACK', 'PERFORM', 'INSPECT']:
                    condition_parts.append(token_texts[i])
                    i += 1
                
                condition = self.translate_condition(" ".join(condition_parts))
                
                # Parse statements inside the loop
                loop_body = []
                while i < len(token_texts) and token_texts[i] != 'END-PERFORM':
                    if token_texts[i] == 'DISPLAY':
                        output_node = self.translate_display_statement(token_texts, i)
                        if output_node:
                            loop_body.append(output_node)
                        i = self.skip_to_next_statement(token_texts, i)
                    elif token_texts[i] == 'ACCEPT':
                        input_node = self.translate_accept_statement(token_texts, i)
                        if input_node:
                            loop_body.append(input_node)
                        i = self.skip_to_next_statement(token_texts, i)
                    elif token_texts[i] == 'MOVE':
                        assignment_node = self.translate_move_statement(token_texts, i)
                        if assignment_node:
                            loop_body.append(assignment_node)
                        i = self.skip_to_next_statement(token_texts, i)
                    elif token_texts[i] == 'ADD':
                        assignment_node = self.translate_add_statement(token_texts, i)
                        if assignment_node:
                            loop_body.append(assignment_node)
                        i = self.skip_to_next_statement(token_texts, i)
                    elif token_texts[i] == 'INSPECT':
                        assignment_node = self.translate_inspect_statement(token_texts, i)
                        if assignment_node:
                            loop_body.append(assignment_node)
                        i = self.skip_to_next_statement(token_texts, i)
                    else:
                        i += 1
                
                # Create loop node
                loop_node = self.builder.loop(condition, loop_body)
                function.body.append(loop_node)
                
                if i < len(token_texts) and token_texts[i] == 'END-PERFORM':
                    i += 1
            else:
                i += 1
    
    def translate_condition(self, condition: str) -> IRNode:
        """
        Translate COBOL condition into IR expression.
        """
        # Simple condition parsing - can be enhanced
        if ' == ' in condition:
            parts = condition.split(' == ')
            if len(parts) == 2:
                left = self.builder.identifier(parts[0].strip())
                right = self.builder.literal(parts[1].strip().strip("'"), "str")
                return self.builder.operation(IROperator.EQUAL, left, right)
        
        # Default to identifier
        return self.builder.identifier(condition.strip())
    
    def translate_display_statement(self, token_texts: List[str], start_idx: int) -> Optional[IROutput]:
        """
        Translate DISPLAY statement into IR output node.
        """
        i = start_idx + 1  # Skip DISPLAY
        values = []
        
        while i < len(token_texts) and token_texts[i] not in ['ACCEPT', 'MOVE', 'ADD', 'SUBTRACT', 'GOBACK', 'PERFORM', 'INSPECT', 'END-PERFORM']:
            if token_texts[i].startswith("'") and token_texts[i].endswith("'"):
                values.append(self.builder.literal(token_texts[i], "str"))
            elif token_texts[i] in self.variables:
                values.append(self.builder.identifier(token_texts[i]))
            i += 1
        
        if values:
            return self.builder.output(values)
        return None
    
    def translate_accept_statement(self, token_texts: List[str], start_idx: int) -> Optional[IRInput]:
        """
        Translate ACCEPT statement into IR input node.
        """
        i = start_idx + 1  # Skip ACCEPT
        if i < len(token_texts) and token_texts[i] in self.variables:
            var_name = token_texts[i]
            var_info = self.variables.get(var_name, {})
            input_type = var_info.get('python_type', 'str')
            return self.builder.input(var_name, input_type)
        return None
    
    def translate_move_statement(self, token_texts: List[str], start_idx: int) -> Optional[IRAssignment]:
        """
        Translate MOVE statement into IR assignment node.
        """
        i = start_idx + 1  # Skip MOVE
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
        
        if source and destination:
            if source.startswith("'") and source.endswith("'"):
                value = self.builder.literal(source, "str")
            elif source.isdigit():
                value = self.builder.literal(int(source), "int")
            else:
                value = self.builder.identifier(source)
            
            python_destination = self._convert_to_python_name(destination)
            return self.builder.assignment(python_destination, value)
        return None
    
    def translate_move_statement_concatenated(self, text: str) -> Optional[IRAssignment]:
        """
        Translate concatenated MOVE statement like "MOVE'JOHN'TOTEST-VAR".
        """
        # Remove "MOVE" prefix
        if text.startswith('MOVE'):
            text = text[4:]  # Remove "MOVE"
        
        # Find the source (before "TO")
        to_pos = text.find('TO')
        if to_pos == -1:
            return None
        
        source = text[:to_pos].strip()
        destination = text[to_pos + 2:].strip()  # Skip "TO"
        
        if source and destination:
            # Handle quoted strings
            if source.startswith("'") and source.endswith("'"):
                value = self.builder.literal(source, "str")
            elif source.isdigit():
                value = self.builder.literal(int(source), "int")
            else:
                value = self.builder.identifier(source)
            
            python_destination = self._convert_to_python_name(destination)
            return self.builder.assignment(python_destination, value)
        return None
    
    def translate_display_statement_concatenated(self, text: str) -> Optional[IROutput]:
        """
        Translate concatenated DISPLAY statement like "DISPLAY'HELLO WORLD'".
        """
        # Remove "DISPLAY" prefix
        if text.startswith('DISPLAY'):
            text = text[7:]  # Remove "DISPLAY"
        
        values = []
        
        # Look for quoted strings
        i = 0
        while i < len(text):
            if text[i] == "'":
                # Find the end quote
                start = i
                i += 1
                while i < len(text) and text[i] != "'":
                    i += 1
                if i < len(text):
                    # Extract the quoted string
                    quoted_text = text[start:i+1]
                    values.append(self.builder.literal(quoted_text, "str"))
                    i += 1
                else:
                    break
            elif text[i].isalnum() or text[i] == '-':
                # Look for variable names
                start = i
                while i < len(text) and (text[i].isalnum() or text[i] == '-'):
                    i += 1
                var_name = text[start:i]
                python_var_name = self._convert_to_python_name(var_name)
                if python_var_name in self.variables:
                    values.append(self.builder.identifier(python_var_name))
            else:
                i += 1
        
        if values:
            return self.builder.output(values)
        return None
    
    def translate_add_statement(self, token_texts: List[str], start_idx: int) -> Optional[IRAssignment]:
        """
        Translate ADD statement into IR assignment node.
        """
        i = start_idx + 1  # Skip ADD
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
        
        if result_var and len(operands) >= 2:
            # Create addition expression
            left = self.builder.identifier(operands[0])
            right = self.builder.identifier(operands[1])
            expr = self.builder.operation(IROperator.ADD, left, right)
            
            for operand in operands[2:]:
                expr = self.builder.operation(IROperator.ADD, expr, self.builder.identifier(operand))
            
            return self.builder.assignment(result_var, expr)
        return None
    
    def translate_inspect_statement(self, token_texts: List[str], start_idx: int) -> Optional[IRAssignment]:
        """
        Translate INSPECT statement into IR assignment node.
        """
        i = start_idx + 1  # Skip INSPECT
        variable = None
        
        while i < len(token_texts) and token_texts[i] != 'CONVERTING':
            if token_texts[i] in self.variables:
                variable = token_texts[i]
                break
            i += 1
        
        if variable:
            # Create upper() call
            var_id = self.builder.identifier(variable)
            # For now, just return the variable (upper() would need a method call node)
            return self.builder.assignment(variable, var_id)
        return None
    
    def skip_to_next_statement(self, token_texts: List[str], current_idx: int) -> int:
        """
        Skip to the next statement in token list.
        """
        i = current_idx
        while i < len(token_texts) and token_texts[i] not in ['DISPLAY', 'ACCEPT', 'MOVE', 'ADD', 'SUBTRACT', 'GOBACK', 'PERFORM', 'INSPECT', 'END-PERFORM']:
            i += 1
        return i
    
    def translate_statement(self, node: LosslessNode, function: IRFunction):
        """
        Translate individual COBOL statements into IR.
        """
        # First try to translate based on node type
        for child in node.children:
            if child.rule_name == "DisplayStatementContext":
                output_node = self.translate_display_node(child)
                if output_node:
                    function.body.append(output_node)
            elif child.rule_name == "AcceptStatementContext":
                input_node = self.translate_accept_node(child)
                if input_node:
                    function.body.append(input_node)
            elif child.rule_name == "GobackStatementContext":
                return_node = self.builder.return_stmt()
                function.body.append(return_node)
        
        # If no children were translated, try text-based translation
        if not any(child.rule_name in ["DisplayStatementContext", "AcceptStatementContext", "GobackStatementContext"] 
                  for child in node.children):
            text = node.get_text().strip()
            if text.startswith('DISPLAY'):
                # Parse concatenated DISPLAY statement manually
                output = self.translate_display_statement_concatenated(text)
                if output:
                    function.body.append(output)
            elif text.startswith('ACCEPT'):
                input_stmt = self.translate_accept_statement(text.split(), 0)
                if input_stmt:
                    function.body.append(input_stmt)
            elif text.startswith('MOVE'):
                # Parse concatenated MOVE statement manually
                assignment = self.translate_move_statement_concatenated(text)
                if assignment:
                    function.body.append(assignment)
            elif text.startswith('GOBACK'):
                return_stmt = self.builder.return_stmt()
                function.body.append(return_stmt)
    
    def translate_display_node(self, node: LosslessNode) -> Optional[IROutput]:
        """Translate DISPLAY node into IR output."""
        # Use the concatenated display method since tokens are empty
        text = node.get_text().strip()
        if text.startswith('DISPLAY'):
            return self.translate_display_statement_concatenated(text)
        
        # Fallback to token-based method
        tokens = node.get_tokens()
        values = []
        
        for token in tokens:
            if hasattr(token, 'text') and token.text:
                if token.text.startswith("'") and token.text.endswith("'"):
                    values.append(self.builder.literal(token.text, "str"))
                elif token.text in self.variables:
                    values.append(self.builder.identifier(token.text))
        
        if values:
            return self.builder.output(values)
        return None
    
    def translate_accept_node(self, node: LosslessNode) -> Optional[IRInput]:
        """Translate ACCEPT node into IR input."""
        tokens = node.get_tokens()
        
        for token in tokens:
            if hasattr(token, 'text') and token.text and token.text != 'ACCEPT':
                if token.text in self.variables:
                    var_info = self.variables.get(token.text, {})
                    input_type = var_info.get('python_type', 'str')
                    return self.builder.input(token.text, input_type)
        return None
    
    def _convert_to_python_name(self, name: str) -> str:
        """Convert COBOL-style name to Python snake_case."""
        if not name:
            return name
        
        # Convert COBOL-style names (TEST-VAR) to Python snake_case (test_var)
        # Replace hyphens with underscores and convert to lowercase
        python_name = name.replace('-', '_').lower()
        
        # Handle special cases like numbers at the start
        if python_name and python_name[0].isdigit():
            python_name = 'var_' + python_name
        
        return python_name 