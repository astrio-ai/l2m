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
from ..parser.cobol_lst import LosslessNode, CobolSemanticAnalyzer

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
        Extract variables from COBOL LST.
        """
        def find_data_entries(node: LosslessNode):
            if node.rule_name == "DataDescriptionEntryContext":
                tokens = node.get_tokens()
                token_texts = [t.text for t in tokens if hasattr(t, 'text') and t.text.strip()]
                
                level = None
                name = None
                pic = None
                
                # Find level number
                for i, token in enumerate(token_texts):
                    if token.isdigit() and i == 0:
                        level = token
                        break
                
                # Find variable name
                for i, token in enumerate(token_texts):
                    if i > 0 and token and not token.isdigit() and not token.startswith('PIC') and token not in ['(', ')', '.', 'TO', 'FROM', 'GIVING']:
                        if not name:
                            name = token
                        break
                
                # Find PIC clause
                for i, token in enumerate(token_texts):
                    if token == 'PIC' and i + 1 < len(token_texts):
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
            self.translate_statement(child, function)
        else:
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
            
            return self.builder.assignment(destination, value)
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
    
    def translate_display_node(self, node: LosslessNode) -> Optional[IROutput]:
        """Translate DISPLAY node into IR output."""
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