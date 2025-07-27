"""
IR to Python Code Generator

This module generates Python code from the language-agnostic IR representation.
"""

from typing import List, Dict, Any
from ..ir.ir import (
    IRNode, IRProgram, IRFunction, IRVariable, IRAssignment, IROperation,
    IRLiteral, IRIdentifier, IRLoop, IRInput, IROutput, IRReturn,
    IRConditional, IRCall, IRVisitor, IRType, IROperator
)

class IRToPythonGenerator(IRVisitor):
    """
    Generates Python code from IR nodes.
    """
    
    def __init__(self):
        self.generated_code: List[str] = []
        self.indent_level = 0
        self.current_function: IRFunction = None
    
    def generate(self, program: IRProgram) -> str:
        """
        Generate Python code from an IR program.
        """
        self.generated_code = []
        self.visit(program)
        return '\n'.join(self.generated_code)
    
    def add_line(self, line: str):
        """Add a line to the generated code with proper indentation."""
        indent = "    " * self.indent_level
        self.generated_code.append(indent + line)
    
    def visit_program(self, node: IRProgram):
        """Visit program node."""
        self.add_line("# Generated Python code from IR")
        self.add_line("")
        
        # Add variable declarations
        if node.variables:
            self.add_line("# Variable declarations")
            for var in node.variables:
                self.visit_variable(var)
            self.add_line("")
        
        # Add functions
        for func in node.functions:
            self.visit_function(func)
        
        # Add main function call
        self.add_line("")
        self.add_line("if __name__ == '__main__':")
        self.indent_level += 1
        self.add_line("main()")
        self.indent_level -= 1
    
    def visit_function(self, node: IRFunction):
        """Visit function node."""
        params_str = ", ".join(node.params) if node.params else ""
        self.add_line(f"def {node.value}({params_str}):")
        self.indent_level += 1
        
        # Visit function body
        for stmt in node.body:
            self.visit(stmt)
        
        self.indent_level -= 1
        self.add_line("")
    
    def visit_variable(self, node: IRVariable):
        """Visit variable node."""
        if node.var_type == 'str':
            self.add_line(f"{node.value} = ''")
        elif node.var_type == 'int':
            self.add_line(f"{node.value} = 0")
        elif node.var_type == 'float':
            self.add_line(f"{node.value} = 0.0")
        elif node.var_type == 'bool':
            self.add_line(f"{node.value} = False")
        else:
            self.add_line(f"{node.value} = None")
    
    def visit_assignment(self, node: IRAssignment):
        """Visit assignment node."""
        value_code = self.visit(node.value)
        self.add_line(f"{node.target} = {value_code}")
    
    def visit_operation(self, node: IROperation):
        """Visit operation node."""
        left_code = self.visit(node.left)
        if node.right:
            right_code = self.visit(node.right)
            return f"({left_code} {node.operator.value} {right_code})"
        else:
            return f"{node.operator.value}({left_code})"
    
    def visit_literal(self, node: IRLiteral):
        """Visit literal node."""
        if node.literal_type == 'str':
            return node.actual_value
        else:
            return str(node.actual_value)
    
    def visit_identifier(self, node: IRIdentifier):
        """Visit identifier node."""
        return node.value
    
    def visit_loop(self, node: IRLoop):
        """Visit loop node."""
        condition_code = self.visit(node.condition)
        self.add_line(f"while {condition_code}:")
        self.indent_level += 1
        
        for stmt in node.body:
            self.visit(stmt)
        
        self.indent_level -= 1
    
    def visit_conditional(self, node: IRConditional):
        """Visit conditional node."""
        condition_code = self.visit(node.condition)
        self.add_line(f"if {condition_code}:")
        self.indent_level += 1
        
        for stmt in node.then_body:
            self.visit(stmt)
        
        self.indent_level -= 1
        
        if node.else_body:
            self.add_line("else:")
            self.indent_level += 1
            
            for stmt in node.else_body:
                self.visit(stmt)
            
            self.indent_level -= 1
    
    def visit_call(self, node: IRCall):
        """Visit function call node."""
        args_code = ", ".join([self.visit(arg) for arg in node.args])
        return f"{node.value}({args_code})"
    
    def visit_input(self, node: IRInput):
        """Visit input node."""
        if node.input_type == 'int':
            self.add_line(f"{node.target} = int(input())")
        elif node.input_type == 'float':
            self.add_line(f"{node.target} = float(input())")
        else:
            self.add_line(f"{node.target} = input()")
    
    def visit_output(self, node: IROutput):
        """Visit output node."""
        if len(node.values) == 1:
            value_code = self.visit(node.values[0])
            self.add_line(f"print({value_code})")
        else:
            # Handle multiple values with concatenation
            value_codes = [self.visit(val) for val in node.values]
            # For now, simple concatenation - could be enhanced
            if all(isinstance(val, IRLiteral) and val.literal_type == 'str' for val in node.values):
                # All strings, concatenate directly
                combined = " + ".join(value_codes)
                self.add_line(f"print({combined})")
            else:
                # Mixed types, use str() conversion
                converted = [f"str({code})" for code in value_codes]
                combined = " + ".join(converted)
                self.add_line(f"print({combined})")
    
    def visit_return(self, node: IRReturn):
        """Visit return node."""
        if node.value:
            value_code = self.visit(node.value)
            self.add_line(f"return {value_code}")
        else:
            self.add_line("return")
    
    def visit_default(self, node: IRNode):
        """Default visitor method."""
        for child in node.children:
            self.visit(child)
        return None

def generate_python_from_ir(program: IRProgram) -> str:
    """
    Generate Python code from an IR program.
    """
    generator = IRToPythonGenerator()
    return generator.generate(program) 