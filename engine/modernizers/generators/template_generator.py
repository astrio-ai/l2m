"""
Template-Based Code Generator

This module uses Jinja2 templates to generate code from IR representations.
"""

import os
from typing import Dict, Any, List
from jinja2 import Environment, FileSystemLoader, Template
from ..ir.ir import IRProgram, IRFunction, IRVariable, IRNode

class TemplateGenerator:
    """
    Generates code from IR using Jinja2 templates.
    """
    
    def __init__(self, template_dir: str = None):
        """
        Initialize the template generator.
        
        Args:
            template_dir: Directory containing Jinja2 templates
        """
        if template_dir is None:
            # Default to the templates directory relative to this file
            current_dir = os.path.dirname(os.path.abspath(__file__))
            template_dir = os.path.join(current_dir, '..', 'templates')
        
        self.template_dir = template_dir
        self.env = Environment(
            loader=FileSystemLoader(template_dir),
            trim_blocks=True,
            lstrip_blocks=True,
            keep_trailing_newline=True
        )
        
        # Add custom filters
        self.env.filters['python_type'] = self.python_type_filter
        self.env.filters['python_value'] = self.python_value_filter
        self.env.filters['python_name'] = self.python_name_filter
        self.env.filters['indent'] = self.indent_filter
        self.env.filters['render_expression'] = self.render_expression_filter
    
    def python_type_filter(self, var_type: str) -> str:
        """Convert IR type to Python type."""
        type_map = {
            'str': 'str',
            'int': 'int', 
            'float': 'float',
            'bool': 'bool'
        }
        return type_map.get(var_type, 'str')
    
    def python_value_filter(self, value: Any, var_type: str) -> str:
        """Convert IR value to Python value."""
        if var_type == 'str':
            return "''"
        elif var_type == 'int':
            return '0'
        elif var_type == 'float':
            return '0.0'
        elif var_type == 'bool':
            return 'False'
        else:
            return 'None'
    
    def python_name_filter(self, name: str) -> str:
        """Convert COBOL-style names to Python snake_case."""
        if not name:
            return name
        
        # Convert COBOL-style names (TEST-VAR) to Python snake_case (test_var)
        # Replace hyphens with underscores and convert to lowercase
        python_name = name.replace('-', '_').lower()
        
        # Handle special cases like numbers at the start
        if python_name and python_name[0].isdigit():
            python_name = 'var_' + python_name
        
        return python_name
    
    def indent_filter(self, text: str, width: int = 4) -> str:
        """Indent text by specified width."""
        if not text:
            return text
        lines = text.split('\n')
        indented_lines = [' ' * width + line if line.strip() else line for line in lines]
        return '\n'.join(indented_lines)
    
    def render_expression_filter(self, expr: Dict[str, Any]) -> str:
        """Render expression data as Python code."""
        if not expr:
            return ''
        
        if expr['type'] == 'literal':
            if expr['literal_type'] == 'str':
                return expr['value']
            else:
                return str(expr['value'])
        elif expr['type'] == 'identifier':
            return expr['name']
        elif expr['type'] == 'operation':
            left = self.render_expression_filter(expr['left'])
            if expr['right']:
                right = self.render_expression_filter(expr['right'])
                return f"({left} {expr['operator']} {right})"
            else:
                return f"{expr['operator']}({left})"
        else:
            return str(expr)
    
    def generate_python_from_ir(self, ir_program: IRProgram) -> str:
        """
        Generate Python code from IR program using templates.
        
        Args:
            ir_program: The IR program to generate code from
            
        Returns:
            Generated Python code as string
        """
        # Prepare template context
        context = self._prepare_context(ir_program)
        
        # Load and render the main template
        template = self.env.get_template('python/main.py.j2')
        return template.render(**context)
    
    def _prepare_context(self, ir_program: IRProgram) -> Dict[str, Any]:
        """
        Prepare context data for template rendering.
        
        Args:
            ir_program: The IR program
            
        Returns:
            Dictionary with template context data
        """
        # Extract variables
        variables = []
        for var in ir_program.variables:
            variables.append({
                'name': var.value,
                'type': var.var_type,
                'initial_value': var.initial_value
            })
        
        # Extract functions
        functions = []
        for func in ir_program.functions:
            function_data = {
                'name': func.value,
                'params': func.params,
                'body': self._extract_function_body(func)
            }
            functions.append(function_data)
        
        return {
            'program_name': ir_program.value,
            'variables': variables,
            'functions': functions,
            'has_variables': len(variables) > 0,
            'has_functions': len(functions) > 0
        }
    
    def _extract_function_body(self, func: IRFunction) -> List[Dict[str, Any]]:
        """
        Extract function body statements for template rendering.
        
        Args:
            func: The IR function
            
        Returns:
            List of statement dictionaries
        """
        statements = []
        
        for stmt in func.body:
            stmt_data = self._extract_statement_data(stmt)
            if stmt_data:
                statements.append(stmt_data)
        
        return statements
    
    def _extract_statement_data(self, stmt: IRNode) -> Dict[str, Any]:
        """
        Extract statement data for template rendering.
        
        Args:
            stmt: The IR statement node
            
        Returns:
            Dictionary with statement data
        """
        if stmt.type.value == 'assignment':
            return {
                'type': 'assignment',
                'target': stmt.target,
                'value': self._extract_expression_data(stmt.value)
            }
        elif stmt.type.value == 'output':
            return {
                'type': 'output',
                'output_values': [self._extract_expression_data(val) for val in stmt.values]
            }
        elif stmt.type.value == 'input':
            return {
                'type': 'input',
                'target': stmt.target,
                'input_type': stmt.input_type
            }
        elif stmt.type.value == 'loop':
            return {
                'type': 'loop',
                'condition': self._extract_expression_data(stmt.condition),
                'body': [self._extract_statement_data(body_stmt) for body_stmt in stmt.body]
            }
        elif stmt.type.value == 'return':
            return {
                'type': 'return',
                'value': self._extract_expression_data(stmt.value) if stmt.value else None
            }
        elif stmt.type.value == 'function':
            return {
                'type': 'function',
                'name': stmt.value,
                'params': stmt.params,
                'body': [self._extract_statement_data(body_stmt) for body_stmt in stmt.body]
            }
        
        return None
    
    def _extract_expression_data(self, expr: IRNode) -> Dict[str, Any]:
        """
        Extract expression data for template rendering.
        
        Args:
            expr: The IR expression node
            
        Returns:
            Dictionary with expression data
        """
        if expr.type.value == 'literal':
            return {
                'type': 'literal',
                'value': expr.actual_value,
                'literal_type': expr.literal_type
            }
        elif expr.type.value == 'identifier':
            return {
                'type': 'identifier',
                'name': expr.value
            }
        elif expr.type.value == 'operation':
            return {
                'type': 'operation',
                'operator': expr.operator.value,
                'left': self._extract_expression_data(expr.left),
                'right': self._extract_expression_data(expr.right) if expr.right else None
            }
        
        return None

def generate_python_from_ir_template(ir_program: IRProgram) -> str:
    """
    Convenience function to generate Python code from IR using templates.
    
    Args:
        ir_program: The IR program to generate code from
        
    Returns:
        Generated Python code as string
    """
    generator = TemplateGenerator()
    return generator.generate_python_from_ir(ir_program) 