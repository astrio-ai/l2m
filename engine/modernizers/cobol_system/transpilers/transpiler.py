"""
COBOL to Python Transpiler with IR-based Code Generation

This module provides functionality to translate COBOL source code into Python,
following the flow: COBOL → LST → IR → Jinja2 Template → Python
"""

import os
import sys
from typing import Dict, List, Optional, Any
from ..parsers.cobol_lst import parse_cobol_source, CobolSemanticAnalyzer, LosslessNode
from ..ir.cobol_to_ir import CobolToIRTranslator
from ..generators.template_generator import TemplateGenerator
from ..generators.python_generator import IRToPythonGenerator
from ..rules.rule_engine import RuleEngine
from .edge_case_detector import EdgeCaseDetector

class CobolTranspiler:
    """
    IR-based transpiler that converts COBOL to Python.
    
    Flow: COBOL → LST → IR → Jinja2 Template → Python
    """
    
    def __init__(self, use_templates: bool = True):
        self.variables: Dict[str, Dict[str, Any]] = {}
        self.rule_engine = RuleEngine()
        self.edge_case_detector = EdgeCaseDetector()
        self.ir_translator = CobolToIRTranslator()
        self.template_generator = TemplateGenerator() if use_templates else None
        self.python_generator = IRToPythonGenerator()
        self.use_templates = use_templates
        
    def transpile_file(self, cobol_file_path: str) -> str:
        """
        Transpile a COBOL file to Python.
        """
        with open(cobol_file_path, 'r') as f:
            cobol_source = f.read()
        
        return self.transpile_source(cobol_source, cobol_file_path)
    
    def transpile_source(self, cobol_source: str, file_path: str = "") -> str:
        """
        Transpile COBOL source code to Python.
        
        Flow: COBOL → LST → IR → Jinja2 Template → Python
        """
        try:
            # Step 1: COBOL → LST (Lossless Syntax Tree)
            print("🔍 Parsing COBOL source...")
            lst, tokens = parse_cobol_source(cobol_source)
            analyzer = CobolSemanticAnalyzer(lst, tokens)
            analyzer.analyze()
            
            # Step 2: Detect edge cases
            edge_cases = self.edge_case_detector.detect_edge_cases(lst, "root")
            self.edge_case_detector.log_edge_cases(edge_cases, file_path)
            
            # Step 3: Extract variables and set up rule engine
            self.extract_variables_from_lst(analyzer.lst_root)
            self.extract_variables(analyzer.symbol_table_root)
            self.rule_engine.set_variables(self.variables)
            
            # Step 4: LST → IR (Intermediate Representation)
            print("🔄 Converting to IR...")
            ir_program = self.ir_translator.translate_program(analyzer)
            
            # Step 5: IR → Python (using templates or direct generator)
            print("🚀 Generating Python code...")
            if self.use_templates and self.template_generator:
                python_code = self.template_generator.generate_python_from_ir(ir_program)
            else:
                python_code = self.python_generator.generate(ir_program)
            
            return python_code
            
        except Exception as e:
            error_msg = f"Transpilation failed: {str(e)}"
            print(f"❌ {error_msg}")
            raise Exception(error_msg)
    
    def transpile_with_analysis(self, cobol_source: str, file_path: str = "") -> Dict[str, Any]:
        """
        Transpile COBOL with detailed analysis information.
        
        Returns:
            Dictionary containing transpilation results and analysis
        """
        try:
            # Parse and analyze
            lst, tokens = parse_cobol_source(cobol_source)
            analyzer = CobolSemanticAnalyzer(lst, tokens)
            analyzer.analyze()
            
            # Extract variables
            self.extract_variables_from_lst(analyzer.lst_root)
            self.extract_variables(analyzer.symbol_table_root)
            self.rule_engine.set_variables(self.variables)
            
            # Convert to IR
            ir_program = self.ir_translator.translate_program(analyzer)
            
            # Generate Python
            if self.use_templates and self.template_generator:
                python_code = self.template_generator.generate_python_from_ir(ir_program)
            else:
                python_code = self.python_generator.generate(ir_program)
            
            return {
                'success': True,
                'lst_root': lst,
                'analyzer': analyzer,
                'ir_program': ir_program,
                'python_code': python_code,
                'variables': analyzer.variables,
                'paragraphs': analyzer.paragraphs,
                'sections': analyzer.sections
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'error_type': type(e).__name__
            }
    
    def extract_variables(self, symbol_node):
        """
        Extract variable information from symbol table.
        """
        if hasattr(symbol_node, 'kind') and symbol_node.kind in ('variable', 'constant'):
            if hasattr(symbol_node, 'name'):
                self.variables[symbol_node.name] = {
                    'kind': symbol_node.kind,
                    'metadata': getattr(symbol_node, 'metadata', {}),
                    'python_type': self.get_python_type(getattr(symbol_node, 'metadata', {}))
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
                        
                        # Extract variable name - look for the full name before PIC
                        pic_start = remaining.find('PIC')
                        if pic_start != -1:
                            name_text = remaining[name_start:pic_start].strip()
                            # Split by spaces and take the first part
                            name_parts = name_text.split()
                            if name_parts:
                                name = name_parts[0]
                            else:
                                name = name_text
                        else:
                            # Fallback to original method
                            name_end = name_start
                            for j in range(name_start, len(remaining)):
                                if remaining[j] in [' ', 'P', 'I', 'C', '(', ')', '.', 'V', 'A', 'L', 'U', 'E']:
                                    break
                                name_end = j + 1
                            
                            if name_start < name_end:
                                name = remaining[name_start:name_end]
                                name = name.rstrip('-')
                        
                        # Clean up the name - remove any trailing characters that aren't part of the variable name
                        if name:
                            # Remove trailing non-alphanumeric characters except hyphens
                            while name and not name[-1].isalnum() and name[-1] != '-':
                                name = name[:-1]
                            # Remove trailing hyphens
                            name = name.rstrip('-')
                        
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

def transpile_cobol_file(input_file: str, output_file: str = None) -> str:
    """
    Convenience function to transpile a COBOL file.
    
    Args:
        input_file: Path to COBOL file
        output_file: Optional output file path
        
    Returns:
        Generated Python code as string
    """
    transpiler = CobolTranspiler()
    return transpiler.transpile_file(input_file)

def transpile_cobol_source(cobol_source: str, output_file: str = None) -> str:
    """
    Convenience function to transpile COBOL source code.
    
    Args:
        cobol_source: COBOL source code as string
        output_file: Optional output file path
        
    Returns:
        Generated Python code as string
    """
    transpiler = CobolTranspiler()
    return transpiler.transpile_source(cobol_source, output_file=output_file) 