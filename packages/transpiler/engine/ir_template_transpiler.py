"""
IR-Based Template Transpiler

This module provides a language-agnostic transpiler that uses an intermediate representation
and Jinja2 templates to convert source languages to target languages.
"""

import os
import sys
from typing import Dict, List, Optional, Any
from .parser.cobol_lst import parse_cobol_source, CobolSemanticAnalyzer
from .ir.cobol_to_ir import CobolToIRTranslator
from .generator.template_generator import generate_python_from_ir_template
from .ir.ir import IRProgram

class IRTemplateTranspiler:
    """
    Language-agnostic transpiler using intermediate representation and Jinja2 templates.
    """
    
    def __init__(self):
        self.cobol_to_ir = CobolToIRTranslator()
    
    def transpile_cobol_to_python(self, cobol_source: str) -> str:
        """
        Transpile COBOL source code to Python using IR and templates.
        """
        # Step 1: Parse COBOL into LST
        lst, tokens = parse_cobol_source(cobol_source)
        analyzer = CobolSemanticAnalyzer(lst, tokens)
        analyzer.analyze()
        
        # Step 2: Translate LST to IR
        ir_program = self.cobol_to_ir.translate_program(analyzer)
        
        # Step 3: Generate Python from IR using templates
        python_code = generate_python_from_ir_template(ir_program)
        
        return python_code
    
    def transpile_cobol_file_to_python(self, cobol_file_path: str) -> str:
        """
        Transpile a COBOL file to Python using IR and templates.
        """
        with open(cobol_file_path, 'r') as f:
            cobol_source = f.read()
        
        return self.transpile_cobol_to_python(cobol_source)
    
    def get_ir_from_cobol(self, cobol_source: str) -> IRProgram:
        """
        Get the IR representation from COBOL source (for debugging/analysis).
        """
        lst, tokens = parse_cobol_source(cobol_source)
        analyzer = CobolSemanticAnalyzer(lst, tokens)
        analyzer.analyze()
        
        return self.cobol_to_ir.translate_program(analyzer)

def transpile_cobol_file_with_templates(input_file: str, output_file: str = None) -> str:
    """
    Convenience function to transpile a COBOL file to Python using IR and templates.
    """
    transpiler = IRTemplateTranspiler()
    python_code = transpiler.transpile_cobol_file_to_python(input_file)
    
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
        print("Usage: python -m packages.transpiler.engine.ir_template_transpiler <cobol_file> [output_file]")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else None
    
    transpile_cobol_file_with_templates(input_file, output_file) 