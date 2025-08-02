#!/usr/bin/env python3
"""
Basic transpilation tests for COBOL to Python conversion.
"""

import os
import sys
import unittest
import pytest

# Add the project root to Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "../..")))

from engine.modernizers.cobol_system.transpilers.transpiler import CobolTranspiler


@pytest.mark.cobol
@pytest.mark.unit
class TestBasicTranspilation(unittest.TestCase):
    """Test basic COBOL to Python transpilation."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.transpiler = CobolTranspiler()
        self.test_data_dir = os.path.join(os.path.dirname(__file__), "../../examples/cobol")
        self.output_dir = os.path.join(os.path.dirname(__file__), "../../output/modernized-python")
        
        # Ensure output directory exists
        os.makedirs(self.output_dir, exist_ok=True)
    
    def test_hello_world(self):
        """Test HELLO.cobol transpilation."""
        input_file = os.path.join(self.test_data_dir, "HELLO.cobol")
        
        if os.path.exists(input_file):
            result = self.transpiler.transpile_file(input_file)
            
            # Check that the result contains expected Python code
            self.assertIn("def main():", result)
            self.assertIn("print('HELLO WORLD!')", result)
            self.assertIn("if __name__ == '__main__':", result)
            
            # Save result
            output_file = os.path.join(self.output_dir, "HELLO.py")
            with open(output_file, 'w') as f:
                f.write(result)
            
            print(f"✅ HELLO.cobol -> {output_file}")
    
    def test_if_statement(self):
        """Test IF_TEST.cobol transpilation."""
        input_file = os.path.join(self.test_data_dir, "IF_TEST.cobol")
        
        if os.path.exists(input_file):
            result = self.transpiler.transpile_file(input_file)
            
            # Check that the result contains variable declarations
            self.assertIn("test_var = ''", result)
            self.assertIn("def main():", result)
            
            # Save result
            output_file = os.path.join(self.output_dir, "IF_TEST.py")
            with open(output_file, 'w') as f:
                f.write(result)
            
            print(f"✅ IF_TEST.cobol -> {output_file}")
    
    def test_perform_loop(self):
        """Test PERFORM_TEST.cobol transpilation."""
        input_file = os.path.join(self.test_data_dir, "PERFORM_TEST.cobol")
        
        if os.path.exists(input_file):
            result = self.transpiler.transpile_file(input_file)
            
            # Check that the result contains loop variables
            self.assertIn("counter = 0", result)
            self.assertIn("def main():", result)
            
            # Save result
            output_file = os.path.join(self.output_dir, "PERFORM_TEST.py")
            with open(output_file, 'w') as f:
                f.write(result)
            
            print(f"✅ PERFORM_TEST.cobol -> {output_file}")
    
    def test_arithmetic_operations(self):
        """Test ADDAMT.cobol transpilation."""
        input_file = os.path.join(self.test_data_dir, "ADDAMT.cobol")
        
        if os.path.exists(input_file):
            result = self.transpiler.transpile_file(input_file)
            
            # Check that the result contains numeric variables
            self.assertIn("amt1_ = 0", result)
            self.assertIn("def main():", result)
            
            # Save result
            output_file = os.path.join(self.output_dir, "ADDAMT.py")
            with open(output_file, 'w') as f:
                f.write(result)
            
            print(f"✅ ADDAMT.cobol -> {output_file}")
    
    def test_string_operations(self):
        """Test PAYROL00.cobol transpilation."""
        input_file = os.path.join(self.test_data_dir, "PAYROL00.cobol")
        
        if os.path.exists(input_file):
            result = self.transpiler.transpile_file(input_file)
            
            # Check that the result contains string operations
            self.assertIn("who = \"Captain COBOL\"", result)
            self.assertIn("print(who)", result)
            self.assertIn("def main():", result)
            
            # Save result
            output_file = os.path.join(self.output_dir, "PAYROL00.py")
            with open(output_file, 'w') as f:
                f.write(result)
            
            print(f"✅ PAYROL00.cobol -> {output_file}")


if __name__ == '__main__':
    unittest.main() 