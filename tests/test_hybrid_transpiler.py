#!/usr/bin/env python3
"""
Hybrid transpiler tests for COBOL to Python conversion with LLM integration.
"""

import os
import sys
import unittest
import pytest

# Add the project root to Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "../..")))

from engine.modernizers.cobol_system.transpilers.hybrid_transpiler import HybridTranspiler


@pytest.mark.cobol
@pytest.mark.integration
class TestHybridTranspiler(unittest.TestCase):
    """Test hybrid COBOL to Python transpilation with LLM integration."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.transpiler = HybridTranspiler()
        self.test_data_dir = os.path.join(os.path.dirname(__file__), "../../examples/cobol")
        self.output_dir = os.path.join(os.path.dirname(__file__), "../../output/modernized-python")
        
        # Ensure output directory exists
        os.makedirs(self.output_dir, exist_ok=True)
    
    def test_hybrid_hello_world(self):
        """Test HELLO.cobol with hybrid transpiler."""
        input_file = os.path.join(self.test_data_dir, "HELLO.cobol")
        
        if os.path.exists(input_file):
            result = self.transpiler.transpile_file(input_file)
            
            # Check that the result contains expected Python code
            self.assertIn("def main():", result)
            self.assertIn("print('HELLO WORLD!')", result)
            self.assertIn("if __name__ == '__main__':", result)
            
            # Save result
            output_file = os.path.join(self.output_dir, "HELLO_hybrid.py")
            with open(output_file, 'w') as f:
                f.write(result)
            
            print(f"✅ HELLO.cobol (hybrid) -> {output_file}")
    
    def test_hybrid_if_statement(self):
        """Test IF_TEST.cobol with hybrid transpiler."""
        input_file = os.path.join(self.test_data_dir, "IF_TEST.cobol")
        
        if os.path.exists(input_file):
            result = self.transpiler.transpile_file(input_file)
            
            # Check that the result contains variable declarations
            self.assertIn("test_var = ''", result)
            self.assertIn("def main():", result)
            
            # Save result
            output_file = os.path.join(self.output_dir, "IF_TEST_hybrid.py")
            with open(output_file, 'w') as f:
                f.write(result)
            
            print(f"✅ IF_TEST.cobol (hybrid) -> {output_file}")
    
    def test_hybrid_string_operations(self):
        """Test PAYROL00.cobol with hybrid transpiler."""
        input_file = os.path.join(self.test_data_dir, "PAYROL00.cobol")
        
        if os.path.exists(input_file):
            result = self.transpiler.transpile_file(input_file)
            
            # Check that the result contains string operations
            self.assertIn("who = \"Captain COBOL\"", result)
            self.assertIn("print(who)", result)
            self.assertIn("def main():", result)
            
            # Save result
            output_file = os.path.join(self.output_dir, "PAYROL00_hybrid.py")
            with open(output_file, 'w') as f:
                f.write(result)
            
            print(f"✅ PAYROL00.cobol (hybrid) -> {output_file}")
    
    def test_translation_stats(self):
        """Test getting translation statistics."""
        input_file = os.path.join(self.test_data_dir, "HELLO.cobol")
        
        if os.path.exists(input_file):
            # Transpile to populate stats
            self.transpiler.transpile_file(input_file)
            
            # Get translation stats
            stats = self.transpiler.get_translation_stats()
            
            # Check that stats contain expected keys
            self.assertIn('total_edge_cases', stats)
            self.assertIn('ai_translations', stats)
            self.assertIn('llm_available', stats)
            
            print(f"✅ Translation stats: {stats}")


if __name__ == '__main__':
    unittest.main() 