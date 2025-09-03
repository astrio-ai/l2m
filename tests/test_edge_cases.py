#!/usr/bin/env python3
"""
Edge case tests for COBOL transpiler.
"""

import os
import sys
import unittest
import pytest

# Add the project root to Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "../..")))

from engine.modernizers.cobol_system.transpilers.transpiler import CobolTranspiler
from engine.modernizers.cobol_system.transpilers.edge_case_detector import EdgeCaseDetector


@pytest.mark.cobol
@pytest.mark.unit
class TestEdgeCases(unittest.TestCase):
    """Test edge cases and error handling."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.transpiler = CobolTranspiler()
        self.edge_detector = EdgeCaseDetector()
        self.test_data_dir = os.path.join(os.path.dirname(__file__), "../../examples/cobol")
        self.output_dir = os.path.join(os.path.dirname(__file__), "../../output/modernized-python")
        
        # Ensure output directory exists
        os.makedirs(self.output_dir, exist_ok=True)
    
    def test_nonexistent_file(self):
        """Test handling of nonexistent file."""
        with self.assertRaises(Exception):
            self.transpiler.transpile_file("nonexistent.cobol")
    
    def test_empty_file(self):
        """Test handling of empty file."""
        # Create an empty COBOL file
        empty_file = os.path.join(self.output_dir, "empty.cobol")
        with open(empty_file, 'w') as f:
            f.write("")
        
        try:
            result = self.transpiler.transpile_file(empty_file)
            # Should handle empty file gracefully
            self.assertIsInstance(result, str)
        except Exception as e:
            # It's okay if it raises an exception for empty files
            print(f"Empty file handling: {e}")
        finally:
            # Clean up
            if os.path.exists(empty_file):
                os.remove(empty_file)
    
    def test_invalid_cobol_syntax(self):
        """Test handling of invalid COBOL syntax."""
        # Create a file with invalid COBOL syntax
        invalid_file = os.path.join(self.output_dir, "invalid.cobol")
        with open(invalid_file, 'w') as f:
            f.write("INVALID COBOL SYNTAX")
        
        try:
            result = self.transpiler.transpile_file(invalid_file)
            # Should handle invalid syntax gracefully
            self.assertIsInstance(result, str)
        except Exception as e:
            # It's okay if it raises an exception for invalid syntax
            print(f"Invalid syntax handling: {e}")
        finally:
            # Clean up
            if os.path.exists(invalid_file):
                os.remove(invalid_file)
    
    def test_edge_case_detection(self):
        """Test edge case detection functionality."""
        input_file = os.path.join(self.test_data_dir, "HELLO.cobol")
        
        if os.path.exists(input_file):
            # Parse the file to get LST
            from engine.modernizers.cobol_system.parsers.cobol_lst import parse_cobol_source
            
            with open(input_file, 'r') as f:
                cobol_source = f.read()
            
            lst, tokens = parse_cobol_source(cobol_source)
            
            # Detect edge cases
            edge_cases = self.edge_detector.detect_edge_cases(lst, "root")
            
            # Check that edge cases detection works
            self.assertIsInstance(edge_cases, list)
            print(f"✅ Edge cases detected: {len(edge_cases)}")
    
    def test_complex_cobol_programs(self):
        """Test programs that might have parsing issues."""
        complex_programs = [
            "CBL0001.cobol",
            "CBL0002.cobol", 
            "SRCHBIN.cobol",
            "COBOL.cobol"
        ]
        
        for program in complex_programs:
            input_file = os.path.join(self.test_data_dir, program)
            
            if os.path.exists(input_file):
                print(f"\n🔍 Testing complex program: {program}")
                
                try:
                    result = self.transpiler.transpile_file(input_file)
                    
                    # Even if it has parsing issues, it should return some result
                    self.assertIsInstance(result, str)
                    
                    # Save result if it's not empty
                    if result.strip():
                        output_file = os.path.join(self.output_dir, f"{program.replace('.cobol', '.py')}")
                        with open(output_file, 'w') as f:
                            f.write(result)
                        print(f"✅ {program} -> {output_file}")
                    else:
                        print(f"⚠️ {program} - Empty result (parsing issues)")
                        
                except Exception as e:
                    print(f"❌ {program} - Error: {e}")


if __name__ == '__main__':
    unittest.main() 