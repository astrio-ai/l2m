#!/usr/bin/env python3
"""
Test script for COBOL transpiler
Demonstrates the transpiler's capabilities with various COBOL programs
"""

import os
import sys

# Add the project root to Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "../..")))

from engine.modernizers.cobol_system.transpilers.transpiler import CobolTranspiler
from engine.modernizers.cobol_system.transpilers.hybrid_transpiler import HybridTranspiler

def test_transpiler():
    """Test the COBOL transpiler with various example programs."""
    
    print("🧪 Testing COBOL Transpiler")
    print("=" * 50)
    
    # Initialize transpiler
    transpiler = CobolTranspiler()
    hybrid_transpiler = HybridTranspiler()
    
    # Test programs that should work
    test_programs = [
        'examples/cobol/HELLO.cobol',
        'examples/cobol/IF_TEST.cobol', 
        'examples/cobol/PERFORM_TEST.cobol',
        'examples/cobol/ADDAMT.cobol',
        'examples/cobol/PAYROL00.cobol',
        'examples/cobol/FILE_IO_TEST.cobol'
    ]
    
    for program in test_programs:
        if os.path.exists(program):
            print(f"\n📁 Testing: {program}")
            print("-" * 30)
            
            try:
                # Test basic transpiler
                result = transpiler.transpile_file(program)
                print("✅ Basic Transpiler Result:")
                print(result)
                
                # Test hybrid transpiler
                hybrid_result = hybrid_transpiler.transpile_file(program)
                print("\n✅ Hybrid Transpiler Result:")
                print(hybrid_result)
                
                # Save output to file in the proper directory
                output_file = f"output/modernized-python/{os.path.basename(program).replace('.cobol', '.py')}"
                os.makedirs('output/modernized-python', exist_ok=True)
                with open(output_file, 'w') as f:
                    f.write(result)
                print(f"\n💾 Saved to: {output_file}")
                
            except Exception as e:
                print(f"❌ Error processing {program}: {e}")
        
        else:
            print(f"❌ File not found: {program}")
    
    print("\n" + "=" * 50)
    print("🎉 Transpiler testing completed!")
    print("📁 Check the 'output/modernized-python/' directory for generated Python files")

if __name__ == "__main__":
    test_transpiler() 