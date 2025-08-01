#!/usr/bin/env python3
"""
Test runner script for the legacy2modern-cli project.

This script runs all tests in the test suite and provides a summary report.
"""

import os
import sys
import subprocess
import time
from pathlib import Path

# Add the project root to sys.path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root))


def run_pytest(test_path, verbose=False):
    """Run pytest on a specific test path."""
    cmd = ["python", "-m", "pytest", test_path]
    
    if verbose:
        cmd.append("-v")
    
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, cwd=project_root)
        return result.returncode == 0, result.stdout, result.stderr
    except Exception as e:
        return False, "", str(e)


def run_all_tests():
    """Run all tests in the test suite."""
    print("🧪 Running Legacy2Modern CLI Test Suite")
    print("=" * 50)
    
    # Define test files
    test_files = [
        "tests/test_cobol_lst.py",
        "tests/test_rule_engine.py", 
        "tests/test_cobol_transpiler.py",
        "tests/test_parser.py"
    ]
    
    total_tests = 0
    passed_tests = 0
    failed_tests = 0
    
    start_time = time.time()
    
    for test_file in test_files:
        if os.path.exists(test_file):
            print(f"\n📋 Running tests in {test_file}...")
            
            success, stdout, stderr = run_pytest(test_file, verbose=True)
            
            if success:
                print(f"✅ {test_file} - PASSED")
                # Count test results from output
                if "passed" in stdout:
                    lines = stdout.split('\n')
                    for line in lines:
                        if "passed" in line and "failed" in line:
                            parts = line.split()
                            for i, part in enumerate(parts):
                                if part == "passed":
                                    passed = int(parts[i-1])
                                    failed = int(parts[i+1])
                                    total_tests += passed + failed
                                    passed_tests += passed
                                    failed_tests += failed
                                    break
            else:
                print(f"❌ {test_file} - FAILED")
                if stderr:
                    print(f"Error: {stderr}")
                failed_tests += 1
        else:
            print(f"⚠️  {test_file} - NOT FOUND")
    
    end_time = time.time()
    duration = end_time - start_time
    
    # Print summary
    print("\n" + "=" * 50)
    print("📊 TEST SUMMARY")
    print("=" * 50)
    print(f"Total Tests: {total_tests}")
    print(f"Passed: {passed_tests}")
    print(f"Failed: {failed_tests}")
    print(f"Duration: {duration:.2f} seconds")
    
    if failed_tests == 0:
        print("\n🎉 All tests passed!")
        return True
    else:
        print(f"\n💥 {failed_tests} test(s) failed!")
        return False


def run_specific_test(test_name):
    """Run a specific test by name."""
    print(f"🧪 Running specific test: {test_name}")
    print("=" * 50)
    
    success, stdout, stderr = run_pytest(f"tests/test_{test_name}.py", verbose=True)
    
    if success:
        print(f"✅ {test_name} tests - PASSED")
    else:
        print(f"❌ {test_name} tests - FAILED")
        if stderr:
            print(f"Error: {stderr}")
    
    return success


def main():
    """Main function to run tests."""
    if len(sys.argv) > 1:
        # Run specific test
        test_name = sys.argv[1]
        success = run_specific_test(test_name)
        sys.exit(0 if success else 1)
    else:
        # Run all tests
        success = run_all_tests()
        sys.exit(0 if success else 1)


if __name__ == "__main__":
    main() 