#!/usr/bin/env python3
"""
Test runner for all sandbox-related tests.

This script runs all sandbox tests in the correct order and provides
a comprehensive summary of the results.
"""

import os
import sys
import subprocess
import time
from pathlib import Path

def run_test(test_file, description):
    """Run a test file and return the result."""
    print(f"\n{'='*60}")
    print(f"🧪 Running: {description}")
    print(f"📁 File: {test_file}")
    print(f"{'='*60}")
    
    try:
        start_time = time.time()
        result = subprocess.run(
            [sys.executable, test_file],
            capture_output=True,
            text=True,
            timeout=300  # 5 minutes timeout
        )
        end_time = time.time()
        
        if result.returncode == 0:
            print("✅ Test completed successfully")
            print(f"⏱️  Duration: {end_time - start_time:.2f} seconds")
            if result.stdout.strip():
                print("\n📋 Output:")
                print(result.stdout)
            return True, result.stdout
        else:
            print("❌ Test failed")
            print(f"⏱️  Duration: {end_time - start_time:.2f} seconds")
            if result.stderr.strip():
                print("\n❌ Error:")
                print(result.stderr)
            if result.stdout.strip():
                print("\n📋 Output:")
                print(result.stdout)
            return False, result.stderr
            
    except subprocess.TimeoutExpired:
        print("⏰ Test timed out (5 minutes)")
        return False, "Timeout"
    except Exception as e:
        print(f"💥 Test execution failed: {e}")
        return False, str(e)


def run_test_with_args(test_file, description, args):
    """Run a test file with arguments and return the result."""
    print(f"\n{'='*60}")
    print(f"🧪 Running: {description}")
    print(f"📁 File: {test_file}")
    print(f"📋 Args: {' '.join(args)}")
    print(f"{'='*60}")
    
    try:
        start_time = time.time()
        result = subprocess.run(
            [sys.executable, test_file] + args,
            capture_output=True,
            text=True,
            timeout=300  # 5 minutes timeout
        )
        end_time = time.time()
        
        if result.returncode == 0:
            print("✅ Test completed successfully")
            print(f"⏱️  Duration: {end_time - start_time:.2f} seconds")
            if result.stdout.strip():
                print("\n📋 Output:")
                print(result.stdout)
            return True, result.stdout
        else:
            print("❌ Test failed")
            print(f"⏱️  Duration: {end_time - start_time:.2f} seconds")
            if result.stderr.strip():
                print("\n❌ Error:")
                print(result.stderr)
            if result.stdout.strip():
                print("\n📋 Output:")
                print(result.stdout)
            return False, result.stderr
            
    except subprocess.TimeoutExpired:
        print("⏰ Test timed out (5 minutes)")
        return False, "Timeout"
    except Exception as e:
        print(f"💥 Test execution failed: {e}")
        return False, str(e)


def main():
    """Run all sandbox tests."""
    print("🎯 Sandbox Test Suite Runner")
    print("=" * 60)
    print("This script runs all sandbox-related tests in the correct order.")
    print("=" * 60)
    
    # Define test files and their descriptions
    tests = [
        ("test_sandbox_basic.py", "Basic Sandbox Environment Test"),
        ("test_sandbox_mock.py", "Mock Integration Test (No AutoGen)"),
        ("test_sandbox_cli.py", "CLI Integration Test"),
        ("test_sandbox_integration.py", "Complete Integration Test")
    ]
    
    # Special handling for CLI test
    cli_test_args = ["node", "--version"]
    
    # Check if we're in the tests directory
    if not Path("test_sandbox_basic.py").exists():
        print("❌ Please run this script from the tests/ directory")
        print("   cd tests && python run_sandbox_tests.py")
        sys.exit(1)
    
    # Run tests
    results = []
    
    for test_file, description in tests:
        if Path(test_file).exists():
            # Special handling for CLI test
            if test_file == "test_sandbox_cli.py":
                success, output = run_test_with_args(test_file, description, cli_test_args)
            else:
                success, output = run_test(test_file, description)
            results.append((test_file, description, success, output))
        else:
            print(f"⚠️  Test file not found: {test_file}")
            results.append((test_file, description, False, "File not found"))
    
    # Summary
    print(f"\n{'='*60}")
    print("📊 Test Results Summary")
    print(f"{'='*60}")
    
    passed = sum(1 for _, _, success, _ in results if success)
    total = len(results)
    
    for test_file, description, success, _ in results:
        status = "✅ PASS" if success else "❌ FAIL"
        print(f"{status} {description} ({test_file})")
    
    print(f"\n🎉 {passed}/{total} tests passed")
    
    if passed == total:
        print("\n🎊 All sandbox tests passed! The integration is working perfectly.")
        print("\n📚 Next Steps:")
        print("1. Use the sandbox in your development workflows")
        print("2. Run /sandbox commands in the CLI")
        print("3. Integrate with your existing agents")
        print("4. Build production applications")
    else:
        print(f"\n⚠️  {total - passed} tests failed. Please check the setup.")
        print("\n🔧 Troubleshooting:")
        print("1. Ensure Docker is running")
        print("2. Build the sandbox image: cd ../sandbox && ./build.sh")
        print("3. Check Docker permissions")
        print("4. Verify AutoGen installation (if needed)")
    
    print(f"\n🔗 For more information:")
    print("   - ../sandbox/README.md")
    print("   - ../engine/agents/autogen_integration/sandbox_executor.py")
    print("   - Use /help in the CLI for sandbox commands")
    
    # Exit with appropriate code
    sys.exit(0 if passed == total else 1)


if __name__ == "__main__":
    main() 