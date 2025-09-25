#!/usr/bin/env python3
"""
Test runner script for the Legacy2Modern multi-agent system.

This script runs all tests and provides detailed reporting.
"""

import os
import sys
import subprocess
from pathlib import Path


def run_command(command, description):
    """Run a command and handle errors."""
    print(f"Running: {description}")
    try:
        result = subprocess.run(command, shell=True, check=True, capture_output=True, text=True)
        print(f"✓ {description} completed successfully")
        return True
    except subprocess.CalledProcessError as e:
        print(f"✗ {description} failed: {e}")
        print(f"Error output: {e.stderr}")
        return False


def main():
    """Main test function."""
    print("Running Legacy2Modern tests...")
    
    # Check if pytest is available
    try:
        import pytest
    except ImportError:
        print("Error: pytest is not installed. Please install it with: pip install pytest")
        sys.exit(1)
    
    # Run different types of tests
    test_commands = [
        ("python -m pytest tests/unit/ -v", "Unit tests"),
        ("python -m pytest tests/integration/ -v", "Integration tests"),
        ("python -m pytest tests/e2e/ -v", "End-to-end tests"),
        ("python -m pytest tests/ -v --cov=src --cov-report=html", "All tests with coverage")
    ]
    
    success_count = 0
    total_count = len(test_commands)
    
    for command, description in test_commands:
        if run_command(command, description):
            success_count += 1
        else:
            print(f"Warning: {description} failed")
    
    # Print summary
    print(f"\nTest Summary:")
    print(f"✓ Passed: {success_count}/{total_count}")
    print(f"✗ Failed: {total_count - success_count}/{total_count}")
    
    if success_count == total_count:
        print("\n✓ All tests passed!")
        sys.exit(0)
    else:
        print("\n✗ Some tests failed!")
        sys.exit(1)


if __name__ == "__main__":
    main()
