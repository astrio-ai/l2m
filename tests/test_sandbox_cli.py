#!/usr/bin/env python3
"""
Simple CLI test for sandbox integration.

Usage:
    python test_sandbox_cli.py [command]
    
Examples:
    python test_sandbox_cli.py "node --version"
    python test_sandbox_cli.py "npm install react"
    python test_sandbox_cli.py "npx create-react-app test-app --yes"
"""

import sys
import os
from pathlib import Path

# Add the parent directory to the path
sys.path.append(str(Path(__file__).parent.parent))

try:
    from engine.agents.autogen_integration.sandbox_executor import execute_in_sandbox
except ImportError as e:
    print(f"Error importing sandbox executor: {e}")
    print("Please ensure the engine module is properly installed")
    sys.exit(1)


def main():
    """Run a command in the sandbox."""
    if len(sys.argv) < 2:
        print("Usage: python test_sandbox_cli.py [command]")
        print("\nExamples:")
        print("  python test_sandbox_cli.py 'node --version'")
        print("  python test_sandbox_cli.py 'npm install react'")
        print("  python test_sandbox_cli.py 'npx create-react-app test-app --yes'")
        sys.exit(1)
    
    command = sys.argv[1]
    
    print(f"🚀 Executing in sandbox: {command}")
    print("=" * 50)
    
    try:
        result = execute_in_sandbox(command)
        print(f"✅ Success: {result}")
    except Exception as e:
        print(f"❌ Error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main() 