#!/usr/bin/env python3
"""
Example usage of the sandbox environment with Autogen's LocalCommandLineCodeExecutor.
This script demonstrates how to run Node.js commands in an isolated Docker container.
"""

import os
import sys
from pathlib import Path

# Add the parent directory to the path to import from engine
sys.path.append(str(Path(__file__).parent.parent))

try:
    from autogen import LocalCommandLineCodeExecutor
except ImportError:
    print("Error: autogen package not found. Please install it with: pip install pyautogen")
    sys.exit(1)


def main():
    """Demonstrate sandbox usage with Autogen."""
    
    print("🚀 Initializing sandbox environment...")
    
    # Initialize the executor with the sandbox Docker image
    executor = LocalCommandLineCodeExecutor(
        docker_image="sandbox:latest",
        work_dir="/workspace"
    )
    
    # Example commands to run in the sandbox
    commands = [
        "node --version",
        "npm --version",
        "git --version",
        "echo 'Sandbox environment is ready!'",
        "pwd",
        "ls -la"
    ]
    
    print("📋 Running example commands in sandbox...")
    
    for i, command in enumerate(commands, 1):
        print(f"\n{i}. Running: {command}")
        try:
            result = executor.execute(command)
            print(f"✅ Success: {result}")
        except Exception as e:
            print(f"❌ Error: {e}")
    
    print("\n🎉 Sandbox demonstration complete!")
    print("\nTo use the sandbox for your own projects:")
    print("1. Build the Docker image: cd sandbox && docker build -t sandbox:latest .")
    print("2. Use the executor in your Autogen agents")
    print("3. Run commands like 'npm install', 'npm run dev', etc.")


def create_react_app_example():
    """Example of creating a React app in the sandbox."""
    
    print("\n🔧 Example: Creating a React app in sandbox...")
    
    executor = LocalCommandLineCodeExecutor(
        docker_image="sandbox:latest",
        work_dir="/workspace"
    )
    
    # This would create a React app (commented out to avoid actual execution)
    commands = [
        "npx create-react-app my-app --yes",
        "cd my-app",
        "npm start"
    ]
    
    print("Commands that would be executed:")
    for cmd in commands:
        print(f"  - {cmd}")
    
    print("\nNote: Uncomment the executor.execute() calls to actually run these commands.")


if __name__ == "__main__":
    main()
    create_react_app_example() 