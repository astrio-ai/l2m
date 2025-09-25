#!/usr/bin/env python3
"""
Setup script for the Legacy2Modern multi-agent system.

This script sets up the development environment and installs
all necessary dependencies.
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
    """Main setup function."""
    print("Setting up Legacy2Modern multi-agent system...")
    
    # Check Python version
    if sys.version_info < (3, 8):
        print("Error: Python 3.8 or higher is required")
        sys.exit(1)
    
    print(f"Python version: {sys.version}")
    
    # Install dependencies
    if not run_command("pip install -r requirements.txt", "Installing dependencies"):
        sys.exit(1)
    
    # Create necessary directories
    directories = [
        "logs",
        "output",
        "backup",
        "temp"
    ]
    
    for directory in directories:
        Path(directory).mkdir(exist_ok=True)
        print(f"✓ Created directory: {directory}")
    
    # Create .env file if it doesn't exist
    if not Path(".env").exists():
        if Path(".env.example").exists():
            run_command("cp .env.example .env", "Creating .env file")
        else:
            print("Warning: .env.example not found, please create .env manually")
    
    # Set up pre-commit hooks if available
    if Path(".pre-commit-config.yaml").exists():
        run_command("pre-commit install", "Installing pre-commit hooks")
    
    print("\n✓ Setup completed successfully!")
    print("\nNext steps:")
    print("1. Edit .env file with your configuration")
    print("2. Run tests: python -m pytest")
    print("3. Start the API server: python -m src.api.server")
    print("4. Use the CLI: python -m src.cli --help")


if __name__ == "__main__":
    main()
