#!/usr/bin/env python3
"""
Simple runner script for Legacy2Modern CLI
Can be run directly: python run_cli.py
"""

import sys
import os

# Add the current directory to Python path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

# Import and run the CLI
from engine.cli.cli import main

if __name__ == "__main__":
    main() 