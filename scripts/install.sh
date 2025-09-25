#!/bin/bash

# Legacy2Modern CLI Installation Script

echo "🚀 Installing Legacy2Modern CLI..."

# Check if Python 3.10+ is installed
python_version=$(python3 --version 2>&1 | grep -oE '[0-9]+\.[0-9]+')
if [[ $(echo "$python_version >= 3.10" | bc -l) -eq 0 ]]; then
    echo "❌ Error: Python 3.10 or higher is required. Current version: $python_version"
    exit 1
fi

echo "✅ Python version check passed: $python_version"

# Install dependencies
echo "📦 Installing dependencies..."
pip3 install -r requirements.txt

# Install the CLI
echo "🔧 Installing CLI..."
pip3 install -e .

echo "✅ Installation completed!"
echo ""
echo "🎉 You can now use the CLI in two ways:"
echo "   1. Run: legacy2modern"
echo "   2. Run: l2m"
echo ""
echo "💡 Try running: legacy2modern"
echo "💡 For help: legacy2modern --help" 