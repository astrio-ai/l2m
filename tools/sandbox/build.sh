#!/bin/bash

# Build script for the sandbox Docker image

set -e

echo "🔨 Building sandbox Docker image..."

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Change to the sandbox directory
cd "$SCRIPT_DIR"

# Build the Docker image
echo "📦 Building Docker image: sandbox:latest"
docker build -t sandbox:latest .

# Check if build was successful
if [ $? -eq 0 ]; then
    echo "✅ Sandbox image built successfully!"
    echo ""
    echo "🎯 Usage examples:"
    echo "  - Run a command: docker run --rm sandbox:latest node --version"
    echo "  - Interactive shell: docker run -it --rm sandbox:latest /bin/bash"
    echo "  - With volume mount: docker run -it --rm -v \$(pwd):/workspace sandbox:latest /bin/bash"
    echo ""
    echo "🔗 For Autogen integration, use:"
    echo "  from autogen import LocalCommandLineCodeExecutor"
    echo "  executor = LocalCommandLineCodeExecutor(docker_image='sandbox:latest', work_dir='/workspace')"
else
    echo "❌ Failed to build sandbox image"
    exit 1
fi 