#!/bin/bash
# Launcher script for L2M Go TUI

set -e

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}Starting L2M Go TUI...${NC}"

# Check if Go is installed
if ! command -v go &> /dev/null; then
    echo -e "${YELLOW}Go is not installed. Please install Go 1.21 or later:${NC}"
    echo "  macOS: brew install go"
    echo "  Linux: https://go.dev/doc/install"
    exit 1
fi

# Navigate to tui-go directory
cd "$(dirname "$0")/tui-go"

# Download dependencies if needed
if [ ! -d "vendor" ] && [ ! -f "go.sum" ]; then
    echo -e "${GREEN}Installing dependencies...${NC}"
    go mod download
fi

# Build if binary doesn't exist or source is newer
if [ ! -f "l2m-tui" ] || [ "main.go" -nt "l2m-tui" ]; then
    echo -e "${GREEN}Building TUI...${NC}"
    go build -o l2m-tui
fi

# Run the TUI
echo -e "${GREEN}Launching...${NC}\n"
./l2m-tui

