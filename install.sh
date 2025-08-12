#!/bin/bash

# Legacy2Modern CLI Installation Script

echo "🚀 Installing Legacy2Modern CLI..."

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${GREEN}✅ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

print_error() {
    echo -e "${RED}❌ $1${NC}"
}

print_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

# Check if Python 3.10+ is installed
python_version=$(python3 --version 2>&1 | grep -oE '[0-9]+\.[0-9]+')
if [[ $(echo "$python_version >= 3.10" | bc -l 2>/dev/null) -eq 0 ]]; then
    print_error "Python 3.10 or higher is required. Current version: $python_version"
    exit 1
fi

print_status "Python version check passed: $python_version"

# Check if pip is available
if ! command -v pip3 &> /dev/null; then
    print_error "pip3 is not installed. Please install pip first."
    exit 1
fi

# Install dependencies
print_info "Installing dependencies..."
pip3 install -r requirements.txt

if [ $? -ne 0 ]; then
    print_error "Failed to install dependencies. Please check your internet connection and try again."
    exit 1
fi

# Install the CLI
print_info "Installing CLI..."
pip3 install -e .

if [ $? -ne 0 ]; then
    print_error "Failed to install CLI. Please check the error messages above."
    exit 1
fi

print_status "Installation completed!"

# Check for .env file
if [ ! -f ".env" ]; then
    print_warning "No .env file found. Creating one from template..."
    if [ -f ".env.template" ]; then
        cp .env.template .env
        print_status "Created .env file from template"
    else
        print_error "No .env.template found. Please create a .env file manually."
    fi
fi

# Check for API keys
print_info "Checking LLM API configuration..."

# Check if any API key is set
if [ -z "$ANTHROPIC_API_KEY" ] && [ -z "$OPENAI_API_KEY" ] && [ -z "$LLM_API_KEY" ]; then
    print_warning "No LLM API keys found in environment variables"
    
    # Check .env file
    if [ -f ".env" ]; then
        # Source the .env file to check its contents
        source .env 2>/dev/null
        
        if [ -z "$ANTHROPIC_API_KEY" ] && [ -z "$OPENAI_API_KEY" ] && [ -z "$LLM_API_KEY" ]; then
            print_error "No API keys found in .env file"
            echo ""
            print_info "To use the CLI with LLM features, you need to set up an API key:"
            echo ""
            echo "1. Get an API key from:"
            echo "   - Anthropic: https://console.anthropic.com/"
            echo "   - OpenAI: https://platform.openai.com/api-keys"
            echo ""
            echo "2. Add it to your .env file:"
            echo "   ANTHROPIC_API_KEY=your_key_here"
            echo "   # OR"
            echo "   OPENAI_API_KEY=your_key_here"
            echo ""
            echo "3. Or set it as an environment variable:"
            echo "   export ANTHROPIC_API_KEY=your_key_here"
            echo ""
        else
            print_status "API key found in .env file"
        fi
    else
        print_error "No .env file found"
    fi
else
    print_status "API key found in environment variables"
fi

# Test AutoGen integration
print_info "Testing AutoGen integration..."
python3 -c "
import sys
try:
    import autogen_core
    print('✅ AutoGen Core available')
except ImportError:
    print('❌ AutoGen Core not available')
    sys.exit(1)
" 2>/dev/null

if [ $? -eq 0 ]; then
    print_status "AutoGen integration is working"
else
    print_warning "AutoGen integration may need setup. Run: python3 scripts/fix_autogen.py"
fi

echo ""
print_status "Installation Summary:"
echo "  • CLI installed: legacy2modern, l2m"
echo "  • Dependencies: ✅"
echo "  • AutoGen: $(python3 -c 'import autogen_core; print("✅")' 2>/dev/null || echo "⚠️")"
echo "  • API Keys: $(if [ -n "$ANTHROPIC_API_KEY$OPENAI_API_KEY$LLM_API_KEY" ]; then echo "✅"; else echo "❌"; fi)"
echo ""

print_info "You can now use the CLI in two ways:"
echo "   1. Run: legacy2modern"
echo "   2. Run: l2m"
echo ""
print_info "For help: legacy2modern --help"
echo ""

if [ -z "$ANTHROPIC_API_KEY" ] && [ -z "$OPENAI_API_KEY" ] && [ -z "$LLM_API_KEY" ]; then
    print_warning "⚠️  LLM features will not work without an API key"
    echo "   Set up your API key in .env file or environment variables"
    echo "   Then restart your terminal or run: source .env"
    echo ""
    print_info "Quick setup: Run: ./scripts/setup_api_keys.sh"
fi

print_status "Installation complete! 🎉" 