#!/bin/bash

# Setup API Keys for Legacy2Modern CLI

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

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

echo "🔑 Legacy2Modern API Key Setup"
echo "=============================="
echo ""

# Check if .env file exists
if [ ! -f ".env" ]; then
    if [ -f ".env.template" ]; then
        print_info "Creating .env file from template..."
        cp .env.template .env
    else
        print_error "No .env.template found. Creating basic .env file..."
        cat > .env << EOF
# LLM API Configuration
# Choose one of the following API keys:

# Option 1: Use Anthropic Claude (recommended)
ANTHROPIC_API_KEY=

# Option 2: Use OpenAI GPT
# OPENAI_API_KEY=

# Option 3: Generic LLM API key (will use Anthropic if available, otherwise OpenAI)
# LLM_API_KEY=
EOF
    fi
fi

print_info "Choose your LLM provider:"
echo "1. Anthropic Claude (recommended)"
echo "2. OpenAI GPT"
echo "3. Skip for now"
echo ""

read -p "Enter your choice (1-3): " choice

case $choice in
    1)
        print_info "Setting up Anthropic API key..."
        echo ""
        echo "To get your Anthropic API key:"
        echo "1. Go to: https://console.anthropic.com/"
        echo "2. Sign up or log in"
        echo "3. Navigate to API Keys"
        echo "4. Create a new API key"
        echo ""
        read -p "Enter your Anthropic API key: " api_key
        
        if [ -n "$api_key" ]; then
            # Update .env file
            if grep -q "ANTHROPIC_API_KEY=" .env; then
                sed -i.bak "s/ANTHROPIC_API_KEY=.*/ANTHROPIC_API_KEY=$api_key/" .env
            else
                echo "ANTHROPIC_API_KEY=$api_key" >> .env
            fi
            print_status "Anthropic API key saved to .env file"
            
            # Set environment variable for current session
            export ANTHROPIC_API_KEY="$api_key"
            print_status "API key set for current session"
        else
            print_warning "No API key provided. Skipping..."
        fi
        ;;
    2)
        print_info "Setting up OpenAI API key..."
        echo ""
        echo "To get your OpenAI API key:"
        echo "1. Go to: https://platform.openai.com/api-keys"
        echo "2. Sign up or log in"
        echo "3. Create a new API key"
        echo ""
        read -p "Enter your OpenAI API key: " api_key
        
        if [ -n "$api_key" ]; then
            # Update .env file
            if grep -q "OPENAI_API_KEY=" .env; then
                sed -i.bak "s/OPENAI_API_KEY=.*/OPENAI_API_KEY=$api_key/" .env
            else
                echo "OPENAI_API_KEY=$api_key" >> .env
            fi
            print_status "OpenAI API key saved to .env file"
            
            # Set environment variable for current session
            export OPENAI_API_KEY="$api_key"
            print_status "API key set for current session"
        else
            print_warning "No API key provided. Skipping..."
        fi
        ;;
    3)
        print_warning "Skipping API key setup"
        ;;
    *)
        print_error "Invalid choice. Exiting..."
        exit 1
        ;;
esac

echo ""
print_info "Testing API key configuration..."

# Source the .env file
if [ -f ".env" ]; then
    source .env 2>/dev/null
fi

# Test the API key
if [ -n "$ANTHROPIC_API_KEY" ]; then
    print_status "Anthropic API key is configured"
elif [ -n "$OPENAI_API_KEY" ]; then
    print_status "OpenAI API key is configured"
elif [ -n "$LLM_API_KEY" ]; then
    print_status "Generic LLM API key is configured"
else
    print_warning "No API key configured"
    echo ""
    print_info "To use LLM features, you'll need to:"
    echo "1. Get an API key from Anthropic or OpenAI"
    echo "2. Add it to your .env file"
    echo "3. Restart your terminal or run: source .env"
fi

echo ""
print_status "Setup complete!"
echo ""
print_info "Next steps:"
echo "1. Test the CLI: legacy2modern --help"
echo "2. If you added an API key, restart your terminal or run: source .env"
echo "3. Try a simple command: legacy2modern" 