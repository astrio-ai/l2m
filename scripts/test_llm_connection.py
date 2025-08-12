#!/usr/bin/env python3
"""
Test LLM Connection Script

This script tests the connection to LLM APIs to verify that the CLI can connect
to the configured LLM service.
"""

import os
import sys
from pathlib import Path

# Add project root to path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root))

# Load environment variables
try:
    from dotenv import load_dotenv
    load_dotenv()
except ImportError:
    pass

def test_anthropic_connection():
    """Test Anthropic API connection"""
    try:
        import anthropic
        
        api_key = os.getenv('ANTHROPIC_API_KEY')
        if not api_key:
            return False, "No ANTHROPIC_API_KEY found"
        
        client = anthropic.Anthropic(api_key=api_key)
        
        # Test with a simple message
        response = client.messages.create(
            model="claude-3-haiku-20240307",
            max_tokens=10,
            messages=[{"role": "user", "content": "Hello"}]
        )
        
        return True, f"✅ Anthropic connection successful: {response.content[0].text[:50]}..."
        
    except ImportError:
        return False, "anthropic package not installed"
    except Exception as e:
        return False, f"Anthropic connection failed: {str(e)}"

def test_openai_connection():
    """Test OpenAI API connection"""
    try:
        from openai import OpenAI
        
        api_key = os.getenv('OPENAI_API_KEY')
        if not api_key:
            return False, "No OPENAI_API_KEY found"
        
        client = OpenAI(api_key=api_key)
        
        # Test with a simple message
        response = client.chat.completions.create(
            model="gpt-3.5-turbo",
            max_tokens=10,
            messages=[{"role": "user", "content": "Hello"}]
        )
        
        return True, f"✅ OpenAI connection successful: {response.choices[0].message.content[:50]}..."
        
    except ImportError:
        return False, "openai package not installed"
    except Exception as e:
        return False, f"OpenAI connection failed: {str(e)}"

def main():
    print("🔍 Testing LLM API Connections")
    print("=" * 40)
    print()
    
    # Check environment variables
    anthropic_key = os.getenv('ANTHROPIC_API_KEY')
    openai_key = os.getenv('OPENAI_API_KEY')
    llm_key = os.getenv('LLM_API_KEY')
    
    print("📋 Environment Check:")
    print(f"  ANTHROPIC_API_KEY: {'✅ Set' if anthropic_key else '❌ Not set'}")
    print(f"  OPENAI_API_KEY: {'✅ Set' if openai_key else '❌ Not set'}")
    print(f"  LLM_API_KEY: {'✅ Set' if llm_key else '❌ Not set'}")
    print()
    
    # Test connections
    print("🔗 Connection Tests:")
    
    # Test Anthropic if key is available
    if anthropic_key:
        success, message = test_anthropic_connection()
        print(f"  Anthropic: {message}")
    else:
        print("  Anthropic: ⏭️  Skipped (no API key)")
    
    # Test OpenAI if key is available
    if openai_key:
        success, message = test_openai_connection()
        print(f"  OpenAI: {message}")
    else:
        print("  OpenAI: ⏭️  Skipped (no API key)")
    
    print()
    
    # Summary
    if not any([anthropic_key, openai_key, llm_key]):
        print("❌ No API keys configured!")
        print()
        print("To fix this:")
        print("1. Get an API key from Anthropic or OpenAI")
        print("2. Add it to your .env file")
        print("3. Or run: ./scripts/setup_api_keys.sh")
        return 1
    elif anthropic_key or openai_key:
        print("✅ API keys are configured!")
        print("   The CLI should be able to connect to LLM services.")
        return 0
    else:
        print("⚠️  Only generic LLM_API_KEY found")
        print("   This may work depending on your setup.")
        return 0

if __name__ == "__main__":
    sys.exit(main()) 