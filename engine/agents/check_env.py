"""
Simple script to check environment variables and .env file loading.
"""

import os
import sys
from pathlib import Path

# Add the engine directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent))

# Try to load .env file
try:
    from dotenv import load_dotenv
    # Find and load .env file from project root
    project_root = Path(__file__).parent.parent.parent
    env_path = project_root / '.env'
    
    print(f"Looking for .env file at: {env_path}")
    
    if env_path.exists():
        load_dotenv(env_path)
        print(f"✅ Loaded .env file from: {env_path}")
    else:
        print(f"❌ .env file not found at {env_path}")
        # Try loading from current directory
        load_dotenv()
        print("Tried loading .env from current directory")
        
except ImportError:
    print("❌ python-dotenv not installed. Install with: pip install python-dotenv")
except Exception as e:
    print(f"❌ Could not load .env file: {e}")

print("\nEnvironment Variables:")
print("=" * 50)

# Check for API keys
api_keys = [
    'ANTHROPIC_API_KEY',
    'OPENAI_API_KEY', 
    'LLM_API_KEY'
]

for key in api_keys:
    value = os.getenv(key)
    if value:
        print(f"✅ {key}: {value[:10]}..." if len(value) > 10 else f"✅ {key}: {value}")
    else:
        print(f"❌ {key}: Not set")

print("\nAll environment variables containing 'API' or 'KEY':")
print("=" * 50)

for key, value in os.environ.items():
    if 'API' in key.upper() or 'KEY' in key.upper():
        if value:
            masked_value = '*' * len(value) if len(value) > 10 else value
            print(f"  {key}: {masked_value}")
        else:
            print(f"  {key}: (empty)")

print("\nCurrent working directory:", os.getcwd())
print("Project root:", Path(__file__).parent.parent.parent) 