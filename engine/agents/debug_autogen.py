"""
Debug script to check AutoGen installation and imports.
"""

import sys
import subprocess

def check_pip_packages():
    """Check what AutoGen packages are installed."""
    print("Checking pip packages...")
    try:
        result = subprocess.run([sys.executable, "-m", "pip", "list"], 
                              capture_output=True, text=True)
        lines = result.stdout.split('\n')
        autogen_packages = [line for line in lines if 'autogen' in line.lower()]
        print("AutoGen packages found:")
        for package in autogen_packages:
            print(f"  {package}")
    except Exception as e:
        print(f"Error checking pip packages: {e}")

def test_autogen_import():
    """Test AutoGen import directly."""
    print("\nTesting AutoGen import...")
    try:
        import autogen_agentchat as autogen
        print(f"✅ AutoGen imported successfully")
        print(f"   Version: {getattr(autogen, '__version__', 'unknown')}")
        print(f"   Module path: {autogen.__file__}")
        
        # Test specific imports from autogen_core
        try:
            from autogen_core import Agent, BaseAgent
            print("✅ Agent and BaseAgent imported from autogen_core")
        except ImportError as e:
            print(f"❌ Agent import failed: {e}")
        
        try:
            from autogen_core import AgentType
            print("✅ AgentType imported")
        except ImportError as e:
            print(f"❌ AgentType import failed: {e}")
        
        try:
            from autogen_core import AgentRuntime
            print("✅ AgentRuntime imported")
        except ImportError as e:
            print(f"❌ AgentRuntime import failed: {e}")
        
    except ImportError as e:
        print(f"❌ AutoGen import failed: {e}")
        return False
    except Exception as e:
        print(f"❌ Unexpected error importing AutoGen: {e}")
        return False
    
    return True

def test_autogen_wrapper_import():
    """Test our AutoGen wrapper import."""
    print("\nTesting AutoGen wrapper import...")
    try:
        # Add the engine directory to the path
        import sys
        from pathlib import Path
        sys.path.insert(0, str(Path(__file__).parent.parent))
        
        from agents.autogen_wrapper import AutoGenAgentWrapper, AutoGenConfig
        print("✅ AutoGen wrapper imported successfully")
        return True
    except ImportError as e:
        print(f"❌ AutoGen wrapper import failed: {e}")
        return False
    except Exception as e:
        print(f"❌ Unexpected error importing wrapper: {e}")
        return False

def main():
    """Main debug function."""
    print("AutoGen Debug Information")
    print("=" * 50)
    
    check_pip_packages()
    
    autogen_ok = test_autogen_import()
    wrapper_ok = test_autogen_wrapper_import()
    
    print("\n" + "=" * 50)
    if autogen_ok and wrapper_ok:
        print("✅ All imports successful!")
        print("AutoGen should work correctly.")
    else:
        print("❌ Some imports failed.")
        print("\nTroubleshooting steps:")
        print("1. Make sure you're in the correct virtual environment")
        print("2. Install AutoGen: pip install -U 'autogen-agentchat' 'autogen-ext[openai]'")
        print("3. Check if there are any conflicting packages")
        print("4. Try: pip install --upgrade autogen-agentchat")

if __name__ == "__main__":
    main() 