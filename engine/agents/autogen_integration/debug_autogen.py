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
        import autogen_agentchat
        print(f"✅ AutoGen imported successfully")
        print(f"   Version: {getattr(autogen_agentchat, '__version__', 'unknown')}")
        print(f"   Module path: {autogen_agentchat.__file__}")
        
        # Also try importing autogen_core
        try:
            import autogen_core
            print(f"✅ AutoGen Core imported successfully")
            print(f"   Version: {getattr(autogen_core, '__version__', 'unknown')}")
        except ImportError as e:
            print(f"❌ AutoGen Core import failed: {e}")
        
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
        # Add the project root to the path
        import sys
        from pathlib import Path
        project_root = Path(__file__).parent.parent.parent.parent
        sys.path.insert(0, str(project_root))
        
        from engine.agents.autogen_integration.autogen_wrapper import AutoGenAgentWrapper, AutoGenConfig
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
    
    # Check Python interpreter
    import sys
    print(f"Python interpreter: {sys.executable}")
    print(f"Python version: {sys.version}")
    
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
        print("1. Make sure you're using the correct Python interpreter")
        print("   - If using conda: conda activate base")
        print("   - If using venv: source .venv/bin/activate")
        print("2. Install AutoGen: pip install -U 'autogen-agentchat' 'autogen-ext[openai]'")
        print("3. Check if there are any conflicting packages")
        print("4. Try: pip install --upgrade autogen-agentchat")
        print("5. If using conda, try: conda install -c conda-forge autogen-agentchat")

if __name__ == "__main__":
    main() 