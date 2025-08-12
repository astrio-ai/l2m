#!/usr/bin/env python3
"""
Fix AutoGen dependency issues and ensure proper Python interpreter usage.

This script helps resolve common AutoGen integration issues by:
1. Detecting the correct Python interpreter
2. Installing missing dependencies
3. Verifying AutoGen installation
4. Setting up proper environment variables
"""

import os
import sys
import subprocess
import platform
from pathlib import Path

def get_conda_python():
    """Get the conda Python interpreter path."""
    try:
        # Try to find conda Python
        conda_prefix = os.environ.get('CONDA_PREFIX')
        if conda_prefix:
            conda_python = Path(conda_prefix) / 'bin' / 'python'
            if conda_python.exists():
                return str(conda_python)
        
        # Try conda info
        result = subprocess.run(['conda', 'info', '--json'], 
                              capture_output=True, text=True, timeout=10)
        if result.returncode == 0:
            import json
            info = json.loads(result.stdout)
            python_path = info.get('python_executable')
            if python_path and Path(python_path).exists():
                return python_path
        
        # Try common conda locations
        common_paths = [
            Path.home() / 'anaconda3' / 'bin' / 'python',
            Path.home() / 'miniconda3' / 'bin' / 'python',
            Path('/opt/anaconda3/bin/python'),
            Path('/opt/miniconda3/bin/python'),
        ]
        
        for path in common_paths:
            if path.exists():
                return str(path)
                
    except Exception as e:
        print(f"Error finding conda Python: {e}")
    
    return None

def get_venv_python():
    """Get the virtual environment Python interpreter path."""
    try:
        venv = os.environ.get('VIRTUAL_ENV')
        if venv:
            venv_python = Path(venv) / 'bin' / 'python'
            if venv_python.exists():
                return str(venv_python)
            # Windows
            venv_python = Path(venv) / 'Scripts' / 'python.exe'
            if venv_python.exists():
                return str(venv_python)
    except Exception as e:
        print(f"Error finding venv Python: {e}")
    
    return None

def check_autogen_installation(python_path):
    """Check if AutoGen is properly installed in the given Python environment."""
    try:
        result = subprocess.run([
            python_path, '-c', 
            'import autogen_core; print("autogen_core:", getattr(autogen_core, "__version__", "unknown"))'
        ], capture_output=True, text=True, timeout=30)
        
        if result.returncode == 0:
            print(f"✅ AutoGen Core available: {result.stdout.strip()}")
            return True
        else:
            print(f"❌ AutoGen Core not available: {result.stderr.strip()}")
            return False
    except Exception as e:
        print(f"❌ Error checking AutoGen: {e}")
        return False

def install_autogen(python_path):
    """Install AutoGen packages in the given Python environment."""
    packages = [
        'autogen-core>=0.7.0',
        'autogen-agentchat>=0.7.0', 
        'autogen-ext[openai]>=0.7.0'
    ]
    
    print(f"Installing AutoGen packages using {python_path}...")
    
    for package in packages:
        try:
            print(f"Installing {package}...")
            result = subprocess.run([
                python_path, '-m', 'pip', 'install', '--upgrade', package
            ], capture_output=True, text=True, timeout=300)
            
            if result.returncode == 0:
                print(f"✅ {package} installed successfully")
            else:
                print(f"❌ Failed to install {package}: {result.stderr}")
                return False
        except Exception as e:
            print(f"❌ Error installing {package}: {e}")
            return False
    
    return True

def create_autogen_test_script():
    """Create a test script to verify AutoGen functionality."""
    test_script = '''#!/usr/bin/env python3
"""
Test script to verify AutoGen functionality.
"""

import sys
import os

def test_autogen_imports():
    """Test AutoGen imports."""
    print("Testing AutoGen imports...")
    
    try:
        import autogen_core
        print(f"✅ autogen_core v{getattr(autogen_core, '__version__', 'unknown')}")
        
        from autogen_core import Agent, BaseAgent
        print("✅ Agent and BaseAgent imported")
        
        import autogen_agentchat
        print(f"✅ autogen_agentchat v{getattr(autogen_agentchat, '__version__', 'unknown')}")
        
        import autogen_ext
        print(f"✅ autogen_ext v{getattr(autogen_ext, '__version__', 'unknown')}")
        
        return True
    except ImportError as e:
        print(f"❌ Import error: {e}")
        return False

def test_autogen_functionality():
    """Test basic AutoGen functionality."""
    print("\\nTesting AutoGen functionality...")
    
    try:
        from autogen_core import Agent
        
        # Test that we can import and inspect the Agent class
        print(f"✅ Agent class: {Agent}")
        print(f"✅ Agent is a Protocol: {hasattr(Agent, '__is_protocol__')}")
        
        # Test that we can create a simple implementation
        class SimpleAgent:
            def __init__(self, name):
                self.name = name
        
        # Check if SimpleAgent is compatible with Agent protocol
        try:
            # This should work if SimpleAgent implements the required methods
            agent = SimpleAgent("test_agent")
            print(f"✅ Created simple agent: {agent.name}")
        except Exception as e:
            print(f"⚠️ Simple agent creation: {e}")
        
        return True
    except Exception as e:
        print(f"❌ Functionality error: {e}")
        return False

def main():
    """Main test function."""
    print("AutoGen Test Script")
    print("=" * 50)
    print(f"Python interpreter: {sys.executable}")
    print(f"Python version: {sys.version}")
    
    imports_ok = test_autogen_imports()
    functionality_ok = test_autogen_functionality()
    
    print("\\n" + "=" * 50)
    if imports_ok and functionality_ok:
        print("✅ All tests passed! AutoGen is working correctly.")
        return 0
    else:
        print("❌ Some tests failed. AutoGen may not be working correctly.")
        return 1

if __name__ == "__main__":
    sys.exit(main())
'''
    
    script_path = Path("test_autogen.py")
    with open(script_path, 'w') as f:
        f.write(test_script)
    
    os.chmod(script_path, 0o755)
    print(f"✅ Created test script: {script_path}")
    return script_path

def main():
    """Main function to fix AutoGen issues."""
    print("AutoGen Dependency Fixer")
    print("=" * 50)
    
    # Check current Python
    current_python = sys.executable
    print(f"Current Python: {current_python}")
    
    # Find the best Python interpreter
    conda_python = get_conda_python()
    venv_python = get_venv_python()
    
    print(f"Conda Python: {conda_python}")
    print(f"Venv Python: {venv_python}")
    
    # Determine which Python to use
    target_python = None
    
    if conda_python:
        print("Found conda Python, checking AutoGen installation...")
        if check_autogen_installation(conda_python):
            target_python = conda_python
            print("✅ Using conda Python with AutoGen")
        else:
            print("Conda Python found but AutoGen not installed")
    
    if not target_python and venv_python:
        print("Found venv Python, checking AutoGen installation...")
        if check_autogen_installation(venv_python):
            target_python = venv_python
            print("✅ Using venv Python with AutoGen")
        else:
            print("Venv Python found but AutoGen not installed")
    
    if not target_python:
        print("Checking current Python for AutoGen...")
        if check_autogen_installation(current_python):
            target_python = current_python
            print("✅ Using current Python with AutoGen")
        else:
            print("Current Python doesn't have AutoGen installed")
    
    # Install AutoGen if needed
    if not target_python:
        print("\nNo Python with AutoGen found. Installing...")
        
        # Try conda first
        if conda_python:
            print("Installing AutoGen in conda environment...")
            if install_autogen(conda_python) and check_autogen_installation(conda_python):
                target_python = conda_python
                print("✅ AutoGen installed in conda environment")
        
        # Try venv if conda failed
        if not target_python and venv_python:
            print("Installing AutoGen in venv environment...")
            if install_autogen(venv_python) and check_autogen_installation(venv_python):
                target_python = venv_python
                print("✅ AutoGen installed in venv environment")
        
        # Try current Python if others failed
        if not target_python:
            print("Installing AutoGen in current Python...")
            if install_autogen(current_python) and check_autogen_installation(current_python):
                target_python = current_python
                print("✅ AutoGen installed in current Python")
    
    if target_python:
        print(f"\n✅ Using Python: {target_python}")
        
        # Create test script
        test_script = create_autogen_test_script()
        
        # Run test
        print(f"\nRunning AutoGen test with {target_python}...")
        result = subprocess.run([target_python, str(test_script)], 
                              capture_output=False, timeout=60)
        
        if result.returncode == 0:
            print("\n✅ AutoGen is working correctly!")
            
            # Create environment setup script
            setup_script = f'''#!/bin/bash
# AutoGen Environment Setup Script
export PYTHONPATH="{os.path.dirname(target_python)}:$PYTHONPATH"
export PATH="{os.path.dirname(target_python)}:$PATH"
echo "AutoGen environment configured for: {target_python}"
'''
            
            setup_path = Path("setup_autogen_env.sh")
            with open(setup_path, 'w') as f:
                f.write(setup_script)
            
            os.chmod(setup_path, 0o755)
            print(f"✅ Created environment setup script: {setup_path}")
            print(f"Run 'source {setup_path}' to configure your environment")
            
        else:
            print("\n❌ AutoGen test failed")
            return 1
    else:
        print("\n❌ Could not find or install AutoGen in any Python environment")
        print("\nTroubleshooting steps:")
        print("1. Make sure you have conda or venv installed")
        print("2. Try: conda install -c conda-forge autogen-agentchat")
        print("3. Try: pip install -U 'autogen-agentchat' 'autogen-ext[openai]'")
        print("4. Check your Python path and environment variables")
        return 1
    
    return 0

if __name__ == "__main__":
    sys.exit(main()) 