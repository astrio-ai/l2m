#!/usr/bin/env python3
"""
Test script to verify the sandbox environment is working correctly.
This script can be run to test basic functionality without requiring Autogen.
"""

import subprocess
import sys
import os
from pathlib import Path


def run_docker_command(cmd, description):
    """Run a Docker command and return the result."""
    try:
        result = subprocess.run(
            cmd,
            shell=True,
            capture_output=True,
            text=True,
            timeout=30
        )
        if result.returncode == 0:
            print(f"✅ {description}")
            if result.stdout.strip():
                print(f"   Output: {result.stdout.strip()}")
            return True
        else:
            print(f"❌ {description}")
            print(f"   Error: {result.stderr.strip()}")
            return False
    except subprocess.TimeoutExpired:
        print(f"⏰ {description} - Timeout")
        return False
    except Exception as e:
        print(f"💥 {description} - Exception: {e}")
        return False


def test_sandbox():
    """Test the sandbox environment."""
    
    print("🧪 Testing sandbox environment...")
    print("=" * 50)
    
    # Check if Docker is available
    if not run_docker_command("docker --version", "Docker is available"):
        print("❌ Docker is not available. Please install Docker first.")
        return False
    
    # Check if sandbox image exists
    if not run_docker_command("docker image inspect sandbox:latest", "Sandbox image exists"):
        print("⚠️  Sandbox image not found. Building it now...")
        if not run_docker_command("cd ../sandbox && ./build.sh", "Building sandbox image"):
            print("❌ Failed to build sandbox image")
            return False
    
    # Test basic Node.js functionality
    tests = [
        ("docker run --rm sandbox:latest node --version", "Node.js is available"),
        ("docker run --rm sandbox:latest npm --version", "npm is available"),
        ("docker run --rm sandbox:latest git --version", "Git is available"),
        ("docker run --rm sandbox:latest python3 --version", "Python 3 is available"),
        ("docker run --rm sandbox:latest which yarn", "Yarn is available"),
        ("docker run --rm sandbox:latest which pnpm", "pnpm is available"),
        ("docker run --rm sandbox:latest which tsc", "TypeScript compiler is available"),
        ("docker run --rm sandbox:latest which eslint", "ESLint is available"),
        ("docker run --rm sandbox:latest which prettier", "Prettier is available"),
    ]
    
    passed = 0
    total = len(tests)
    
    for cmd, desc in tests:
        if run_docker_command(cmd, desc):
            passed += 1
        print()
    
    print("=" * 50)
    print(f"📊 Test Results: {passed}/{total} tests passed")
    
    if passed == total:
        print("🎉 All tests passed! Sandbox is ready to use.")
        return True
    else:
        print("⚠️  Some tests failed. Please check the sandbox setup.")
        return False


def test_autogen_integration():
    """Test Autogen integration (if available)."""
    
    print("\n🔗 Testing Autogen integration...")
    print("=" * 50)
    
    try:
        import sys
        from pathlib import Path
        sys.path.append(str(Path(__file__).parent.parent))
        from engine.agents.autogen_integration.sandbox_executor import SandboxExecutor, SandboxConfig
        config = SandboxConfig()
        executor = SandboxExecutor(config)
        
        # Test if AutoGen executor is being used
        if hasattr(executor.executor, 'execute_code_blocks'):
            print("✅ AutoGen DockerCommandLineCodeExecutor is working!")
            print("   Using real AutoGen integration with Docker sandbox")
            
            # Test a simple command
            result = executor.execute('echo "AutoGen test"')
            if result.get('success'):
                print("✅ AutoGen command execution successful")
                print(f"   Output: {result.get('stdout', '').strip()}")
            else:
                print("❌ AutoGen command execution failed")
                print(f"   Error: {result.get('stderr', '')}")
        else:
            print("ℹ️  Using fallback executor (AutoGen not available)")
        
        executor.cleanup()
        return True
        
    except Exception as e:
        print(f"❌ AutoGen integration test failed: {e}")
        return False


if __name__ == "__main__":
    print("🚀 Sandbox Test Suite")
    print("=" * 50)
    
    # Test basic functionality
    basic_ok = test_sandbox()
    
    # Test Autogen integration
    autogen_ok = test_autogen_integration()
    
    print("\n" + "=" * 50)
    print("📋 Summary:")
    print(f"   Basic functionality: {'✅' if basic_ok else '❌'}")
    print(f"   AutoGen integration: {'✅' if autogen_ok else 'ℹ️  (using fallback)'}")
    
    if basic_ok:
        print("\n🎯 Next steps:")
        print("   1. Use the sandbox with AutoGen agents (when available)")
        print("   2. Run development commands in the isolated environment")
        print("   3. Build and test Node.js applications")
        print("   4. The fallback executor provides full functionality")
    else:
        print("\n🔧 Please fix the issues above before using the sandbox.") 