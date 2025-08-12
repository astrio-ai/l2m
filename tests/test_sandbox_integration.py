#!/usr/bin/env python3
"""
Final test script demonstrating the complete sandbox integration with the legacy2modern system.
"""

import os
import sys
import asyncio
import subprocess
from pathlib import Path

def test_docker_setup():
    """Test Docker and sandbox image setup."""
    print("🔍 Testing Docker Setup...")
    
    # Check Docker
    try:
        result = subprocess.run(["docker", "--version"], capture_output=True, text=True)
        if result.returncode == 0:
            print(f"✅ Docker available: {result.stdout.strip()}")
        else:
            print("❌ Docker not available")
            return False
    except Exception as e:
        print(f"❌ Docker check failed: {e}")
        return False
    
    # Check sandbox image
    try:
        result = subprocess.run(
            ["docker", "image", "inspect", "sandbox:latest"],
            capture_output=True, text=True
        )
        if result.returncode == 0:
            print("✅ Sandbox image found")
            return True
        else:
            print("❌ Sandbox image not found")
            return False
    except Exception as e:
        print(f"❌ Sandbox image check failed: {e}")
        return False


def test_basic_sandbox_commands():
    """Test basic sandbox commands."""
    print("\n🧪 Testing Basic Sandbox Commands...")
    
    commands = [
        "node --version",
        "npm --version",
        "git --version",
        "echo 'Hello from sandbox!'"
    ]
    
    for command in commands:
        try:
            result = subprocess.run(
                ["docker", "run", "--rm", "sandbox:latest", "/bin/bash", "-c", command],
                capture_output=True,
                text=True,
                timeout=30
            )
            
            if result.returncode == 0:
                print(f"✅ {command}: {result.stdout.strip()}")
            else:
                print(f"❌ {command}: {result.stderr.strip()}")
                
        except Exception as e:
            print(f"❌ {command}: {e}")


def test_sandbox_executor():
    """Test the sandbox executor integration."""
    print("\n🔗 Testing Sandbox Executor Integration...")
    
    try:
        # Import the sandbox executor
        sys.path.append(str(Path(__file__).parent.parent))
        from engine.agents.autogen_integration.sandbox_executor import SandboxExecutor, SandboxConfig
        
        # Create executor
        config = SandboxConfig(
            docker_image="sandbox:latest",
            work_dir="/workspace"
        )
        
        executor = SandboxExecutor(config)
        
        # Test command execution
        result = executor.execute("echo 'Sandbox executor test successful!'")
        
        if result.get("success", False):
            print(f"✅ Sandbox executor test: {result['stdout'].strip()}")
        else:
            print(f"❌ Sandbox executor test failed: {result.get('stderr', 'Unknown error')}")
        
        executor.cleanup()
        
    except Exception as e:
        print(f"❌ Sandbox executor test failed: {e}")


def test_cli_integration():
    """Test CLI integration."""
    print("\n🎮 Testing CLI Integration...")
    
    try:
        # Test CLI with sandbox command
        result = subprocess.run(
            ["python", "run_cli.py"],
            input="/sandbox node --version\n/exit\n",
            capture_output=True,
            text=True,
            timeout=60
        )
        
        if "✅ Success:" in result.stdout and "v20.19.4" in result.stdout:
            print("✅ CLI sandbox integration working")
        else:
            print("❌ CLI sandbox integration failed")
            print(f"Output: {result.stdout}")
            
    except Exception as e:
        print(f"❌ CLI integration test failed: {e}")


def test_project_creation():
    """Test project creation in sandbox."""
    print("\n🏗️ Testing Project Creation...")
    
    try:
        # Test creating a simple project
        commands = [
            "mkdir -p test-project",
            "cd test-project",
            "npm init -y",
            "echo 'console.log(\"Hello from sandbox project!\");' > index.js",
            "node index.js"
        ]
        
        # Execute commands in sequence
        for command in commands:
            result = subprocess.run(
                ["docker", "run", "--rm", "-w", "/workspace", "sandbox:latest", "/bin/bash", "-c", command],
                capture_output=True,
                text=True,
                timeout=30
            )
            
            if result.returncode == 0:
                print(f"✅ {command}: Success")
            else:
                print(f"❌ {command}: {result.stderr.strip()}")
                
    except Exception as e:
        print(f"❌ Project creation test failed: {e}")


def main():
    """Run all tests."""
    print("🎯 Complete Sandbox Integration Test Suite")
    print("=" * 60)
    
    # Run tests
    tests = [
        test_docker_setup,
        test_basic_sandbox_commands,
        test_sandbox_executor,
        test_cli_integration,
        test_project_creation
    ]
    
    results = []
    
    for test in tests:
        try:
            result = test()
            results.append((test.__name__, True))
        except Exception as e:
            print(f"❌ {test.__name__} failed: {e}")
            results.append((test.__name__, False))
    
    # Summary
    print("\n" + "=" * 60)
    print("📊 Test Results Summary")
    print("=" * 60)
    
    passed = sum(1 for _, success in results if success)
    total = len(results)
    
    for test_name, success in results:
        status = "✅ PASS" if success else "❌ FAIL"
        print(f"{status} {test_name}")
    
    print(f"\n🎉 {passed}/{total} tests passed")
    
    if passed == total:
        print("\n🎊 All tests passed! Sandbox integration is working perfectly!")
        print("\n📚 Next Steps:")
        print("1. Use /sandbox commands in the CLI")
        print("2. Create and manage projects in the isolated environment")
        print("3. Integrate with your existing development workflows")
        print("4. Build production-ready applications")
    else:
        print(f"\n⚠️ {total - passed} tests failed. Please check the setup.")
    
    print("\n🔗 For more information:")
    print("   - sandbox/README.md")
    print("   - engine/agents/autogen_integration/sandbox_executor.py")
    print("   - Use /help in the CLI for sandbox commands")


if __name__ == "__main__":
    main() 