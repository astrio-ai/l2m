#!/usr/bin/env python3
"""
Test script for sandbox integration with mock LocalCommandLineCodeExecutor.
This allows us to test the integration even if LocalCommandLineCodeExecutor is not available.
"""

import os
import sys
import subprocess
import logging
from pathlib import Path

# Add the parent directory to the path
sys.path.append(str(Path(__file__).parent.parent))

# Configure logging
logging.basicConfig(level=logging.INFO)

# Mock LocalCommandLineCodeExecutor for testing
class MockLocalCommandLineCodeExecutor:
    """Mock implementation of LocalCommandLineCodeExecutor for testing."""
    
    def __init__(self, docker_image="sandbox:latest", work_dir="/workspace", **kwargs):
        self.docker_image = docker_image
        self.work_dir = work_dir
        self.kwargs = kwargs
        print(f"Mock executor initialized with image: {docker_image}, work_dir: {work_dir}")
    
    def execute(self, command):
        """Execute a command using Docker directly."""
        try:
            # Run the command in the Docker container
            docker_cmd = [
                "docker", "run", "--rm",
                "-w", self.work_dir,
                self.docker_image,
                "/bin/bash", "-c", command
            ]
            
            print(f"Executing: {' '.join(docker_cmd)}")
            
            result = subprocess.run(
                docker_cmd,
                capture_output=True,
                text=True,
                timeout=60
            )
            
            return {
                "exit_code": result.returncode,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "success": result.returncode == 0
            }
            
        except subprocess.TimeoutExpired:
            return {
                "exit_code": -1,
                "stdout": "",
                "stderr": "Command timed out",
                "success": False
            }
        except Exception as e:
            return {
                "exit_code": -1,
                "stdout": "",
                "stderr": str(e),
                "success": False
            }


def test_basic_commands():
    """Test basic command execution."""
    print("\n" + "="*60)
    print("🧪 Testing Basic Commands")
    print("="*60)
    
    executor = MockLocalCommandLineCodeExecutor()
    
    test_commands = [
        "node --version",
        "npm --version",
        "git --version",
        "echo 'Hello from sandbox!'",
        "pwd",
        "ls -la"
    ]
    
    for command in test_commands:
        print(f"\n📋 Testing: {command}")
        try:
            result = executor.execute(command)
            if result["success"]:
                print(f"✅ Success: {result['stdout'].strip()}")
            else:
                print(f"❌ Failed: {result['stderr']}")
        except Exception as e:
            print(f"❌ Error: {e}")


def test_npm_operations():
    """Test npm operations."""
    print("\n" + "="*60)
    print("📦 Testing NPM Operations")
    print("="*60)
    
    executor = MockLocalCommandLineCodeExecutor()
    
    npm_commands = [
        "npm --version",
        "npm list -g --depth=0",
        "npm cache clean --force"
    ]
    
    for command in npm_commands:
        print(f"\n📋 Testing: {command}")
        try:
            result = executor.execute(command)
            if result["success"]:
                print(f"✅ Success: {result['stdout'].strip()[:100]}...")
            else:
                print(f"❌ Failed: {result['stderr']}")
        except Exception as e:
            print(f"❌ Error: {e}")


def test_project_creation():
    """Test project creation."""
    print("\n" + "="*60)
    print("🏗️ Testing Project Creation")
    print("="*60)
    
    executor = MockLocalCommandLineCodeExecutor()
    
    # Test creating a simple project structure
    project_commands = [
        "mkdir -p test-project",
        "cd test-project",
        "npm init -y",
        "echo 'console.log(\"Hello World\");' > index.js",
        "node index.js",
        "ls -la"
    ]
    
    for command in project_commands:
        print(f"\n📋 Testing: {command}")
        try:
            result = executor.execute(command)
            if result["success"]:
                print(f"✅ Success: {result['stdout'].strip()[:100]}...")
            else:
                print(f"❌ Failed: {result['stderr']}")
        except Exception as e:
            print(f"❌ Error: {e}")


def test_sandbox_executor_integration():
    """Test the sandbox executor integration."""
    print("\n" + "="*60)
    print("🔗 Testing Sandbox Executor Integration")
    print("="*60)
    
    try:
        # Import our sandbox executor
        from engine.agents.autogen_integration.sandbox_executor import SandboxExecutor, SandboxConfig
        
        # Create a sandbox executor with mock configuration
        config = SandboxConfig(
            docker_image="sandbox:latest",
            work_dir="/workspace"
        )
        
        # Test the executor
        executor = SandboxExecutor(config)
        
        # Test basic command
        result = executor.execute("echo 'Sandbox integration test successful!'")
        print(f"✅ Sandbox executor test: {result}")
        
        executor.cleanup()
        
    except ImportError as e:
        print(f"⚠️ Import error (expected if LocalCommandLineCodeExecutor not available): {e}")
        print("This is expected if LocalCommandLineCodeExecutor is not available in this AutoGen version.")
    except Exception as e:
        print(f"❌ Error: {e}")


def main():
    """Run all tests."""
    print("🎯 Sandbox Integration Test Suite")
    print("="*60)
    print("Testing sandbox functionality with mock LocalCommandLineCodeExecutor")
    print("="*60)
    
    # Check if Docker is available
    try:
        result = subprocess.run(["docker", "--version"], capture_output=True, text=True)
        if result.returncode != 0:
            print("❌ Docker is not available. Please install Docker first.")
            return
        print(f"✅ Docker available: {result.stdout.strip()}")
    except Exception as e:
        print(f"❌ Docker check failed: {e}")
        return
    
    # Check if sandbox image exists
    try:
        result = subprocess.run(
            ["docker", "image", "inspect", "sandbox:latest"],
            capture_output=True,
            text=True
        )
        if result.returncode != 0:
            print("❌ Sandbox image not found. Please build it first with: cd sandbox && ./build.sh")
            return
        print("✅ Sandbox image found")
    except Exception as e:
        print(f"❌ Sandbox image check failed: {e}")
        return
    
    # Run tests
    tests = [
        test_basic_commands,
        test_npm_operations,
        test_project_creation,
        test_sandbox_executor_integration
    ]
    
    for test in tests:
        try:
            test()
        except Exception as e:
            print(f"❌ Test failed: {e}")
            continue
    
    print("\n" + "="*60)
    print("🎉 Sandbox Integration Test Suite Completed!")
    print("="*60)
    print("\n📚 Next Steps:")
    print("1. Install LocalCommandLineCodeExecutor when available")
    print("2. Use the sandbox in your existing agents")
    print("3. Build production-ready applications")


if __name__ == "__main__":
    main() 