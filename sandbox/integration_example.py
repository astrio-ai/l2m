#!/usr/bin/env python3
"""
Comprehensive example of sandbox integration with Autogen's LocalCommandLineCodeExecutor.

This script demonstrates various use cases for the sandbox environment:
1. Basic command execution
2. Project setup and development
3. Dependency management
4. Development server management
5. Integration with existing agents
"""

import os
import sys
import asyncio
import logging
from pathlib import Path

# Add the parent directory to the path to import from engine
sys.path.append(str(Path(__file__).parent.parent))

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)

try:
    from engine.agents.autogen_integration.sandbox_executor import (
        SandboxConfig, SandboxExecutor, SandboxAgent,
        create_sandbox_executor, create_sandbox_agent, execute_in_sandbox
    )
    from engine.agents.autogen_integration.autogen_wrapper import AutoGenAgentWrapper
    from engine.agents.core_agents.base_agent import BaseAgent, AgentRole
    from engine.agents.utilities.ai import AI
except ImportError as e:
    print(f"Error importing required modules: {e}")
    print("Please ensure the engine module is properly installed and accessible")
    sys.exit(1)


def demo_basic_execution():
    """Demonstrate basic command execution in the sandbox."""
    print("\n" + "="*60)
    print("🚀 DEMO 1: Basic Command Execution")
    print("="*60)
    
    try:
        # Create a sandbox executor
        executor = create_sandbox_executor()
        
        # Execute basic commands
        commands = [
            "node --version",
            "npm --version",
            "git --version",
            "python3 --version",
            "echo 'Hello from sandbox!'",
            "pwd",
            "ls -la"
        ]
        
        for command in commands:
            print(f"\n📋 Executing: {command}")
            try:
                result = executor.execute(command)
                print(f"✅ Success: {result}")
            except Exception as e:
                print(f"❌ Error: {e}")
        
        executor.cleanup()
        print("\n✅ Basic execution demo completed successfully!")
        
    except Exception as e:
        print(f"❌ Basic execution demo failed: {e}")


def demo_project_setup():
    """Demonstrate project setup in the sandbox."""
    print("\n" + "="*60)
    print("🏗️  DEMO 2: Project Setup")
    print("="*60)
    
    try:
        # Create a sandbox agent for project management
        agent = create_sandbox_agent("project-manager")
        
        # Set up a React project
        print("\n📦 Setting up React project...")
        result = agent.setup_project("react", "demo-app")
        print(f"✅ React project setup: {result}")
        
        # Navigate to project and install dependencies
        print("\n📦 Installing dependencies...")
        commands = [
            "cd demo-app",
            "npm install",
            "npm list --depth=0"  # Show installed packages
        ]
        
        task = {
            "type": "sequence",
            "commands": commands
        }
        
        result = agent.execute_task(task)
        print(f"✅ Dependencies installed: {result}")
        
        agent.cleanup()
        print("\n✅ Project setup demo completed successfully!")
        
    except Exception as e:
        print(f"❌ Project setup demo failed: {e}")


def demo_development_workflow():
    """Demonstrate a complete development workflow."""
    print("\n" + "="*60)
    print("🔄 DEMO 3: Development Workflow")
    print("="*60)
    
    try:
        # Create a sandbox executor with custom configuration
        config = SandboxConfig(
            command_timeout=600,  # 10 minutes
            env_vars={
                "NODE_ENV": "development",
                "CI": "false"
            }
        )
        
        executor = SandboxExecutor(config)
        
        # Complete workflow: create, setup, and run a Next.js app
        workflow_commands = [
            # Create Next.js app
            "npx create-next-app@latest workflow-demo --typescript --tailwind --eslint --app --src-dir --import-alias '@/*' --yes",
            
            # Navigate to project
            "cd workflow-demo",
            
            # Install additional dependencies
            "npm install axios react-query",
            
            # Create a simple component
            "mkdir -p src/components",
            '''echo 'import React from "react";

export default function DemoComponent() {
  return (
    <div className="p-4 bg-blue-100 rounded-lg">
      <h2 className="text-xl font-bold text-blue-800">Hello from Sandbox!</h2>
      <p className="text-blue-600">This component was created in the isolated environment.</p>
    </div>
  );
}' > src/components/DemoComponent.tsx''',
            
            # Update the main page to use the component
            '''echo 'import DemoComponent from "@/components/DemoComponent";

export default function Home() {
  return (
    <main className="flex min-h-screen flex-col items-center justify-between p-24">
      <DemoComponent />
    </main>
  );
}' > src/app/page.tsx''',
            
            # Build the project
            "npm run build",
            
            # Show build output
            "ls -la .next"
        ]
        
        print("\n🔄 Executing development workflow...")
        results = executor.execute_sequence(workflow_commands, stop_on_error=False)
        
        # Display results
        successful = sum(1 for r in results if r.get("success", False))
        total = len(results)
        
        print(f"\n📊 Workflow Results: {successful}/{total} commands successful")
        
        for i, result in enumerate(results):
            status = "✅" if result.get("success", False) else "❌"
            print(f"{status} Step {i+1}: {result['command']}")
        
        executor.cleanup()
        print("\n✅ Development workflow demo completed!")
        
    except Exception as e:
        print(f"❌ Development workflow demo failed: {e}")


def demo_agent_integration():
    """Demonstrate integration with existing agents."""
    print("\n" + "="*60)
    print("🤖 DEMO 4: Agent Integration")
    print("="*60)
    
    try:
        # Create a base agent (this would normally be configured with AI)
        class DemoAgent(BaseAgent):
            def __init__(self):
                super().__init__(
                    name="demo-agent",
                    role=AgentRole.MODERNIZER,
                    ai=AI()  # This would be configured with actual API keys
                )
                self.sandbox_executor = create_sandbox_executor()
            
            async def execute_sandbox_task(self, task_description: str) -> dict:
                """Execute a task in the sandbox based on AI-generated commands."""
                # In a real scenario, the AI would generate commands based on the task
                # For demo purposes, we'll use predefined commands
                
                if "react" in task_description.lower():
                    commands = [
                        "npx create-react-app ai-demo --yes",
                        "cd ai-demo",
                        "npm install",
                        "echo 'AI-generated React app created successfully!'"
                    ]
                elif "next" in task_description.lower():
                    commands = [
                        "npx create-next-app@latest ai-next-demo --typescript --yes",
                        "cd ai-next-demo",
                        "npm install",
                        "echo 'AI-generated Next.js app created successfully!'"
                    ]
                else:
                    commands = [
                        "echo 'AI task executed in sandbox'",
                        "pwd",
                        "ls -la"
                    ]
                
                results = self.sandbox_executor.execute_sequence(commands)
                return {
                    "task": task_description,
                    "commands": commands,
                    "results": results,
                    "success": all(r.get("success", False) for r in results)
                }
            
            def cleanup(self):
                if hasattr(self, 'sandbox_executor'):
                    self.sandbox_executor.cleanup()
        
        # Create and use the demo agent
        agent = DemoAgent()
        
        # Execute some AI-driven tasks
        tasks = [
            "Create a React application",
            "Create a Next.js application",
            "Show current directory contents"
        ]
        
        print("\n🤖 Executing AI-driven tasks...")
        
        for task in tasks:
            print(f"\n📋 Task: {task}")
            try:
                # In a real scenario, this would be async
                result = asyncio.run(agent.execute_sandbox_task(task))
                status = "✅" if result["success"] else "❌"
                print(f"{status} Task completed: {result['success']}")
            except Exception as e:
                print(f"❌ Task failed: {e}")
        
        agent.cleanup()
        print("\n✅ Agent integration demo completed!")
        
    except Exception as e:
        print(f"❌ Agent integration demo failed: {e}")


def demo_error_handling():
    """Demonstrate error handling and recovery."""
    print("\n" + "="*60)
    print("🛡️  DEMO 5: Error Handling")
    print("="*60)
    
    try:
        executor = create_sandbox_executor()
        
        # Test commands that might fail
        test_commands = [
            "echo 'This will succeed'",
            "nonexistent-command",  # This will fail
            "echo 'This should still work'",
            "cd nonexistent-directory",  # This will fail
            "echo 'Recovery successful'"
        ]
        
        print("\n🛡️ Testing error handling...")
        results = executor.execute_sequence(test_commands, stop_on_error=False)
        
        print(f"\n📊 Error Handling Results:")
        for i, result in enumerate(results):
            status = "✅" if result.get("success", False) else "❌"
            command = result["command"]
            if result.get("success", False):
                print(f"{status} Command {i+1}: {command}")
            else:
                error = result.get("error", "Unknown error")
                print(f"{status} Command {i+1}: {command} - Error: {error}")
        
        executor.cleanup()
        print("\n✅ Error handling demo completed!")
        
    except Exception as e:
        print(f"❌ Error handling demo failed: {e}")


def main():
    """Run all demos."""
    print("🎯 Sandbox Integration Demo Suite")
    print("="*60)
    print("This demo showcases the integration of Autogen's LocalCommandLineCodeExecutor")
    print("with the Docker sandbox environment for isolated code execution.")
    print("="*60)
    
    # Check if Docker is available
    try:
        import subprocess
        result = subprocess.run(["docker", "--version"], capture_output=True, text=True)
        if result.returncode != 0:
            print("❌ Docker is not available. Please install Docker first.")
            return
        print(f"✅ Docker available: {result.stdout.strip()}")
    except Exception as e:
        print(f"❌ Docker check failed: {e}")
        return
    
    # Run all demos
    demos = [
        demo_basic_execution,
        demo_project_setup,
        demo_development_workflow,
        demo_agent_integration,
        demo_error_handling
    ]
    
    for demo in demos:
        try:
            demo()
        except KeyboardInterrupt:
            print("\n⏹️  Demo interrupted by user")
            break
        except Exception as e:
            print(f"\n❌ Demo failed with error: {e}")
            continue
    
    print("\n" + "="*60)
    print("🎉 Sandbox Integration Demo Suite Completed!")
    print("="*60)
    print("\n📚 Next Steps:")
    print("1. Use the sandbox in your own projects")
    print("2. Integrate with your existing agents")
    print("3. Customize the sandbox configuration")
    print("4. Build production-ready applications")
    print("\n🔗 For more information, see:")
    print("   - sandbox/README.md")
    print("   - engine/agents/autogen_integration/sandbox_executor.py")


if __name__ == "__main__":
    main() 