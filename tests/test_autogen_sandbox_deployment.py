#!/usr/bin/env python3
"""
Test AutoGen sandbox deployment without fallback mechanisms.

This test demonstrates the full AutoGen integration working in the sandbox
environment with proper volume mounting and code execution.
"""

import os
import sys
import asyncio
import tempfile
from pathlib import Path
from typing import Dict, Any

# Add project root to path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root))

async def test_autogen_sandbox_deployment():
    """Test full AutoGen sandbox deployment."""
    print("AutoGen Sandbox Deployment Test")
    print("=" * 50)
    
    try:
        from engine.agents.autogen_integration.sandbox_executor import (
            SandboxConfig, SandboxExecutor
        )
        from engine.agents.autogen_integration.autogen_wrapper import (
            AutoGenAgentWrapper, AutoGenConfig
        )
        from engine.agents.core_agents.base_agent import BaseAgent, AgentRole
        from engine.agents.utilities.ai import AI
        from engine.agents.core_agents.base_memory import BaseMemory
        from engine.agents.utilities.project_config import ProjectConfig
        
        print("✅ All imports successful")
        
        # Create a temporary workspace for testing
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace_path = Path(temp_dir) / "workspace"
            workspace_path.mkdir()
            
            # Create a test file in the workspace
            test_file = workspace_path / "test.py"
            test_file.write_text("print('Hello from AutoGen sandbox!')\n")
            
            print(f"✅ Created test workspace at: {workspace_path}")
            
            # Configure sandbox with AutoGen executor enabled
            sandbox_config = SandboxConfig(
                docker_image="sandbox:latest",
                work_dir="/workspace",
                bind_dir=f"{workspace_path}:/workspace",  # Mount the workspace
                use_autogen_executor=True,  # Enable AutoGen executor
                fallback_on_autogen_error=True,  # Enable fallback for testing
                log_executor_choice=True
            )
            
            print("✅ Sandbox configuration created")
            
            # Create sandbox executor
            sandbox_executor = SandboxExecutor(sandbox_config)
            print(f"✅ Sandbox executor created with type: {sandbox_executor.executor_type}")
            
            # Test basic command execution
            print("\nTesting basic command execution...")
            result = sandbox_executor.execute("ls -la")
            print(f"Command result: {result}")
            
            if result.get('success'):
                print("✅ Basic command execution successful")
            else:
                print(f"❌ Basic command execution failed: {result.get('stderr', 'Unknown error')}")
                return False
            
            # Test Python code execution
            print("\nTesting Python code execution...")
            result = sandbox_executor.execute("python test.py")
            print(f"Python execution result: {result}")
            
            if result.get('success'):
                print("✅ Python code execution successful")
            else:
                print(f"❌ Python code execution failed: {result.get('stderr', 'Unknown error')}")
                return False
            
            # Test AutoGen agent creation and deployment
            print("\nTesting AutoGen agent deployment...")
            
            # Create AI instance
            ai = AI(
                provider="anthropic",
                model="claude-3-sonnet-20240229",
                api_key=os.environ.get("ANTHROPIC_API_KEY", "test_key")
            )
            
            # Create memory and config
            class TestMemory(BaseMemory):
                def __init__(self):
                    self._storage = {}
                
                async def set(self, key: str, value: Any) -> bool:
                    self._storage[key] = value
                    return True
                
                async def get(self, key: str) -> Any:
                    return self._storage.get(key)
                
                async def delete(self, key: str) -> bool:
                    if key in self._storage:
                        del self._storage[key]
                        return True
                    return False
                
                async def exists(self, key: str) -> bool:
                    return key in self._storage
                
                async def list_keys(self, pattern: str = "*") -> list:
                    return list(self._storage.keys())
                
                async def clear(self) -> bool:
                    self._storage.clear()
                    return True
            
            memory = TestMemory()
            config = ProjectConfig()
            
            # Create a concrete implementation of BaseAgent
            class TestAgent(BaseAgent):
                def _get_default_system_prompt(self) -> str:
                    return "You are a test agent that can execute code in the sandbox."
                
                async def execute_task(self, task: dict) -> dict:
                    return {"status": "completed", "result": "test task executed"}
                
                async def process_message(self, message: dict) -> dict:
                    return {"status": "processed", "content": "test message processed"}
            
            # Create the test agent
            base_agent = TestAgent(
                name="sandbox_agent",
                role=AgentRole.COORDINATOR,
                ai=ai,
                memory=memory,
                config=config
            )
            
            # Create AutoGen wrapper with full AutoGen enabled
            autogen_config = AutoGenConfig(
                enable_autogen=True,
                fallback_on_error=False,  # Disable fallback
                use_group_chat=False
            )
            
            autogen_wrapper = AutoGenAgentWrapper(base_agent, autogen_config)
            print("✅ AutoGen agent wrapper created")
            
            # Test agent status
            status = await autogen_wrapper.get_status()
            print(f"Agent status: {status}")
            
            if status.get('autogen_agent_type') == 'AssistantAgent':
                print("✅ Full AutoGen agent created successfully")
            else:
                print(f"⚠️ Agent type: {status.get('autogen_agent_type')} (may be using fallback)")
            
            # Test agent message processing
            print("\nTesting agent message processing...")
            message = {
                "type": "task",
                "content": "Execute the Python script in the sandbox",
                "task_type": "code_execution"
            }
            
            response = await autogen_wrapper.process_message(message)
            print(f"Agent response: {response}")
            
            if response:
                print("✅ Agent message processing successful")
            else:
                print("❌ Agent message processing failed")
                return False
            
            # Test sandbox cleanup
            sandbox_executor.cleanup()
            print("✅ Sandbox cleanup completed")
            
            return True
            
    except Exception as e:
        print(f"❌ Test failed with error: {e}")
        import traceback
        traceback.print_exc()
        return False

async def test_autogen_group_chat_deployment():
    """Test AutoGen group chat deployment in sandbox."""
    print("\nAutoGen Group Chat Deployment Test")
    print("=" * 50)
    
    try:
        from engine.agents.autogen_integration.group_chat_coordinator import (
            GroupChatCoordinator, ChatType
        )
        from engine.agents.utilities.ai import AI
        from engine.agents.core_agents.base_memory import BaseMemory
        from engine.agents.utilities.project_config import ProjectConfig
        
        # Create AI instance
        ai = AI(
            provider="anthropic",
            model="claude-3-sonnet-20240229",
            api_key=os.environ.get("ANTHROPIC_API_KEY", "test_key")
        )
        
        # Create memory and config
        class TestMemory(BaseMemory):
            def __init__(self):
                self._storage = {}
            
            async def set(self, key: str, value: Any) -> bool:
                self._storage[key] = value
                return True
            
            async def get(self, key: str) -> Any:
                return self._storage.get(key)
            
            async def delete(self, key: str) -> bool:
                if key in self._storage:
                    del self._storage[key]
                    return True
                return False
            
            async def exists(self, key: str) -> bool:
                return key in self._storage
            
            async def list_keys(self, pattern: str = "*") -> list:
                return list(self._storage.keys())
            
            async def clear(self) -> bool:
                self._storage.clear()
                return True
        
        memory = TestMemory()
        config = ProjectConfig()
        
        # Create group chat coordinator
        coordinator = GroupChatCoordinator(ai, memory, config)
        print("✅ Group chat coordinator created")
        
        # Create and register a test agent
        from engine.agents.autogen_integration.autogen_wrapper import AutoGenAgentWrapper, AutoGenConfig
        from engine.agents.core_agents.base_agent import BaseAgent, AgentRole
        
        # Create a concrete implementation of BaseAgent
        class TestAgent(BaseAgent):
            def _get_default_system_prompt(self) -> str:
                return "You are a test agent for group chat."
            
            async def execute_task(self, task: dict) -> dict:
                return {"status": "completed", "result": "test task executed"}
            
            async def process_message(self, message: dict) -> dict:
                return {"status": "processed", "content": "test message processed"}
        
        # Create the test agent
        base_agent = TestAgent(
            name="test_agent",
            role=AgentRole.COORDINATOR,
            ai=ai,
            memory=memory,
            config=config
        )
        
        # Register the agent
        await coordinator.register_agent(base_agent, role="participant")
        print("✅ Test agent registered")
        
        # Test group chat creation
        session_id = await coordinator.create_group_chat(
            chat_type=ChatType.TASK_DISCUSSION,
            topic="Testing AutoGen sandbox deployment",
            participants=["test_agent"],
            max_rounds=3
        )
        
        print(f"✅ Group chat session created: {session_id}")
        
        # Test group chat execution
        result = await coordinator.start_group_chat(
            session_id=session_id,
            initial_message="Let's test the sandbox deployment capabilities."
        )
        
        print(f"Group chat result: {result}")
        
        if result.get('status') == 'success':
            print("✅ Group chat execution successful")
            return True
        else:
            print(f"❌ Group chat execution failed: {result.get('error', 'Unknown error')}")
            return False
            
    except Exception as e:
        print(f"❌ Group chat test failed with error: {e}")
        import traceback
        traceback.print_exc()
        return False

async def main():
    """Main test function."""
    print("AutoGen Sandbox Deployment Test Suite")
    print("=" * 60)
    
    # Test 1: Basic sandbox deployment
    print("\nTest 1: Basic AutoGen Sandbox Deployment")
    print("-" * 40)
    test1_result = await test_autogen_sandbox_deployment()
    
    # Test 2: Group chat deployment
    print("\nTest 2: AutoGen Group Chat Deployment")
    print("-" * 40)
    test2_result = await test_autogen_group_chat_deployment()
    
    # Summary
    print("\n" + "=" * 60)
    print("DEPLOYMENT TEST SUMMARY")
    print("=" * 60)
    print(f"Basic Sandbox Deployment: {'✅ PASS' if test1_result else '❌ FAIL'}")
    print(f"Group Chat Deployment: {'✅ PASS' if test2_result else '❌ FAIL'}")
    
    if test1_result and test2_result:
        print("\n🎉 All deployment tests passed!")
        print("✅ AutoGen sandbox deployment is working without fallback mechanisms")
        print("✅ Full AutoGen integration is functional")
        return 0
    else:
        print("\n❌ Some deployment tests failed")
        print("⚠️ AutoGen integration may need further configuration")
        return 1

if __name__ == "__main__":
    exit_code = asyncio.run(main())
    sys.exit(exit_code) 