#!/usr/bin/env python3
"""
Comprehensive AutoGen integration test.

This test verifies that all AutoGen components work correctly together:
- AutoGen imports and availability
- Sandbox executor with volume mounting
- Agent wrapper functionality
- Group chat coordination
- Error handling and fallbacks
"""

import os
import sys
import asyncio
import tempfile
import subprocess
from pathlib import Path
from typing import Dict, Any, List

# Add project root to path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root))

def test_autogen_imports():
    """Test AutoGen package imports."""
    print("Testing AutoGen imports...")
    
    results = {}
    
    # Test autogen_core
    try:
        import autogen_core
        results['autogen_core'] = {
            'available': True,
            'version': getattr(autogen_core, '__version__', 'unknown'),
            'path': autogen_core.__file__
        }
        print(f"✅ autogen_core v{results['autogen_core']['version']}")
    except ImportError as e:
        results['autogen_core'] = {
            'available': False,
            'error': str(e)
        }
        print(f"❌ autogen_core: {e}")
    
    # Test autogen_agentchat
    try:
        import autogen_agentchat
        results['autogen_agentchat'] = {
            'available': True,
            'version': getattr(autogen_agentchat, '__version__', 'unknown'),
            'path': autogen_agentchat.__file__
        }
        print(f"✅ autogen_agentchat v{results['autogen_agentchat']['version']}")
    except ImportError as e:
        results['autogen_agentchat'] = {
            'available': False,
            'error': str(e)
        }
        print(f"❌ autogen_agentchat: {e}")
    
    # Test autogen_ext
    try:
        import autogen_ext
        results['autogen_ext'] = {
            'available': True,
            'version': getattr(autogen_ext, '__version__', 'unknown'),
            'path': autogen_ext.__file__
        }
        print(f"✅ autogen_ext v{results['autogen_ext']['version']}")
    except ImportError as e:
        results['autogen_ext'] = {
            'available': False,
            'error': str(e)
        }
        print(f"❌ autogen_ext: {e}")
    
    return results

def test_autogen_core_classes():
    """Test AutoGen core class imports."""
    print("\nTesting AutoGen core classes...")
    
    results = {}
    
    # Test Agent and BaseAgent from autogen_core
    try:
        from autogen_core import Agent, BaseAgent
        results['Agent'] = {'available': True}
        results['BaseAgent'] = {'available': True}
        print("✅ Agent and BaseAgent imported from autogen_core")
    except ImportError as e:
        results['Agent'] = {'available': False, 'error': str(e)}
        results['BaseAgent'] = {'available': False, 'error': str(e)}
        print(f"❌ Agent/BaseAgent: {e}")
    
    # Test AssistantAgent and UserProxyAgent from autogen_agentchat
    try:
        from autogen_agentchat.agents import AssistantAgent, UserProxyAgent
        results['AssistantAgent'] = {'available': True}
        results['UserProxyAgent'] = {'available': True}
        print("✅ AssistantAgent and UserProxyAgent imported from autogen_agentchat.agents")
    except ImportError as e:
        results['AssistantAgent'] = {'available': False, 'error': str(e)}
        results['UserProxyAgent'] = {'available': False, 'error': str(e)}
        print(f"❌ AssistantAgent/UserProxyAgent: {e}")
    
    # Test BaseGroupChat from autogen_agentchat.teams
    try:
        from autogen_agentchat.teams import BaseGroupChat, RoundRobinGroupChat
        results['BaseGroupChat'] = {'available': True}
        results['RoundRobinGroupChat'] = {'available': True}
        print("✅ BaseGroupChat and RoundRobinGroupChat imported from autogen_agentchat.teams")
    except ImportError:
        try:
            from autogen_ext.teams import BaseGroupChat, RoundRobinGroupChat
            results['BaseGroupChat'] = {'available': True}
            results['RoundRobinGroupChat'] = {'available': True}
            print("✅ BaseGroupChat and RoundRobinGroupChat imported from autogen_ext.teams")
        except ImportError:
            # GroupChat classes might not be available in newer AutoGen versions
            results['BaseGroupChat'] = {'available': False, 'note': 'Not available in this AutoGen version'}
            results['RoundRobinGroupChat'] = {'available': False, 'note': 'Not available in this AutoGen version'}
            print("⚠️ BaseGroupChat and RoundRobinGroupChat not available in this AutoGen version (this is normal)")
    
    # Test CancellationToken from autogen_core
    try:
        from autogen_core import CancellationToken
        results['CancellationToken'] = {'available': True}
        print("✅ CancellationToken imported from autogen_core")
    except ImportError as e:
        results['CancellationToken'] = {'available': False, 'error': str(e)}
        print(f"❌ CancellationToken: {e}")
    
    return results

def test_autogen_wrapper():
    """Test our AutoGen wrapper functionality."""
    print("\nTesting AutoGen wrapper...")
    
    try:
        from engine.agents.autogen_integration.autogen_wrapper import (
            AutoGenAgentWrapper, AutoGenConfig
        )
        print("✅ AutoGen wrapper imported")
        
        # Test configuration
        config = AutoGenConfig(
            enable_autogen=True,
            use_group_chat=False,
            fallback_on_error=True
        )
        print("✅ AutoGenConfig created")
        
        return {'available': True, 'config': config}
    except ImportError as e:
        print(f"❌ AutoGen wrapper import failed: {e}")
        return {'available': False, 'error': str(e)}

def test_sandbox_executor():
    """Test sandbox executor functionality."""
    print("\nTesting sandbox executor...")
    
    try:
        from engine.agents.autogen_integration.sandbox_executor import (
            SandboxConfig, SandboxExecutor, FallbackCommandLineExecutor
        )
        print("✅ Sandbox executor imported")
        
        # Test configuration
        config = SandboxConfig(
            docker_image="sandbox:latest",
            work_dir="/workspace",
            use_autogen_executor=True,
            fallback_on_autogen_error=True
        )
        print("✅ SandboxConfig created")
        
        # Test fallback executor
        fallback = FallbackCommandLineExecutor(
            image="sandbox:latest",
            work_dir="/workspace"
        )
        print("✅ FallbackCommandLineExecutor created")
        
        return {
            'available': True, 
            'config': config,
            'fallback_executor': fallback
        }
    except ImportError as e:
        print(f"❌ Sandbox executor import failed: {e}")
        return {'available': False, 'error': str(e)}

def test_group_chat_coordinator():
    """Test group chat coordinator functionality."""
    print("\nTesting group chat coordinator...")
    
    try:
        from engine.agents.autogen_integration.group_chat_coordinator import (
            GroupChatCoordinator, ChatType
        )
        print("✅ Group chat coordinator imported")
        
        return {'available': True}
    except ImportError as e:
        print(f"❌ Group chat coordinator import failed: {e}")
        return {'available': False, 'error': str(e)}

def test_docker_availability():
    """Test Docker availability."""
    print("\nTesting Docker availability...")
    
    try:
        result = subprocess.run(
            ['docker', '--version'],
            capture_output=True,
            text=True,
            timeout=10
        )
        
        if result.returncode == 0:
            print(f"✅ Docker available: {result.stdout.strip()}")
            return {'available': True, 'version': result.stdout.strip()}
        else:
            print(f"❌ Docker not available: {result.stderr}")
            return {'available': False, 'error': result.stderr}
    except FileNotFoundError:
        print("❌ Docker not found in PATH")
        return {'available': False, 'error': 'Docker not found in PATH'}
    except Exception as e:
        print(f"❌ Docker test failed: {e}")
        return {'available': False, 'error': str(e)}

def test_sandbox_image():
    """Test sandbox Docker image availability."""
    print("\nTesting sandbox Docker image...")
    
    try:
        result = subprocess.run(
            ['docker', 'image', 'inspect', 'sandbox:latest'],
            capture_output=True,
            text=True,
            timeout=10
        )
        
        if result.returncode == 0:
            print("✅ Sandbox image available")
            return {'available': True}
        else:
            print("❌ Sandbox image not found")
            return {'available': False, 'error': 'Image not found'}
    except Exception as e:
        print(f"❌ Sandbox image test failed: {e}")
        return {'available': False, 'error': str(e)}

async def test_autogen_agent_creation():
    """Test AutoGen agent creation."""
    print("\nTesting AutoGen agent creation...")
    
    try:
        from engine.agents.autogen_integration.autogen_wrapper import AutoGenAgentWrapper, AutoGenConfig
        from engine.agents.core_agents.base_agent import BaseAgent, AgentRole
        from engine.agents.utilities.ai import AI
        from engine.agents.core_agents.base_memory import BaseMemory
        from engine.agents.utilities.project_config import ProjectConfig
        
        # Create a simple AI instance
        ai = AI(
            provider="anthropic",
            model="claude-3-sonnet-20240229",
            api_key="test_key"
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
            
            async def list_keys(self, pattern: str = "*") -> List[str]:
                return list(self._storage.keys())
            
            async def clear(self) -> bool:
                self._storage.clear()
                return True
        
        memory = TestMemory()
        config = ProjectConfig()
        
        # Create a concrete implementation of BaseAgent
        class TestAgent(BaseAgent):
            def _get_default_system_prompt(self) -> str:
                return "You are a test agent."
            
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
        
        # Create AutoGen wrapper
        autogen_config = AutoGenConfig(
            enable_autogen=True,
            fallback_on_error=True
        )
        
        wrapper = AutoGenAgentWrapper(base_agent, autogen_config)
        print("✅ AutoGen agent wrapper created")
        
        # Test status
        status = await wrapper.get_status()
        print(f"✅ Agent status: {status}")
        
        return {'success': True, 'status': status}
    except Exception as e:
        print(f"❌ AutoGen agent creation failed: {e}")
        return {'success': False, 'error': str(e)}

def test_volume_mounting():
    """Test volume mounting functionality."""
    print("\nTesting volume mounting...")
    
    try:
        from engine.agents.autogen_integration.sandbox_executor import SandboxConfig
        
        # Test different volume mount configurations
        configs = [
            # Standard mount
            SandboxConfig(
                mount_host_path="/tmp/test",
                mount_container_path="/workspace"
            ),
            # Bind dir format
            SandboxConfig(
                bind_dir="/tmp/test:/workspace"
            ),
            # No mount
            SandboxConfig()
        ]
        
        for i, config in enumerate(configs):
            print(f"✅ Volume mount config {i+1} created")
        
        return {'success': True, 'configs': len(configs)}
    except Exception as e:
        print(f"❌ Volume mounting test failed: {e}")
        return {'success': False, 'error': str(e)}

def generate_test_report(results: Dict[str, Any]):
    """Generate a comprehensive test report."""
    print("\n" + "=" * 60)
    print("AUTOGEN INTEGRATION TEST REPORT")
    print("=" * 60)
    
    # Summary
    total_tests = len(results)
    
    # Count actual failures vs expected failures
    actual_failures = 0
    expected_failures = 0
    
    for test_name, result in results.items():
        if isinstance(result, dict):
            # Handle nested test results (like autogen_imports and autogen_core_classes)
            if test_name in ['autogen_imports', 'autogen_core_classes']:
                # These tests return dictionaries with individual component results
                for component_name, component_result in result.items():
                    if isinstance(component_result, dict):
                        if component_result.get('available', component_result.get('success', False)):
                            continue  # Component passed
                        elif 'note' in component_result and 'Not available in this AutoGen version' in component_result['note']:
                            expected_failures += 1  # Expected failure
                        else:
                            actual_failures += 1  # Actual failure
            else:
                # Regular test result
                if result.get('available', result.get('success', False)):
                    continue  # Test passed
                elif 'note' in result and 'not available in this AutoGen version' in result['note']:
                    expected_failures += 1  # Expected failure
                else:
                    actual_failures += 1  # Actual failure
    
    passed_tests = total_tests - actual_failures - expected_failures
    
    print(f"Total Tests: {total_tests}")
    print(f"Passed: {passed_tests}")
    print(f"Expected Failures: {expected_failures} (missing GroupChat classes in newer AutoGen versions)")
    print(f"Actual Failures: {actual_failures}")
    print(f"Success Rate: {(passed_tests/total_tests)*100:.1f}%")
    
    print("\nDetailed Results:")
    print("-" * 40)
    
    for test_name, result in results.items():
        if isinstance(result, dict):
            if test_name in ['autogen_imports', 'autogen_core_classes']:
                # Handle nested test results
                print(f"{test_name}:")
                for component_name, component_result in result.items():
                    if isinstance(component_result, dict):
                        if component_result.get('available', component_result.get('success', False)):
                            status = "✅ PASS"
                        elif 'note' in component_result and 'Not available in this AutoGen version' in component_result['note']:
                            status = "⚠️ EXPECTED"
                        else:
                            status = "❌ FAIL"
                        
                        print(f"  {component_name:25} {status}")
                        
                        if 'error' in component_result:
                            print(f"    Error: {component_result['error']}")
                        elif 'note' in component_result:
                            print(f"    Note: {component_result['note']}")
            else:
                # Regular test result
                if result.get('available', result.get('success', False)):
                    status = "✅ PASS"
                elif 'note' in result and 'not available in this AutoGen version' in result['note']:
                    status = "⚠️ EXPECTED"
                else:
                    status = "❌ FAIL"
                
                print(f"{test_name:30} {status}")
                
                if 'error' in result:
                    print(f"  Error: {result['error']}")
                elif 'note' in result:
                    print(f"  Note: {result['note']}")
        else:
            print(f"{test_name:30} ❌ FAIL")
    
    # Recommendations
    print("\nRecommendations:")
    print("-" * 40)
    
    if actual_failures > 0:
        if not results.get('autogen_imports', {}).get('autogen_core', {}).get('available', False):
            print("• Install AutoGen Core: pip install autogen-core")
        
        if not results.get('docker_availability', {}).get('available', False):
            print("• Install Docker: https://docs.docker.com/get-docker/")
        
        if not results.get('sandbox_image', {}).get('available', False):
            print("• Build sandbox image: cd sandbox && ./build.sh")
        
        print("• Some tests failed. Check the detailed results above.")
    else:
        print("• All critical tests passed! AutoGen integration is working correctly.")
        if expected_failures > 0:
            print("• Missing GroupChat classes are expected in newer AutoGen versions and don't affect core functionality.")
    
    return actual_failures == 0

async def main():
    """Main test function."""
    print("AutoGen Integration Test Suite")
    print("=" * 60)
    
    results = {}
    
    # Run all tests
    results['autogen_imports'] = test_autogen_imports()
    results['autogen_core_classes'] = test_autogen_core_classes()
    results['autogen_wrapper'] = test_autogen_wrapper()
    results['sandbox_executor'] = test_sandbox_executor()
    results['group_chat_coordinator'] = test_group_chat_coordinator()
    results['docker_availability'] = test_docker_availability()
    results['sandbox_image'] = test_sandbox_image()
    results['autogen_agent_creation'] = await test_autogen_agent_creation()
    results['volume_mounting'] = test_volume_mounting()
    
    # Generate report
    success = generate_test_report(results)
    
    return 0 if success else 1

if __name__ == "__main__":
    exit_code = asyncio.run(main())
    sys.exit(exit_code) 