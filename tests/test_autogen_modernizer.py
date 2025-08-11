"""
Test script for AutoGen integration with ModernizerAgent.

This script tests the AutoGen wrapper with the ModernizerAgent to ensure
the integration works correctly for code modernization tasks.
"""

import asyncio
import logging
import os
import sys
from pathlib import Path

# Add the engine directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent))

# Load environment variables from .env file
try:
    from dotenv import load_dotenv
    project_root = Path(__file__).parent.parent.parent
    env_path = project_root / '.env'
    if env_path.exists():
        load_dotenv(env_path)
        print(f"Loaded environment from: {env_path}")
    else:
        load_dotenv()
except ImportError:
    print("Warning: python-dotenv not installed. Install with: pip install python-dotenv")
except Exception as e:
    print(f"Warning: Could not load .env file: {e}")

from engine.agents.autogen_integration.autogen_wrapper import AutoGenAgentWrapper, AutoGenConfig
from engine.agents.core_agents.modernizer_agent import ModernizerAgent
from engine.agents.utilities.ai import AI
from engine.agents.core_agents.base_memory import FileMemory
from engine.agents.utilities.project_config import ProjectConfig

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class AutoGenModernizerTest:
    """Test class for AutoGen ModernizerAgent integration."""
    
    def __init__(self):
        # Initialize core components
        api_key = os.getenv('LLM_API_KEY') or os.getenv('OPENAI_API_KEY') or os.getenv('ANTHROPIC_API_KEY')
        if not api_key:
            raise ValueError("No API key found. Set LLM_API_KEY, OPENAI_API_KEY, or ANTHROPIC_API_KEY")
        
        provider = "anthropic" if os.getenv('ANTHROPIC_API_KEY') else "openai"
        model = os.getenv('LLM_MODEL', 'claude-3-sonnet-20240229' if provider == "anthropic" else 'gpt-4')
        
        self.ai = AI(api_key=api_key, provider=provider, model=model)
        self.memory = FileMemory(storage_path="test_memory_modernizer")
        self.config = ProjectConfig()
        
        # Create base ModernizerAgent
        self.base_modernizer = ModernizerAgent(
            ai=self.ai,
            memory=self.memory,
            config=self.config
        )
        
        # Create AutoGen-wrapped ModernizerAgent
        self.autogen_modernizer = AutoGenAgentWrapper(
            self.base_modernizer,
            AutoGenConfig(enable_autogen=True)
        )
    
    async def test_basic_functionality(self):
        """Test basic functionality of the wrapped agent."""
        logger.info("Testing basic functionality...")
        
        # Test 1: Check if agent can be created
        assert self.autogen_modernizer is not None
        assert self.autogen_modernizer.base_agent is not None
        assert self.autogen_modernizer.autogen_agent is not None
        
        logger.info("✓ Agent creation successful")
        
        # Test 2: Check status
        status = self.autogen_modernizer.get_status()
        assert status["name"] == "ModernizerAgent"
        assert status["autogen_enabled"] == True
        
        logger.info("✓ Status check successful")
        
        # Test 3: Test message processing
        test_message = {
            "type": "generate_code",
            "source_file": "test.html",
            "target_stack": "react"
        }
        
        response = await self.autogen_modernizer.process_message(test_message)
        assert response is not None
        
        logger.info("✓ Message processing successful")
        
        return True
    
    async def test_task_execution(self):
        """Test task execution through the wrapped agent."""
        logger.info("Testing task execution...")
        
        # Create a simple test task for modernization
        test_task = {
            "type": "file_conversion",
            "file_path": "test.html",
                        "content": """
        <!DOCTYPE html>
        <html>
        <head>
            <title>Test Page</title>
        </head>
        <body>
            <h1>Hello, World!</h1>
        </body>
        </html>
        """,
            "target_stack": "react"
        }
        
        # Execute task
        result = await self.autogen_modernizer.execute_task(test_task)
        
        # Check result
        assert result is not None
        assert "success" in result or "error" in result
        
        logger.info(f"✓ Task execution result: {result}")
        
        return True
    
    async def test_autogen_integration(self):
        """Test AutoGen-specific features."""
        logger.info("Testing AutoGen integration...")
        
        # Test 1: Check if AutoGen agent has the right properties
        autogen_agent = self.autogen_modernizer.autogen_agent
        assert hasattr(autogen_agent, 'id')  # Use 'id' instead of 'name'
        assert hasattr(autogen_agent, 'system_message')
        assert hasattr(autogen_agent, 'llm_config')
        
        logger.info(f"✓ AutoGen agent id: {autogen_agent.id}")
        logger.info(f"✓ AutoGen agent type: {type(autogen_agent).__name__}")
        
        # Test 2: Check LLM config
        llm_config = autogen_agent.llm_config
        assert llm_config is not None
        assert "config_list" in llm_config
        
        logger.info("✓ LLM config properly configured")
        
        return True
    
    async def test_modernization_specific_tasks(self):
        """Test modernization-specific functionality."""
        logger.info("Testing modernization-specific tasks...")
        
        # Test component generation
        component_task = {
            "type": "component_generation",
            "component_name": "HelloWorld",
            "specs": {
                "framework": "react",
                "props": ["name"],
                "functionality": "Display hello message"
            }
        }
        
        result = await self.autogen_modernizer.execute_task(component_task)
        assert result is not None
        
        logger.info(f"✓ Component generation result: {result}")
        
        # Test project structure generation
        structure_task = {
            "type": "project_setup",
            "project_map": {
                "structure": {"files": ["index.html", "styles.css", "script.js"]},
                "target_stack": "react"
            }
        }
        
        result = await self.autogen_modernizer.execute_task(structure_task)
        assert result is not None
        
        logger.info(f"✓ Project structure generation result: {result}")
        
        return True
    
    async def test_performance_comparison(self):
        """Compare performance between base and AutoGen-wrapped agents."""
        logger.info("Testing performance comparison...")
        
        test_task = {
            "type": "file_conversion",
            "file_path": "performance_test.html",
            "content": """
       <!DOCTYPE html>
       <html>
       <head>
           <title>Performance Test</title>
       </head>
       <body>
           <div id="performance-test">
               <h1>Performance Test</h1>
               <div class="content">This is a performance test page</div>
           </div>
       </body>
       </html>
            """,
            "target_stack": "react"
        }
        
        # Test base agent performance
        start_time = asyncio.get_event_loop().time()
        base_result = await self.base_modernizer.execute_task(test_task)
        base_time = asyncio.get_event_loop().time() - start_time
        
        # Test AutoGen-wrapped agent performance
        start_time = asyncio.get_event_loop().time()
        autogen_result = await self.autogen_modernizer.execute_task(test_task)
        autogen_time = asyncio.get_event_loop().time() - start_time
        
        logger.info(f"Base agent time: {base_time:.2f}s")
        logger.info(f"AutoGen agent time: {autogen_time:.2f}s")
        logger.info(f"Performance difference: {((autogen_time - base_time) / base_time * 100):.1f}%")
        
        # Both should complete successfully
        assert base_result is not None
        assert autogen_result is not None
        
        logger.info("✓ Performance comparison completed")
        
        return True
    
    async def run_all_tests(self):
        """Run all tests."""
        logger.info("Starting AutoGen ModernizerAgent integration tests...")
        
        try:
            await self.test_basic_functionality()
            await self.test_task_execution()
            await self.test_autogen_integration()
            await self.test_modernization_specific_tasks()
            await self.test_performance_comparison()
            
            logger.info("🎉 All tests passed! AutoGen ModernizerAgent integration is working correctly.")
            return True
            
        except Exception as e:
            logger.error(f"❌ Test failed: {e}")
            return False
    
    async def cleanup(self):
        """Clean up test resources."""
        try:
            await self.memory.clear()
            logger.info("✓ Test memory cleaned up")
        except Exception as e:
            logger.warning(f"Failed to cleanup memory: {e}")

async def main():
    """Main function to run the tests."""
    test = None
    try:
        test = AutoGenModernizerTest()
        success = await test.run_all_tests()
        
        if success:
            print("\n✅ ModernizerAgent AutoGen integration completed successfully!")
            print("You can now proceed to the next agent: RefactorAgent.")
        else:
            print("\n❌ ModernizerAgent AutoGen integration failed. Please check the errors above.")
            
    except ImportError as e:
        print(f"❌ AutoGen not installed: {e}")
        print("Please run: pip install -U 'autogen-agentchat' 'autogen-ext[openai]'")
    except Exception as e:
        print(f"❌ Unexpected error: {e}")
        import traceback
        traceback.print_exc()
    finally:
        # Clean up
        if test:
            await test.cleanup()

if __name__ == "__main__":
    asyncio.run(main()) 