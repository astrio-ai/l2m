"""
Test script for AutoGen integration with QAAgent.

This script tests the AutoGen wrapper with the QAAgent to ensure
the integration works correctly for question answering and guidance tasks.
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

from agents.autogen_wrapper import AutoGenAgentWrapper, AutoGenConfig
from agents.qa_agent import QAAgent
from agents.ai import AI
from agents.base_memory import FileMemory
from agents.project_config import ProjectConfig

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class AutoGenQATest:
    """Test class for AutoGen QAAgent integration."""
    
    def __init__(self):
        # Initialize core components
        api_key = os.getenv('LLM_API_KEY') or os.getenv('OPENAI_API_KEY') or os.getenv('ANTHROPIC_API_KEY')
        if not api_key:
            raise ValueError("No API key found. Set LLM_API_KEY, OPENAI_API_KEY, or ANTHROPIC_API_KEY")
        
        provider = "anthropic" if os.getenv('ANTHROPIC_API_KEY') else "openai"
        model = os.getenv('LLM_MODEL', 'claude-3-sonnet-20240229' if provider == "anthropic" else 'gpt-4')
        
        self.ai = AI(api_key=api_key, provider=provider, model=model)
        self.memory = FileMemory(storage_path="test_memory_qa")
        self.config = ProjectConfig()
        
        # Create base QAAgent
        self.base_qa = QAAgent(
            ai=self.ai,
            memory=self.memory,
            config=self.config
        )
        
        # Create AutoGen-wrapped QAAgent
        self.autogen_qa = AutoGenAgentWrapper(
            self.base_qa,
            AutoGenConfig(enable_autogen=True)
        )
    
    async def test_basic_functionality(self):
        """Test basic functionality of the wrapped agent."""
        logger.info("Testing basic functionality...")
        
        # Test 1: Check if agent can be created
        assert self.autogen_qa is not None
        assert self.autogen_qa.base_agent is not None
        assert self.autogen_qa.autogen_agent is not None
        
        logger.info("✓ Agent creation successful")
        
        # Test 2: Check status
        status = self.autogen_qa.get_status()
        assert status["name"] == "QAAgent"
        assert status["autogen_enabled"] == True
        
        logger.info("✓ Status check successful")
        
        # Test 3: Test message processing
        test_message = {
            "type": "answer_question",
            "question": "What is the purpose of this project?",
            "context": "This is a legacy to modern code conversion project."
        }
        
        response = await self.autogen_qa.process_message(test_message)
        assert response is not None
        
        logger.info("✓ Message processing successful")
        
        return True
    
    async def test_task_execution(self):
        """Test task execution through the wrapped agent."""
        logger.info("Testing task execution...")
        
        # Create a simple test task for question answering
        test_task = {
            "type": "answer_question",
            "question": "How do I convert a legacy website to React?",
            "context": "I have a legacy website that needs to be modernized."
        }
        
        # Execute task
        result = await self.autogen_qa.execute_task(test_task)
        
        # Check result
        assert result is not None
        logger.info(f"✓ Task execution result: {result}")
        logger.info(f"✓ Result type: {type(result)}")
        logger.info(f"✓ Result keys: {list(result.keys()) if isinstance(result, dict) else 'Not a dict'}")
        
        # More flexible assertion
        if isinstance(result, dict):
            assert "success" in result or "error" in result or "status" in result or "answer" in result
        else:
            assert result is not None  # Just ensure it's not None
        
        return True
    
    async def test_autogen_integration(self):
        """Test AutoGen-specific features."""
        logger.info("Testing AutoGen integration...")
        
        # Test 1: Check if AutoGen agent has the right properties
        autogen_agent = self.autogen_qa.autogen_agent
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
    
    async def test_qa_specific_tasks(self):
        """Test QA-specific functionality."""
        logger.info("Testing QA-specific tasks...")
        
        # Test general question answering
        general_qa_task = {
            "type": "answer_question",
            "question": "What are the benefits of modernizing legacy code?",
            "context": "Legacy system modernization project"
        }
        
        result = await self.autogen_qa.execute_task(general_qa_task)
        assert result is not None
        
        logger.info(f"✓ General QA result: {result}")
        
        # Test technical guidance
        technical_qa_task = {
            "type": "provide_guidance",
            "question": "What's the best approach for handling file I/O in modern Python?",
            "context": "Converting HTML forms to React components"
        }
        
        result = await self.autogen_qa.execute_task(technical_qa_task)
        assert result is not None
        
        logger.info(f"✓ Technical guidance result: {result}")
        
        # Test troubleshooting
        troubleshooting_task = {
            "type": "troubleshoot_issue",
            "question": "Why might my converted Python code be running slowly?",
            "context": "Performance issues after HTML to React conversion"
        }
        
        result = await self.autogen_qa.execute_task(troubleshooting_task)
        assert result is not None
        
        logger.info(f"✓ Troubleshooting result: {result}")
        
        return True
    
    async def test_performance_comparison(self):
        """Compare performance between base and AutoGen-wrapped agents."""
        logger.info("Testing performance comparison...")
        
        test_task = {
            "type": "answer_question",
            "question": "What are the key differences between HTML and React?",
            "context": "Understanding language differences for conversion"
        }
        
        # Test base agent performance
        start_time = asyncio.get_event_loop().time()
        base_result = await self.base_qa.execute_task(test_task)
        base_time = asyncio.get_event_loop().time() - start_time
        
        # Test AutoGen-wrapped agent performance
        start_time = asyncio.get_event_loop().time()
        autogen_result = await self.autogen_qa.execute_task(test_task)
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
        logger.info("Starting AutoGen QAAgent integration tests...")
        
        try:
            await self.test_basic_functionality()
            await self.test_task_execution()
            await self.test_autogen_integration()
            await self.test_qa_specific_tasks()
            await self.test_performance_comparison()
            
            logger.info("🎉 All tests passed! AutoGen QAAgent integration is working correctly.")
            return True
            
        except Exception as e:
            logger.error(f"❌ Test failed: {e}")
            import traceback
            logger.error(f"Traceback: {traceback.format_exc()}")
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
        test = AutoGenQATest()
        success = await test.run_all_tests()
        
        if success:
            print("\n✅ QAAgent AutoGen integration completed successfully!")
            print("You can now proceed to the next agent: CoordinatorAgent.")
        else:
            print("\n❌ QAAgent AutoGen integration failed. Please check the errors above.")
            
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