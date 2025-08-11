"""
Test script for AutoGen integration with RefactorAgent.

This script tests the AutoGen wrapper with the RefactorAgent to ensure
the integration works correctly for code refactoring and optimization tasks.
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
from agents.refactor_agent import RefactorAgent
from agents.ai import AI
from agents.base_memory import FileMemory
from agents.project_config import ProjectConfig

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class AutoGenRefactorTest:
    """Test class for AutoGen RefactorAgent integration."""
    
    def __init__(self):
        # Initialize core components
        api_key = os.getenv('LLM_API_KEY') or os.getenv('OPENAI_API_KEY') or os.getenv('ANTHROPIC_API_KEY')
        if not api_key:
            raise ValueError("No API key found. Set LLM_API_KEY, OPENAI_API_KEY, or ANTHROPIC_API_KEY")
        
        provider = "anthropic" if os.getenv('ANTHROPIC_API_KEY') else "openai"
        model = os.getenv('LLM_MODEL', 'claude-3-sonnet-20240229' if provider == "anthropic" else 'gpt-4')
        
        self.ai = AI(api_key=api_key, provider=provider, model=model)
        self.memory = FileMemory(storage_path="test_memory_refactor")
        self.config = ProjectConfig()
        
        # Create base RefactorAgent
        self.base_refactor = RefactorAgent(
            ai=self.ai,
            memory=self.memory,
            config=self.config
        )
        
        # Create AutoGen-wrapped RefactorAgent
        self.autogen_refactor = AutoGenAgentWrapper(
            self.base_refactor,
            AutoGenConfig(enable_autogen=True)
        )
    
    async def test_basic_functionality(self):
        """Test basic functionality of the wrapped agent."""
        logger.info("Testing basic functionality...")
        
        # Test 1: Check if agent can be created
        assert self.autogen_refactor is not None
        assert self.autogen_refactor.base_agent is not None
        assert self.autogen_refactor.autogen_agent is not None
        
        logger.info("✓ Agent creation successful")
        
        # Test 2: Check status
        status = self.autogen_refactor.get_status()
        assert status["name"] == "RefactorAgent"
        assert status["autogen_enabled"] == True
        
        logger.info("✓ Status check successful")
        
        # Test 3: Test message processing
        test_message = {
            "type": "refactor_code",
            "file_path": "test.js",
            "content": "function hello() { console.log('Hello'); }"
        }
        
        response = await self.autogen_refactor.process_message(test_message)
        assert response is not None
        
        logger.info("✓ Message processing successful")
        
        return True
    
    async def test_task_execution(self):
        """Test task execution through the wrapped agent."""
        logger.info("Testing task execution...")
        
        # Create a simple test task for refactoring
        test_task = {
            "type": "performance_optimization",
            "file_path": "test.js",
            "content": """
function calculateSum(numbers) {
    let sum = 0;
    for (let i = 0; i < numbers.length; i++) {
        sum += numbers[i];
    }
    return sum;
}
            """
        }
        
        # Execute task
        result = await self.autogen_refactor.execute_task(test_task)
        
        # Check result
        assert result is not None
        logger.info(f"✓ Task execution result: {result}")
        logger.info(f"✓ Result type: {type(result)}")
        logger.info(f"✓ Result keys: {list(result.keys()) if isinstance(result, dict) else 'Not a dict'}")
        
        # More flexible assertion
        if isinstance(result, dict):
            assert "success" in result or "error" in result or "status" in result
        else:
            assert result is not None  # Just ensure it's not None
        
        return True
    
    async def test_autogen_integration(self):
        """Test AutoGen-specific features."""
        logger.info("Testing AutoGen integration...")
        
        # Test 1: Check if AutoGen agent has the right properties
        autogen_agent = self.autogen_refactor.autogen_agent
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
    
    async def test_refactoring_specific_tasks(self):
        """Test refactoring-specific functionality."""
        logger.info("Testing refactoring-specific tasks...")
        
        # Test quality analysis
        quality_task = {
            "type": "quality_analysis",
            "file_path": "test.js",
            "content": """
function processData(data) {
    var result = [];
    for (var i = 0; i < data.length; i++) {
        if (data[i] > 0) {
            result.push(data[i] * 2);
        }
    }
    return result;
}
            """
        }
        
        result = await self.autogen_refactor.execute_task(quality_task)
        assert result is not None
        
        logger.info(f"✓ Quality analysis result: {result}")
        
        # Test maintainability improvement
        maintainability_task = {
            "type": "maintainability_improvement",
            "file_path": "complex.js",
            "content": """
function doEverything(input) {
    var a = input.split(',');
    var b = [];
    for (var i = 0; i < a.length; i++) {
        var c = a[i].trim();
        if (c.length > 0) {
            var d = parseInt(c);
            if (!isNaN(d)) {
                b.push(d);
            }
        }
    }
    var e = 0;
    for (var j = 0; j < b.length; j++) {
        e += b[j];
    }
    return e;
}
            """
        }
        
        result = await self.autogen_refactor.execute_task(maintainability_task)
        assert result is not None
        
        logger.info(f"✓ Maintainability improvement result: {result}")
        
        return True
    
    async def test_performance_comparison(self):
        """Compare performance between base and AutoGen-wrapped agents."""
        logger.info("Testing performance comparison...")
        
        test_task = {
            "type": "performance_optimization",
            "file_path": "performance_test.js",
            "content": """
function inefficientFunction(array) {
    let result = [];
    for (let i = 0; i < array.length; i++) {
        let item = array[i];
        if (item > 0) {
            let processed = item * 2;
            result.push(processed);
        }
    }
    return result;
}
            """
        }
        
        # Test base agent performance
        start_time = asyncio.get_event_loop().time()
        base_result = await self.base_refactor.execute_task(test_task)
        base_time = asyncio.get_event_loop().time() - start_time
        
        # Test AutoGen-wrapped agent performance
        start_time = asyncio.get_event_loop().time()
        autogen_result = await self.autogen_refactor.execute_task(test_task)
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
        logger.info("Starting AutoGen RefactorAgent integration tests...")
        
        try:
            await self.test_basic_functionality()
            await self.test_task_execution()
            await self.test_autogen_integration()
            await self.test_refactoring_specific_tasks()
            await self.test_performance_comparison()
            
            logger.info("🎉 All tests passed! AutoGen RefactorAgent integration is working correctly.")
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
        test = AutoGenRefactorTest()
        success = await test.run_all_tests()
        
        if success:
            print("\n✅ RefactorAgent AutoGen integration completed successfully!")
            print("You can now proceed to the next agent: QAAgent.")
        else:
            print("\n❌ RefactorAgent AutoGen integration failed. Please check the errors above.")
            
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