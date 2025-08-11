"""
Test script for AutoGen integration with Anthropic API.

This script specifically tests the AutoGen wrapper with Anthropic to ensure
the integration works correctly with ANTHROPIC_API_KEY.
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
    # Find and load .env file from project root
    project_root = Path(__file__).parent.parent.parent
    env_path = project_root / '.env'
    if env_path.exists():
        load_dotenv(env_path)
        print(f"Loaded environment from: {env_path}")
    else:
        print(f"Warning: .env file not found at {env_path}")
        # Try loading from current directory
        load_dotenv()
except ImportError:
    print("Warning: python-dotenv not installed. Install with: pip install python-dotenv")
except Exception as e:
    print(f"Warning: Could not load .env file: {e}")

from agents.autogen_wrapper import AutoGenAgentWrapper, AutoGenConfig, create_llm_config
from agents.parser_agent import ParserAgent
from agents.ai import AI
from agents.base_memory import FileMemory  # Use concrete implementation
from agents.project_config import ProjectConfig

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class AnthropicAutoGenTest:
    """Test class for AutoGen with Anthropic integration."""
    
    def __init__(self):
        # Check for Anthropic API key
        api_key = os.getenv('ANTHROPIC_API_KEY')
        if not api_key:
            # Try alternative environment variable names
            api_key = os.getenv('LLM_API_KEY') or os.getenv('OPENAI_API_KEY')
            if api_key:
                print(f"Found API key from alternative environment variable")
            else:
                print("Available environment variables:")
                for key, value in os.environ.items():
                    if 'API' in key or 'KEY' in key:
                        print(f"  {key}: {'*' * len(value) if value else 'None'}")
                raise ValueError("ANTHROPIC_API_KEY not found. Please set it in your .env file or environment.")
        
        print(f"Using API key: {api_key[:10]}..." if api_key else "No API key found")
        
        # Initialize AI with Anthropic
        self.ai = AI(
            api_key=api_key,
            provider="anthropic",
            model="claude-3-sonnet-20240229"
        )
        
        # Use FileMemory instead of BaseMemory
        self.memory = FileMemory(storage_path="test_memory")
        self.config = ProjectConfig()
        
        # Create base ParserAgent
        self.base_parser = ParserAgent(
            ai=self.ai,
            memory=self.memory,
            config=self.config
        )
        
        # Create AutoGen-wrapped ParserAgent
        self.autogen_parser = AutoGenAgentWrapper(
            self.base_parser,
            AutoGenConfig(enable_autogen=True)
        )
    
    async def test_anthropic_config(self):
        """Test that Anthropic configuration is correct."""
        logger.info("Testing Anthropic configuration...")
        
        # Check AI configuration
        assert self.ai.provider == "anthropic"
        assert "claude" in self.ai.model.lower()
        assert self.ai.api_key is not None
        
        logger.info(f"✓ AI configured for Anthropic: {self.ai.provider}")
        logger.info(f"✓ Model: {self.ai.model}")
        
        # Check AutoGen agent configuration
        autogen_agent = self.autogen_parser.autogen_agent
        llm_config = autogen_agent.llm_config
        
        assert llm_config is not None
        assert "config_list" in llm_config
        assert len(llm_config["config_list"]) > 0
        
        config_item = llm_config["config_list"][0]
        assert config_item["api_type"] == "anthropic"
        assert config_item["api_base"] == "https://api.anthropic.com"
        assert "claude" in config_item["model"].lower()
        
        logger.info(f"✓ AutoGen LLM config: {config_item}")
        
        return True
    
    async def test_anthropic_communication(self):
        """Test communication with Anthropic through AutoGen."""
        logger.info("Testing Anthropic communication through AutoGen...")
        
        # Test simple message
        test_message = {
            "type": "analysis",
            "content": "Please analyze this HTML code: <!DOCTYPE html><html><head><title>Test</title></head><body><h1>Hello</h1></body></html>"
        }
        
        response = await self.autogen_parser.process_message(test_message)
        
        assert response is not None
        logger.info(f"✓ Anthropic response received: {response}")
        
        return True
    
    async def test_anthropic_task_execution(self):
        """Test task execution with Anthropic."""
        logger.info("Testing task execution with Anthropic...")
        
        test_task = {
            "type": "file_analysis",
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
        """
        }
        
        result = await self.autogen_parser.execute_task(test_task)
        
        assert result is not None
        logger.info(f"✓ Task execution result: {result}")
        
        return True
    
    async def test_llm_config_creation(self):
        """Test the helper function for creating LLM configs."""
        logger.info("Testing LLM config creation helper...")
        
        # Test Anthropic config
        anthropic_config = create_llm_config(
            api_key="test-key",
            provider="anthropic",
            model="claude-3-sonnet-20240229"
        )
        
        assert anthropic_config["config_list"][0]["api_type"] == "anthropic"
        assert anthropic_config["config_list"][0]["api_base"] == "https://api.anthropic.com"
        
        # Test OpenAI config
        openai_config = create_llm_config(
            api_key="test-key",
            provider="openai",
            model="gpt-4"
        )
        
        assert openai_config["config_list"][0]["api_type"] == "openai"
        assert openai_config["config_list"][0]["api_base"] == "https://api.openai.com/v1"
        
        logger.info("✓ LLM config creation helper working correctly")
        
        return True
    
    async def run_all_tests(self):
        """Run all Anthropic AutoGen tests."""
        logger.info("Starting Anthropic AutoGen integration tests...")
        
        try:
            await self.test_anthropic_config()
            await self.test_anthropic_communication()
            await self.test_anthropic_task_execution()
            await self.test_llm_config_creation()
            
            logger.info(" All Anthropic AutoGen tests passed!")
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
    """Main function to run the Anthropic tests."""
    test = None
    try:
        test = AnthropicAutoGenTest()
        success = await test.run_all_tests()
        
        if success:
            print("\n✅ Anthropic AutoGen integration working correctly!")
            print("You can now use ANTHROPIC_API_KEY with AutoGen.")
        else:
            print("\n❌ Anthropic AutoGen tests failed. Please check the errors above.")
            
    except ValueError as e:
        print(f"❌ Configuration error: {e}")
        print("Please set ANTHROPIC_API_KEY in your .env file or environment.")
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