"""
Simple test script for Group Chat Coordinator functionality.
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

from engine.agents.autogen_integration.group_chat_coordinator import GroupChatCoordinator, ChatType
from engine.agents.autogen_integration.autogen_wrapper import AutoGenConfig
from engine.agents.core_agents.parser_agent import ParserAgent
from engine.agents.core_agents.modernizer_agent import ModernizerAgent
from engine.agents.core_agents.refactor_agent import RefactorAgent
from engine.agents.core_agents.qa_agent import QAAgent
from engine.agents.core_agents.coordinator_agent import CoordinatorAgent
from engine.agents.utilities.ai import AI
from engine.agents.core_agents.base_memory import FileMemory
from engine.agents.utilities.project_config import ProjectConfig

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

async def test_basic_group_chat():
    """Test basic group chat functionality."""
    print("Testing basic group chat functionality...")
    
    try:
        # Initialize components
        api_key = os.getenv('ANTHROPIC_API_KEY')
        if not api_key:
            print("❌ No API key found")
            return False
        
        ai = AI(api_key=api_key, provider="anthropic", model="claude-3-sonnet-20240229")
        memory = FileMemory(storage_path="test_memory_simple")
        config = ProjectConfig()
        
        # Create group chat coordinator
        group_chat = GroupChatCoordinator(
            ai=ai,
            memory=memory,
            config=config,
            autogen_config=AutoGenConfig(enable_autogen=True, use_group_chat=True)
        )
        
        # Create and register agents
        agents = {
            "parser": ParserAgent(ai=ai, memory=memory, config=config),
            "modernizer": ModernizerAgent(ai=ai, memory=memory, config=config),
            "refactor": RefactorAgent(ai=ai, memory=memory, config=config),
            "qa": QAAgent(ai=ai, memory=memory, config=config),
            "coordinator": CoordinatorAgent(name="CoordinatorAgent", ai=ai, memory=memory, config=config)
        }
        
        # Register agents
        for name, agent in agents.items():
            agent_id = await group_chat.register_agent(agent, role=name)
            print(f"✓ Registered {name} as {agent_id}")
        
        # Test session creation
        session_id = await group_chat.create_group_chat(
            chat_type=ChatType.WORKFLOW_COORDINATION,
            topic="Simple Test",
            participants=list(group_chat.agents.keys()),
            max_rounds=3
        )
        print(f"✓ Created session: {session_id}")
        
        # Test session status
        status = await group_chat.get_session_status(session_id)
        print(f"✓ Session status: {status}")
        
        # Test coordination stats
        stats = await group_chat.get_coordination_stats()
        print(f"✓ Coordination stats: {stats}")
        
        # Test simple group chat
        result = await group_chat.start_group_chat(session_id, "Hello, let's test our coordination!")
        print(f"✓ Group chat result: {result['status']}")
        
        # Test session closure
        close_result = await group_chat.close_session(session_id)
        print(f"✓ Session closed: {close_result['status']}")
        
        print("✅ Basic group chat test completed successfully!")
        return True
        
    except Exception as e:
        print(f"❌ Test failed: {e}")
        import traceback
        traceback.print_exc()
        return False

async def main():
    """Main function."""
    success = await test_basic_group_chat()
    
    if success:
        print("\n🎉 Phase 3 Group Chat functionality is working!")
    else:
        print("\n❌ Group Chat functionality needs more work.")

if __name__ == "__main__":
    asyncio.run(main()) 