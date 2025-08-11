"""
Test script for Group Chat Coordinator functionality.

This script tests the comprehensive group chat system that enables
multi-agent communication and coordination using AutoGen.
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

from agents.group_chat_coordinator import GroupChatCoordinator, ChatType
from agents.autogen_wrapper import AutoGenConfig
from agents.parser_agent import ParserAgent
from agents.modernizer_agent import ModernizerAgent
from agents.refactor_agent import RefactorAgent
from agents.qa_agent import QAAgent
from agents.coordinator_agent import CoordinatorAgent
from agents.ai import AI
from agents.base_memory import FileMemory
from agents.project_config import ProjectConfig

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class GroupChatTest:
    """Test class for Group Chat Coordinator functionality."""
    
    def __init__(self):
        # Initialize core components
        api_key = os.getenv('LLM_API_KEY') or os.getenv('OPENAI_API_KEY') or os.getenv('ANTHROPIC_API_KEY')
        if not api_key:
            raise ValueError("No API key found. Set LLM_API_KEY, OPENAI_API_KEY, or ANTHROPIC_API_KEY")
        
        provider = "anthropic" if os.getenv('ANTHROPIC_API_KEY') else "openai"
        model = os.getenv('LLM_MODEL', 'claude-3-sonnet-20240229' if provider == "anthropic" else 'gpt-4')
        
        self.ai = AI(api_key=api_key, provider=provider, model=model)
        self.memory = FileMemory(storage_path="test_memory_group_chat")
        self.config = ProjectConfig()
        
        # Create AutoGen config for group chat
        self.autogen_config = AutoGenConfig(
            enable_autogen=True,
            use_group_chat=True,
            human_in_the_loop=False,
            max_consecutive_auto_reply=5
        )
        
        # Create group chat coordinator
        self.group_chat = GroupChatCoordinator(
            ai=self.ai,
            memory=self.memory,
            config=self.config,
            autogen_config=self.autogen_config
        )
        
        # Create and register agents
        self.agents = {}
        self._create_agents()
    
    def _create_agents(self):
        """Create and register all agents for group chat."""
        # Create base agents
        parser = ParserAgent(
            ai=self.ai,
            memory=self.memory,
            config=self.config
        )
        
        modernizer = ModernizerAgent(
            ai=self.ai,
            memory=self.memory,
            config=self.config
        )
        
        refactor = RefactorAgent(
            ai=self.ai,
            memory=self.memory,
            config=self.config
        )
        
        qa = QAAgent(
            ai=self.ai,
            memory=self.memory,
            config=self.config
        )
        
        coordinator = CoordinatorAgent(
            name="CoordinatorAgent",
            ai=self.ai,
            memory=self.memory,
            config=self.config
        )
        
        # Store agents
        self.agents = {
            "parser": parser,
            "modernizer": modernizer,
            "refactor": refactor,
            "qa": qa,
            "coordinator": coordinator
        }
    
    async def test_agent_registration(self):
        """Test agent registration with the group chat coordinator."""
        logger.info("Testing agent registration...")
        
        try:
            # Register all agents
            agent_ids = []
            for name, agent in self.agents.items():
                agent_id = await self.group_chat.register_agent(agent, role=name)
                agent_ids.append(agent_id)
                logger.info(f"✓ Registered {name} as {agent_id}")
            
            # Check registration
            stats = await self.group_chat.get_coordination_stats()
            assert stats["registered_agents"] == len(self.agents)
            assert len(stats["agent_roles"]) == len(self.agents)
            
            logger.info(f"✓ All {len(agent_ids)} agents registered successfully")
            return True
            
        except Exception as e:
            logger.error(f"❌ Agent registration failed: {e}")
            return False
    
    async def test_group_chat_creation(self):
        """Test group chat session creation."""
        logger.info("Testing group chat creation...")
        
        try:
            # Create a workflow coordination session
            session_id = await self.group_chat.create_group_chat(
                chat_type=ChatType.WORKFLOW_COORDINATION,
                topic="Test Workflow Coordination",
                participants=list(self.group_chat.agents.keys()),
                max_rounds=5
            )
            
            assert session_id is not None
            assert session_id in self.group_chat.active_sessions
            
            # Check session status
            status = await self.group_chat.get_session_status(session_id)
            assert status["status"] != "not_found"
            assert status["topic"] == "Test Workflow Coordination"
            assert status["is_active"] == True
            
            logger.info(f"✓ Group chat session {session_id} created successfully")
            return session_id
            
        except Exception as e:
            logger.error(f"❌ Group chat creation failed: {e}")
            return None
    
    async def test_workflow_coordination(self):
        """Test workflow coordination using group chat."""
        logger.info("Testing workflow coordination...")
        
        try:
            # Create workflow configuration
            workflow_config = {
                "name": "Test Legacy to Modern Conversion",
                "project_path": "/test/project",
                "target_stack": "react",
                "files": ["main.js", "utils.js", "styles.css"],
                "workflow_id": "test_workflow_123"
            }
            
            # Coordinate workflow
            result = await self.group_chat.coordinate_workflow(workflow_config)
            
            assert result is not None
            assert "coordination_result" in result
            assert "coordination_plan" in result
            
            logger.info(f"✓ Workflow coordination completed: {result['status']}")
            logger.info(f"✓ Coordination plan extracted: {len(result['coordination_plan'])} items")
            
            return True
            
        except Exception as e:
            logger.error(f"❌ Workflow coordination failed: {e}")
            return False
    
    async def test_problem_solving(self):
        """Test problem solving using group chat."""
        logger.info("Testing problem solving...")
        
        try:
            # Define a problem to solve
            problem = """
            We have a legacy website that needs to be modernized to React.
            The system has complex business logic and file I/O operations.
            We need to determine the best approach for:
            1. Handling the complex business logic
            2. Converting file operations to modern APIs
            3. Ensuring data integrity during migration
            4. Testing strategy for the modernized system
            """
            
            # Solve the problem
            result = await self.group_chat.solve_problem(problem)
            
            assert result is not None
            assert "solution" in result
            assert "discussion" in result
            
            solution = result["solution"]
            logger.info(f"✓ Problem solving completed: {result['status']}")
            logger.info(f"✓ Solution extracted: {len(solution.get('recommendations', []))} recommendations")
            logger.info(f"✓ Agents involved: {len(solution.get('agents_involved', []))}")
            
            return True
            
        except Exception as e:
            logger.error(f"❌ Problem solving failed: {e}")
            return False
    
    async def test_code_review(self):
        """Test code review using group chat."""
        logger.info("Testing code review...")
        
        try:
            # Sample code to review
            code_content = """
            function processUserData(userData) {
                var result = [];
                for (var i = 0; i < userData.length; i++) {
                    if (userData[i].active) {
                        result.push({
                            id: userData[i].id,
                            name: userData[i].name,
                            email: userData[i].email
                        });
                    }
                }
                return result;
            }
            """
            
            # Review the code
            result = await self.group_chat.review_code(code_content, "code quality and best practices")
            
            assert result is not None
            assert "code_review" in result
            assert "discussion" in result
            
            review = result["code_review"]
            logger.info(f"✓ Code review completed: {result['status']}")
            logger.info(f"✓ Issues found: {len(review.get('issues', []))}")
            logger.info(f"✓ Suggestions made: {len(review.get('suggestions', []))}")
            logger.info(f"✓ Quality score: {review.get('quality_score', 0)}/10")
            
            return True
            
        except Exception as e:
            logger.error(f"❌ Code review failed: {e}")
            return False
    
    async def test_session_management(self):
        """Test session management functionality."""
        logger.info("Testing session management...")
        
        try:
            # Create multiple sessions
            session1 = await self.group_chat.create_group_chat(
                chat_type=ChatType.TASK_DISCUSSION,
                topic="Task Discussion Session",
                participants=list(self.group_chat.agents.keys())[:3],  # First 3 agents
                max_rounds=3
            )
            
            session2 = await self.group_chat.create_group_chat(
                chat_type=ChatType.PLANNING,
                topic="Planning Session",
                participants=list(self.group_chat.agents.keys())[2:],  # Last 3 agents
                max_rounds=3
            )
            
            # Check active sessions
            stats = await self.group_chat.get_coordination_stats()
            assert stats["active_sessions"] == 2
            
            # Get session status
            status1 = await self.group_chat.get_session_status(session1)
            status2 = await self.group_chat.get_session_status(session2)
            
            assert status1["status"] != "not_found"
            assert status2["status"] != "not_found"
            
            # Close one session
            close_result = await self.group_chat.close_session(session1)
            assert close_result["status"] == "closed"
            
            # Check updated stats
            stats_after = await self.group_chat.get_coordination_stats()
            assert stats_after["active_sessions"] == 1
            
            logger.info("✓ Session management working correctly")
            return True
            
        except Exception as e:
            logger.error(f"❌ Session management failed: {e}")
            return False
    
    async def test_fallback_coordination(self):
        """Test fallback coordination when AutoGen GroupChat is not available."""
        logger.info("Testing fallback coordination...")
        
        try:
            # Create a simple coordination session
            session_id = await self.group_chat.create_group_chat(
                chat_type=ChatType.TASK_DISCUSSION,
                topic="Fallback Test",
                participants=list(self.group_chat.agents.keys()),
                max_rounds=3
            )
            
            # Start the chat (this will use fallback if AutoGen GroupChat is not available)
            result = await self.group_chat.start_group_chat(
                session_id,
                "Let's discuss how to handle this task efficiently."
            )
            
            assert result is not None
            assert "status" in result
            
            if result["status"] == "success":
                logger.info("✓ Fallback coordination working correctly")
                logger.info(f"✓ Messages exchanged: {len(result.get('messages', []))}")
            else:
                logger.warning(f"⚠️ Coordination result: {result}")
            
            return True
            
        except Exception as e:
            logger.error(f"❌ Fallback coordination failed: {e}")
            return False
    
    async def test_coordination_stats(self):
        """Test coordination statistics and reporting."""
        logger.info("Testing coordination stats...")
        
        try:
            # Get comprehensive stats
            stats = await self.group_chat.get_coordination_stats()
            
            # Verify stats structure
            required_fields = ["total_sessions", "active_sessions", "total_messages", "registered_agents", "agent_roles"]
            for field in required_fields:
                assert field in stats
            
            logger.info(f"✓ Coordination stats:")
            logger.info(f"  - Total sessions: {stats['total_sessions']}")
            logger.info(f"  - Active sessions: {stats['active_sessions']}")
            logger.info(f"  - Total messages: {stats['total_messages']}")
            logger.info(f"  - Registered agents: {stats['registered_agents']}")
            logger.info(f"  - Agent roles: {stats['agent_roles']}")
            
            return True
            
        except Exception as e:
            logger.error(f"❌ Coordination stats failed: {e}")
            return False
    
    async def run_all_tests(self):
        """Run all group chat tests."""
        logger.info("Starting Group Chat Coordinator tests...")
        
        try:
            # Test 1: Agent registration
            if not await self.test_agent_registration():
                return False
            
            # Test 2: Group chat creation
            session_id = await self.test_group_chat_creation()
            if not session_id:
                return False
            
            # Test 3: Workflow coordination
            if not await self.test_workflow_coordination():
                return False
            
            # Test 4: Problem solving
            if not await self.test_problem_solving():
                return False
            
            # Test 5: Code review
            if not await self.test_code_review():
                return False
            
            # Test 6: Session management
            if not await self.test_session_management():
                return False
            
            # Test 7: Fallback coordination
            if not await self.test_fallback_coordination():
                return False
            
            # Test 8: Coordination stats
            if not await self.test_coordination_stats():
                return False
            
            logger.info("🎉 All Group Chat Coordinator tests passed!")
            return True
            
        except Exception as e:
            logger.error(f"❌ Group chat test failed: {e}")
            import traceback
            logger.error(f"Traceback: {traceback.format_exc()}")
            return False
    
    async def cleanup(self):
        """Clean up test resources."""
        try:
            # Close all active sessions
            for session_id in list(self.group_chat.active_sessions.keys()):
                await self.group_chat.close_session(session_id)
            
            # Clear memory
            await self.memory.clear()
            logger.info("✓ Test resources cleaned up")
        except Exception as e:
            logger.warning(f"Failed to cleanup: {e}")

async def main():
    """Main function to run the group chat tests."""
    test = None
    try:
        test = GroupChatTest()
        success = await test.run_all_tests()
        
        if success:
            print("\n✅ Group Chat Coordinator implementation completed successfully!")
            print("🎉 Phase 3 is now complete! Multi-agent group chat functionality is working.")
            print("You can now proceed to Phase 4: Human-in-the-Loop (Optional).")
        else:
            print("\n❌ Group Chat Coordinator implementation failed. Please check the errors above.")
            
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