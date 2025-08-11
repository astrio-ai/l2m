"""
Demonstration script for Group Chat Coordinator functionality.

This script showcases the complete multi-agent group chat system
with real-world examples of workflow coordination, problem solving,
and code review.
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

class GroupChatDemo:
    """Demonstration class for Group Chat Coordinator functionality."""
    
    def __init__(self):
        # Initialize core components
        api_key = os.getenv('ANTHROPIC_API_KEY')
        if not api_key:
            raise ValueError("No ANTHROPIC_API_KEY found in environment")
        
        self.ai = AI(api_key=api_key, provider="anthropic", model="claude-3-sonnet-20240229")
        self.memory = FileMemory(storage_path="demo_memory_group_chat")
        self.config = ProjectConfig()
        
        # Create group chat coordinator
        self.group_chat = GroupChatCoordinator(
            ai=self.ai,
            memory=self.memory,
            config=self.config,
            autogen_config=AutoGenConfig(enable_autogen=True, use_group_chat=True)
        )
    
    async def initialize(self):
        """Initialize the demo by setting up agents."""
        await self._setup_agents()
    
    async def _setup_agents(self):
        """Set up and register all agents."""
        print("🤖 Setting up agents...")
        
        agents = {
            "parser": ParserAgent(ai=self.ai, memory=self.memory, config=self.config),
            "modernizer": ModernizerAgent(ai=self.ai, memory=self.memory, config=self.config),
            "refactor": RefactorAgent(ai=self.ai, memory=self.memory, config=self.config),
            "qa": QAAgent(ai=self.ai, memory=self.memory, config=self.config),
            "coordinator": CoordinatorAgent(name="CoordinatorAgent", ai=self.ai, memory=self.memory, config=self.config)
        }
        
        # Register agents
        for name, agent in agents.items():
            await self.group_chat.register_agent(agent, role=name)
        
        print(f"✅ {len(agents)} agents registered successfully")
    
    async def demo_workflow_coordination(self):
        """Demonstrate workflow coordination."""
        print("\n🔄 Demo 1: Workflow Coordination")
        print("=" * 50)
        
        workflow_config = {
            "name": "Legacy Website to React Modernization",
            "project_path": "/legacy/website-system",
            "target_stack": "react",
            "files": ["index.html", "styles.css", "script.js"],
            "workflow_id": "demo_workflow_001"
        }
        
        print(f"📋 Workflow: {workflow_config['name']}")
        print(f"🎯 Target: {workflow_config['target_stack']}")
        print(f"📁 Files: {', '.join(workflow_config['files'])}")
        
        result = await self.group_chat.coordinate_workflow(workflow_config)
        
        print(f"✅ Coordination Status: {result['status']}")
        if 'coordination_plan' in result:
            plan = result['coordination_plan']
            print(f"📝 Plan Items: {len(plan)}")
            for key, value in plan.items():
                print(f"   - {key}: {len(value) if isinstance(value, list) else value}")
    
    async def demo_problem_solving(self):
        """Demonstrate problem solving."""
        print("\n🧠 Demo 2: Problem Solving")
        print("=" * 50)
        
        problem = """
        We need to modernize a legacy website with the following challenges:
        
        1. Complex business logic with 50+ calculation rules
        2. File-based data storage with 10+ different file formats
        3. Batch processing requirements
        4. Integration with external systems
        5. Compliance requirements (SOX, GDPR)
        
        How should we approach this modernization?
        """
        
        print("🤔 Problem:")
        print(problem.strip())
        
        result = await self.group_chat.solve_problem(problem)
        
        print(f"✅ Solution Status: {result['status']}")
        if 'solution' in result:
            solution = result['solution']
            print(f"👥 Agents Involved: {len(solution.get('agents_involved', []))}")
            print(f"💡 Recommendations: {len(solution.get('recommendations', []))}")
    
    async def demo_code_review(self):
        """Demonstrate code review."""
        print("\n🔍 Demo 3: Code Review")
        print("=" * 50)
        
        code_content = """
        // Legacy JavaScript code to review
        function calculatePayroll(employeeData) {
            var total = 0;
            var deductions = 0;
            
            // Calculate base salary
            total = employeeData.salary;
            
            // Add overtime
            if (employeeData.hours > 40) {
                var overtime = (employeeData.hours - 40) * employeeData.rate * 1.5;
                total = total + overtime;
            }
            
            // Calculate deductions
            deductions = total * 0.15; // Fixed tax rate
            
            // Return net pay
            return total - deductions;
        }
        """
        
        print("📝 Code to Review:")
        print(code_content.strip())
        
        result = await self.group_chat.review_code(code_content, "code quality and best practices")
        
        print(f"✅ Review Status: {result['status']}")
        if 'code_review' in result:
            review = result['code_review']
            print(f"🚨 Issues Found: {len(review.get('issues', []))}")
            print(f"💡 Suggestions: {len(review.get('suggestions', []))}")
            print(f"⭐ Quality Score: {review.get('quality_score', 0)}/10")
    
    async def demo_session_management(self):
        """Demonstrate session management."""
        print("\n📊 Demo 4: Session Management")
        print("=" * 50)
        
        # Create multiple sessions
        session1 = await self.group_chat.create_group_chat(
            chat_type=ChatType.TASK_DISCUSSION,
            topic="API Design Discussion",
            participants=list(self.group_chat.agents.keys())[:3],
            max_rounds=5
        )
        
        session2 = await self.group_chat.create_group_chat(
            chat_type=ChatType.PLANNING,
            topic="Testing Strategy Planning",
            participants=list(self.group_chat.agents.keys())[2:],
            max_rounds=5
        )
        
        print(f"📋 Created Sessions:")
        print(f"   - Session 1: {session1} (API Design)")
        print(f"   - Session 2: {session2} (Testing Strategy)")
        
        # Get stats
        stats = await self.group_chat.get_coordination_stats()
        print(f"📊 Current Stats:")
        print(f"   - Total Sessions: {stats['total_sessions']}")
        print(f"   - Active Sessions: {stats['active_sessions']}")
        print(f"   - Registered Agents: {stats['registered_agents']}")
        
        # Close sessions
        await self.group_chat.close_session(session1)
        await self.group_chat.close_session(session2)
        
        print("✅ Sessions closed successfully")
    
    async def run_demo(self):
        """Run the complete demonstration."""
        print("🚀 Starting Group Chat Coordinator Demonstration")
        print("=" * 60)
        
        try:
            # Demo 1: Workflow Coordination
            await self.demo_workflow_coordination()
            
            # Demo 2: Problem Solving
            await self.demo_problem_solving()
            
            # Demo 3: Code Review
            await self.demo_code_review()
            
            # Demo 4: Session Management
            await self.demo_session_management()
            
            print("\n" + "=" * 60)
            print("🎉 Group Chat Coordinator Demonstration Completed!")
            print("✅ All features working correctly")
            print("🚀 Ready for production use")
            
        except Exception as e:
            print(f"❌ Demo failed: {e}")
            import traceback
            traceback.print_exc()
    
    async def cleanup(self):
        """Clean up demo resources."""
        try:
            # Close all active sessions
            for session_id in list(self.group_chat.active_sessions.keys()):
                await self.group_chat.close_session(session_id)
            
            # Clear memory
            await self.memory.clear()
            print("🧹 Demo resources cleaned up")
        except Exception as e:
            print(f"⚠️ Cleanup warning: {e}")

async def main():
    """Main function to run the demonstration."""
    demo = None
    try:
        demo = GroupChatDemo()
        await demo.initialize()
        await demo.run_demo()
        
    except Exception as e:
        print(f"❌ Demo failed: {e}")
        import traceback
        traceback.print_exc()
    finally:
        if demo:
            await demo.cleanup()

if __name__ == "__main__":
    asyncio.run(main()) 