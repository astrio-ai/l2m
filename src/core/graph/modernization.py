"""
Swarm-based modernization workflow using langgraph-swarm.

This module implements a multi-agent swarm system where agents dynamically
hand off control to one another based on their specializations.
"""

from typing import Dict, Any, List
from langchain_core.messages import HumanMessage, SystemMessage
from langchain_anthropic import ChatAnthropic
from langchain_openai import ChatOpenAI
from langgraph.prebuilt import create_react_agent
from langgraph_swarm import create_swarm, create_handoff_tool
from langgraph.checkpoint.memory import InMemorySaver

from src.core.llm.provider_factory import LLMProviderFactory
from src.core.tools.code_tools import CodeAnalyzerTool, DependencyAnalyzerTool, ModernizationPlannerTool, RiskAssessmentTool
from src.core.tools.file_tools import FileReaderTool, DirectoryScannerTool, FileWriterTool, BackupTool
from src.core.tools.search_tools import PatternSearchTool, ReferenceFinderTool, CodeDiscoveryTool
from src.config.settings import Settings
from src.utils.logger import get_logger

logger = get_logger(__name__)


class ModernizationWorkflow:
    """Swarm-based modernization workflow with dynamic agent handoffs."""
    
    def __init__(self, settings: Settings):
        """Initialize the swarm modernization workflow."""
        self.settings = settings
        self.llm_provider = LLMProviderFactory.create_provider(settings)
        self.model = self._create_langchain_model()
        self.app = None
        self._build_swarm()
    
    def _create_langchain_model(self):
        """Create a LangChain-compatible model instance."""
        provider_name = self.settings.llm_provider.lower()
        
        if provider_name == "anthropic":
            api_key = self.settings.anthropic_api_key or self.settings.llm_api_key
            return ChatAnthropic(
                model=self.llm_provider.get_model(),
                api_key=api_key,
                temperature=self.settings.llm_temperature,
                max_tokens=self.settings.llm_max_tokens
            )
        elif provider_name == "openai":
            api_key = self.settings.openai_api_key or self.settings.llm_api_key
            return ChatOpenAI(
                model=self.llm_provider.get_model(),
                api_key=api_key,
                temperature=self.settings.llm_temperature,
                max_tokens=self.settings.llm_max_tokens
            )
        else:
            raise ValueError(f"Unsupported provider for LangChain: {provider_name}")
    
    def _build_swarm(self):
        """Build the swarm multi-agent system."""
        try:
            # Create handoff tools for dynamic agent transitions
            transfer_to_planner = create_handoff_tool(
                agent_name="planner",
                description="Transfer to the modernization planner for creating transformation plans."
            )
            
            transfer_to_executor = create_handoff_tool(
                agent_name="executor", 
                description="Transfer to the code executor for implementing transformations."
            )
            
            transfer_to_reviewer = create_handoff_tool(
                agent_name="reviewer",
                description="Transfer to the code reviewer for quality assessment."
            )
            
            transfer_to_validator = create_handoff_tool(
                agent_name="validator",
                description="Transfer to the validator for final validation."
            )
            
            transfer_to_analyzer = create_handoff_tool(
                agent_name="analyzer",
                description="Transfer back to the analyzer for additional analysis."
            )
            
            # Create analyzer agent
            analyzer = create_react_agent(
                model=self.model,
                tools=[
                    transfer_to_planner
                ],
                prompt="""You are a specialized code analysis agent for legacy codebases.
                Your role is to:
                1. Analyze code structure and organization
                2. Identify dependencies and relationships
                3. Detect legacy patterns and anti-patterns
                4. Understand the codebase's functionality
                5. Provide insights for modernization planning
                
                Focus on understanding the codebase thoroughly before making any
                modernization recommendations. When analysis is complete, transfer
                to the planner agent.""",
                name="analyzer"
            )
            
            # Create planner agent
            planner = create_react_agent(
                model=self.model,
                tools=[
                    transfer_to_executor,
                    transfer_to_analyzer
                ],
                prompt="""You are a tactical code transformation planner for legacy codebases.
                Your role is to:
                1. Analyze the structured code data (procedures, data structures, dependencies)
                2. Generate specific transformation rules mapping legacy code to target language
                3. Create executable transformation tasks for the Executor Agent
                4. Provide concrete code snippets and patterns for transformation
                
                Focus on producing actionable transformation rules, not high-level strategy.
                Output must be structured data that the Executor Agent can immediately execute.
                When planning is complete, transfer to the executor agent.""",
                name="planner"
            )
            
            # Create executor agent
            executor = create_react_agent(
                model=self.model,
                tools=[
                    transfer_to_reviewer,
                    transfer_to_planner
                ],
                prompt="""You are an AI-powered code transformation execution agent.
                Your role is to:
                1. Execute modernization plans using AI-powered code generation
                2. Transform legacy code patterns to modern equivalents using LLM capabilities
                3. Apply intelligent language-specific transformations
                4. Generate complete, functional modern code from legacy code
                5. Create modular, well-structured output files
                6. Maintain code functionality while modernizing structure
                
                You use AI to understand the original code's intent and generate equivalent
                modern code that preserves functionality while following best practices.
                Focus on creating production-ready, maintainable code.
                When transformation is complete, transfer to the reviewer agent.""",
                name="executor"
            )
            
            # Create reviewer agent
            reviewer = create_react_agent(
                model=self.model,
                tools=[
                    transfer_to_validator,
                    transfer_to_executor
                ],
                prompt="""You are a specialized code review and quality analysis agent.
                Your role is to:
                1. Review transformed code for quality and correctness
                2. Analyze code structure and organization
                3. Detect potential issues and improvements
                4. Ensure adherence to modern coding standards
                5. Provide recommendations for code improvements
                
                Focus on ensuring the transformed code meets quality standards
                and maintains the original functionality. When review is complete,
                transfer to the validator agent.""",
                name="reviewer"
            )
            
            # Create validator agent
            validator = create_react_agent(
                model=self.model,
                tools=[
                    transfer_to_reviewer
                ],
                prompt="""You are a specialized validation and compliance agent.
                Your role is to:
                1. Validate that transformed code meets requirements
                2. Check compliance with coding standards
                3. Verify functionality preservation
                4. Run integration tests and validation
                5. Provide final assessment of modernization success
                
                Focus on ensuring the modernization meets all requirements
                and the transformed code is ready for production use.
                If issues are found, transfer back to the reviewer agent.""",
                name="validator"
            )
            
            # Create the swarm
            self.app = create_swarm(
                agents=[analyzer, planner, executor, reviewer, validator],
                default_active_agent="analyzer"
            ).compile(checkpointer=InMemorySaver())
            
            logger.info("Swarm modernization workflow created successfully")
            
        except Exception as e:
            logger.error(f"Error creating swarm workflow: {e}")
            raise
    
    async def run(self, codebase_path: str, target_language: str = "python", 
                  modernization_goals: List[str] = None) -> Dict[str, Any]:
        """Run the swarm modernization workflow."""
        if modernization_goals is None:
            modernization_goals = []
        
        try:
            # Prepare initial message
            initial_message = HumanMessage(
                content=f"""Please modernize the legacy codebase at {codebase_path} to {target_language}.
                
                Modernization Goals:
                {', '.join(modernization_goals) if modernization_goals else 'General modernization and code improvement'}
                
                Please analyze the codebase, create a transformation plan, execute the modernization,
                review the results, and validate the final output.
                
                The workflow should:
                1. Analyze the legacy code structure and dependencies
                2. Create a detailed transformation plan
                3. Execute the code transformation using AI
                4. Review the transformed code for quality
                5. Validate the final result
                
                Start with analysis of the codebase."""
            )
            
            # Run the swarm workflow
            config = {"configurable": {"thread_id": f"modernization_{codebase_path.replace('/', '_')}"}}
            
            result = await self.app.ainvoke(
                {"messages": [initial_message]},
                config=config
            )
            
            logger.info("Swarm modernization workflow completed successfully")
            return result
            
        except Exception as e:
            logger.error(f"Error running swarm workflow: {e}")
            raise
    
    def stream(self, codebase_path: str, target_language: str = "python", 
               modernization_goals: List[str] = None):
        """Stream the swarm modernization workflow."""
        if modernization_goals is None:
            modernization_goals = []
        
        try:
            # Prepare initial message
            initial_message = HumanMessage(
                content=f"""Please modernize the legacy codebase at {codebase_path} to {target_language}.
                
                Modernization Goals:
                {', '.join(modernization_goals) if modernization_goals else 'General modernization and code improvement'}
                
                Please analyze the codebase, create a transformation plan, execute the modernization,
                review the results, and validate the final output.
                
                The workflow should:
                1. Analyze the legacy code structure and dependencies
                2. Create a detailed transformation plan
                3. Execute the code transformation using AI
                4. Review the transformed code for quality
                5. Validate the final result
                
                Start with analysis of the codebase."""
            )
            
            # Stream the swarm workflow
            config = {"configurable": {"thread_id": f"modernization_{codebase_path.replace('/', '_')}"}}
            
            for chunk in self.app.stream(
                {"messages": [initial_message]},
                config=config
            ):
                yield chunk
                
        except Exception as e:
            logger.error(f"Error streaming swarm workflow: {e}")
            raise
