"""
Code transformation execution agent.

This agent executes the modernization plan by transforming legacy code
into modern equivalents.
"""

from typing import Dict, Any, List
from src.core.agents.base_agent import BaseAgent
from src.core.state.agent_state import AgentState
from src.core.tools.code_tools import CodeTransformerTool, PatternReplacerTool
from src.core.tools.file_tools import FileWriterTool, BackupTool, FileReaderTool, DirectoryScannerTool
from src.core.tools.search_tools import PatternSearchTool, ReferenceFinderTool
from src.core.tools.handoff_tools import transfer_to_reviewer
from src.utils.logger import get_logger

logger = get_logger(__name__)


class ExecutorAgent(BaseAgent):
    """Agent responsible for executing code transformations."""
    
    def __init__(self, settings):
        """Initialize the executor agent."""
        tools = [
            CodeTransformerTool(),
            PatternReplacerTool(),
            FileWriterTool(),
            BackupTool(),
            FileReaderTool(),
            DirectoryScannerTool(),
            PatternSearchTool(),
            ReferenceFinderTool(),
            transfer_to_reviewer
        ]
        super().__init__(settings, tools)
    
    async def run(self, state: AgentState) -> AgentState:
        """Run the executor agent with AI-powered code transformation."""
        self.log_activity("Starting AI-powered code transformation execution")
        
        try:
            # Create backup of original code
            backup_location = state.get("backup_location", "/tmp/legacy2modern_backup")
            backup_path = await self.use_tool(
                "create_backup",
                source_path=state["codebase_path"],
                backup_location=backup_location
            )
            
            # Get transformation plan from handoff or state
            handoff_data = state.get("handoff_to_executor", {})
            transformation_plan = handoff_data.get("transformation_plan") or state.get("transformation_plan", {})
            
            if not transformation_plan:
                self.logger.warning("No transformation plan found, creating basic plan")
                transformation_plan = self._create_basic_transformation_plan(state)
            
            # Execute AI-powered transformations
            transformation_results = await self._execute_ai_transformations(
                transformation_plan, 
                state["codebase_path"], 
                state["target_language"],
                state.get("analysis_results", {})
            )
            
            # Apply pattern replacements
            pattern_results = await self.use_tool(
                "apply_pattern_replacements",
                patterns=state.get("modernization_plan", {}).get("patterns", []),
                source_path=state["codebase_path"]
            )
            
            # Update state with execution results
            state["transformation_results"] = transformation_results
            state["pattern_results"] = pattern_results
            state["backup_path"] = backup_path
            
            self.log_activity("AI-powered code transformation execution completed", {
                "phases_executed": len(transformation_results),
                "patterns_applied": len(pattern_results.get("replacements_made", [])) if isinstance(pattern_results.get("replacements_made"), list) else 0,
                "backup_created": backup_path,
                "transformation_files_created": len([r for r in transformation_results if r.get("files_transformed")])
            })
            
        except Exception as e:
            self.logger.error(f"Error in executor agent: {e}")
            state["error"] = str(e)
        
        return state
    
    def _create_basic_transformation_plan(self, state: AgentState) -> Dict[str, Any]:
        """Create a basic transformation plan if none exists."""
        return {
            "transformation_rules": [],
            "data_structure_mappings": [],
            "file_io_transformations": [],
            "execution_order": ["data_structures", "file_operations", "procedures"],
            "transformation_phases": [
                {
                    "phase_name": "basic_transformation",
                    "rules": ["transformation_rules"],
                    "output_files": ["transformed_code.py"]
                }
            ]
        }
    
    async def _execute_ai_transformations(self, transformation_plan: Dict[str, Any], 
                                        source_path: str, target_language: str,
                                        analysis_results: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Execute AI-powered code transformations."""
        transformation_results = []
        
        # Read the source code
        source_code = await self._read_source_code(source_path)
        
        # Execute transformation phases in order
        execution_order = transformation_plan.get("execution_order", ["data_structures", "file_operations", "procedures"])
        
        for phase_type in execution_order:
            if phase_type == "data_structures":
                result = await self._transform_data_structures(
                    transformation_plan.get("data_structure_mappings", []),
                    source_code, target_language, analysis_results
                )
                transformation_results.append(result)
                
            elif phase_type == "file_operations":
                result = await self._transform_file_operations(
                    transformation_plan.get("file_io_transformations", []),
                    source_code, target_language, analysis_results
                )
                transformation_results.append(result)
                
            elif phase_type == "procedures":
                result = await self._transform_procedures(
                    transformation_plan.get("transformation_rules", []),
                    source_code, target_language, analysis_results
                )
                transformation_results.append(result)
        
        # Generate main application file
        main_result = await self._generate_main_application(
            transformation_results, source_code, target_language, analysis_results
        )
        transformation_results.append(main_result)
        
        return transformation_results
    
    async def _read_source_code(self, source_path: str) -> str:
        """Read the source code file."""
        try:
            with open(source_path, 'r', encoding='utf-8', errors='ignore') as f:
                return f.read()
        except Exception as e:
            self.logger.error(f"Error reading source code: {e}")
            return ""
    
    async def _transform_data_structures(self, data_mappings: List[Dict[str, Any]], 
                                       source_code: str, target_language: str,
                                       analysis_results: Dict[str, Any]) -> Dict[str, Any]:
        """Transform data structures using AI."""
        if not data_mappings:
            return {"phase": "data_structures", "files_transformed": [], "success": True}
        
        # Create AI prompt for data structure transformation
        prompt = f"""
        Transform the following COBOL data structures to {target_language}:

        SOURCE CODE:
        {source_code}

        DATA STRUCTURE MAPPINGS:
        {data_mappings}

        ANALYSIS RESULTS:
        {analysis_results.get("structure", {}).get("data_structures", [])}

        Please generate {target_language} classes/data structures that preserve the original COBOL data hierarchy.
        Include proper type annotations, docstrings, and maintain the original field names and relationships.

        Return only the {target_language} code for the data structures, formatted as a complete module.
        """
        
        # Use LLM to generate transformed data structures
        from langchain_core.messages import HumanMessage
        messages = [HumanMessage(content=prompt)]
        llm_response = await self.process_messages(messages)
        
        if llm_response:
            transformed_code = llm_response[0].content
            # Write to file
            output_file = "models.py"
            await self._write_transformed_file(output_file, transformed_code)
            
            return {
                "phase": "data_structures",
                "files_transformed": [output_file],
                "transformations_applied": len(data_mappings),
                "success": True,
                "output_path": f"transformed/{output_file}",
                "llm_generated_code": transformed_code
            }
        
        return {"phase": "data_structures", "files_transformed": [], "success": False}
    
    async def _transform_file_operations(self, file_transformations: List[Dict[str, Any]], 
                                       source_code: str, target_language: str,
                                       analysis_results: Dict[str, Any]) -> Dict[str, Any]:
        """Transform file operations using AI."""
        if not file_transformations:
            return {"phase": "file_operations", "files_transformed": [], "success": True}
        
        # Create AI prompt for file operations transformation
        prompt = f"""
        Transform the following COBOL file operations to {target_language}:

        SOURCE CODE:
        {source_code}

        FILE TRANSFORMATIONS:
        {file_transformations}

        ANALYSIS RESULTS:
        {analysis_results.get("structure", {}).get("file_dependencies", [])}

        Please generate {target_language} file handling code that replaces COBOL file operations.
        Use modern file handling patterns and include proper error handling.

        Return only the {target_language} code for file operations, formatted as a complete module.
        """
        
        # Use LLM to generate transformed file operations
        from langchain_core.messages import HumanMessage
        messages = [HumanMessage(content=prompt)]
        llm_response = await self.process_messages(messages)
        
        if llm_response:
            transformed_code = llm_response[0].content
            # Write to file
            output_file = "file_operations.py"
            await self._write_transformed_file(output_file, transformed_code)
            
            return {
                "phase": "file_operations",
                "files_transformed": [output_file],
                "transformations_applied": len(file_transformations),
                "success": True,
                "output_path": f"transformed/{output_file}",
                "llm_generated_code": transformed_code
            }
        
        return {"phase": "file_operations", "files_transformed": [], "success": False}
    
    async def _transform_procedures(self, transformation_rules: List[Dict[str, Any]], 
                                  source_code: str, target_language: str,
                                  analysis_results: Dict[str, Any]) -> Dict[str, Any]:
        """Transform procedures using AI."""
        if not transformation_rules:
            return {"phase": "procedures", "files_transformed": [], "success": True}
        
        # Create AI prompt for procedure transformation
        prompt = f"""
        Transform the following COBOL procedures to {target_language}:

        SOURCE CODE:
        {source_code}

        TRANSFORMATION RULES:
        {transformation_rules}

        ANALYSIS RESULTS:
        {analysis_results.get("structure", {}).get("procedures", [])}

        Please generate {target_language} functions that implement the COBOL procedures.
        Maintain the original business logic while using modern {target_language} patterns.
        Include proper error handling, logging, and type annotations.

        Return only the {target_language} code for the procedures, formatted as a complete module.
        """
        
        # Use LLM to generate transformed procedures
        from langchain_core.messages import HumanMessage
        messages = [HumanMessage(content=prompt)]
        llm_response = await self.process_messages(messages)
        
        if llm_response:
            transformed_code = llm_response[0].content
            # Write to file
            output_file = "procedures.py"
            await self._write_transformed_file(output_file, transformed_code)
            
            return {
                "phase": "procedures",
                "files_transformed": [output_file],
                "transformations_applied": len(transformation_rules),
                "success": True,
                "output_path": f"transformed/{output_file}",
                "llm_generated_code": transformed_code
            }
        
        return {"phase": "procedures", "files_transformed": [], "success": False}
    
    async def _generate_main_application(self, transformation_results: List[Dict[str, Any]], 
                                       source_code: str, target_language: str,
                                       analysis_results: Dict[str, Any]) -> Dict[str, Any]:
        """Generate main application file using AI."""
        
        # Create AI prompt for main application
        prompt = f"""
        Generate a main application file in {target_language} that orchestrates the transformed COBOL program:

        ORIGINAL COBOL CODE:
        {source_code}

        TRANSFORMATION RESULTS:
        {transformation_results}

        ANALYSIS RESULTS:
        {analysis_results}

        Create a main application that:
        1. Imports all the transformed modules
        2. Implements the main program logic
        3. Handles the execution flow from the original COBOL program
        4. Includes proper error handling and logging
        5. Follows modern {target_language} best practices

        Return only the {target_language} code for the main application.
        """
        
        # Use LLM to generate main application
        from langchain_core.messages import HumanMessage
        messages = [HumanMessage(content=prompt)]
        llm_response = await self.process_messages(messages)
        
        if llm_response:
            main_code = llm_response[0].content
            # Write to file
            output_file = "main.py"
            await self._write_transformed_file(output_file, main_code)
            
            return {
                "phase": "main_application",
                "files_transformed": [output_file],
                "transformations_applied": 1,
                "success": True,
                "output_path": f"transformed/{output_file}",
                "llm_generated_code": main_code
            }
        
        return {"phase": "main_application", "files_transformed": [], "success": False}
    
    async def _write_transformed_file(self, filename: str, content: str) -> None:
        """Write transformed code to file."""
        import os
        
        # Create output directory
        output_dir = "transformed"
        os.makedirs(output_dir, exist_ok=True)
        
        # Write file
        file_path = os.path.join(output_dir, filename)
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        self.logger.info(f"Generated transformed file: {file_path}")
    
    def get_system_prompt(self) -> str:
        """Get the system prompt for the executor agent."""
        return """
        You are an AI-powered code transformation execution agent.
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
        """
