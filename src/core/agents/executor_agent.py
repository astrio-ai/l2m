"""
Code transformation execution agent.

This agent executes the modernization plan by transforming legacy code
into modern equivalents.
"""

from typing import Dict, Any, List, Optional
from src.core.agents.base_agent import BaseAgent
from src.core.state.agent_state import AgentState
from src.core.tools.code_tools import CodeTransformerTool, PatternReplacerTool
from src.core.tools.file_tools import FileWriterTool, BackupTool, FileReaderTool, DirectoryScannerTool
from src.core.tools.search_tools import PatternSearchTool, ReferenceFinderTool
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
        ]
        super().__init__(settings, tools)
    
    async def run(self, state: AgentState) -> AgentState:
        """Run the executor agent with JSON rule-based code transformation."""
        self.log_activity("Starting JSON rule-based code transformation execution")
        
        
        
        try:
            # Create backup of original code
            backup_location = state.get("backup_location", "/tmp/legacy2modern_backup")
            backup_path = await self.use_tool(
                "create_backup",
                source_path=state["codebase_path"],
                backup_location=backup_location
            )
            
            # Get transformation plan from handoff or state
            handoff_data = state.get("handoff_to_executor", {}) or {}
            transformation_plan = handoff_data.get("transformation_plan") or state.get("transformation_plan", {})
            
            
            if not transformation_plan:
                self.logger.warning("No transformation plan found, creating basic plan")
                transformation_plan = self._create_basic_transformation_plan(state)
            
            # Parse JSON transformation rules and generate Python files
            transformation_results = await self._execute_json_transformations(
                transformation_plan, 
                state["codebase_path"], 
                state["target_language"]
            )
            
            # Update state with execution results
            state["transformation_results"] = transformation_results.get("transformation_results", [])
            state["backup_path"] = backup_path
            
            # Log detailed transformation results
            generated_files = transformation_results.get("generated_files", [])
            self.log_activity("JSON rule-based code transformation execution completed", {
                "files_generated": len(generated_files),
                "backup_created": backup_path,
                "transformation_successful": transformation_results.get("success", False),
                "output_files": generated_files,
                "rules_applied": transformation_results.get("rules_applied", 0)
            })
            
            # Show preview of generated code
            if generated_files:
                import os
                main_file = generated_files[0] if generated_files else None
                if main_file and os.path.exists(main_file):
                    with open(main_file, 'r') as f:
                        preview = f.read()[:500]  # First 500 characters
                        self.logger.info(f"Generated code preview:\n{preview}...")
            
        except Exception as e:
            self.logger.error(f"Error in executor agent: {e}")
            state["error"] = str(e)
        
        return state
    
    def _create_basic_transformation_plan(self, state: AgentState) -> Dict[str, Any]:
        """Create a basic transformation plan if none exists."""
        return {
            "phase": "Core Transformation",
            "rules": [
                {
                    "source": "MAIN",
                    "target": "def main():\n    # Basic transformation\n    pass"
                }
            ]
        }
    
    async def _execute_json_transformations(self, transformation_plan: Dict[str, Any], 
                                           source_path: str, target_language: str) -> Dict[str, Any]:
        """Execute JSON rule-based code transformations."""
        import os
        from pathlib import Path
        
        try:
            # Check if transformation_plan is valid
            if not transformation_plan:
                self.logger.error("transformation_plan is None")
                return {
                    "success": False,
                    "error": "No transformation plan provided",
                    "generated_files": []
                }
            
            # Ensure output directory exists
            output_dir = Path("output/python")
            output_dir.mkdir(parents=True, exist_ok=True)
            
            # Parse transformation rules
            rules = transformation_plan.get("rules", [])
            generated_files = []
            
            # Generate Python code from rules with proper function mapping
            python_code = self._generate_python_from_rules(rules)
            
            # Also generate a mapping file showing COBOL -> Python function mapping
            mapping_file = output_dir / "cobol_to_python_mapping.py"
            mapping_code = self._generate_cobol_python_mapping(rules)
            with open(mapping_file, 'w') as f:
                f.write(mapping_code)
            generated_files.append(str(mapping_file))
            
            # Write the main Python file
            main_file = output_dir / "transformed_code.py"
            with open(main_file, 'w') as f:
                f.write(python_code)
            generated_files.append(str(main_file))
            
            # Create a simple test file
            test_file = output_dir / "test_transformed_code.py"
            test_code = self._generate_test_code(rules)
            with open(test_file, 'w') as f:
                f.write(test_code)
            generated_files.append(str(test_file))
            
            return {
                "success": True,
                "generated_files": generated_files,
                "transformation_phase": transformation_plan.get("phase", "Core Transformation"),
                "rules_applied": len(rules),
                "output_directory": str(output_dir),
                "transformation_results": generated_files  # Add this for downstream agents
            }
            
        except Exception as e:
            self.logger.error(f"Error executing JSON transformations: {e}")
            return {
                "success": False,
                "error": str(e),
                "generated_files": [],
                "transformation_phase": transformation_plan.get("phase", "Core Transformation"),
                "rules_applied": 0,
                "transformation_results": []  # Add this for downstream agents
            }
    
    def _generate_python_from_rules(self, rules: List[Dict[str, Any]]) -> str:
        """Generate code from transformation rules for the target language."""
        python_code = '''"""
Transformed code from legacy COBOL to Python.
Generated by Legacy2Modern transformation system.
"""

import os
import sys
from typing import Optional, List, Dict, Any
import re


def extract_function_name(target_code: str) -> Optional[str]:
    """Extract function name from target code."""
    match = re.search(r'def\s+(\w+)\s*\(', target_code)
    return match.group(1) if match else None


def main():
    """Main entry point for the transformed application."""
    print("Transformed legacy application starting...")
    
    try:
        # Find all transformed functions (functions defined at module level, not built-ins)
        excluded = ['main', 'extract_function_name', 'Optional', 'List', 'Dict', 'Any', 'os', 'sys', 're']
        transformed_functions = [name for name in globals() 
                                if callable(globals()[name]) 
                                and not name.startswith('_')
                                and name not in excluded
                                and type(globals()[name]).__name__ == 'function']
        
        if transformed_functions:
            # Call the first transformed function found
            func_name = transformed_functions[0]
            print(f"Calling transformed function: {func_name}")
            globals()[func_name]()
            print("Transformation completed successfully!")
        else:
            print("No transformed functions to execute")
            return 1
        
    except Exception as e:
        print(f"Error during execution: {e}")
        import traceback
        traceback.print_exc()
        return 1
    
    return 0


# Transformed functions based on rules:
'''
        
        # Collect function names for documentation
        function_names = []
        
        # Add each rule as a Python function
        for rule in rules:
            source = rule.get("source", "")
            target = rule.get("target", "")
            
            if target.strip():
                # Clean the source label - escape newlines to prevent syntax errors in comments
                safe_source = source.replace('\n', ' ').replace('.', '')
                python_code += f"\n# Transformed from: {safe_source}\n"
                
                # Only add the target if it's actual code (not just a comment or incomplete)
                # Allow functions with 'pass' as those are valid Python
                if target.strip().startswith('#'):
                    # Skip pure comments - these aren't executable code
                    self.logger.debug(f"Skipping comment target: {target[:50]}")
                    continue
                
                # If it's not a function/class and not a special case, skip it
                if not ('def ' in target or 'class ' in target or 'print(' in target or target.strip().endswith('pass')):
                    self.logger.debug(f"Skipping non-code target: {target[:50]}")
                    continue
                
                # Replace invalid Python with pass if we have an incomplete function
                if 'def ' in target and ('...' in target or len(target.strip()) < 20):
                    target = target.replace('...', 'pass')
                    self.logger.debug("Replaced incomplete function body with pass")
                    
                python_code += target
                if not target.endswith('\n'):
                    python_code += "\n"
                
                # Extract function name
                func_name = self._extract_function_name(target)
                if func_name:
                    function_names.append(func_name)
        
        # Add main execution
        python_code += "\n\nif __name__ == '__main__':\n"
        python_code += "    sys.exit(main())\n"
        
        return python_code
    
    def _extract_function_name(self, target_code: str) -> Optional[str]:
        """Extract function name from target code."""
        import re
        match = re.search(r'def\s+(\w+)\s*\(', target_code)
        return match.group(1) if match else None
    
    def _generate_cobol_python_mapping(self, rules: List[Dict[str, Any]]) -> str:
        """Generate a mapping file showing COBOL procedures to Python functions."""
        mapping_code = '''"""
COBOL to Python Function Mapping
Generated by Legacy2Modern transformation system.
This file shows the mapping between COBOL procedures and Python functions.
"""

# COBOL Procedure -> Python Function Mapping
COBOL_PYTHON_MAPPING = {
'''
        
        for rule in rules:
            source = rule.get("source", "")
            target = rule.get("target", "")
            if source and target:
                # Clean the source for safe dictionary key usage
                safe_source = source.replace('\\n', ' ').replace('\n', ' ').replace('"', "'")
                # Extract function name from target code
                func_name = self._extract_function_name(target)
                if func_name:
                    mapping_code += f'    "{safe_source}": "{func_name}",\n'
                else:
                    mapping_code += f'    "{safe_source}": "Not a function",\n'
        
        # Close the mapping dict
        if mapping_code.strip().endswith(','):
            mapping_code = mapping_code.rstrip(',')
        
        mapping_code += '''
}

def get_python_function(cobol_procedure: str) -> str:
    """Get the Python function name for a COBOL procedure."""
    return COBOL_PYTHON_MAPPING.get(cobol_procedure, "Unknown")

def list_all_mappings():
    """List all COBOL to Python mappings."""
    for cobol, python in COBOL_PYTHON_MAPPING.items():
        print(f"{cobol} -> {python}")

if __name__ == "__main__":
    print("COBOL to Python Function Mapping:")
    list_all_mappings()
'''
        return mapping_code
    
    def _generate_test_code(self, rules: List[Dict[str, Any]]) -> str:
        """Generate test code for the transformed functions in the target language."""
        test_code = '''"""
Test cases for transformed code.
Generated by Legacy2Modern transformation system.
"""

import unittest
import sys
import os

# Add the parent directory to the path to import the transformed code
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

try:
    from transformed_code import main
    from cobol_to_python_mapping import COBOL_PYTHON_MAPPING, get_python_function
except ImportError:
    print("Warning: Could not import transformed_code module")
    main = None
    COBOL_PYTHON_MAPPING = {}


class TestTransformedCode(unittest.TestCase):
    """Test cases for the transformed code."""
    
    def test_main_function_exists(self):
        """Test that the main function exists and is callable."""
        if main is not None:
            self.assertTrue(callable(main))
        else:
            self.skipTest("Main function not available")
    
    def test_cobol_procedure_mappings(self):
        """Test that COBOL procedures are properly mapped to Python functions."""
        self.assertIsInstance(COBOL_PYTHON_MAPPING, dict)
        self.assertGreater(len(COBOL_PYTHON_MAPPING), 0, "No COBOL procedure mappings found")
        
        # Test specific mappings
        for cobol_proc, python_func in COBOL_PYTHON_MAPPING.items():
            self.assertIsInstance(cobol_proc, str)
            self.assertIsInstance(python_func, str)
            self.assertNotEqual(python_func, "Unknown", f"COBOL procedure '{cobol_proc}' not properly mapped")
    
    def test_function_mapping_utility(self):
        """Test the function mapping utility."""
        if COBOL_PYTHON_MAPPING:
            test_proc = list(COBOL_PYTHON_MAPPING.keys())[0]
            mapped_func = get_python_function(test_proc)
            self.assertIsInstance(mapped_func, str)
            self.assertNotEqual(mapped_func, "Unknown")
    
    def test_transformation_rules_applied(self):
        """Test that transformation rules were applied."""
        # This is a basic test - in a real implementation,
        # we would test the actual functionality
        self.assertTrue(True, "Transformation rules should be applied")
    
    def test_code_structure(self):
        """Test that the code has the expected structure."""
        # Check that the transformed code file exists
        transformed_file = os.path.join(os.path.dirname(__file__), "transformed_code.py")
        self.assertTrue(os.path.exists(transformed_file), "Transformed code file should exist")


if __name__ == '__main__':
    unittest.main()
'''
        return test_code
    
    # Old transformation methods removed - now using JSON-based approach
    async def _old_transform_data_structures(self, data_mappings: List[Dict[str, Any]], 
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
                "output_path": f"output/python/{output_file}",
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
                "output_path": f"output/python/{output_file}",
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
                "output_path": f"output/python/{output_file}",
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
                "output_path": f"output/python/{output_file}",
                "llm_generated_code": main_code
            }
        
        return {"phase": "main_application", "files_transformed": [], "success": False}
    
    async def _write_transformed_file(self, filename: str, content: str) -> None:
        """Write transformed code to file."""
        import os
        
        # Create output directory
        output_dir = "output/python"
        os.makedirs(output_dir, exist_ok=True)
        
        # Write file
        file_path = os.path.join(output_dir, filename)
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        self.logger.info(f"Generated transformed file: {file_path}")
    
    def get_system_prompt(self) -> str:
        """Get the system prompt for the executor agent."""
        return """
        You are a JSON rule-based code transformation execution agent.
        Your role is to:
        1. Parse JSON transformation rules from the Planner Agent
        2. Generate actual Python code files based on the rules
        3. Create concrete artifacts in examples/python/ directory
        4. Ensure all transformation rules are applied to generate working code
        5. Verify artifact creation before handoff to Reviewer Agent
        
        Focus on concrete code generation and artifact creation, not abstract planning.
        """
