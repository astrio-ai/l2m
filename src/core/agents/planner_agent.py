"""
Modernization planning agent.

This agent creates detailed modernization plans based on the analysis
results from the analyzer agent.
"""

from typing import Dict, Any, List
from src.core.agents.base_agent import BaseAgent
from src.core.state.agent_state import AgentState
from src.core.tools.code_tools import ModernizationPlannerTool, RiskAssessmentTool
from src.core.tools.search_tools import PatternSearchTool, ReferenceFinderTool, CodeDiscoveryTool
from src.core.tools.file_tools import FileReaderTool, DirectoryScannerTool
from src.core.tools.handoff_tools import transfer_to_executor
from src.utils.logger import get_logger

logger = get_logger(__name__)


class PlannerAgent(BaseAgent):
    """Agent responsible for creating modernization plans."""
    
    def __init__(self, settings):
        """Initialize the planner agent."""
        tools = [
            ModernizationPlannerTool(),
            RiskAssessmentTool(),
            PatternSearchTool(),
            ReferenceFinderTool(),
            CodeDiscoveryTool(),
            FileReaderTool(),
            DirectoryScannerTool(),
            transfer_to_executor
        ]
        super().__init__(settings, tools)
    
    async def run(self, state: AgentState) -> AgentState:
        """Run the planner agent."""
        self.log_activity("Starting modernization planning")
        
        try:
            # Step 1: Gather raw data using tools
            modernization_plan = await self.use_tool(
                "create_modernization_plan",
                analysis_results=state["analysis_results"],
                target_language=state["target_language"],
                modernization_goals=state["modernization_goals"]
            )
            
            risk_assessment = await self.use_tool(
                "assess_modernization_risks",
                plan=modernization_plan,
                codebase_complexity=state["analysis_results"].get("structure", {})
            )
            
            # Step 2: Use LLM to generate tactical transformation rules
            transformation_prompt = f"""
            Based on the following COBOL code analysis, generate specific transformation rules for converting to {state["target_language"]}:

            COBOL ANALYSIS DATA:
            - Procedures: {state["analysis_results"]["structure"].get("procedures", [])}
            - Data Structures: {state["analysis_results"]["structure"].get("data_structures", [])}
            - File Dependencies: {state["analysis_results"]["structure"].get("file_dependencies", [])}
            - Program Metadata: {state["analysis_results"]["structure"].get("program_metadata", {})}
            - Complexity Score: {state["analysis_results"]["structure"].get("complexity_metrics", {}).get("complexity_score", 0)}

            TARGET LANGUAGE: {state["target_language"]}

            Generate a JSON response with this exact structure:
            {{
                "transformation_rules": [
                    {{
                        "source_procedure": "OPEN-FILES",
                        "target_function": "open_files",
                        "transformation_type": "procedure_to_function",
                        "target_code": "def open_files():\n    # Open input and output files\n    pass",
                        "dependencies": ["file_operations"],
                        "complexity": "low"
                    }}
                ],
                "data_structure_mappings": [
                    {{
                        "source_structure": "PRINT-REC",
                        "target_class": "PrintRecord",
                        "transformation_type": "cobol_record_to_class",
                        "target_code": "class PrintRecord:\n    def __init__(self):\n        self.acct_no = None\n        self.last_name = None\n        # ... other fields",
                        "dependencies": ["data_models"]
                    }}
                ],
                "file_io_transformations": [
                    {{
                        "source_file": "PRINT-LINE",
                        "target_operation": "write_output",
                        "transformation_type": "cobol_file_to_function",
                        "target_code": "def write_output(data):\n    # Write data to output file\n    pass",
                        "dependencies": ["file_operations"]
                    }}
                ],
                "execution_order": ["data_structures", "file_operations", "procedures"],
                "transformation_phases": [
                    {{
                        "phase_name": "data_structure_conversion",
                        "rules": ["data_structure_mappings"],
                        "output_files": ["models.py"]
                    }},
                    {{
                        "phase_name": "file_operations_conversion", 
                        "rules": ["file_io_transformations"],
                        "output_files": ["file_ops.py"]
                    }},
                    {{
                        "phase_name": "business_logic_conversion",
                        "rules": ["transformation_rules"],
                        "output_files": ["main.py"]
                    }}
                ]
            }}

            Focus on generating concrete, executable transformation rules that map COBOL constructs to {state["target_language"]} code.
            """
            
            # Use LLM to generate tactical transformation rules
            from langchain_core.messages import HumanMessage
            messages = [HumanMessage(content=transformation_prompt)]
            llm_response = await self.process_messages(messages)
            
            # Extract LLM transformation rules
            llm_transformation_rules = llm_response[0].content if llm_response else "No transformation rules available"
            
            # Step 3: Parse and structure the transformation rules
            transformation_plan = self._parse_transformation_rules(llm_transformation_rules, state["analysis_results"])
            
            # Step 4: Store handoff information for workflow
            state["handoff_to_executor"] = {
                "transformation_plan": transformation_plan,
                "source_code_path": state["codebase_path"],
                "target_language": state["target_language"]
            }
            
            # Store planning results in state
            state["modernization_plan"] = modernization_plan
            state["risk_assessment"] = risk_assessment
            state["transformation_plan"] = transformation_plan
            state["llm_transformation_rules"] = llm_transformation_rules
            
            # Add tactical planning summary
            state["planning_summary"] = {
                "transformation_rules_count": len(transformation_plan.get("transformation_rules", [])),
                "data_structure_mappings_count": len(transformation_plan.get("data_structure_mappings", [])),
                "file_io_transformations_count": len(transformation_plan.get("file_io_transformations", [])),
                "execution_phases": len(transformation_plan.get("transformation_phases", [])),
                "complexity_level": self._assess_planning_complexity(state["analysis_results"]),
                "next_agent": "executor"
            }
            
            self.log_activity("Tactical transformation planning completed, ready for Executor Agent", {
                "transformation_rules": len(transformation_plan.get("transformation_rules", [])),
                "data_mappings": len(transformation_plan.get("data_structure_mappings", [])),
                "file_transformations": len(transformation_plan.get("file_io_transformations", [])),
                "execution_phases": len(transformation_plan.get("transformation_phases", [])),
                "handoff_prepared": True
            })
            
        except Exception as e:
            self.logger.error(f"Error in planner agent: {e}")
            state["error"] = str(e)
        
        return state
    
    def _parse_transformation_rules(self, llm_response: str, analysis: Dict[str, Any]) -> Dict[str, Any]:
        """Parse LLM response into structured transformation rules."""
        import json
        import re
        
        try:
            # Try to extract JSON from LLM response
            json_match = re.search(r'\{.*\}', llm_response, re.DOTALL)
            if json_match:
                transformation_data = json.loads(json_match.group())
                return transformation_data
        except (json.JSONDecodeError, AttributeError):
            pass
        
        # Fallback: Generate transformation rules from analysis data
        return self._generate_fallback_transformation_rules(analysis)
    
    def _generate_fallback_transformation_rules(self, analysis: Dict[str, Any]) -> Dict[str, Any]:
        """Generate transformation rules from analysis data when LLM parsing fails."""
        procedures = analysis.get("structure", {}).get("procedures", [])
        data_structures = analysis.get("structure", {}).get("data_structures", [])
        file_dependencies = analysis.get("structure", {}).get("file_dependencies", [])
        
        # Generate transformation rules for procedures
        transformation_rules = []
        for proc in procedures:
            rule = {
                "source_procedure": proc["name"],
                "target_function": proc["name"].lower().replace("-", "_"),
                "transformation_type": "procedure_to_function",
                "target_code": f"def {proc['name'].lower().replace('-', '_')}():\n    # Converted from COBOL {proc['name']}\n    pass",
                "dependencies": ["file_operations"],
                "complexity": "medium"
            }
            transformation_rules.append(rule)
        
        # Generate data structure mappings
        data_structure_mappings = []
        for struct in data_structures:
            mapping = {
                "source_structure": struct["name"],
                "target_class": struct["name"].title().replace("-", ""),
                "transformation_type": "cobol_record_to_class",
                "target_code": f"class {struct['name'].title().replace('-', '')}:\n    def __init__(self):\n        # Converted from COBOL {struct['name']}\n        pass",
                "dependencies": ["data_models"]
            }
            data_structure_mappings.append(mapping)
        
        # Generate file I/O transformations
        file_io_transformations = []
        for dep in file_dependencies:
            transformation = {
                "source_file": dep["logical_name"],
                "target_operation": dep["logical_name"].lower().replace("-", "_"),
                "transformation_type": "cobol_file_to_function",
                "target_code": f"def {dep['logical_name'].lower().replace('-', '_')}():\n    # Converted from COBOL file {dep['logical_name']}\n    pass",
                "dependencies": ["file_operations"]
            }
            file_io_transformations.append(transformation)
        
        return {
            "transformation_rules": transformation_rules,
            "data_structure_mappings": data_structure_mappings,
            "file_io_transformations": file_io_transformations,
            "execution_order": ["data_structures", "file_operations", "procedures"],
            "transformation_phases": [
                {
                    "phase_name": "data_structure_conversion",
                    "rules": ["data_structure_mappings"],
                    "output_files": ["models.py"]
                },
                {
                    "phase_name": "file_operations_conversion",
                    "rules": ["file_io_transformations"],
                    "output_files": ["file_ops.py"]
                },
                {
                    "phase_name": "business_logic_conversion",
                    "rules": ["transformation_rules"],
                    "output_files": ["main.py"]
                }
            ]
        }
    
    def _assess_planning_complexity(self, analysis: Dict[str, Any]) -> str:
        """Assess the complexity level for planning purposes."""
        complexity = analysis.get("structure", {}).get("complexity_metrics", {}).get("complexity_score", 0)
        return self._get_complexity_level(complexity)
    
    def _get_complexity_level(self, score: float) -> str:
        """Get complexity level based on score."""
        if score < 20:
            return "LOW"
        elif score < 50:
            return "MEDIUM"
        else:
            return "HIGH"
    
    
    def get_system_prompt(self) -> str:
        """Get the system prompt for the planner agent."""
        return """
        You are a tactical code transformation planner for legacy codebases.
        Your role is to:
        1. Analyze the structured code data (procedures, data structures, dependencies)
        2. Generate specific transformation rules mapping legacy code to target language
        3. Create executable transformation tasks for the Executor Agent
        4. Provide concrete code snippets and patterns for transformation
        
        Focus on producing actionable transformation rules, not high-level strategy.
        Output must be structured data that the Executor Agent can immediately execute.
        """
