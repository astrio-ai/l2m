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
            DirectoryScannerTool()
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
            # Get business goals for context
            modernization_goals = state.get("modernization_goals", [])
            business_context = f"""
            BUSINESS GOALS: {modernization_goals if modernization_goals else "General modernization to Python"}
            """
            
            transformation_prompt = f"""
            You are a code transformation expert. Based on the COBOL analysis below, generate ONLY a JSON response with transformation rules.

            {business_context}

            COBOL ANALYSIS:
            - Procedures: {state["analysis_results"]["structure"].get("procedures", [])}
            - Data Structures: {state["analysis_results"]["structure"].get("data_structures", [])}
            - File Dependencies: {state["analysis_results"]["structure"].get("file_dependencies", [])}
            - Program Metadata: {state["analysis_results"]["structure"].get("program_metadata", {})}
            - Complexity Score: {state["analysis_results"]["structure"].get("complexity_metrics", {}).get("complexity_score", 0)}

            TARGET LANGUAGE: {state["target_language"]}

            CRITICAL: Respond with ONLY valid JSON in this exact format (no other text):
            {{
                "phase": "Core Transformation",
                "business_goals": {modernization_goals},
                "rules": [
                    {{
                        "source": "OPEN-FILES",
                        "target": "def open_files():\\n    with open('acctrec.txt') as f:\\n        ..."
                    }},
                    {{
                        "source": "WRITE-HEADERS", 
                        "target": "def write_headers():\\n    print('HEADER')"
                    }}
                ]
            }}

            Generate transformation rules for each COBOL procedure and data structure. 
            Consider the business goals when designing the transformation approach.
            Return ONLY the JSON object.
            """
            
            # Use LLM to generate tactical transformation rules
            from langchain_core.messages import HumanMessage
            messages = [HumanMessage(content=transformation_prompt)]
            llm_response = await self.process_messages(messages)
            
            # Extract LLM transformation rules
            llm_transformation_rules = llm_response[0].content if llm_response else "No transformation rules available"
            
            # Debug: Log the LLM response for troubleshooting
            self.logger.info(f"LLM response length: {len(llm_transformation_rules)} characters")
            self.logger.debug(f"LLM response preview: {llm_transformation_rules[:200]}...")
            
            # Step 3: Parse and structure the transformation rules
            transformation_plan = self._parse_transformation_rules(llm_transformation_rules, state["analysis_results"])
            
            # Store planning results in state (this is what the Executor Agent will use)
            state["modernization_plan"] = modernization_plan
            state["risk_assessment"] = risk_assessment
            state["transformation_plan"] = transformation_plan
            state["llm_transformation_rules"] = llm_transformation_rules
            
            # Debug: Log what we're storing
            self.logger.info(f"Storing transformation_plan with {len(transformation_plan.get('rules', []))} rules")
            self.logger.info(f"Transformation plan keys: {list(transformation_plan.keys())}")
            
            # Also store handoff information for compatibility
            state["handoff_to_executor"] = {
                "transformation_plan": transformation_plan,
                "source_code_path": state["codebase_path"],
                "target_language": state["target_language"]
            }
            
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
        
        # Update the state directly and return it
        state["modernization_plan"] = modernization_plan
        state["risk_assessment"] = risk_assessment
        state["transformation_plan"] = transformation_plan
        state["llm_transformation_rules"] = llm_transformation_rules
        state["planning_summary"] = state.get("planning_summary", {})
        state["handoff_to_executor"] = state.get("handoff_to_executor", {})
        
        # Debug: Log what we're returning
        self.logger.info(f"Planner returning state with keys: {list(state.keys())}")
        self.logger.info(f"transformation_plan in result: {'transformation_plan' in state}")
        if 'transformation_plan' in state:
            self.logger.info(f"transformation_plan rules count: {len(state['transformation_plan'].get('rules', []))}")
        
        return state
    
    def _parse_transformation_rules(self, llm_response: str, analysis: Dict[str, Any]) -> Dict[str, Any]:
        """Parse LLM response into structured transformation rules."""
        import json
        import re
        
        try:
            # Try multiple JSON extraction patterns
            patterns = [
                r'\{.*\}',  # Basic JSON object
                r'```json\s*(\{.*?\})\s*```',  # JSON in code block
                r'```\s*(\{.*?\})\s*```',  # JSON in generic code block
            ]
            
            for pattern in patterns:
                json_match = re.search(pattern, llm_response, re.DOTALL)
                if json_match:
                    json_str = json_match.group(1) if len(json_match.groups()) > 0 else json_match.group(0)
                    try:
                        transformation_data = json.loads(json_str)
                        # Validate that we have the expected structure
                        if "phase" in transformation_data and "rules" in transformation_data:
                            self.logger.info(f"Successfully parsed JSON transformation rules: {len(transformation_data.get('rules', []))} rules")
                            return transformation_data
                    except json.JSONDecodeError:
                        continue
            
            # Try to find JSON-like structure even if not perfectly formatted
            if "phase" in llm_response and "rules" in llm_response:
                # Extract just the JSON part more carefully
                start_idx = llm_response.find('{')
                end_idx = llm_response.rfind('}') + 1
                if start_idx != -1 and end_idx > start_idx:
                    json_str = llm_response[start_idx:end_idx]
                    try:
                        transformation_data = json.loads(json_str)
                        if "phase" in transformation_data and "rules" in transformation_data:
                            self.logger.info(f"Successfully parsed JSON transformation rules (extracted): {len(transformation_data.get('rules', []))} rules")
                            return transformation_data
                    except json.JSONDecodeError:
                        pass
                        
        except Exception as e:
            self.logger.warning(f"Error parsing JSON from LLM response: {e}")
        
        # Fallback: Generate transformation rules from analysis data
        self.logger.warning("Using fallback transformation rules - LLM did not generate valid JSON")
        return self._generate_fallback_transformation_rules(analysis)
    
    def _generate_fallback_transformation_rules(self, analysis: Dict[str, Any]) -> Dict[str, Any]:
        """Generate transformation rules from analysis data when LLM parsing fails."""
        procedures = analysis.get("structure", {}).get("procedures", [])
        data_structures = analysis.get("structure", {}).get("data_structures", [])
        file_dependencies = analysis.get("structure", {}).get("file_dependencies", [])
        
        # Generate transformation rules in the expected JSON format
        rules = []
        
        # Add procedure transformation rules
        for proc in procedures:
            rule = {
                "source": proc["name"],
                "target": f"def {proc['name'].lower().replace('-', '_')}():\n    # Converted from COBOL {proc['name']}\n    pass"
            }
            rules.append(rule)
        
        # Add data structure transformation rules
        for struct in data_structures:
            rule = {
                "source": struct["name"],
                "target": f"class {struct['name'].title().replace('-', '')}:\n    def __init__(self):\n        # Converted from COBOL {struct['name']}\n        pass"
            }
            rules.append(rule)
        
        # Add file I/O transformation rules
        for dep in file_dependencies:
            rule = {
                "source": dep["logical_name"],
                "target": f"def {dep['logical_name'].lower().replace('-', '_')}():\n    # Converted from COBOL file {dep['logical_name']}\n    pass"
            }
            rules.append(rule)
        
        return {
            "phase": "Core Transformation",
            "rules": rules
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
        
        CRITICAL: You MUST output ONLY valid JSON in this exact format:
        {
            "phase": "Core Transformation",
            "rules": [
                {
                    "source": "OPEN-FILES",
                    "target": "def open_files():\n    with open('acctrec.txt') as f:\n        ..."
                },
                {
                    "source": "WRITE-HEADERS", 
                    "target": "def write_headers():\n    print('HEADER')"
                }
            ]
        }
        
        Focus on producing actionable transformation rules, not high-level strategy.
        Output must be structured JSON data that the Executor Agent can immediately execute.
        """
