"""
Handoff tools for agent-to-agent communication in the multi-agent system.
"""

from typing import Annotated, Dict, Any, List
from langchain_core.tools import tool, InjectedToolCallId
from langgraph.prebuilt import InjectedState
from langgraph.types import Command, Send
from langgraph.graph import MessagesState


def create_handoff_tool(*, agent_name: str, description: str | None = None):
    """Create a handoff tool for transferring control to another agent."""
    name = f"transfer_to_{agent_name}"
    description = description or f"Transfer to {agent_name}"

    @tool(name, description=description)
    def handoff_tool(
        state: Annotated[MessagesState, InjectedState], 
        tool_call_id: Annotated[str, InjectedToolCallId],
    ) -> Command:
        tool_message = {
            "role": "tool",
            "content": f"Successfully transferred to {agent_name}",
            "name": name,
            "tool_call_id": tool_call_id,
        }
        return Command(  
            goto=agent_name,  
            update={"messages": state["messages"] + [tool_message]},  
            graph=Command.PARENT,  
        )
    return handoff_tool


def create_task_description_handoff_tool(
    *, agent_name: str, description: str | None = None
):
    """Create a handoff tool that passes specific task description to the next agent."""
    name = f"transfer_to_{agent_name}"
    description = description or f"Ask {agent_name} for help."

    @tool(name, description=description)
    def handoff_tool(
        # this is populated by the calling agent
        task_description: Annotated[
            str,
            "Description of what the next agent should do, including all of the relevant context.",
        ],
        # these parameters are ignored by the LLM
        state: Annotated[MessagesState, InjectedState],
    ) -> Command:
        task_description_message = {"role": "user", "content": task_description}
        agent_input = {**state, "messages": [task_description_message]}
        return Command(
            goto=[Send(agent_name, agent_input)],
            graph=Command.PARENT,
        )

    return handoff_tool


def create_transformation_handoff_tool(
    *, agent_name: str, description: str | None = None
):
    """Create a handoff tool specifically for passing transformation rules to the Executor agent."""
    name = f"transfer_to_{agent_name}"
    description = description or f"Transfer transformation rules to {agent_name}"

    @tool(name, description=description)
    def handoff_tool(
        transformation_plan: Annotated[
            Dict[str, Any],
            "Complete transformation plan with rules, phases, and execution order",
        ],
        source_code_path: Annotated[
            str,
            "Path to the source code file to be transformed",
        ],
        target_language: Annotated[
            str,
            "Target language for the transformation",
        ],
        state: Annotated[MessagesState, InjectedState],
    ) -> Command:
        # Create a structured message with transformation data
        transformation_message = {
            "role": "user", 
            "content": f"Execute transformation plan for {target_language} on {source_code_path}",
            "transformation_plan": transformation_plan,
            "source_code_path": source_code_path,
            "target_language": target_language,
            "execution_context": {
                "transformation_rules": transformation_plan.get("transformation_rules", []),
                "data_structure_mappings": transformation_plan.get("data_structure_mappings", []),
                "file_io_transformations": transformation_plan.get("file_io_transformations", []),
                "execution_phases": transformation_plan.get("transformation_phases", []),
                "execution_order": transformation_plan.get("execution_order", [])
            }
        }
        agent_input = {**state, "messages": [transformation_message]}
        return Command(
            goto=[Send(agent_name, agent_input)],
            graph=Command.PARENT,
        )

    return handoff_tool


def create_analysis_handoff_tool(
    *, agent_name: str, description: str | None = None
):
    """Create a handoff tool for passing analysis results to the Planner agent."""
    name = f"transfer_to_{agent_name}"
    description = description or f"Transfer analysis results to {agent_name}"

    @tool(name, description=description)
    def handoff_tool(
        analysis_results: Annotated[
            Dict[str, Any],
            "Code analysis results including procedures, data structures, and dependencies",
        ],
        target_language: Annotated[
            str,
            "Target language for modernization",
        ],
        modernization_goals: Annotated[
            List[str],
            "Goals for the modernization process",
        ],
        state: Annotated[MessagesState, InjectedState],
    ) -> Command:
        # Create a structured message with analysis data
        analysis_message = {
            "role": "user", 
            "content": f"Create transformation plan for {target_language} based on analysis: {analysis_results}",
            "analysis_results": analysis_results,
            "target_language": target_language,
            "modernization_goals": modernization_goals
        }
        agent_input = {**state, "messages": [analysis_message]}
        return Command(
            goto=[Send(agent_name, agent_input)],
            graph=Command.PARENT,
        )

    return handoff_tool


# Predefined handoff tools for the modernization workflow
transfer_to_planner = create_analysis_handoff_tool(
    agent_name="planner",
    description="Transfer analysis results to the Planner agent for transformation planning"
)

transfer_to_executor = create_transformation_handoff_tool(
    agent_name="executor", 
    description="Transfer complete transformation plan to the Executor agent for AI-powered code transformation"
)

transfer_to_reviewer = create_handoff_tool(
    agent_name="reviewer",
    description="Transfer transformed code to the Reviewer agent for quality review"
)

transfer_to_tester = create_handoff_tool(
    agent_name="tester",
    description="Transfer code to the Tester agent for test generation and execution"
)

transfer_to_validator = create_handoff_tool(
    agent_name="validator",
    description="Transfer code to the Validator agent for final validation"
)
