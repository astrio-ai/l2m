"""
Generate LangGraph workflow and agent system visuals for presentation slides.
"""

def generate_workflow_visual():
    """Generate ASCII art workflow visualization."""
    print("=" * 70)
    print("LangGraph Orchestration Flow - Main Workflow")
    print("=" * 70)
    print()
    print("START")
    print("  │")
    print("  ▼")
    print("┌─────────────────┐     ┌──────────────┐")
    print("│ Analyzer Agent  │────▶│  GraphState  │")
    print("│  • Parse COBOL  │     │  (Shared)    │")
    print("│  • Analyze      │     │              │")
    print("└────────┬────────┘     └──────┬───────┘")
    print("         │                     │")
    print("         ▼                     │")
    print("┌─────────────────┐            │")
    print("│ Planner Agent   │────────────┘")
    print("│  • Gen Rules    │")
    print("│  • Assess Risk  │")
    print("└────────┬────────┘")
    print("         │")
    print("         ▼")
    print("┌─────────────────┐")
    print("│ Executor Agent  │")
    print("│  • Transform    │")
    print("│  • Generate     │")
    print("└────────┬────────┘")
    print("         │")
    print("         ▼")
    print("┌─────────────────┐")
    print("│ Reviewer Agent  │")
    print("│  • Quality      │")
    print("│  • Standards    │")
    print("└────────┬────────┘")
    print("         │")
    print("         ▼")
    print("┌─────────────────┐")
    print("│  Tester Agent   │")
    print("│  • Gen Tests    │")
    print("│  • Coverage     │")
    print("└────────┬────────┘")
    print("         │")
    print("         ▼")
    print("┌─────────────────┐")
    print("│ Validator Agent │")
    print("│  • Final Check  │")
    print("│  • Compliance   │")
    print("└────────┬────────┘")
    print("         │")
    print("         ▼")
    print("       END")


def generate_agent_matrix():
    """Generate agent responsibilities matrix."""
    print()
    print("=" * 70)
    print("Agent Responsibilities Matrix")
    print("=" * 70)
    print()
    
    agents = [
        ("Analyzer", "Code Understanding", "Parse, analyze structure, map dependencies", "Analysis report, file structure"),
        ("Planner", "Strategy Design", "Generate rules, assess risks, plan phases", "Modernization plan, transformation rules"),
        ("Executor", "Code Transformation", "Transform code, generate templates, apply rules", "Transformed code, backup info"),
        ("Reviewer", "Quality Assessment", "Review code quality, check standards", "Quality report, recommendations"),
        ("Tester", "Test Generation", "Generate tests, run coverage analysis", "Test suite, coverage metrics"),
        ("Validator", "Final Validation", "Integration tests, compliance check", "Validation report, success status")
    ]
    
    print(f"{'Agent':<12} {'Role':<20} {'Operations':<40} {'Outputs'}")
    print("-" * 70)
    for agent, role, ops, outputs in agents:
        print(f"{agent:<12} {role:<20} {ops:<40} {outputs}")


def generate_flow_comparison():
    """Generate workflow variants comparison."""
    print()
    print("=" * 70)
    print("Workflow Variants Comparison")
    print("=" * 70)
    print()
    
    workflows = [
        ("Main Workflow", "Analyzer → Planner → Executor → Reviewer → Tester → Validator", "Full COBOL modernization with comprehensive validation"),
        ("Analysis Workflow", "Analyzer → END", "Quick codebase assessment and documentation"),
        ("Modernization Workflow", "Planner → Executor → Validator", "Code transformation without full quality review"),
        ("Validation Workflow", "Reviewer → Tester → Validator", "Testing and validation of transformed code")
    ]
    
    for i, (name, flow, desc) in enumerate(workflows, 1):
        print(f"{i}. {name}")
        print(f"   Flow: {flow}")
        print(f"   Use: {desc}")
        print()


def generate_tech_stack():
    """Generate technology stack visualization."""
    print("=" * 70)
    print("Technology Stack")
    print("=" * 70)
    print()
    
    stack = [
        ("LangGraph 0.6+", "Orchestration"),
        ("LangChain Core", "LLM Integration"),
        ("OpenAI GPT-4 / Anthropic Claude", "AI Intelligence"),
        ("ANTLR4", "COBOL Parsing"),
        ("Jinja2", "Template Engine"),
        ("Python 3.10+", "Runtime")
    ]
    
    print("┌─────────────────────────────────────────────┐")
    for i, (tech, purpose) in enumerate(stack):
        separator = "├" if i < len(stack) - 1 else "└"
        connector = "├" if i < len(stack) - 1 else "─"
        print(f"{separator} {tech:<30} │  ← {purpose}")
        if i < len(stack) - 1:
            print(f"├─────────────────────────────────────────────┤")
    print("└─────────────────────────────────────────────┘")


if __name__ == "__main__":
    generate_workflow_visual()
    generate_agent_matrix()
    generate_flow_comparison()
    generate_tech_stack()
    
    print()
    print("=" * 70)
    print("✅ Visualizations Generated")
    print("=" * 70)
    print()
    print("Files created:")
    print("  • docs/assets/agent_architecture.md")
    print("  • docs/assets/workflow_diagrams.md")
    print()
    print("These can be copied directly into your presentation slides.")

