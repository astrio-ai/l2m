"""
Modernization Pipeline - Orchestrates full agent flow.

Coordinates all agents for complete COBOL to Python modernization workflow.
"""

from typing import Optional, Dict, Any
from agents import SQLiteSession, Agent
from src.config import get_settings
from src.utils.logger import get_logger
from src.agents.analyzer_agent import AnalyzerAgent
from src.agents.translator_agent import TranslatorAgent
from src.agents.reviewer_agent import ReviewerAgent
from src.agents.tester_agent import TesterAgent
from src.agents.refactor_agent import RefactorAgent
from src.agents.orchestrator_agent import OrchestratorAgent

logger = get_logger(__name__)


class ModernizationPipeline:
    """Orchestrates the complete modernization workflow."""
    
    def __init__(self, session_id: Optional[str] = None, use_handoffs: bool = True):
        """Initialize the modernization pipeline.
        
        Args:
            session_id: Optional session ID for maintaining conversation history
            use_handoffs: Whether to use agent handoffs (orchestrator) or sequential execution
        """
        self.settings = get_settings()
        self.session_id = session_id or "default"
        self.use_handoffs = use_handoffs
        
        # Initialize agents
        self.analyzer = AnalyzerAgent()
        self.translator = TranslatorAgent()
        self.reviewer = ReviewerAgent()
        self.tester = TesterAgent()
        self.refactor = RefactorAgent()
        
        # Create session if needed
        self.session = SQLiteSession(self.session_id, str(self.settings.db_path)) if self.session_id else None
        
        # Set up orchestrator with handoffs if enabled
        if use_handoffs:
            handoff_agents = [
                self.analyzer.agent,
                self.translator.agent,
                self.reviewer.agent,
                self.tester.agent,
                self.refactor.agent,
            ]
            self.orchestrator = OrchestratorAgent(handoff_agents=handoff_agents)
        else:
            self.orchestrator = None
    
    async def run(self, cobol_file_path: str) -> Dict[str, Any]:
        """Run the complete modernization pipeline.
        
        Args:
            cobol_file_path: Path to COBOL file to modernize
            
        Returns:
            Dictionary with modernization results
        """
        logger.info(f"Starting modernization pipeline for {cobol_file_path}")
        
        results = {
            "cobol_file": cobol_file_path,
            "analysis": None,
            "translation": None,
            "review": None,
            "tests": None,
            "refactored": None,
        }
        
        try:
            if self.use_handoffs and self.orchestrator:
                # Use orchestrator with handoffs
                logger.info("Using orchestrator with agent handoffs")
                prompt = f"""Modernize this COBOL file to Python: {cobol_file_path}
                
                The workflow should be:
                1. Analyze the COBOL code structure
                2. Translate to Python
                3. Review the translated code
                4. Generate tests
                5. Refactor if needed
                
                Coordinate the handoffs between agents as needed."""
                
                output = await self.orchestrator.run(prompt, session=self.session)
                results["orchestrator_output"] = output
            else:
                # Sequential execution without handoffs
                logger.info("Using sequential agent execution")
                
                # Read COBOL file content
                from pathlib import Path
                cobol_file = Path(cobol_file_path)
                if not cobol_file.exists():
                    results["error"] = f"COBOL file not found: {cobol_file_path}"
                    return results
                
                cobol_content = cobol_file.read_text(encoding="utf-8", errors="ignore")
                logger.info(f"Read COBOL file: {len(cobol_content)} characters")
                
                # Step 1: Analyze COBOL
                logger.info("Step 1: Analyzing COBOL code")
                analysis_prompt = f"""Analyze this COBOL program:

File: {cobol_file_path}

COBOL Code:
```
{cobol_content}
```

Use the analyze_cobol tool to extract the structure, then provide a detailed analysis of:
- Program structure (PROGRAM-ID, divisions)
- Variable declarations (WORKING-STORAGE)
- Procedures and logic flow
- Dependencies between sections"""
                results["analysis"] = await self.analyzer.run(analysis_prompt, session=self.session)
                
                # Step 2: Translate to Python
                logger.info("Step 2: Translating to Python")
                translation_prompt = f"""Translate this COBOL code to modern Python:

Original COBOL:
```
{cobol_content}
```

Analysis Results:
{results['analysis']}

Generate clean, modern Python code that:
- Preserves the original functionality
- Uses type hints and docstrings
- Follows PEP 8 style guidelines
- Includes a main() function if appropriate"""
                results["translation"] = await self.translator.run(translation_prompt, session=self.session)
                
                # Step 3: Review code
                logger.info("Step 3: Reviewing translated code")
                review_prompt = f"""Review this Python code for quality and correctness:

Python Code:
```
{results['translation']}
```

Original COBOL for reference:
```
{cobol_content}
```

Check for:
- Correctness (does it match COBOL logic?)
- PEP 8 compliance
- Code quality and best practices
- Potential bugs or improvements"""
                results["review"] = await self.reviewer.run(review_prompt, session=self.session)
                
                # Step 4: Generate tests
                logger.info("Step 4: Generating tests")
                test_prompt = f"""Generate comprehensive unit tests for this Python code:

Python Code:
```
{results['translation']}
```

Original COBOL:
```
{cobol_content}
```

Generate pytest tests that:
- Test all functions
- Verify correct behavior matches COBOL logic
- Include edge cases
- Use pytest conventions"""
                results["tests"] = await self.tester.run(test_prompt, session=self.session)
                
                # Step 5: Refactor (optional)
                logger.info("Step 5: Refactoring code")
                refactor_prompt = f"""Refactor this Python code to improve readability and maintainability:

Python Code:
```
{results['translation']}
```

Improve:
- Code structure and organization
- Add/improve type hints and docstrings
- Extract patterns if needed
- Optimize where appropriate

Maintain functional equivalence."""
                results["refactored"] = await self.refactor.run(refactor_prompt, session=self.session)
            
            logger.info("Modernization pipeline completed successfully")
            return results
            
        except Exception as e:
            logger.error(f"Error in modernization pipeline: {e}")
            results["error"] = str(e)
            return results

