"""
Modernization Pipeline - Orchestrates full agent flow.

Coordinates all agents for complete COBOL to Python modernization workflow.
"""

from typing import Optional, Dict, Any
from agents import SQLiteSession
from src.config import get_settings
from src.utils.logger import get_logger
from src.agents.analyzer_agent import AnalyzerAgent
from src.agents.translator_agent import TranslatorAgent
from src.agents.reviewer_agent import ReviewerAgent
from src.agents.tester_agent import TesterAgent
from src.agents.refactor_agent import RefactorAgent

logger = get_logger(__name__)


class ModernizationPipeline:
    """Orchestrates the complete modernization workflow."""
    
    def __init__(self, session_id: Optional[str] = None):
        """Initialize the modernization pipeline.
        
        Args:
            session_id: Optional session ID for maintaining conversation history
        """
        self.settings = get_settings()
        self.session_id = session_id or "default"
        
        # Initialize agents
        self.analyzer = AnalyzerAgent()
        self.translator = TranslatorAgent()
        self.reviewer = ReviewerAgent()
        self.tester = TesterAgent()
        self.refactor = RefactorAgent()
        
        # Create session if needed
        self.session = SQLiteSession(self.session_id, str(self.settings.db_path)) if self.session_id else None
    
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
            # Step 1: Analyze COBOL
            logger.info("Step 1: Analyzing COBOL code")
            analysis_prompt = f"Analyze this COBOL file: {cobol_file_path}"
            results["analysis"] = await self.analyzer.run(analysis_prompt, session=self.session)
            
            # Step 2: Translate to Python
            logger.info("Step 2: Translating to Python")
            translation_prompt = f"Translate the analyzed COBOL code to modern Python:\n{results['analysis']}"
            results["translation"] = await self.translator.run(translation_prompt, session=self.session)
            
            # Step 3: Review code
            logger.info("Step 3: Reviewing translated code")
            review_prompt = f"Review this Python code for quality:\n{results['translation']}"
            results["review"] = await self.reviewer.run(review_prompt, session=self.session)
            
            # Step 4: Generate tests
            logger.info("Step 4: Generating tests")
            test_prompt = f"Generate comprehensive tests for this Python code:\n{results['translation']}"
            results["tests"] = await self.tester.run(test_prompt, session=self.session)
            
            # Step 5: Refactor (optional)
            logger.info("Step 5: Refactoring code")
            refactor_prompt = f"Refactor this code to improve readability and maintainability:\n{results['translation']}"
            results["refactored"] = await self.refactor.run(refactor_prompt, session=self.session)
            
            logger.info("Modernization pipeline completed successfully")
            return results
            
        except Exception as e:
            logger.error(f"Error in modernization pipeline: {e}")
            results["error"] = str(e)
            return results

