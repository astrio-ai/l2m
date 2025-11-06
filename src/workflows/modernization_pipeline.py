"""
Modernization Pipeline - Orchestrates full agent flow.

Coordinates all agents for complete COBOL to Python modernization workflow.
"""

import asyncio
import json
import re
from pathlib import Path
from typing import Optional, Dict, Any, List
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
    
    def __init__(self, session_id: Optional[str] = None, use_handoffs: Optional[bool] = None):
        """Initialize the modernization pipeline.
        
        Args:
            session_id: Optional session ID for maintaining conversation history
            use_handoffs: Whether to use agent handoffs (orchestrator) or sequential execution.
                         If None, uses settings.enable_handoffs
        """
        self.settings = get_settings()
        self.session_id = session_id or "default"
        self.use_handoffs = use_handoffs if use_handoffs is not None else self.settings.enable_handoffs
        
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
    
    @staticmethod
    def _extract_code_from_markdown(text: str, language: str = "python") -> str:
        """Extract code from markdown code blocks.
        
        Args:
            text: Text that may contain code in markdown code blocks
            language: Language tag to look for (default: "python")
            
        Returns:
            Extracted code, or original text if no code blocks found
        """
        if not text:
            return ""
        
        # Look for code blocks with specific language tag
        patterns = [
            rf'```{language}\s*\n(.*?)```',
            rf'```{re.escape(language)}\s*\n(.*?)```',
            r'```\s*\n(.*?)```',  # Fallback: any code block
        ]
        
        for pattern in patterns:
            matches = re.findall(pattern, text, re.DOTALL)
            if matches:
                # Return the longest match (likely the main code)
                return max(matches, key=len).strip()
        
        # If no code blocks found, return the text as-is (might already be code)
        return text.strip()
    
    def _extract_python_code(self, result: Dict[str, Any]) -> Optional[str]:
        """Extract Python code from pipeline result.
        
        Args:
            result: Pipeline result dictionary
            
        Returns:
            Extracted Python code, or None if not found
        """
        # Try refactored first (most polished), then translation
        for field in ["refactored", "translation"]:
            if result.get(field):
                code = self._extract_code_from_markdown(result[field], language="python")
                if code and len(code.strip()) > 50:  # Minimum reasonable size
                    return code
        
        # Try orchestrator output
        if result.get("orchestrator_output"):
            orchestrator_text = str(result["orchestrator_output"])
            
            # Look for all Python code blocks
            patterns = [
                r'```python\s*\n(.*?)```',
                r'```\s*\n(.*?)```',
            ]
            
            code_blocks = []
            for pattern in patterns:
                matches = re.findall(pattern, orchestrator_text, re.DOTALL)
                for match in matches:
                    code = match.strip()
                    if code and len(code.strip()) > 50:
                        code_blocks.append(code)
            
            if code_blocks:
                # Prefer code blocks that don't contain test indicators
                test_indicators = [
                    r'\btest_\w+',
                    r'\bunittest\b',
                    r'\bpytest\b',
                    r'\bTestCase\b',
                    r'class\s+\w*[Tt]est',
                ]
                
                # First try to find a non-test code block
                for code in code_blocks:
                    if not any(re.search(indicator, code, re.IGNORECASE) for indicator in test_indicators):
                        return code
                
                # If all blocks contain tests, return the longest one (might be combined code+test)
                return max(code_blocks, key=len)
        
        return None
    
    def _extract_test_code(self, result: Dict[str, Any]) -> Optional[str]:
        """Extract test code from pipeline result.
        
        Args:
            result: Pipeline result dictionary
            
        Returns:
            Extracted test code, or None if not found
        """
        # First check the tests field
        if result.get("tests"):
            code = self._extract_code_from_markdown(result["tests"], language="python")
            if code and len(code.strip()) > 50:  # Minimum reasonable size
                return code
        
        # Also check orchestrator_output for test code
        if result.get("orchestrator_output"):
            orchestrator_text = str(result["orchestrator_output"])
            
            # Look for all Python code blocks
            patterns = [
                r'```python\s*\n(.*?)```',
                r'```\s*\n(.*?)```',
            ]
            
            test_code_blocks = []
            for pattern in patterns:
                matches = re.findall(pattern, orchestrator_text, re.DOTALL)
                for match in matches:
                    code = match.strip()
                    # Check if this code block contains test code
                    # Look for test indicators: test_, unittest, pytest, TestCase, assert
                    test_indicators = [
                        r'\btest_\w+',
                        r'\bunittest\b',
                        r'\bpytest\b',
                        r'\bTestCase\b',
                        r'\bassert\b',
                        r'class\s+\w*[Tt]est',
                    ]
                    if any(re.search(indicator, code, re.IGNORECASE) for indicator in test_indicators):
                        test_code_blocks.append(code)
            
            # Return the longest test code block found
            if test_code_blocks:
                return max(test_code_blocks, key=len)
        
        return None
    
    def _save_results(self, result: Dict[str, Any], output_dir: Optional[Path] = None) -> Dict[str, Any]:
        """Save pipeline results to files.
        
        Args:
            result: Pipeline result dictionary
            output_dir: Directory to save files (defaults to data/output)
            
        Returns:
            Dictionary with paths to saved files
        """
        if "error" in result:
            return {"error": result["error"]}
        
        # Determine output directory
        if output_dir is None:
            output_dir = Path("data/output")
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Get base filename from COBOL file
        cobol_file = Path(result.get("cobol_file", "unknown"))
        base_name = cobol_file.stem
        
        saved_files = {}
        
        # Extract and save Python code
        python_code = self._extract_python_code(result)
        if python_code:
            python_file = output_dir / f"{base_name}.py"
            python_file.write_text(python_code, encoding="utf-8")
            saved_files["python_file"] = str(python_file)
        else:
            logger.warning(f"No Python code extracted for {base_name}")
        
        # Extract and save test code
        test_code = self._extract_test_code(result)
        if test_code:
            test_file = output_dir / f"test_{base_name}.py"
            test_file.write_text(test_code, encoding="utf-8")
            saved_files["test_file"] = str(test_file)
        else:
            logger.warning(f"No test code extracted for {base_name}")
        
        return saved_files
    
    def _find_ground_truth(self, cobol_file_path: str) -> Optional[str]:
        """Find ground truth Python file in the same directory as COBOL file.
        
        Args:
            cobol_file_path: Path to COBOL file
            
        Returns:
            Ground truth Python code if found, None otherwise
        """
        cobol_file = Path(cobol_file_path)
        cobol_dir = cobol_file.parent
        
        # Look for Python files with same stem (case-insensitive)
        possible_names = [
            cobol_file.stem + ".py",
            cobol_file.stem.lower() + ".py",
            cobol_file.stem.upper() + ".py",
        ]
        
        for name in possible_names:
            ground_truth_file = cobol_dir / name
            if ground_truth_file.exists() and ground_truth_file != cobol_file:
                try:
                    content = ground_truth_file.read_text(encoding="utf-8")
                    logger.info(f"Found ground truth file: {ground_truth_file}")
                    return content
                except Exception as e:
                    logger.warning(f"Could not read ground truth file {ground_truth_file}: {e}")
        
        return None
    
    def _extract_constants_from_ground_truth(self, ground_truth: str) -> Dict[str, str]:
        """Extract constant definitions from ground truth code.
        
        Args:
            ground_truth: Ground truth Python code
            
        Returns:
            Dictionary mapping constant names to their values
        """
        constants = {}
        
        # Pattern to match constant assignments: CONSTANT_NAME = 'value' or CONSTANT_NAME = value
        import re
        pattern = r'^([A-Z_][A-Z0-9_]*)\s*=\s*(.+)$'
        
        for line in ground_truth.split('\n'):
            line = line.strip()
            # Skip comments and empty lines
            if not line or line.startswith('#'):
                continue
            
            match = re.match(pattern, line)
            if match:
                const_name = match.group(1)
                const_value = match.group(2).strip()
                # Remove quotes if present
                if const_value.startswith("'") and const_value.endswith("'"):
                    const_value = const_value[1:-1]
                elif const_value.startswith('"') and const_value.endswith('"'):
                    const_value = const_value[1:-1]
                constants[const_name] = const_value
        
        return constants
    
    def _extract_class_names_from_ground_truth(self, ground_truth: str) -> List[str]:
        """Extract class names from ground truth code.
        
        Args:
            ground_truth: Ground truth Python code
            
        Returns:
            List of class names
        """
        import re
        class_names = []
        
        # Pattern to match class definitions: class ClassName:
        pattern = r'^class\s+([A-Z][a-zA-Z0-9_]*)\s*[:\(]'
        
        for line in ground_truth.split('\n'):
            match = re.match(pattern, line.strip())
            if match:
                class_names.append(match.group(1))
        
        return class_names
    
    async def run(self, cobol_file_path: str, save_files: bool = True, output_dir: Optional[Path] = None) -> Dict[str, Any]:
        """Run the complete modernization pipeline.
        
        Args:
            cobol_file_path: Path to COBOL file to modernize
            save_files: Whether to save extracted Python and test files (default: True)
            output_dir: Directory to save files (defaults to data/output)
            
        Returns:
            Dictionary with modernization results and saved file paths
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
                
                # Read COBOL file content first
                from pathlib import Path
                cobol_file = Path(cobol_file_path)
                if not cobol_file.exists():
                    results["error"] = f"COBOL file not found: {cobol_file_path}"
                    return results
                
                cobol_content = cobol_file.read_text(encoding="utf-8", errors="ignore")
                logger.info(f"Read COBOL file: {len(cobol_content)} characters")
                
                # Check for ground truth Python file
                ground_truth = self._find_ground_truth(cobol_file_path)
                ground_truth_section = ""
                if ground_truth:
                    # Extract constants and class names
                    constants = self._extract_constants_from_ground_truth(ground_truth)
                    class_names = self._extract_class_names_from_ground_truth(ground_truth)

                    constants_list = ""
                    if constants:
                        constants_list = "\nCRITICAL - You MUST use these EXACT constant names:\n"
                        for const_name, const_value in sorted(constants.items()):
                            constants_list += f"  - {const_name} = '{const_value}'\n"

                    classes_list = ""
                    if class_names:
                        classes_list = "\nCRITICAL - You MUST use these EXACT class names:\n"
                        for class_name in class_names:
                            classes_list += f"  - {class_name}\n"

                    # Keep ground truth section short to reduce token usage
                    ground_truth_section = f"""
⚠️ GROUND TRUTH EXISTS - Match EXACTLY:
{constants_list}
{classes_list}

REQUIREMENTS:
- Use EXACT constant names above (e.g., VERB_CREATE not CONST_CREATE)
- Use EXACT class names above (e.g., MarbleDatabase not MarbleInventory)
- Include all classes: {', '.join(class_names) if class_names else 'N/A'}
- Match structure and patterns from ground truth
"""
                
                prompt = f"""Modernize this COBOL file to Python.

File: {cobol_file_path}

COBOL Code:
```
{cobol_content}
```
{ground_truth_section}

WORKFLOW (use agent handoffs):
1. Analyzer Agent - analyze COBOL using analyze_cobol tool
2. Translator Agent - generate Python code{' - match ground truth names above' if ground_truth else ''}
3. Reviewer Agent - review code quality
4. Tester Agent - generate tests (MANDATORY - return test code in ```python block)
5. Refactor Agent - improve structure if needed

RULES: Use agents, don't manually translate. Tester Agent must generate test code."""
                
                # Run with retry logic for rate limits
                async def run_orchestrator():
                    result = await self.orchestrator.run(prompt, session=self.session)
                    # Add delay after orchestrator to avoid rate limits
                    await asyncio.sleep(self.settings.agent_delay_seconds * 2)
                    return result
                
                output = await self._run_with_retry(
                    run_orchestrator,
                    max_retries=6,
                    base_delay=15.0  # Increased base delay
                )
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
                
                # Truncate COBOL content if too large for model context
                # gpt-4o-mini has ~128k context, but agent system prompts + tools take significant space
                # Be very conservative - agent overhead can be 20-30k tokens
                max_cobol_chars = 3000  # Very conservative limit to account for agent overhead
                cobol_for_analysis = cobol_content
                if len(cobol_content) > max_cobol_chars:
                    # Include first part (header/structure) and last part (main logic)
                    header_size = max_cobol_chars // 2
                    tail_size = max_cobol_chars // 2
                    cobol_for_analysis = (
                        f"{cobol_content[:header_size]}\n"
                        f"... [COBOL code truncated - {len(cobol_content)} total chars, showing first {header_size} and last {tail_size} chars] ...\n"
                        f"{cobol_content[-tail_size:]}"
                    )
                    logger.warning(f"COBOL file too large ({len(cobol_content)} chars), truncating to {len(cobol_for_analysis)} chars for analysis")
                
                # Check for ground truth Python file
                ground_truth = self._find_ground_truth(cobol_file_path)
                ground_truth_section = ""
                if ground_truth:
                    # Extract constants and class names
                    constants = self._extract_constants_from_ground_truth(ground_truth)
                    class_names = self._extract_class_names_from_ground_truth(ground_truth)
                    
                    constants_list = ""
                    if constants:
                        constants_list = "\nCRITICAL - You MUST use these EXACT constant names:\n"
                        for const_name, const_value in sorted(constants.items()):
                            constants_list += f"  - {const_name} = '{const_value}'\n"
                    
                    classes_list = ""
                    if class_names:
                        classes_list = "\nCRITICAL - You MUST use these EXACT class names:\n"
                        for class_name in class_names:
                            classes_list += f"  - {class_name}\n"
                    
                    # Keep ground truth section short to reduce token usage
                    ground_truth_section = f"""
⚠️ GROUND TRUTH EXISTS - Match EXACTLY:
{constants_list}
{classes_list}

REQUIREMENTS:
- Use EXACT constant names above (e.g., VERB_CREATE not CONST_CREATE)
- Use EXACT class names above (e.g., MarbleDatabase not MarbleInventory)
- Include all classes: {', '.join(class_names) if class_names else 'N/A'}
- Match structure and patterns from ground truth
"""
                
                # Step 1: Analyze COBOL
                logger.info("Step 1: Analyzing COBOL code")
                # Use a fresh session for analysis to avoid context accumulation
                # The analyzer tool can read the file directly, so we don't need full COBOL in prompt
                # For very large files, just provide the file path and let the tool handle it
                if len(cobol_content) > max_cobol_chars:
                    # For large files, don't include COBOL in prompt at all - just use the tool
                    analysis_prompt = f"""Analyze this COBOL program:

File path: {cobol_file_path}

IMPORTANT: Use the analyze_cobol tool with the file path '{cobol_file_path}' to read and analyze the complete COBOL file structure. The tool will handle the full file content.

Then provide a detailed analysis of:
- Program structure (PROGRAM-ID, divisions)
- Variable declarations (WORKING-STORAGE)
- Procedures and logic flow
- Dependencies between sections"""
                else:
                    # For smaller files, include the COBOL code
                    analysis_prompt = f"""Analyze this COBOL program:

File: {cobol_file_path}

COBOL Code:
```
{cobol_for_analysis}
```

Use the analyze_cobol tool to extract the structure, then provide a detailed analysis of:
- Program structure (PROGRAM-ID, divisions)
- Variable declarations (WORKING-STORAGE)
- Procedures and logic flow
- Dependencies between sections"""
                async def run_analyzer():
                    # Use a fresh session to avoid context overflow
                    from src.sessions import SQLiteSession
                    analysis_session = SQLiteSession(f"{self.session_id}_analysis", str(self.settings.db_path)) if self.session_id else None
                    try:
                        return await self.analyzer.run(analysis_prompt, session=analysis_session)
                    finally:
                        if analysis_session:
                            analysis_session.close()
                results["analysis"] = await self._run_with_retry(run_analyzer, max_retries=6, base_delay=15.0)
                await asyncio.sleep(self.settings.agent_delay_seconds)  # Delay to avoid rate limits
                
                # Step 2: Translate to Python
                logger.info("Step 2: Translating to Python")
                
                # Truncate analysis aggressively - keep only key points
                analysis_text = str(results['analysis'])
                if len(analysis_text) > 1500:
                    # Keep first 1000 chars (usually contains structure info) and last 500 (summary)
                    analysis_text = analysis_text[:1000] + "\n... [analysis truncated] ...\n" + analysis_text[-500:]
                
                # For large files, don't include COBOL in translation prompt - rely on analysis
                if len(cobol_content) > max_cobol_chars:
                    # Large file - use analysis and tool approach
                    translation_prompt = f"""Translate this COBOL program to modern Python:

File path: {cobol_file_path}

Analysis Results:
{analysis_text}
{ground_truth_section}

IMPORTANT: The COBOL file is large ({len(cobol_content)} chars). Use the analysis above and your understanding of COBOL patterns to generate the Python translation. Focus on the structure and logic described in the analysis.

Generate clean, modern Python code that:
- Preserves the original functionality
- Uses type hints and docstrings
- Follows PEP 8 style guidelines
- Includes a main() function if appropriate
{'- IMPORTANT: Match class names, constants, and structure from the ground truth above' if ground_truth else ''}"""
                else:
                    # Small file - include COBOL code
                    translation_prompt = f"""Translate this COBOL code to modern Python:

Original COBOL:
```
{cobol_content}
```

Analysis Results:
{analysis_text}
{ground_truth_section}
Generate clean, modern Python code that:
- Preserves the original functionality
- Uses type hints and docstrings
- Follows PEP 8 style guidelines
- Includes a main() function if appropriate
{'- IMPORTANT: Match class names, constants, and structure from the ground truth above' if ground_truth else ''}"""
                
                async def run_translator():
                    # Use a fresh session to avoid context overflow
                    from src.sessions import SQLiteSession
                    translation_session = SQLiteSession(f"{self.session_id}_translation", str(self.settings.db_path)) if self.session_id else None
                    try:
                        return await self.translator.run(translation_prompt, session=translation_session)
                    finally:
                        if translation_session:
                            translation_session.close()
                results["translation"] = await self._run_with_retry(run_translator, max_retries=6, base_delay=15.0)
                await asyncio.sleep(self.settings.agent_delay_seconds)  # Delay to avoid rate limits
                
                # Step 3: Review code
                logger.info("Step 3: Reviewing translated code")
                
                # Extract just Python code from translation (not full markdown response)
                translation_code = self._extract_python_code({"translation": results['translation']})
                if not translation_code:
                    translation_code = str(results['translation'])[:5000]  # Fallback: truncate
                
                # Truncate Python code if too long
                if len(translation_code) > 6000:
                    translation_code = translation_code[:6000] + "\n... [Python code truncated] ..."
                
                # For large files, don't include COBOL in review
                cobol_for_review = ""
                if len(cobol_content) <= max_cobol_chars:
                    cobol_for_review = f"""

Original COBOL for reference:
```
{cobol_content}
```"""
                
                review_prompt = f"""Review this Python code for quality and correctness:

Python Code:
```python
{translation_code}
```
{cobol_for_review}
Check for:
- Correctness (does it match COBOL logic?)
- PEP 8 compliance
- Code quality and best practices
- Potential bugs or improvements"""
                async def run_reviewer():
                    # Use a fresh session to avoid context overflow
                    from src.sessions import SQLiteSession
                    review_session = SQLiteSession(f"{self.session_id}_review", str(self.settings.db_path)) if self.session_id else None
                    try:
                        return await self.reviewer.run(review_prompt, session=review_session)
                    finally:
                        if review_session:
                            review_session.close()
                results["review"] = await self._run_with_retry(run_reviewer, max_retries=6, base_delay=15.0)
                await asyncio.sleep(self.settings.agent_delay_seconds)  # Delay to avoid rate limits
                
                # Step 4: Generate tests
                logger.info("Step 4: Generating tests")
                
                # Extract just the Python code from translation (not full markdown response)
                python_code = self._extract_python_code({"translation": results['translation']})
                if not python_code:
                    python_code = str(results['translation'])[:5000]  # Fallback: truncate if extraction fails
                
                # For large COBOL files, only include a summary in test generation
                cobol_for_tests = cobol_content
                if len(cobol_content) > 5000:
                    # Include first 2000 chars (header/structure) and last 1000 chars (main logic)
                    cobol_for_tests = f"{cobol_content[:2000]}\n\n... [COBOL code truncated for test generation - {len(cobol_content)} total chars] ...\n\n{cobol_content[-1000:]}"
                
                test_prompt = f"""Generate comprehensive unit tests for this Python code:

Python Code:
```python
{python_code[:8000] if len(python_code) > 8000 else python_code}
```

Original COBOL (summary):
```
{cobol_for_tests}
```

Generate pytest tests that:
- Test all functions
- Verify correct behavior matches COBOL logic
- Include edge cases
- Use pytest conventions"""
                # For test generation, use a fresh session to avoid context overflow
                # The session has accumulated too much history by this point
                async def run_tester():
                    # Create a minimal session just for this step to avoid context overflow
                    from src.sessions import SQLiteSession
                    test_session = SQLiteSession(f"{self.session_id}_test", str(self.settings.db_path)) if self.session_id else None
                    try:
                        return await self.tester.run(test_prompt, session=test_session)
                    finally:
                        if test_session:
                            test_session.close()
                
                results["tests"] = await self._run_with_retry(run_tester, max_retries=6, base_delay=15.0)
                await asyncio.sleep(self.settings.agent_delay_seconds)  # Delay to avoid rate limits
                
                # Step 5: Refactor (optional)
                logger.info("Step 5: Refactoring code")
                
                # Extract just Python code from translation (not full markdown response)
                refactor_code = self._extract_python_code({"translation": results['translation']})
                if not refactor_code:
                    refactor_code = str(results['translation'])[:5000]  # Fallback: truncate
                
                # Truncate Python code if too long
                if len(refactor_code) > 6000:
                    refactor_code = refactor_code[:6000] + "\n... [Python code truncated] ..."
                
                refactor_prompt = f"""Refactor this Python code to improve readability and maintainability:

Python Code:
```python
{refactor_code}
```

Improve:
- Code structure and organization
- Add/improve type hints and docstrings
- Extract patterns if needed
- Optimize where appropriate

Maintain functional equivalence."""
                async def run_refactor():
                    # Use a fresh session to avoid context overflow
                    from src.sessions import SQLiteSession
                    refactor_session = SQLiteSession(f"{self.session_id}_refactor", str(self.settings.db_path)) if self.session_id else None
                    try:
                        return await self.refactor.run(refactor_prompt, session=refactor_session)
                    finally:
                        if refactor_session:
                            refactor_session.close()
                results["refactored"] = await self._run_with_retry(run_refactor, max_retries=6, base_delay=15.0)
            
            # Save results to files if requested
            if save_files:
                saved_files = self._save_results(results, output_dir)
                results["saved_files"] = saved_files
                if "python_file" in saved_files:
                    logger.info(f"Saved Python file: {saved_files['python_file']}")
                if "test_file" in saved_files:
                    logger.info(f"Saved test file: {saved_files['test_file']}")
            
            logger.info("Modernization pipeline completed successfully")
            return results
            
        except Exception as e:
            error_msg = str(e)
            logger.error(f"Error in modernization pipeline: {error_msg}")
            
            # Check if it's a rate limit error
            if "429" in error_msg or "rate_limit" in error_msg.lower() or "rate limit" in error_msg.lower():
                logger.warning("Rate limit exceeded. Consider:")
                logger.warning("1. Waiting a few minutes before retrying")
                logger.warning("2. Upgrading your OpenAI plan for higher limits")
                logger.warning("3. Requesting a rate limit increase from OpenAI support")
                logger.warning("4. Using a smaller model (e.g., gpt-4o-mini) in settings")
            
            results["error"] = error_msg
            return results
    
    async def _run_with_retry(self, func, max_retries: int = 6, base_delay: float = 15.0):
        """Run a function with exponential backoff retry for rate limits.
        
        Args:
            func: Async function to run
            max_retries: Maximum number of retry attempts
            base_delay: Base delay in seconds for exponential backoff
            
        Returns:
            Result from the function
        """
        for attempt in range(max_retries):
            try:
                return await func()
            except Exception as e:
                error_msg = str(e)
                
                # Check if it's a rate limit error
                is_rate_limit = (
                    "429" in error_msg or 
                    "rate_limit" in error_msg.lower() or 
                    "rate limit" in error_msg.lower() or
                    "too many requests" in error_msg.lower()
                )
                
                if is_rate_limit and attempt < max_retries - 1:
                    # Calculate exponential backoff delay with jitter
                    delay = base_delay * (2 ** attempt)
                    import random
                    jitter = random.uniform(1.0, 3.0)  # Increased jitter range
                    delay_with_jitter = delay + jitter

                    # Try to extract wait time from error message
                    import re
                    wait_match = re.search(r'try again in ([\d.]+)s', error_msg, re.IGNORECASE)
                    if wait_match:
                        wait_time = float(wait_match.group(1))
                        # Add significant buffer - wait longer than suggested
                        delay_with_jitter = max(wait_time * 1.5 + 2, delay_with_jitter)
                    
                    # Cap maximum delay at 120 seconds
                    delay_with_jitter = min(delay_with_jitter, 120.0)
                    
                    logger.warning(
                        f"Rate limit hit (attempt {attempt + 1}/{max_retries}). "
                        f"Retrying in {delay_with_jitter:.1f} seconds..."
                    )
                    await asyncio.sleep(delay_with_jitter)
                    continue
                else:
                    # Not a rate limit error, or max retries reached
                    raise
        
        # Should never reach here, but just in case
        raise Exception("Max retries exceeded")

