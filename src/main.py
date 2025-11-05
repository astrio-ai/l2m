"""
Entry point for CLI / API execution.
"""

import asyncio
import sys
import re
from pathlib import Path
from src.config import get_settings
from src.workflows.modernization_pipeline import ModernizationPipeline
from src.utils.logger import get_logger

logger = get_logger(__name__)


def extract_python_code(text: str) -> str:
    """Extract Python code from text, handling markdown code blocks."""
    if not text:
        return ""
    
    # Check for markdown code blocks
    python_block_match = re.search(r'```python\s*\n(.*?)```', text, re.DOTALL)
    if python_block_match:
        return python_block_match.group(1).strip()
    
    # Check for generic code blocks
    code_block_match = re.search(r'```\s*\n(.*?)```', text, re.DOTALL)
    if code_block_match:
        return code_block_match.group(1).strip()
    
    # If no code blocks, return the text as-is (might already be Python code)
    return text.strip()


def save_python_file(output_dir: Path, cobol_file_path: str, python_code: str) -> Path:
    """Save Python code to a file in the output directory.
    
    Args:
        output_dir: Directory to save the file
        cobol_file_path: Path to the original COBOL file
        python_code: Python code to save
        
    Returns:
        Path to the saved Python file
    """
    # Create output directory if it doesn't exist
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Generate output filename based on COBOL filename
    cobol_path = Path(cobol_file_path)
    output_filename = cobol_path.stem + ".py"
    output_file = output_dir / output_filename
    
    # Write Python code to file
    output_file.write_text(python_code, encoding="utf-8")
    
    return output_file


async def main():
    """Main entry point for CLI."""
    if len(sys.argv) < 2:
        print("Usage: python -m src.main <cobol_file_path>")
        sys.exit(1)
    
    cobol_file = sys.argv[1]
    
    if not Path(cobol_file).exists():
        print(f"Error: File not found: {cobol_file}")
        sys.exit(1)
    
    settings = get_settings()
    logger.info(f"Starting modernization for {cobol_file}")
    
    pipeline = ModernizationPipeline()
    results = await pipeline.run(cobol_file)
    
    print("\n" + "="*70)
    print("Modernization Results")
    print("="*70)
    print(f"\nCOBOL File: {results.get('cobol_file')}")
    
    if "error" in results:
        print(f"\n❌ Error: {results['error']}")
        sys.exit(1)
    
    # Print analysis results
    if results.get("analysis"):
        print("\n" + "-"*70)
        print("📊 ANALYSIS")
        print("-"*70)
        analysis_text = str(results["analysis"])
        print(analysis_text[:1000] + "..." if len(analysis_text) > 1000 else analysis_text)
    
    # Print translation results and save to file
    if results.get("translation"):
        print("\n" + "-"*70)
        print("🐍 TRANSLATION (Python Code)")
        print("-"*70)
        translation_text = str(results["translation"])
        print(translation_text[:2000] + "..." if len(translation_text) > 2000 else translation_text)
        
        # Extract and save Python code
        python_code = extract_python_code(translation_text)
        if python_code:
            output_file = save_python_file(settings.output_path, cobol_file, python_code)
            print(f"\n💾 Saved Python code to: {output_file}")
    elif results.get("orchestrator_output"):
        # Try to extract Python code from orchestrator output if translation not available
        orchestrator_text = str(results["orchestrator_output"])
        python_code = extract_python_code(orchestrator_text)
        if python_code and "def " in python_code or "print(" in python_code:
            output_file = save_python_file(settings.output_path, cobol_file, python_code)
            print(f"\n💾 Saved Python code to: {output_file}")
    
    # Print review results
    if results.get("review"):
        print("\n" + "-"*70)
        print("📝 REVIEW")
        print("-"*70)
        review_text = str(results["review"])
        print(review_text[:1000] + "..." if len(review_text) > 1000 else review_text)
    
    # Print test results
    if results.get("tests"):
        print("\n" + "-"*70)
        print("🧪 TESTS")
        print("-"*70)
        tests_text = str(results["tests"])
        print(tests_text[:1000] + "..." if len(tests_text) > 1000 else tests_text)
    
    # Print refactored code and save to file (prefer refactored over translation)
    if results.get("refactored"):
        print("\n" + "-"*70)
        print("🔧 REFACTORED CODE")
        print("-"*70)
        refactored_text = str(results["refactored"])
        print(refactored_text[:2000] + "..." if len(refactored_text) > 2000 else refactored_text)
        
        # Extract and save refactored Python code
        python_code = extract_python_code(refactored_text)
        if python_code:
            output_file = save_python_file(settings.output_path, cobol_file, python_code)
            print(f"\n💾 Saved refactored Python code to: {output_file}")
    
    # Print orchestrator output if using handoffs
    if results.get("orchestrator_output"):
        print("\n" + "-"*70)
        print("🎯 ORCHESTRATOR OUTPUT")
        print("-"*70)
        orchestrator_text = str(results["orchestrator_output"])
        print(orchestrator_text[:2000] + "..." if len(orchestrator_text) > 2000 else orchestrator_text)
    
    print("\n" + "="*70)
    print("✅ Modernization Complete!")
    print("="*70)


if __name__ == "__main__":
    asyncio.run(main())

