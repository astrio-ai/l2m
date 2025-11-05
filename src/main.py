"""
Entry point for CLI / API execution.
"""

import asyncio
import sys
from pathlib import Path
from src.config import get_settings
from src.workflows.modernization_pipeline import ModernizationPipeline
from src.utils.logger import get_logger

logger = get_logger(__name__)


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
    
    # Print translation results
    if results.get("translation"):
        print("\n" + "-"*70)
        print("🐍 TRANSLATION (Python Code)")
        print("-"*70)
        translation_text = str(results["translation"])
        print(translation_text[:2000] + "..." if len(translation_text) > 2000 else translation_text)
    
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
    
    # Print refactored code
    if results.get("refactored"):
        print("\n" + "-"*70)
        print("🔧 REFACTORED CODE")
        print("-"*70)
        refactored_text = str(results["refactored"])
        print(refactored_text[:2000] + "..." if len(refactored_text) > 2000 else refactored_text)
    
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

