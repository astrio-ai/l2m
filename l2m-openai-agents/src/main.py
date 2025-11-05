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
    
    print("\n✅ Analysis Complete")
    print(f"✅ Translation Complete")
    print(f"✅ Review Complete")
    print(f"✅ Tests Generated")
    print(f"✅ Refactoring Complete")
    
    print("\n" + "="*70)


if __name__ == "__main__":
    asyncio.run(main())

