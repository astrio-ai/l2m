"""
Example: Running the modernization pipeline.

Demonstrates how to use the ModernizationPipeline to convert COBOL to Python.
"""

import asyncio
from pathlib import Path
from src.workflows.modernization_pipeline import ModernizationPipeline
from src.config import get_settings


async def main():
    """Run the modernization pipeline example."""
    # Initialize settings
    settings = get_settings()
    
    # Example COBOL file path
    cobol_file = Path(__file__).parent.parent / "data" / "samples" / "sample1.cbl"
    
    if not cobol_file.exists():
        print(f"Error: Sample file not found at {cobol_file}")
        print("Please create a sample COBOL file first.")
        return
    
    # Create pipeline with session
    pipeline = ModernizationPipeline(session_id="example_session")
    
    # Run modernization
    print(f"Modernizing {cobol_file}...")
    results = await pipeline.run(str(cobol_file))
    
    # Print results
    print("\n" + "="*70)
    print("Modernization Results")
    print("="*70)
    
    for key, value in results.items():
        if key != "error" and value:
            print(f"\n{key.upper()}:")
            print("-" * 70)
            print(str(value)[:500] + "..." if len(str(value)) > 500 else str(value))
    
    if "error" in results:
        print(f"\n❌ Error: {results['error']}")


if __name__ == "__main__":
    asyncio.run(main())

