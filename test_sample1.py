"""
Test script to run modernization pipeline on sample1.cbl
"""

import asyncio
import sys
from pathlib import Path
from src.workflows.modernization_pipeline import ModernizationPipeline
from src.config import get_settings

# Ensure we can import from src
sys.path.insert(0, str(Path(__file__).parent))


async def main():
    """Test the modernization pipeline with sample1.cbl"""
    # Get the sample file path
    sample_file = Path(__file__).parent / "data" / "samples" / "sample1.cbl"
    
    if not sample_file.exists():
        print(f"❌ Error: Sample file not found at {sample_file}")
        return
    
    print(f"📄 Testing with: {sample_file}")
    print(f"📄 File content:")
    print("-" * 70)
    print(sample_file.read_text())
    print("-" * 70)
    print()
    
    # Check settings
    try:
        settings = get_settings()
        print(f"✅ Settings loaded")
        print(f"   Model: {settings.openai_model}")
        print(f"   Temperature: {settings.openai_temperature}")
        print()
    except Exception as e:
        print(f"⚠️  Warning: Could not load settings: {e}")
        print("   Make sure .env file exists with OPENAI_API_KEY")
        print()
    
    # Create pipeline
    print("🚀 Creating modernization pipeline...")
    try:
        # Use sequential mode for testing (more predictable)
        pipeline = ModernizationPipeline(session_id="test_sample1", use_handoffs=False)
        print("✅ Pipeline created")
        print()
    except Exception as e:
        print(f"❌ Error creating pipeline: {e}")
        import traceback
        traceback.print_exc()
        return
    
    # Run the pipeline
    print("🔄 Running modernization pipeline...")
    print()
    try:
        results = await pipeline.run(str(sample_file))
        
        print("=" * 70)
        print("📊 RESULTS")
        print("=" * 70)
        print()
        
        if "error" in results:
            print(f"❌ Error occurred: {results['error']}")
            return
        
        # Display results
        if results.get("analysis"):
            print("📋 Analysis:")
            print("-" * 70)
            print(results["analysis"][:500] + "..." if len(results["analysis"]) > 500 else results["analysis"])
            print()
        
        if results.get("translation"):
            print("🐍 Translation:")
            print("-" * 70)
            print(results["translation"][:1000] + "..." if len(results["translation"]) > 1000 else results["translation"])
            print()
        
        if results.get("review"):
            print("✅ Review:")
            print("-" * 70)
            print(results["review"][:500] + "..." if len(results["review"]) > 500 else results["review"])
            print()
        
        if results.get("tests"):
            print("🧪 Tests:")
            print("-" * 70)
            print(results["tests"][:500] + "..." if len(results["tests"]) > 500 else results["tests"])
            print()
        
        if results.get("refactored"):
            print("🔧 Refactored:")
            print("-" * 70)
            print(results["refactored"][:500] + "..." if len(results["refactored"]) > 500 else results["refactored"])
            print()
        
        print("=" * 70)
        print("✅ Modernization pipeline completed!")
        print("=" * 70)
        
    except Exception as e:
        print(f"❌ Error running pipeline: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    asyncio.run(main())

