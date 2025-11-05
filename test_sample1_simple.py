"""
Simple test script to run modernization pipeline on sample1.cbl
Works with Python 3.10+
"""

import asyncio
import sys
from pathlib import Path

# Add src to path
project_root = Path(__file__).parent
sys.path.insert(0, str(project_root))

async def main():
    """Test the modernization pipeline with sample1.cbl"""
    try:
        from src.workflows.modernization_pipeline import ModernizationPipeline
        from src.config import get_settings
    except ImportError as e:
        print(f"❌ Import error: {e}")
        print("\n💡 Make sure you:")
        print("   1. Have Python 3.10+ installed")
        print("   2. Have activated your virtual environment")
        print("   3. Have installed requirements: pip install -r requirements.txt")
        return
    
    # Get the sample file path
    sample_file = project_root / "data" / "samples" / "sample1.cbl"
    
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
        
        # Check API key
        if not settings.openai_api_key or settings.openai_api_key == "your-api-key-here":
            print("⚠️  Warning: OPENAI_API_KEY not set or using placeholder")
            print("   Please set OPENAI_API_KEY in .env file or environment")
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
    print("   This will call OpenAI API - make sure your API key is set!")
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
            analysis = results["analysis"]
            print(analysis[:500] + "..." if len(analysis) > 500 else analysis)
            print()
        
        if results.get("translation"):
            print("🐍 Translation:")
            print("-" * 70)
            translation = results["translation"]
            print(translation[:1000] + "..." if len(translation) > 1000 else translation)
            print()
        
        if results.get("review"):
            print("✅ Review:")
            print("-" * 70)
            review = results["review"]
            print(review[:500] + "..." if len(review) > 500 else review)
            print()
        
        if results.get("tests"):
            print("🧪 Tests:")
            print("-" * 70)
            tests = results["tests"]
            print(tests[:500] + "..." if len(tests) > 500 else tests)
            print()
        
        if results.get("refactored"):
            print("🔧 Refactored:")
            print("-" * 70)
            refactored = results["refactored"]
            print(refactored[:500] + "..." if len(refactored) > 500 else refactored)
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

