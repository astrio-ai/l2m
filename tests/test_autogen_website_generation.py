#!/usr/bin/env python3
"""
Test script for AutoGen Sandbox Agent Website Generation

This script demonstrates how to use the AutoGen sandbox agent to automatically
generate, build, and run websites in the isolated sandbox environment.
"""

import asyncio
import sys
from pathlib import Path

# Add the parent directory to the path to import engine modules
sys.path.append(str(Path(__file__).parent.parent))

from engine.agents.autogen_integration import (
    WebsiteSpec, 
    GenerationResult, 
    AutoGenSandboxAgent,
    create_autogen_sandbox_agent,
    generate_website_async
)

class MockLLMClient:
    """Mock LLM client for testing purposes."""
    
    async def chat(self, prompt: str) -> str:
        """Mock chat response."""
        print(f"🤖 LLM Prompt: {prompt[:100]}...")
        
        # Return some common fixes based on the error
        if "npm install" in prompt and "error" in prompt.lower():
            return "npm cache clean --force && npm install"
        elif "build" in prompt and "error" in prompt.lower():
            return "npm run build --verbose"
        elif "server" in prompt and "error" in prompt.lower():
            return "npm start -- --port 3000 --host 0.0.0.0"
        else:
            return "echo 'Fix applied'"
    
    async def generate(self, prompt: str) -> str:
        """Mock generate response."""
        return await self.chat(prompt)

async def test_website_generation():
    """Test website generation with different frameworks."""
    
    print("🚀 AutoGen Sandbox Agent Website Generation Test")
    print("=" * 60)
    
    # Create a mock LLM client
    llm_client = MockLLMClient()
    
    # Test cases
    test_cases = [
        WebsiteSpec(
            name="my-react-app",
            description="A modern React application with beautiful UI",
            framework="react",
            features=["Responsive Design", "Modern UI", "Fast Performance"],
            port=3000
        ),
        WebsiteSpec(
            name="my-next-app",
            description="A Next.js application with TypeScript and Tailwind",
            framework="next",
            features=["TypeScript", "Tailwind CSS", "SEO Optimized"],
            port=3001
        ),
        WebsiteSpec(
            name="my-vue-app",
            description="A Vue.js application with component-based architecture",
            framework="vue",
            features=["Vue 3", "Composition API", "Vue Router"],
            port=3002
        ),
        WebsiteSpec(
            name="my-vanilla-app",
            description="A simple vanilla HTML/CSS/JS website",
            framework="vanilla",
            features=["Pure HTML", "CSS Grid", "Vanilla JavaScript"],
            port=3003
        )
    ]
    
    # Create the AutoGen sandbox agent
    agent = create_autogen_sandbox_agent("website-generator", llm_client=llm_client)
    
    try:
        for i, spec in enumerate(test_cases, 1):
            print(f"\n🧪 Test {i}: Generating {spec.framework} website")
            print(f"   Name: {spec.name}")
            print(f"   Description: {spec.description}")
            print(f"   Features: {', '.join(spec.features)}")
            print("-" * 40)
            
            # Generate the website
            result = await agent.generate_website(spec, max_retries=2)
            
            if result.success:
                print(f"✅ Success! Website deployed at: {result.website_url}")
                print(f"   Files created: {len(result.files_created or [])}")
            else:
                print(f"❌ Failed: {result.error_message}")
            
            print()
    
    finally:
        # Clean up
        agent.cleanup()
    
    # Show summary
    print("📊 Generation Summary")
    print("=" * 40)
    websites = agent.list_websites()
    for website_name in websites:
        status = agent.get_website_status(website_name)
        if status:
            print(f"   {website_name}: {'✅ Success' if status.success else '❌ Failed'}")
            if status.website_url:
                print(f"      URL: {status.website_url}")

async def test_single_website():
    """Test generating a single website with detailed output."""
    
    print("\n🎯 Single Website Generation Test")
    print("=" * 50)
    
    # Create a mock LLM client
    llm_client = MockLLMClient()
    
    # Create a React website spec
    spec = WebsiteSpec(
        name="demo-react-app",
        description="A demo React application showcasing modern web development",
        framework="react",
        features=["React Hooks", "Modern JavaScript", "Responsive Design", "Fast Loading"],
        port=3000
    )
    
    print(f"📋 Website Specification:")
    print(f"   Name: {spec.name}")
    print(f"   Framework: {spec.framework}")
    print(f"   Description: {spec.description}")
    print(f"   Features: {', '.join(spec.features)}")
    print(f"   Port: {spec.port}")
    print()
    
    # Generate the website
    result = await generate_website_async(spec, llm_client=llm_client)
    
    print("📊 Generation Result:")
    print(f"   Success: {result.success}")
    if result.website_url:
        print(f"   URL: {result.website_url}")
    if result.error_message:
        print(f"   Error: {result.error_message}")
    if result.logs:
        print(f"   Logs: {len(result.logs)} entries")
    if result.files_created:
        print(f"   Files: {len(result.files_created)} created")

async def main():
    """Main test function."""
    try:
        # Test multiple website generation
        await test_website_generation()
        
        # Test single website generation
        await test_single_website()
        
        print("\n🎉 All tests completed!")
        
    except Exception as e:
        print(f"❌ Test failed with error: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    asyncio.run(main()) 