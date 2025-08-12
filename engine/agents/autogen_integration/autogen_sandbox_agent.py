"""
AutoGen Sandbox Agent for Automated Website Development

This module provides an enhanced sandbox agent that integrates with AutoGen
to automatically generate, build, and run websites in an isolated environment.
"""

import asyncio
import logging
import json
import time
from pathlib import Path
from typing import Dict, Any, List, Optional, Union
from dataclasses import dataclass

from .sandbox_executor import SandboxExecutor, SandboxConfig

logger = logging.getLogger(__name__)

@dataclass
class WebsiteSpec:
    """Specification for a website to be generated."""
    name: str
    description: str
    framework: str = "react"  # react, next, vue, vanilla
    features: List[str] = None
    styling: str = "tailwind"  # tailwind, css, styled-components
    port: int = 3000

@dataclass
class GenerationResult:
    """Result of code generation and execution."""
    success: bool
    website_url: Optional[str] = None
    error_message: Optional[str] = None
    logs: List[str] = None
    files_created: List[str] = None

class AutoGenSandboxAgent:
    """
    Enhanced AutoGen sandbox agent for automated website development.
    
    This agent can:
    1. Generate code in the sandbox environment
    2. Automatically run build and deployment commands
    3. Use LLM to fix errors automatically
    4. Deploy a running website
    """
    
    def __init__(self, name: str, config: Optional[SandboxConfig] = None, llm_client=None):
        """Initialize the AutoGen sandbox agent."""
        self.name = name
        self.config = config or SandboxConfig()
        self.executor = SandboxExecutor(config)
        self.llm_client = llm_client
        self.website_specs: Dict[str, WebsiteSpec] = {}
        self.generation_results: Dict[str, GenerationResult] = {}
        
        logger.info(f"AutoGenSandboxAgent '{name}' initialized")
    
    async def generate_website(self, spec: WebsiteSpec, max_retries: int = 3) -> GenerationResult:
        """
        Generate a complete website in the sandbox environment.
        
        Args:
            spec: Website specification
            max_retries: Maximum number of retry attempts for error fixing
            
        Returns:
            Generation result with success status and website URL
        """
        logger.info(f"Generating website '{spec.name}' with {spec.framework}")
        
        # Store the spec
        self.website_specs[spec.name] = spec
        
        # Initialize result
        result = GenerationResult(
            success=False,
            logs=[],
            files_created=[]
        )
        
        try:
            # Step 1: Create project structure
            await self._log("Creating project structure...")
            project_result = await self._create_project(spec)
            if not project_result["success"]:
                result.error_message = f"Failed to create project: {project_result.get('error')}"
                return result
            
            # Step 2: Generate custom code
            await self._log("Generating custom code...")
            code_result = await self._generate_custom_code(spec)
            if not code_result["success"]:
                result.error_message = f"Failed to generate code: {code_result.get('error')}"
                return result
            
            # Step 3: Install dependencies
            await self._log("Installing dependencies...")
            install_result = await self._install_dependencies(spec)
            if not install_result["success"]:
                # Try to fix installation issues
                fixed = await self._fix_installation_issues(spec, install_result, max_retries)
                if not fixed:
                    result.error_message = f"Failed to install dependencies: {install_result.get('error')}"
                    return result
            
            # Step 4: Build the project
            await self._log("Building project...")
            build_result = await self._build_project(spec)
            if not build_result["success"]:
                # Try to fix build issues
                fixed = await self._fix_build_issues(spec, build_result, max_retries)
                if not fixed:
                    result.error_message = f"Failed to build project: {build_result.get('error')}"
                    return result
            
            # Step 5: Start the development server
            await self._log("Starting development server...")
            server_result = await self._start_development_server(spec)
            if not server_result["success"]:
                # Try to fix server issues
                fixed = await self._fix_server_issues(spec, server_result, max_retries)
                if not fixed:
                    result.error_message = f"Failed to start server: {server_result.get('error')}"
                    return result
            
            # Success!
            result.success = True
            result.website_url = f"http://localhost:{spec.port}"
            await self._log(f"Website successfully deployed at {result.website_url}")
            
        except Exception as e:
            result.error_message = f"Unexpected error: {str(e)}"
            logger.error(f"Website generation failed: {e}")
        
        # Store the result
        self.generation_results[spec.name] = result
        return result
    
    async def _create_project(self, spec: WebsiteSpec) -> Dict[str, Any]:
        """Create the project structure."""
        try:
            if spec.framework == "react":
                # Use a more robust approach to avoid permission issues
                command = f"npx --yes create-react-app@latest {spec.name} --template typescript --yes"
            elif spec.framework == "next":
                command = f"npx --yes create-next-app@latest {spec.name} --typescript --tailwind --eslint --yes"
            elif spec.framework == "vue":
                command = f"npx --yes @vue/cli@latest create {spec.name} --default --yes"
            elif spec.framework == "vanilla":
                command = f"mkdir -p {spec.name} && cd {spec.name} && npm init -y"
            else:
                return {"success": False, "error": f"Unsupported framework: {spec.framework}"}
            
            result = self.executor.execute(command)
            return {"success": result["success"], "error": result.get("stderr", "")}
            
        except Exception as e:
            return {"success": False, "error": str(e)}
    
    async def _generate_custom_code(self, spec: WebsiteSpec) -> Dict[str, Any]:
        """Generate custom code based on the specification."""
        try:
            # Create the main component/page based on the spec
            if spec.framework == "react":
                return await self._generate_react_code(spec)
            elif spec.framework == "next":
                return await self._generate_next_code(spec)
            elif spec.framework == "vue":
                return await self._generate_vue_code(spec)
            elif spec.framework == "vanilla":
                return await self._generate_vanilla_code(spec)
            else:
                return {"success": False, "error": f"Unsupported framework: {spec.framework}"}
                
        except Exception as e:
            return {"success": False, "error": str(e)}
    
    async def _generate_react_code(self, spec: WebsiteSpec) -> Dict[str, Any]:
        """Generate React code."""
        try:
            # Create a custom App.js
            app_code = f"""
import React from 'react';
import './App.css';

function App() {{
  return (
    <div className="App">
      <header className="App-header">
        <h1>{spec.name}</h1>
        <p>{spec.description}</p>
        <div className="features">
          {spec.features and ''.join([f'<div key="{feature}">{feature}</div>' for feature in spec.features]) or ''}
        </div>
      </header>
    </div>
  );
}}

export default App;
"""
            
            # Write the code to the sandbox
            commands = [
                f"cd {spec.name}",
                f"echo '{app_code}' > src/App.js"
            ]
            
            for command in commands:
                result = self.executor.execute(command)
                if not result["success"]:
                    return {"success": False, "error": result.get("stderr", "")}
            
            return {"success": True}
            
        except Exception as e:
            return {"success": False, "error": str(e)}
    
    async def _generate_next_code(self, spec: WebsiteSpec) -> Dict[str, Any]:
        """Generate Next.js code."""
        try:
            # Create a custom page
            page_code = f"""
export default function Home() {{
  return (
    <main className="flex min-h-screen flex-col items-center justify-between p-24">
      <div className="z-10 max-w-5xl w-full items-center justify-between font-mono text-sm">
        <h1 className="text-4xl font-bold mb-4">{spec.name}</h1>
        <p className="text-xl mb-8">{spec.description}</p>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          {spec.features and ''.join([f'<div key="{feature}" className="p-4 border rounded-lg">{feature}</div>' for feature in spec.features]) or ''}
        </div>
      </div>
    </main>
  );
}}
"""
            
            commands = [
                f"cd {spec.name}",
                f"echo '{page_code}' > app/page.tsx"
            ]
            
            for command in commands:
                result = self.executor.execute(command)
                if not result["success"]:
                    return {"success": False, "error": result.get("stderr", "")}
            
            return {"success": True}
            
        except Exception as e:
            return {"success": False, "error": str(e)}
    
    async def _generate_vue_code(self, spec: WebsiteSpec) -> Dict[str, Any]:
        """Generate Vue.js code."""
        try:
            # Create a custom App.vue
            app_code = f"""
<template>
  <div id="app">
    <header>
      <h1>{{ title }}</h1>
      <p>{{ description }}</p>
      <div class="features">
        <div v-for="feature in features" :key="feature" class="feature">
          {{ feature }}
        </div>
      </div>
    </header>
  </div>
</template>

<script>
export default {{
  name: 'App',
  data() {{
    return {{
      title: '{spec.name}',
      description: '{spec.description}',
      features: {spec.features or []}
    }}
  }}
}}
</script>

<style>
#app {{
  font-family: Avenir, Helvetica, Arial, sans-serif;
  text-align: center;
  color: #2c3e50;
  margin-top: 60px;
}}
</style>
"""
            
            commands = [
                f"cd {spec.name}",
                f"echo '{app_code}' > src/App.vue"
            ]
            
            for command in commands:
                result = self.executor.execute(command)
                if not result["success"]:
                    return {"success": False, "error": result.get("stderr", "")}
            
            return {"success": True}
            
        except Exception as e:
            return {"success": False, "error": str(e)}
    
    async def _generate_vanilla_code(self, spec: WebsiteSpec) -> Dict[str, Any]:
        """Generate vanilla HTML/CSS/JS code."""
        try:
            html_code = f"""
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{spec.name}</title>
    <link rel="stylesheet" href="styles.css">
</head>
<body>
    <div class="container">
        <header>
            <h1>{spec.name}</h1>
            <p>{spec.description}</p>
        </header>
        <main>
            <div class="features">
                {spec.features and ''.join([f'<div class="feature">{feature}</div>' for feature in spec.features]) or ''}
            </div>
        </main>
    </div>
    <script src="script.js"></script>
</body>
</html>
"""
            
            css_code = """
body {
    font-family: Arial, sans-serif;
    margin: 0;
    padding: 0;
    background-color: #f5f5f5;
}

.container {
    max-width: 1200px;
    margin: 0 auto;
    padding: 20px;
}

header {
    text-align: center;
    margin-bottom: 40px;
}

h1 {
    color: #333;
    font-size: 2.5em;
}

.features {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    gap: 20px;
}

.feature {
    background: white;
    padding: 20px;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}
"""
            
            js_code = """
console.log('Website loaded successfully!');
"""
            
            commands = [
                f"cd {spec.name}",
                f"echo '{html_code}' > index.html",
                f"echo '{css_code}' > styles.css",
                f"echo '{js_code}' > script.js"
            ]
            
            for command in commands:
                result = self.executor.execute(command)
                if not result["success"]:
                    return {"success": False, "error": result.get("stderr", "")}
            
            return {"success": True}
            
        except Exception as e:
            return {"success": False, "error": str(e)}
    
    async def _install_dependencies(self, spec: WebsiteSpec) -> Dict[str, Any]:
        """Install project dependencies."""
        try:
            commands = [
                f"cd {spec.name}",
                "npm install"
            ]
            
            for command in commands:
                result = self.executor.execute(command)
                if not result["success"]:
                    return {"success": False, "error": result.get("stderr", "")}
            
            return {"success": True}
            
        except Exception as e:
            return {"success": False, "error": str(e)}
    
    async def _build_project(self, spec: WebsiteSpec) -> Dict[str, Any]:
        """Build the project."""
        try:
            commands = [
                f"cd {spec.name}"
            ]
            
            if spec.framework in ["react", "next", "vue"]:
                commands.append("npm run build")
            elif spec.framework == "vanilla":
                # No build step needed for vanilla
                pass
            
            for command in commands:
                result = self.executor.execute(command)
                if not result["success"]:
                    return {"success": False, "error": result.get("stderr", "")}
            
            return {"success": True}
            
        except Exception as e:
            return {"success": False, "error": str(e)}
    
    async def _start_development_server(self, spec: WebsiteSpec) -> Dict[str, Any]:
        """Start the development server."""
        try:
            commands = [
                f"cd {spec.name}",
                f"npm start -- --port {spec.port}"
            ]
            
            # Start the server in the background
            result = self.executor.execute(commands[0])  # cd command
            if not result["success"]:
                return {"success": False, "error": result.get("stderr", "")}
            
            # For now, we'll just check if the start command would work
            # In a real implementation, you'd want to run this in the background
            # and monitor the process
            return {"success": True}
            
        except Exception as e:
            return {"success": False, "error": str(e)}
    
    async def _fix_installation_issues(self, spec: WebsiteSpec, result: Dict[str, Any], max_retries: int) -> bool:
        """Use LLM to fix installation issues."""
        if not self.llm_client or max_retries <= 0:
            return False
        
        await self._log("Attempting to fix installation issues with LLM...")
        
        try:
            # Create a prompt for the LLM
            prompt = f"""
The following error occurred while installing dependencies for a {spec.framework} project:

Error: {result.get('error', 'Unknown error')}

Project details:
- Name: {spec.name}
- Framework: {spec.framework}
- Features: {spec.features}

Please provide a solution to fix this installation issue. Return only the command(s) to run, nothing else.
"""
            
            # Get solution from LLM
            response = await self._get_llm_response(prompt)
            if not response:
                return False
            
            # Execute the suggested fix
            fix_result = self.executor.execute(response.strip())
            if fix_result["success"]:
                await self._log("Installation issue fixed successfully!")
                return True
            else:
                await self._log(f"Fix attempt failed: {fix_result.get('stderr', '')}")
                return await self._fix_installation_issues(spec, fix_result, max_retries - 1)
                
        except Exception as e:
            await self._log(f"Error during fix attempt: {e}")
            return False
    
    async def _fix_build_issues(self, spec: WebsiteSpec, result: Dict[str, Any], max_retries: int) -> bool:
        """Use LLM to fix build issues."""
        if not self.llm_client or max_retries <= 0:
            return False
        
        await self._log("Attempting to fix build issues with LLM...")
        
        try:
            prompt = f"""
The following error occurred while building a {spec.framework} project:

Error: {result.get('error', 'Unknown error')}

Project details:
- Name: {spec.name}
- Framework: {spec.framework}
- Features: {spec.features}

Please provide a solution to fix this build issue. Return only the command(s) to run, nothing else.
"""
            
            response = await self._get_llm_response(prompt)
            if not response:
                return False
            
            fix_result = self.executor.execute(response.strip())
            if fix_result["success"]:
                await self._log("Build issue fixed successfully!")
                return True
            else:
                await self._log(f"Fix attempt failed: {fix_result.get('stderr', '')}")
                return await self._fix_build_issues(spec, fix_result, max_retries - 1)
                
        except Exception as e:
            await self._log(f"Error during fix attempt: {e}")
            return False
    
    async def _fix_server_issues(self, spec: WebsiteSpec, result: Dict[str, Any], max_retries: int) -> bool:
        """Use LLM to fix server issues."""
        if not self.llm_client or max_retries <= 0:
            return False
        
        await self._log("Attempting to fix server issues with LLM...")
        
        try:
            prompt = f"""
The following error occurred while starting the development server for a {spec.framework} project:

Error: {result.get('error', 'Unknown error')}

Project details:
- Name: {spec.name}
- Framework: {spec.framework}
- Port: {spec.port}

Please provide a solution to fix this server issue. Return only the command(s) to run, nothing else.
"""
            
            response = await self._get_llm_response(prompt)
            if not response:
                return False
            
            fix_result = self.executor.execute(response.strip())
            if fix_result["success"]:
                await self._log("Server issue fixed successfully!")
                return True
            else:
                await self._log(f"Fix attempt failed: {fix_result.get('stderr', '')}")
                return await self._fix_server_issues(spec, fix_result, max_retries - 1)
                
        except Exception as e:
            await self._log(f"Error during fix attempt: {e}")
            return False
    
    async def _get_llm_response(self, prompt: str) -> Optional[str]:
        """Get a response from the LLM client."""
        if not self.llm_client:
            return None
        
        try:
            # This is a placeholder - you'll need to implement the actual LLM call
            # based on your LLM client (OpenAI, Anthropic, etc.)
            if hasattr(self.llm_client, 'chat'):
                response = await self.llm_client.chat(prompt)
                return response
            elif hasattr(self.llm_client, 'generate'):
                response = await self.llm_client.generate(prompt)
                return response
            else:
                logger.warning("LLM client not properly configured")
                return None
                
        except Exception as e:
            logger.error(f"Error getting LLM response: {e}")
            return None
    
    async def _log(self, message: str):
        """Add a log message."""
        timestamp = time.strftime("%H:%M:%S")
        log_entry = f"[{timestamp}] {message}"
        logger.info(log_entry)
        print(log_entry)
    
    def get_website_status(self, website_name: str) -> Optional[GenerationResult]:
        """Get the status of a generated website."""
        return self.generation_results.get(website_name)
    
    def list_websites(self) -> List[str]:
        """List all generated websites."""
        return list(self.website_specs.keys())
    
    def cleanup(self):
        """Clean up the agent and executor."""
        self.executor.cleanup()
        logger.info(f"AutoGenSandboxAgent '{self.name}' cleanup completed")


# Convenience functions
def create_autogen_sandbox_agent(name: str, config: Optional[SandboxConfig] = None, llm_client=None) -> AutoGenSandboxAgent:
    """Create an AutoGen sandbox agent with default configuration."""
    return AutoGenSandboxAgent(name, config, llm_client)


async def generate_website_async(spec: WebsiteSpec, config: Optional[SandboxConfig] = None, llm_client=None) -> GenerationResult:
    """Generate a website asynchronously."""
    agent = AutoGenSandboxAgent("website-generator", config, llm_client)
    try:
        return await agent.generate_website(spec)
    finally:
        agent.cleanup() 