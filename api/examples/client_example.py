#!/usr/bin/env python3
"""
Example Python client for the Legacy2Modern API

This script demonstrates how to use the API to transpile COBOL code
and modernize websites programmatically.
"""

import requests
import json
import os
from pathlib import Path


class Legacy2ModernClient:
    """Client for interacting with the Legacy2Modern API."""
    
    def __init__(self, base_url="http://localhost:8001"):
        self.base_url = base_url
        self.session = requests.Session()
    
    def health_check(self):
        """Check the health status of the API."""
        try:
            response = self.session.get(f"{self.base_url}/health")
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            print(f"Health check failed: {e}")
            return None
    
    def get_frameworks(self):
        """Get list of supported frameworks."""
        try:
            response = self.session.get(f"{self.base_url}/frameworks")
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            print(f"Failed to get frameworks: {e}")
            return None
    
    def transpile_cobol(self, source_code, output_file=None, llm_config=None):
        """
        Transpile COBOL code to Python.
        
        Args:
            source_code: COBOL source code as string
            output_file: Optional output file path
            llm_config: Optional LLM configuration
            
        Returns:
            API response as dictionary
        """
        payload = {
            "source_code": source_code,
            "output_file": output_file
        }
        
        if llm_config:
            payload["llm_config"] = llm_config
        
        try:
            response = self.session.post(
                f"{self.base_url}/transpile/cobol",
                json=payload
            )
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            print(f"COBOL transpilation failed: {e}")
            return None
    
    def transpile_cobol_file(self, file_path, output_file=None, llm_config=None):
        """
        Transpile a COBOL file to Python.
        
        Args:
            file_path: Path to COBOL file
            output_file: Optional output file path
            llm_config: Optional LLM configuration
            
        Returns:
            API response as dictionary
        """
        if not os.path.exists(file_path):
            print(f"File not found: {file_path}")
            return None
        
        files = {'file': open(file_path, 'rb')}
        data = {}
        
        if output_file:
            data['output_file'] = output_file
        
        if llm_config:
            data['llm_config'] = json.dumps(llm_config)
        
        try:
            response = self.session.post(
                f"{self.base_url}/transpile/cobol/file",
                files=files,
                data=data
            )
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            print(f"COBOL file transpilation failed: {e}")
            return None
        finally:
            files['file'].close()
    
    def modernize_website(self, input_path, output_dir, target_framework="react", analyze_only=False):
        """
        Modernize a legacy website to a modern framework.
        
        Args:
            input_path: Path to HTML file
            output_dir: Output directory for modern website
            target_framework: Target framework (react, nextjs, astro)
            analyze_only: Only analyze without generating code
            
        Returns:
            API response as dictionary
        """
        payload = {
            "input_path": input_path,
            "output_dir": output_dir,
            "target_framework": target_framework,
            "analyze_only": analyze_only
        }
        
        try:
            response = self.session.post(
                f"{self.base_url}/modernize/website",
                json=payload
            )
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            print(f"Website modernization failed: {e}")
            return None
    
    def analyze_website(self, input_path):
        """
        Analyze a legacy website without modernizing it.
        
        Args:
            input_path: Path to HTML file
            
        Returns:
            API response as dictionary
        """
        payload = {"input_path": input_path}
        
        try:
            response = self.session.post(
                f"{self.base_url}/analyze/website",
                json=payload
            )
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            print(f"Website analysis failed: {e}")
            return None
    
    def analyze_code(self, source_code, target_code=None):
        """
        Analyze source code and optionally compare with target code.
        
        Args:
            source_code: Source code to analyze
            target_code: Optional target code for comparison
            
        Returns:
            API response as dictionary
        """
        payload = {"source_code": source_code}
        
        if target_code:
            payload["target_code"] = target_code
        
        try:
            response = self.session.post(
                f"{self.base_url}/analyze/code",
                json=payload
            )
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            print(f"Code analysis failed: {e}")
            return None


def main():
    """Example usage of the Legacy2Modern API client."""
    print("🚀 Legacy2Modern API Client Example")
    print("=" * 50)
    
    # Initialize client
    client = Legacy2ModernClient()
    
    # Check API health
    print("\n1. Checking API health...")
    health = client.health_check()
    if health:
        print(f"✅ API Status: {health['status']}")
        print(f"📅 Timestamp: {health['timestamp']}")
        for component, status in health['components'].items():
            print(f"   {component}: {status}")
    else:
        print("❌ API is not responding")
        return
    
    # Get supported frameworks
    print("\n2. Getting supported frameworks...")
    frameworks = client.get_frameworks()
    if frameworks:
        print(f"✅ Supported frameworks: {', '.join(frameworks['supported_frameworks'])}")
    
    # Example COBOL transpilation
    print("\n3. Transpiling COBOL code...")
    cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           HELLO-WORLD.
       
       PROCEDURE DIVISION.
           DISPLAY 'Hello, World!'
           STOP RUN.
    """
    
    result = client.transpile_cobol(cobol_code)
    if result and result.get('success'):
        print("✅ COBOL transpilation successful!")
        print(f"📝 Message: {result['message']}")
        if result.get('generated_code'):
            print("🐍 Generated Python code:")
            print(result['generated_code'])
    else:
        print("❌ COBOL transpilation failed")
        if result:
            print(f"Error: {result.get('message', 'Unknown error')}")
    
    # Example code analysis
    print("\n4. Analyzing Python code...")
    python_code = """
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

# Test the function
result = fibonacci(10)
print(f"Fibonacci(10) = {result}")
    """
    
    analysis = client.analyze_code(python_code)
    if analysis and analysis.get('success'):
        print("✅ Code analysis successful!")
        print(f"📊 Analysis results:")
        for key, value in analysis['analysis'].items():
            print(f"   {key}: {value}")
    else:
        print("❌ Code analysis failed")
    
    print("\n🎉 Example completed!")
    print("\n💡 Try running the API server and test with your own code:")
    print("   python api/run_server.py")


if __name__ == "__main__":
    main() 