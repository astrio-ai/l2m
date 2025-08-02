"""
Static Site LLM Augmentor

This module provides AI-assisted translation for complex HTML/website constructs
that cannot be handled by the rule-based system.
"""

import os
import logging
import json
import hashlib
import time
from typing import Dict, Any, List, Optional, Union
from dataclasses import dataclass, field
from abc import ABC, abstractmethod
import requests
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

@dataclass
class StaticSiteLLMConfig:
    """Configuration for Static Site LLM API calls."""
    api_key: str
    model: str
    temperature: float
    max_tokens: int = 2000
    provider: str = "local"  # openai, anthropic, local
    cache_enabled: bool = True
    cache_ttl: int = 3600  # 1 hour
    retry_attempts: int = 3
    retry_delay: float = 1.0
    
    @classmethod
    def from_env(cls):
        """Create config from environment variables."""
        # Handle invalid temperature gracefully
        try:
            temperature = float(os.getenv('DEFAULT_LLM_TEMPERATURE', '0.1'))
        except ValueError:
            temperature = 0.1  # Default fallback
        
        # Get provider and set appropriate defaults
        provider = os.getenv('LLM_PROVIDER', 'anthropic')  # Default to Claude
        api_key = os.getenv('LLM_API_KEY', '')
        
        # Set model based on provider
        if provider == 'anthropic':
            default_model = 'claude-3-5-sonnet-20241022'  # Latest Claude model
        elif provider == 'local':
            default_model = 'llama2'
        else:
            default_model = 'claude-3-5-sonnet-20241022'
        
        return cls(
            api_key=api_key,
            model=os.getenv('LLM_MODEL', default_model),
            temperature=temperature,
            provider=provider,
            cache_enabled=os.getenv('LLM_CACHE_ENABLED', 'true').lower() == 'true',
            cache_ttl=int(os.getenv('LLM_CACHE_TTL', '3600')),
            retry_attempts=int(os.getenv('LLM_RETRY_ATTEMPTS', '3')),
            retry_delay=float(os.getenv('LLM_RETRY_DELAY', '1.0'))
        )


class StaticSiteLLMProvider(ABC):
    """Abstract base class for Static Site LLM providers."""
    
    @abstractmethod
    def generate_response(self, messages: List[Dict[str, str]], config: StaticSiteLLMConfig) -> str:
        """Generate response from LLM."""
        pass


class StaticSiteClaudeProvider(StaticSiteLLMProvider):
    """Claude API provider for static site modernization."""
    
    def generate_response(self, messages: List[Dict[str, str]], config: StaticSiteLLMConfig) -> str:
        """Generate response using Claude API."""
        logger = logging.getLogger(__name__)
        logger.info(f"Generating response with Claude API: {config.model}")
        
        # Convert messages to Claude format
        claude_messages = []
        for msg in messages:
            if msg["role"] == "system":
                claude_messages.append({
                    "role": "user",
                    "content": f"System: {msg['content']}"
                })
            else:
                claude_messages.append({
                    "role": msg["role"],
                    "content": msg["content"]
                })
        
        logger.info(f"Sending request to Claude API with prompt length: {len(str(claude_messages))}")
        
        response = requests.post(
            "https://api.anthropic.com/v1/messages",
            headers={
                "Content-Type": "application/json",
                "x-api-key": config.api_key,
                "anthropic-version": "2023-06-01"
            },
            json={
                "model": config.model,
                "messages": claude_messages,
                "max_tokens": config.max_tokens,
                "temperature": config.temperature
            },
            timeout=60  # 60 second timeout
        )
        
        if response.status_code == 200:
            result = response.json()
            if "content" in result and len(result["content"]) > 0:
                response_text = result["content"][0]["text"].strip()
                # Clean up markdown code blocks if present
                if "```tsx" in response_text:
                    # Extract code from markdown blocks
                    import re
                    tsx_blocks = re.findall(r'```tsx\s*\n(.*?)\n```', response_text, re.DOTALL)
                    if tsx_blocks:
                        return tsx_blocks[0].strip()
                    # Fallback: remove markdown formatting
                    response_text = re.sub(r'```tsx\s*\n', '', response_text)
                    response_text = re.sub(r'\n```', '', response_text)
                elif "```jsx" in response_text:
                    # Extract code from markdown blocks
                    import re
                    jsx_blocks = re.findall(r'```jsx\s*\n(.*?)\n```', response_text, re.DOTALL)
                    if jsx_blocks:
                        return jsx_blocks[0].strip()
                    # Fallback: remove markdown formatting
                    response_text = re.sub(r'```jsx\s*\n', '', response_text)
                    response_text = re.sub(r'\n```', '', response_text)
                return response_text
            else:
                raise Exception(f"Claude API response missing 'content' field: {result}")
        else:
            raise Exception(f"Claude API request failed: {response.status_code} - {response.text}")


class StaticSiteLocalProvider(StaticSiteLLMProvider):
    """Local LLM provider for static site modernization (e.g., Ollama)."""
    
    def generate_response(self, messages: List[Dict[str, str]], config: StaticSiteLLMConfig) -> str:
        """Generate response using local LLM."""
        logger = logging.getLogger(__name__)
        logger.info(f"Generating response with local LLM: {config.model}")
        
        # Convert to Ollama format
        prompt = ""
        for msg in messages:
            if msg["role"] == "system":
                prompt += f"System: {msg['content']}\n\n"
            elif msg["role"] == "user":
                prompt += f"User: {msg['content']}\n\n"
            elif msg["role"] == "assistant":
                prompt += f"Assistant: {msg['content']}\n\n"
        
        logger.info(f"Sending request to Ollama with prompt length: {len(prompt)}")
        
        response = requests.post(
            "http://localhost:11434/api/generate",
            json={
                "model": config.model,
                "prompt": prompt,
                "stream": False
            },
            timeout=60  # 60 second timeout
        )
        
        if response.status_code == 200:
            result = response.json()
            if "response" in result:
                response_text = result["response"].strip()
                # Clean up markdown code blocks if present
                if "```tsx" in response_text:
                    # Extract code from markdown blocks
                    import re
                    tsx_blocks = re.findall(r'```tsx\s*\n(.*?)\n```', response_text, re.DOTALL)
                    if tsx_blocks:
                        return tsx_blocks[0].strip()
                    # Fallback: remove markdown formatting
                    response_text = re.sub(r'```tsx\s*\n', '', response_text)
                    response_text = re.sub(r'\n```', '', response_text)
                elif "```jsx" in response_text:
                    # Extract code from markdown blocks
                    import re
                    jsx_blocks = re.findall(r'```jsx\s*\n(.*?)\n```', response_text, re.DOTALL)
                    if jsx_blocks:
                        return jsx_blocks[0].strip()
                    # Fallback: remove markdown formatting
                    response_text = re.sub(r'```jsx\s*\n', '', response_text)
                    response_text = re.sub(r'\n```', '', response_text)
                return response_text
            else:
                raise Exception(f"Local LLM response missing 'response' field: {result}")
        else:
            raise Exception(f"Local LLM request failed: {response.status_code} - {response.text}")


class StaticSiteResponseCache:
    """Simple in-memory cache for Static Site LLM responses."""
    
    def __init__(self, ttl: int = 3600):
        self.cache: Dict[str, Dict[str, Any]] = {}
        self.ttl = ttl
    
    def get(self, key: str) -> Optional[str]:
        """Get cached response if not expired."""
        if key in self.cache:
            entry = self.cache[key]
            if time.time() - entry['timestamp'] < self.ttl:
                return entry['response']
            else:
                del self.cache[key]
        return None
    
    def set(self, key: str, response: str):
        """Cache a response."""
        self.cache[key] = {
            'response': response,
            'timestamp': time.time()
        }
    
    def clear(self):
        """Clear all cached responses."""
        self.cache.clear()


class StaticSiteLLMAugmentor:
    """
    Handles AI-assisted translation of complex HTML/website constructs.
    """
    
    def __init__(self, config: Optional[StaticSiteLLMConfig] = None):
        self.config = config or StaticSiteLLMConfig.from_env()
        self.logger = logging.getLogger(__name__)
        self.cache = StaticSiteResponseCache(self.config.cache_ttl) if self.config.cache_enabled else None
        
        # Set up provider
        self.provider = self._create_provider()
    
    def _create_provider(self) -> Optional[StaticSiteLLMProvider]:
        """Create the appropriate LLM provider."""
        if self.config.provider == "local":
            return StaticSiteLocalProvider()
        elif self.config.provider == "anthropic":
            return StaticSiteClaudeProvider()
        else:
            self.logger.warning(f"Unsupported provider: {self.config.provider}")
            return None
    
    def can_augment(self) -> bool:
        """Check if LLM augmentation is available."""
        return self.provider is not None
    
    def _generate_cache_key(self, website_component: Dict[str, Any]) -> str:
        """Generate cache key for website component."""
        content = json.dumps(website_component, sort_keys=True)
        return hashlib.md5(content.encode()).hexdigest()
    
    def _create_enhanced_prompt(self, website_component: Dict[str, Any]) -> str:
        """Create enhanced prompt for website component translation."""
        component_type = website_component.get('type', 'unknown')
        component_id = website_component.get('id', '')
        component_title = website_component.get('title', '')
        component_content = website_component.get('content', '')
        component_classes = website_component.get('classes', [])
        component_attributes = website_component.get('attributes', {})
        
        prompt = f"""Convert this HTML component to a modern React TypeScript component.

Component Details:
- Type: {component_type}
- ID: {component_id}
- Title: {component_title}
- Content: {component_content}
- CSS Classes: {json.dumps(component_classes)}
- Attributes: {json.dumps(component_attributes)}

CRITICAL REQUIREMENTS:
1. Convert to functional React component with TypeScript
2. Use ONLY these imports: React, useState, useEffect, useCallback (if needed)
3. DO NOT import any external libraries like formik, yup, react-router-dom, @tailwindcss
4. Use Tailwind CSS utility classes for styling (no external CSS libraries)
5. Convert Bootstrap classes to Tailwind equivalents
6. Make it responsive using Tailwind responsive classes
7. Use modern React patterns (hooks, etc.)
8. Include proper TypeScript interfaces
9. Use .tsx extension and TypeScript syntax
10. Generate clean, production-ready code

ALLOWED IMPORTS ONLY:
- import React from 'react';
- import {{ useState, useEffect, useCallback }} from 'react';

FORBIDDEN IMPORTS:
- DO NOT import formik, yup, react-router-dom, @tailwindcss, or any external libraries
- DO NOT import any CSS libraries or frameworks

Please provide ONLY the React TypeScript component code in TSX format, no explanations or other text.

Example format:
```tsx
import React, {{ useState }} from 'react';

interface ComponentProps {{
  // Add props if needed
}}

const Component: React.FC<ComponentProps> = () => {{
  const [state, setState] = useState<string>('');
  
  return (
    <div className="p-4 bg-white rounded-lg shadow-md">
      <h2 className="text-2xl font-bold text-gray-900 mb-4">Component Title</h2>
      <div className="text-gray-600">
        Component content here
      </div>
    </div>
  );
}};

export default Component;
```"""
        
        return prompt
    
    def _create_system_message(self) -> str:
        """Create system message for website modernization."""
        return """You are an expert web developer specializing in modernizing legacy websites to React TypeScript applications. You convert HTML + Bootstrap + jQuery websites to modern React + TypeScript + Tailwind CSS applications.

CRITICAL RULES:
1. ONLY use these imports: React, useState, useEffect, useCallback (if needed)
2. NEVER import external libraries like formik, yup, react-router-dom, @tailwindcss
3. Use ONLY Tailwind CSS utility classes for styling
4. Generate clean, production-ready TypeScript React code
5. Follow modern React patterns and best practices
6. Include proper TypeScript interfaces and types
7. Make components responsive using Tailwind responsive classes

Your expertise includes:
- Converting HTML components to React TypeScript components
- Transforming Bootstrap CSS to Tailwind CSS utility classes
- Converting jQuery JavaScript to React hooks
- Implementing responsive design patterns
- Following modern React and TypeScript best practices
- Creating maintainable and scalable code

IMPORTANT: Only use built-in React hooks and Tailwind CSS. Do not import any external libraries or frameworks."""
    
    def translate_website_component(self, website_component: Dict[str, Any]) -> Optional[str]:
        """
        Translate a website component using LLM.
        
        Args:
            website_component: Component data to translate
            
        Returns:
            Translated React TypeScript component or None if failed
        """
        if not self.can_augment():
            self.logger.warning("LLM augmentation not available")
            return None
        
        # Check cache first
        cache_key = self._generate_cache_key(website_component)
        if self.cache:
            cached_response = self.cache.get(cache_key)
            if cached_response:
                self.logger.info("Using cached response for website component")
                return cached_response
        
        try:
            # Create enhanced prompt
            prompt = self._create_enhanced_prompt(website_component)
            
            # Create messages
            messages = [
                {"role": "system", "content": self._create_system_message()},
                {"role": "user", "content": prompt}
            ]
            
            # Generate response with retry
            response = self._generate_with_retry(messages)
            
            if response and self.cache:
                self.cache.set(cache_key, response)
            
            self.logger.info("Successfully translated website component")
            return response
            
        except Exception as e:
            self.logger.error(f"Failed to translate website component: {e}")
            return None
    
    def _generate_with_retry(self, messages: List[Dict[str, str]]) -> Optional[str]:
        """Generate LLM response with retry logic."""
        for attempt in range(self.config.retry_attempts):
            try:
                return self.provider.generate_response(messages, self.config)
            except Exception as e:
                self.logger.warning(f"LLM request failed (attempt {attempt + 1}/{self.config.retry_attempts}): {e}")
                if attempt < self.config.retry_attempts - 1:
                    time.sleep(self.config.retry_delay)
                else:
                    raise e
        
        return None
    
    def batch_translate_components(self, components: List[Dict[str, Any]]) -> Dict[str, str]:
        """
        Batch translate multiple website components.
        
        Args:
            components: List of component data to translate
            
        Returns:
            Dictionary mapping component IDs to translated code
        """
        translations = {}
        
        for component in components:
            component_id = component.get('id', f'component_{len(translations)}')
            translation = self.translate_website_component(component)
            
            if translation:
                translations[component_id] = translation
            else:
                self.logger.warning(f"Failed to translate component: {component_id}")
        
        return translations
    
    def generate_integration_code(self, translations: Dict[str, str]) -> str:
        """
        Generate integration code for translated components.
        
        Args:
            translations: Dictionary of component translations
            
        Returns:
            Integration code
        """
        integration_code = """import React from 'react';

// Import all translated components
"""
        
        for component_id, translation in translations.items():
            # Extract component name from translation
            import re
            component_match = re.search(r'const\s+(\w+):\s+React\.FC', translation)
            if component_match:
                component_name = component_match.group(1)
                integration_code += f"import {component_name} from './{component_name}.tsx';\n"
        
        integration_code += """
// Main App component that integrates all translated components
const App: React.FC = () => {
  return (
    <div className="App">
"""
        
        for component_id, translation in translations.items():
            import re
            component_match = re.search(r'const\s+(\w+):\s+React\.FC', translation)
            if component_match:
                component_name = component_match.group(1)
                integration_code += f"      <{component_name} />\n"
        
        integration_code += """    </div>
  );
};

export default App;"""
        
        return integration_code
    
    def clear_cache(self):
        """Clear the response cache."""
        if self.cache:
            self.cache.clear()
    
    def get_cache_stats(self) -> Dict[str, Any]:
        """Get cache statistics."""
        if self.cache:
            return {
                'cache_size': len(self.cache.cache),
                'cache_ttl': self.cache.ttl
            }
        return {'cache_enabled': False} 