#!/usr/bin/env python3
"""
LLM Integration tests for static site modernization.
"""

import os
import sys
import unittest
import pytest
from unittest.mock import Mock, patch

# Add the project root to Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "../..")))

from engine.modernizers.static_site.transpilers.llm_augmentor import (
    StaticSiteLLMAugmentor, 
    StaticSiteLLMConfig,
    StaticSiteClaudeProvider,
    StaticSiteLocalProvider
)


@pytest.mark.static_site
@pytest.mark.llm
class TestStaticSiteLLMIntegration(unittest.TestCase):
    """Test LLM integration for static site modernization."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.config = StaticSiteLLMConfig(
            api_key="test-key",
            model="claude-3-5-sonnet-20241022",
            temperature=0.1,
            provider="anthropic"
        )
        self.test_data_dir = os.path.join(os.path.dirname(__file__), "../../examples/website")
    
    def test_static_site_llm_config_initialization(self):
        """Test StaticSiteLLMConfig initialization."""
        config = StaticSiteLLMConfig(
            api_key="test-key",
            model="test-model",
            temperature=0.2,
            provider="anthropic"
        )
        
        self.assertEqual(config.api_key, "test-key")
        self.assertEqual(config.model, "test-model")
        self.assertEqual(config.temperature, 0.2)
        self.assertEqual(config.provider, "anthropic")
        self.assertTrue(config.cache_enabled)
    
    def test_static_site_llm_config_from_env(self):
        """Test StaticSiteLLMConfig creation from environment variables."""
        with patch.dict(os.environ, {
            'LLM_API_KEY': 'env-test-key',
            'LLM_MODEL': 'env-test-model',
            'LLM_PROVIDER': 'anthropic',
            'DEFAULT_LLM_TEMPERATURE': '0.3'
        }):
            config = StaticSiteLLMConfig.from_env()
            
            self.assertEqual(config.api_key, 'env-test-key')
            self.assertEqual(config.model, 'env-test-model')
            self.assertEqual(config.provider, 'anthropic')
            self.assertEqual(config.temperature, 0.3)
    
    def test_static_site_llm_augmentor_initialization(self):
        """Test StaticSiteLLMAugmentor initialization."""
        augmentor = StaticSiteLLMAugmentor(self.config)
        
        self.assertIsNotNone(augmentor)
        self.assertEqual(augmentor.config.model, "claude-3-5-sonnet-20241022")
        self.assertEqual(augmentor.config.temperature, 0.1)
    
    @patch('requests.post')
    def test_static_site_llm_augmentor_can_augment(self, mock_post):
        """Test if StaticSiteLLMAugmentor can augment."""
        augmentor = StaticSiteLLMAugmentor(self.config)
        
        # Mock successful response
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.json.return_value = {"content": [{"text": "test response"}]}
        mock_post.return_value = mock_response
        
        result = augmentor.can_augment()
        self.assertTrue(result)
    
    @patch('requests.post')
    def test_static_site_llm_augmentor_translate_website_component(self, mock_post):
        """Test website component translation."""
        augmentor = StaticSiteLLMAugmentor(self.config)
        
        # Mock successful response
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "content": [{
                "text": "```tsx\nconst TestComponent = () => {\n  return <div>Test</div>;\n};\n\nexport default TestComponent;\n```"
            }]
        }
        mock_post.return_value = mock_response
        
        website_component = {
            "type": "navigation",
            "id": "nav",
            "title": "Navigation",
            "content": "<nav class=\"navbar\">...</nav>",
            "classes": ["navbar", "navbar-expand-lg"],
            "attributes": {}
        }
        
        result = augmentor.translate_website_component(website_component)
        
        self.assertIsNotNone(result)
        self.assertIn("const TestComponent", result)
        self.assertIn("export default TestComponent", result)
    
    @patch('requests.post')
    def test_static_site_llm_augmentor_batch_translate(self, mock_post):
        """Test batch component translation."""
        augmentor = StaticSiteLLMAugmentor(self.config)
        
        # Mock successful response
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "content": [{
                "text": "```tsx\nconst TestComponent = () => {\n  return <div>Test</div>;\n};\n\nexport default TestComponent;\n```"
            }]
        }
        mock_post.return_value = mock_response
        
        components = [
            {
                "type": "navigation",
                "id": "nav",
                "title": "Navigation",
                "content": "<nav>...</nav>",
                "classes": ["navbar"],
                "attributes": {}
            },
            {
                "type": "hero",
                "id": "hero",
                "title": "Hero Section",
                "content": "<section>...</section>",
                "classes": ["hero"],
                "attributes": {}
            }
        ]
        
        results = augmentor.batch_translate_components(components)
        
        self.assertIsInstance(results, dict)
        self.assertEqual(len(results), 2)
        
        # Check that both components were translated
        for component_id, result in results.items():
            self.assertIsNotNone(result)
            self.assertIn("const TestComponent", result)
    
    @patch('requests.post')
    def test_static_site_llm_augmentor_generate_integration_code(self, mock_post):
        """Test integration code generation."""
        augmentor = StaticSiteLLMAugmentor(self.config)
        
        # Mock successful response
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "content": [{
                "text": "import React from 'react';\n\nconst App: React.FC = () => {\n  return (\n    <div className=\"App\">\n    </div>\n  );\n};\n\nexport default App;"
            }]
        }
        mock_post.return_value = mock_response
        
        translations = {
            "nav": "const Navigation = () => <nav>...</nav>;",
            "hero": "const Hero = () => <section>...</section>;"
        }
        
        result = augmentor.generate_integration_code(translations)
        
        self.assertIsNotNone(result)
        self.assertIn("import React", result)
        self.assertIn("const App", result)
        self.assertIn("export default App", result)
    
    def test_static_site_claude_provider_initialization(self):
        """Test StaticSiteClaudeProvider initialization."""
        provider = StaticSiteClaudeProvider()
        
        self.assertIsNotNone(provider)
    
    @patch('requests.post')
    def test_static_site_claude_provider_generate_response(self, mock_post):
        """Test Claude provider response generation."""
        provider = StaticSiteClaudeProvider()
        
        # Mock successful response
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "content": [{"text": "test response"}]
        }
        mock_post.return_value = mock_response
        
        messages = [
            {"role": "system", "content": "You are a helpful assistant."},
            {"role": "user", "content": "Say hello"}
        ]
        
        result = provider.generate_response(messages, self.config)
        
        self.assertEqual(result, "test response")
    
    def test_static_site_local_provider_initialization(self):
        """Test StaticSiteLocalProvider initialization."""
        provider = StaticSiteLocalProvider()
        
        self.assertIsNotNone(provider)
    
    @patch('requests.post')
    def test_static_site_local_provider_generate_response(self, mock_post):
        """Test local provider response generation."""
        provider = StaticSiteLocalProvider()
        
        # Mock successful response
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.json.return_value = {"response": "test response"}
        mock_post.return_value = mock_response
        
        messages = [
            {"role": "system", "content": "You are a helpful assistant."},
            {"role": "user", "content": "Say hello"}
        ]
        
        result = provider.generate_response(messages, self.config)
        
        self.assertEqual(result, "test response")
    
    @patch('requests.post')
    def test_static_site_llm_augmentor_error_handling(self, mock_post):
        """Test LLM augmentor error handling."""
        augmentor = StaticSiteLLMAugmentor(self.config)
        
        # Mock failed response
        mock_response = Mock()
        mock_response.status_code = 500
        mock_response.text = "Internal Server Error"
        mock_post.return_value = mock_response
        
        website_component = {
            "type": "navigation",
            "id": "nav",
            "title": "Navigation",
            "content": "<nav>...</nav>",
            "classes": ["navbar"],
            "attributes": {}
        }
        
        # Should handle error gracefully
        result = augmentor.translate_website_component(website_component)
        
        # Should return None when LLM fails
        self.assertIsNone(result)
    
    def test_static_site_llm_augmentor_cache(self):
        """Test LLM augmentor caching functionality."""
        augmentor = StaticSiteLLMAugmentor(self.config)
        
        # Test cache operations
        augmentor.cache.set("test_key", "test_value")
        cached_value = augmentor.cache.get("test_key")
        
        self.assertEqual(cached_value, "test_value")
    
    def test_static_site_llm_augmentor_cache_stats(self):
        """Test LLM augmentor cache statistics."""
        augmentor = StaticSiteLLMAugmentor(self.config)
        
        # Add some test data to cache
        augmentor.cache.set("key1", "value1")
        augmentor.cache.set("key2", "value2")
        
        stats = augmentor.get_cache_stats()
        
        # Check that stats contains expected keys
        self.assertIn("cache_size", stats)
        self.assertIn("cache_ttl", stats)
    
    def test_static_site_llm_augmentor_clear_cache(self):
        """Test LLM augmentor cache clearing."""
        augmentor = StaticSiteLLMAugmentor(self.config)
        
        # Add some test data to cache
        augmentor.cache.set("key1", "value1")
        augmentor.cache.set("key2", "value2")
        
        # Clear cache
        augmentor.clear_cache()
        
        # Check that cache is empty
        cached_value = augmentor.cache.get("key1")
        self.assertIsNone(cached_value)
    
    def test_static_site_llm_augmentor_generate_cache_key(self):
        """Test cache key generation."""
        augmentor = StaticSiteLLMAugmentor(self.config)
        
        website_component = {
            "type": "navigation",
            "id": "nav",
            "title": "Navigation",
            "content": "<nav>...</nav>",
            "classes": ["navbar"],
            "attributes": {}
        }
        
        cache_key = augmentor._generate_cache_key(website_component)
        
        self.assertIsInstance(cache_key, str)
        # Cache key should be a hash, not contain the original text
        self.assertGreater(len(cache_key), 10)
    
    @patch('requests.post')
    def test_static_site_llm_augmentor_retry_mechanism(self, mock_post):
        """Test retry mechanism for failed requests."""
        augmentor = StaticSiteLLMAugmentor(self.config)
        
        # Mock failed responses followed by success
        mock_response_fail = Mock()
        mock_response_fail.status_code = 500
        mock_response_fail.text = "Internal Server Error"
        
        mock_response_success = Mock()
        mock_response_success.status_code = 200
        mock_response_success.json.return_value = {
            "content": [{"text": "success response"}]
        }
        
        # First two calls fail, third succeeds
        mock_post.side_effect = [mock_response_fail, mock_response_fail, mock_response_success]
        
        website_component = {
            "type": "navigation",
            "id": "nav",
            "title": "Navigation",
            "content": "<nav>...</nav>",
            "classes": ["navbar"],
            "attributes": {}
        }
        
        result = augmentor.translate_website_component(website_component)
        
        # Should eventually succeed after retries
        self.assertEqual(result, "success response")
        self.assertEqual(mock_post.call_count, 3)


if __name__ == '__main__':
    unittest.main() 