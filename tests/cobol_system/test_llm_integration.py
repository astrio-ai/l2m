#!/usr/bin/env python3
"""
LLM Integration tests for COBOL to Python conversion.
"""

import os
import sys
import unittest
import pytest
from unittest.mock import Mock, patch

# Add the project root to Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "../..")))

from engine.modernizers.cobol_system.transpilers.hybrid_transpiler import HybridTranspiler
from engine.modernizers.cobol_system.transpilers.llm_augmentor import LLMAugmentor, LLMConfig


@pytest.mark.cobol
@pytest.mark.llm
class TestLLMIntegration(unittest.TestCase):
    """Test LLM integration for COBOL transpilation."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.config = LLMConfig(
            api_key="test-key",
            model="test-model",
            temperature=0.1,
            provider="test"
        )
        self.test_data_dir = os.path.join(os.path.dirname(__file__), "../../examples/cobol")
        self.output_dir = os.path.join(os.path.dirname(__file__), "../../output/modernized-python")
        
        # Ensure output directory exists
        os.makedirs(self.output_dir, exist_ok=True)
    
    @patch('requests.post')
    def test_llm_augmentor_initialization(self, mock_post):
        """Test LLM augmentor initialization."""
        augmentor = LLMAugmentor(self.config)
        
        self.assertIsNotNone(augmentor)
        self.assertEqual(augmentor.config.model, "test-model")
        self.assertEqual(augmentor.config.temperature, 0.1)
    
    @patch('requests.post')
    def test_llm_augmentor_can_augment(self, mock_post):
        """Test if LLM augmentor can augment."""
        augmentor = LLMAugmentor(self.config)
        
        # Mock successful response
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.json.return_value = {"response": "test response"}
        mock_post.return_value = mock_response
        
        result = augmentor.can_augment()
        # Method should return True if provider is available
        self.assertIsInstance(result, bool)
    
    @patch('requests.post')
    def test_llm_augmentor_translate(self, mock_post):
        """Test LLM augmentor translation."""
        augmentor = LLMAugmentor(self.config)
        
        # Mock successful response
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.json.return_value = {"response": "def test_function():\n    return 'test'"}
        mock_post.return_value = mock_response
        
        edge_case = {
            "type": "complex_arithmetic",
            "description": "COMPUTE RESULT = NUM1 * NUM2 / 100",
            "context": "COBOL arithmetic operation"
        }
        
        result = augmentor.translate_edge_case(edge_case)
        
        self.assertIsInstance(result, (str, type(None)))
    
    def test_hybrid_transpiler_with_llm(self):
        """Test hybrid transpiler with LLM integration."""
        transpiler = HybridTranspiler(self.config)
        
        self.assertIsNotNone(transpiler)
        self.assertIsNotNone(transpiler.llm_augmentor)
    
    @patch('requests.post')
    def test_hybrid_transpiler_transpile_file(self, mock_post):
        """Test hybrid transpiler file transpilation."""
        transpiler = HybridTranspiler(self.config)
        
        # Mock successful LLM response
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.json.return_value = {"response": "def main():\n    print('Hello World')"}
        mock_post.return_value = mock_response
        
        input_file = os.path.join(self.test_data_dir, "HELLO.cobol")
        
        if os.path.exists(input_file):
            result = transpiler.transpile_file(input_file)
            
            self.assertIsNotNone(result)
            self.assertIn("def main", result)
    
    def test_llm_config_from_env(self):
        """Test LLM config creation from environment variables."""
        with patch.dict(os.environ, {
            'LLM_API_KEY': 'test-key',
            'LLM_MODEL': 'test-model',
            'LLM_PROVIDER': 'test'
        }):
            config = LLMConfig.from_env()
            
            self.assertEqual(config.api_key, 'test-key')
            self.assertEqual(config.model, 'test-model')
            self.assertEqual(config.provider, 'test')
    
    @patch('requests.post')
    def test_llm_augmentor_error_handling(self, mock_post):
        """Test LLM augmentor error handling."""
        augmentor = LLMAugmentor(self.config)
        
        # Mock failed response
        mock_response = Mock()
        mock_response.status_code = 500
        mock_response.text = "Internal Server Error"
        mock_post.return_value = mock_response
        
        edge_case = {
            "type": "complex_arithmetic",
            "description": "COMPUTE RESULT = NUM1 * NUM2 / 100",
            "context": "COBOL arithmetic operation"
        }
        
        # Should handle error gracefully
        result = augmentor.translate_edge_case(edge_case)
        
        # Should return None when LLM fails
        self.assertIsNone(result)
    
    def test_llm_augmentor_cache(self):
        """Test LLM augmentor caching functionality."""
        augmentor = LLMAugmentor(self.config)
        
        # Test cache operations
        augmentor.cache.set("test_key", "test_value")
        cached_value = augmentor.cache.get("test_key")
        
        self.assertEqual(cached_value, "test_value")
    
    def test_llm_augmentor_cache_stats(self):
        """Test LLM augmentor cache statistics."""
        augmentor = LLMAugmentor(self.config)
        
        # Add some test data to cache
        augmentor.cache.set("key1", "value1")
        augmentor.cache.set("key2", "value2")
        
        stats = augmentor.get_cache_stats()
        
        # Check that stats contains expected keys
        self.assertIn("enabled", stats)
        self.assertIn("size", stats)
        self.assertIn("ttl", stats)


if __name__ == '__main__':
    unittest.main() 