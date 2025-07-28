"""
Tests for LLM Augmentor

This module tests the LLM augmentation functionality for AI-assisted
COBOL to Python translation.
"""

import os
import sys
import pytest
from unittest.mock import Mock, patch

# Add the project root to sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from packages.transpiler.engine.llm_augmentor import LLMAugmentor, LLMConfig


class TestLLMConfig:
    """Test the LLMConfig class."""
    
    def test_from_env_with_valid_config(self):
        """Test creating config from environment variables."""
        with patch.dict(os.environ, {
            'LLM_API_KEY': 'test_key',
            'LLM_MODEL': 'gpt-4',
            'DEFAULT_LLM_TEMPERATURE': '0.2'
        }):
            config = LLMConfig.from_env()
            
            assert config.api_key == 'test_key'
            assert config.model == 'gpt-4'
            assert config.temperature == 0.2
            assert config.max_tokens == 2000  # Default value
    
    def test_from_env_with_missing_values(self):
        """Test creating config with missing environment variables."""
        with patch.dict(os.environ, {}, clear=True):
            config = LLMConfig.from_env()
            
            assert config.api_key == ''
            assert config.model == 'gpt-4'  # Default value
            assert config.temperature == 0.1  # Default value
    
    def test_from_env_with_invalid_temperature(self):
        """Test creating config with invalid temperature."""
        with patch.dict(os.environ, {
            'LLM_API_KEY': 'test_key',
            'DEFAULT_LLM_TEMPERATURE': 'invalid'
        }):
            config = LLMConfig.from_env()
            
            assert config.api_key == 'test_key'
            assert config.temperature == 0.1  # Should fall back to default


class TestLLMAugmentor:
    """Test the LLMAugmentor class."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.config = LLMConfig(
            api_key='test_key',
            model='gpt-4',
            temperature=0.1
        )
        self.augmentor = LLMAugmentor(self.config)
    
    def test_can_augment_with_valid_config(self):
        """Test that augmentation is available with valid config."""
        assert self.augmentor.can_augment()
    
    def test_can_augment_without_api_key(self):
        """Test that augmentation is not available without API key."""
        config = LLMConfig(api_key='', model='gpt-4', temperature=0.1)
        augmentor = LLMAugmentor(config)
        
        assert not augmentor.can_augment()
    
    def test_generate_cobol_to_python_prompt(self):
        """Test prompt generation for COBOL to Python translation."""
        edge_case = {
            'type': 'complex_construct',
            'context': 'test_context',
            'all_tokens': ['COMPUTE', 'RESULT', '=', 'A', '+', 'B']
        }
        
        prompt = self.augmentor.generate_cobol_to_python_prompt(edge_case)
        
        assert 'COBOL to Python translator' in prompt
        assert 'complex_construct' in prompt
        assert 'test_context' in prompt
        assert 'COMPUTE' in prompt
        assert 'RESULT' in prompt
    
    @patch('openai.ChatCompletion.create')
    def test_translate_edge_case_success(self, mock_create):
        """Test successful edge case translation."""
        # Mock successful API response
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message.content = "result = a + b"
        mock_create.return_value = mock_response
        
        edge_case = {
            'type': 'complex_construct',
            'all_tokens': ['COMPUTE', 'RESULT', '=', 'A', '+', 'B']
        }
        
        result = self.augmentor.translate_edge_case(edge_case)
        
        assert result == "result = a + b"
        mock_create.assert_called_once()
    
    @patch('openai.ChatCompletion.create')
    def test_translate_edge_case_failure(self, mock_create):
        """Test edge case translation failure."""
        # Mock API failure
        mock_create.side_effect = Exception("API Error")
        
        edge_case = {
            'type': 'complex_construct',
            'all_tokens': ['COMPUTE', 'RESULT', '=', 'A', '+', 'B']
        }
        
        result = self.augmentor.translate_edge_case(edge_case)
        
        assert result is None
    
    def test_translate_edge_case_no_api_key(self):
        """Test edge case translation without API key."""
        config = LLMConfig(api_key='', model='gpt-4', temperature=0.1)
        augmentor = LLMAugmentor(config)
        
        edge_case = {
            'type': 'complex_construct',
            'all_tokens': ['COMPUTE', 'RESULT', '=', 'A', '+', 'B']
        }
        
        result = augmentor.translate_edge_case(edge_case)
        
        assert result is None
    
    @patch('openai.ChatCompletion.create')
    def test_batch_translate_edge_cases(self, mock_create):
        """Test batch translation of edge cases."""
        # Mock successful API responses
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message.content = "result = a + b"
        mock_create.return_value = mock_response
        
        edge_cases = [
            {
                'type': 'complex_construct',
                'all_tokens': ['COMPUTE', 'RESULT', '=', 'A', '+', 'B']
            },
            {
                'type': 'unknown_tokens',
                'all_tokens': ['UNKNOWN', 'TOKEN']
            }
        ]
        
        results = self.augmentor.batch_translate_edge_cases(edge_cases)
        
        assert len(results) == 2
        assert 'edge_case_0_complex_construct' in results
        assert 'edge_case_1_unknown_tokens' in results
        assert results['edge_case_0_complex_construct'] == "result = a + b"
        assert results['edge_case_1_unknown_tokens'] == "result = a + b"
    
    def test_batch_translate_edge_cases_no_api_key(self):
        """Test batch translation without API key."""
        config = LLMConfig(api_key='', model='gpt-4', temperature=0.1)
        augmentor = LLMAugmentor(config)
        
        edge_cases = [
            {
                'type': 'complex_construct',
                'all_tokens': ['COMPUTE', 'RESULT', '=', 'A', '+', 'B']
            }
        ]
        
        results = augmentor.batch_translate_edge_cases(edge_cases)
        
        assert results == {}
    
    def test_generate_integration_code(self):
        """Test generation of integration code."""
        translations = {
            'edge_case_0_complex_construct': 'result = a + b',
            'edge_case_1_unknown_tokens': 'unknown_token()'
        }
        
        integration_code = self.augmentor.generate_integration_code(translations)
        
        assert '# AI-Generated Code Snippets' in integration_code
        assert '# ========================' in integration_code
        assert 'edge_case_0_complex_construct' in integration_code
        assert 'edge_case_1_unknown_tokens' in integration_code
        assert 'result = a + b' in integration_code
        assert 'unknown_token()' in integration_code
    
    def test_generate_integration_code_empty(self):
        """Test generation of integration code with empty translations."""
        integration_code = self.augmentor.generate_integration_code({})
        
        assert integration_code == ""


if __name__ == "__main__":
    pytest.main([__file__]) 