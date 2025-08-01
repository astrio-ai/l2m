"""
Test LLM Integration

This module tests the LLM integration capabilities including
analysis, optimization, and code review features.
"""

import os
import sys
import pytest
from unittest.mock import Mock, patch

# Add the project root to sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from packages.transpiler.engine.llm_augmentor import LLMConfig, LLMAugmentor
from packages.llm_agent import LLMAgent, AnalysisResult, OptimizationResult, ReviewResult


class TestLLMIntegration:
    """Test LLM integration capabilities."""
    
    def setup_method(self):
        """Set up test fixtures."""
        # Create a mock LLM config for testing
        self.mock_config = LLMConfig(
            api_key="test-key",
            model="gpt-4",
            temperature=0.1,
            provider="openai",
            cache_enabled=True,
            cache_ttl=3600,
            retry_attempts=3,
            retry_delay=1.0
        )
    
    def test_llm_config_from_env(self):
        """Test LLM configuration creation from environment variables."""
        with patch.dict(os.environ, {
            'LLM_API_KEY': 'test-key',
            'LLM_MODEL': 'gpt-4',
            'LLM_PROVIDER': 'openai',
            'DEFAULT_LLM_TEMPERATURE': '0.1',
            'LLM_CACHE_ENABLED': 'true',
            'LLM_CACHE_TTL': '3600',
            'LLM_RETRY_ATTEMPTS': '3',
            'LLM_RETRY_DELAY': '1.0'
        }):
            config = LLMConfig.from_env()
            assert config.api_key == 'test-key'
            assert config.model == 'gpt-4'
            assert config.provider == 'openai'
            assert config.cache_enabled is True
            assert config.cache_ttl == 3600
    
    def test_llm_augmentor_initialization(self):
        """Test LLM augmentor initialization."""
        augmentor = LLMAugmentor(self.mock_config)
        assert augmentor.config == self.mock_config
        assert augmentor.can_augment() is True
    
    def test_llm_agent_initialization(self):
        """Test LLM agent initialization."""
        with patch('packages.llm_agent.agent.LLMAgent._create_provider') as mock_create:
            mock_create.return_value = Mock()
            agent = LLMAgent(self.mock_config)
            assert agent.config == self.mock_config
            assert agent.llm_augmentor is not None
    
    def test_cache_functionality(self):
        """Test response caching functionality."""
        augmentor = LLMAugmentor(self.mock_config)
        
        # Test cache operations
        test_key = "test_cache_key"
        test_response = "cached response"
        
        # Set cache
        augmentor.cache.set(test_key, test_response)
        
        # Get from cache
        cached = augmentor.cache.get(test_key)
        assert cached == test_response
        
        # Test cache stats
        stats = augmentor.get_cache_stats()
        assert stats["enabled"] is True
        assert stats["size"] == 1
    
    def test_enhanced_prompt_generation(self):
        """Test enhanced prompt generation."""
        augmentor = LLMAugmentor(self.mock_config)
        
        edge_case = {
            'type': 'complex_construct',
            'all_tokens': ['COMPUTE', 'RESULT', '=', 'A', '*', 'B'],
            'context': 'Mathematical computation',
            'severity': 'high'
        }
        
        prompt = augmentor._create_enhanced_prompt(edge_case)
        
        assert 'COBOL to Python translator' in prompt
        assert 'complex_construct' in prompt
        assert 'COMPUTE' in prompt
        assert 'REQUIREMENTS' in prompt
    
    def test_system_message_generation(self):
        """Test system message generation."""
        augmentor = LLMAugmentor(self.mock_config)
        
        system_message = augmentor._create_system_message()
        
        assert 'COBOL to Python translator' in system_message
        assert 'modern, maintainable Python code' in system_message
        assert 'Python best practices' in system_message
    
    @patch('packages.transpiler.engine.llm_augmentor.openai')
    def test_translate_edge_case_with_mock(self, mock_openai):
        """Test edge case translation with mocked OpenAI."""
        # Mock OpenAI response
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message.content = "result = a * b"
        mock_openai.ChatCompletion.create.return_value = mock_response
        
        augmentor = LLMAugmentor(self.mock_config)
        
        edge_case = {
            'type': 'compute_statement',
            'all_tokens': ['COMPUTE', 'RESULT', '=', 'A', '*', 'B'],
            'context': 'Mathematical computation'
        }
        
        result = augmentor.translate_edge_case(edge_case)
        
        assert result == "result = a * b"
        mock_openai.ChatCompletion.create.assert_called_once()
    
    def test_fallback_analysis(self):
        """Test fallback analysis when AI is not available."""
        # Create config without API key
        no_ai_config = LLMConfig(
            api_key="",
            model="gpt-4",
            temperature=0.1
        )
        
        augmentor = LLMAugmentor(no_ai_config)
        agent = LLMAgent(no_ai_config)
        
        source_code = "DISPLAY 'HELLO WORLD'"
        target_code = "print('HELLO WORLD')"
        
        # Test fallback analysis
        analysis = agent.analyze_code(source_code, target_code, "cobol-python")
        
        assert isinstance(analysis, AnalysisResult)
        assert analysis.complexity_score > 0
        assert analysis.maintainability_score > 0
        assert analysis.confidence < 1.0  # Lower confidence for fallback
    
    def test_fallback_optimization(self):
        """Test fallback optimization when AI is not available."""
        no_ai_config = LLMConfig(
            api_key="",
            model="gpt-4",
            temperature=0.1
        )
        
        agent = LLMAgent(no_ai_config)
        
        code = "print('Hello World')"
        
        # Test fallback optimization
        optimization = agent.optimize_code(code, "python")
        
        assert isinstance(optimization, OptimizationResult)
        assert optimization.original_code == code
        assert optimization.optimized_code == code  # No changes in fallback
        assert optimization.confidence < 1.0
    
    def test_fallback_review(self):
        """Test fallback review when AI is not available."""
        no_ai_config = LLMConfig(
            api_key="",
            model="gpt-4",
            temperature=0.1
        )
        
        agent = LLMAgent(no_ai_config)
        
        code = "print('Hello World')"
        
        # Test fallback review
        review = agent.review_code(code, "python")
        
        assert isinstance(review, ReviewResult)
        assert review.severity == "medium"
        assert review.confidence < 1.0
        assert len(review.issues) > 0  # Should have fallback message
    
    def test_agent_capabilities(self):
        """Test agent capabilities reporting."""
        agent = LLMAgent(self.mock_config)
        
        capabilities = agent.get_agent_capabilities()
        
        assert "llm_available" in capabilities
        assert "provider" in capabilities
        assert "model" in capabilities
        assert "cache_enabled" in capabilities
        assert "capabilities" in capabilities
        
        expected_capabilities = [
            "code_analysis",
            "code_optimization",
            "code_review",
            "documentation_generation",
            "transformation_validation"
        ]
        
        for capability in expected_capabilities:
            assert capability in capabilities["capabilities"]
    
    def test_cache_clear_functionality(self):
        """Test cache clearing functionality."""
        augmentor = LLMAugmentor(self.mock_config)
        
        # Add some test data to cache
        augmentor.cache.set("test1", "response1")
        augmentor.cache.set("test2", "response2")
        
        assert len(augmentor.cache.cache) == 2
        
        # Clear cache
        augmentor.clear_cache()
        
        assert len(augmentor.cache.cache) == 0
    
    def test_retry_logic(self):
        """Test retry logic with exponential backoff."""
        augmentor = LLMAugmentor(self.mock_config)
        
        # Mock provider that fails twice then succeeds
        mock_provider = Mock()
        mock_provider.generate_response.side_effect = [
            Exception("API Error"),
            Exception("API Error"),
            "Success response"
        ]
        
        augmentor.provider = mock_provider
        
        messages = [{"role": "user", "content": "test"}]
        
        # Test retry logic
        result = augmentor._generate_with_retry(messages)
        
        assert result == "Success response"
        assert mock_provider.generate_response.call_count == 3
    
    def test_cli_integration(self):
        """Test CLI integration."""
        # This test would require more complex mocking of the CLI
        # For now, we'll test the basic structure
        from packages.cli.cli import create_llm_config, setup_logging
        
        # Test config creation
        with patch.dict(os.environ, {'LLM_API_KEY': 'test-key'}):
            config = create_llm_config()
            assert config is not None
        
        # Test logging setup
        setup_logging(verbose=True)
        # Should not raise any exceptions


if __name__ == "__main__":
    pytest.main([__file__]) 