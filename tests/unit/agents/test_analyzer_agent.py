"""
Unit tests for the analyzer agent.

This module contains unit tests for the analyzer agent
functionality and behavior.
"""

import pytest
from unittest.mock import Mock, patch, AsyncMock
from src.core.agents.analyzer_agent import AnalyzerAgent
from src.core.state.agent_state import AgentState
from src.config.settings import Settings


class TestAnalyzerAgent:
    """Test cases for the analyzer agent."""
    
    @pytest.fixture
    def settings(self):
        """Create settings fixture."""
        return Settings()
    
    @pytest.fixture
    def analyzer_agent(self, settings):
        """Create analyzer agent fixture."""
        return AnalyzerAgent(settings)
    
    @pytest.fixture
    def mock_state(self):
        """Create mock agent state."""
        return AgentState(
            agent_id="analyzer",
            agent_type="analyzer",
            codebase_path="/test/codebase",
            target_language="python",
            modernization_goals=["maintainability"]
        )
    
    def test_analyzer_agent_initialization(self, analyzer_agent):
        """Test analyzer agent initialization."""
        assert analyzer_agent is not None
        assert analyzer_agent.settings is not None
        assert len(analyzer_agent.tools) > 0
    
    def test_get_system_prompt(self, analyzer_agent):
        """Test system prompt generation."""
        prompt = analyzer_agent.get_system_prompt()
        assert isinstance(prompt, str)
        assert len(prompt) > 0
        assert "analyzer" in prompt.lower()
    
    @pytest.mark.asyncio
    async def test_run_analyzer_agent(self, analyzer_agent, mock_state):
        """Test analyzer agent execution."""
        with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
            # Mock tool responses
            mock_use_tool.side_effect = [
                {"files": ["file1.cobol", "file2.cobol"]},
                {"dependencies": ["dep1", "dep2"]},
                {"patterns": ["pattern1", "pattern2"]}
            ]
            
            result = await analyzer_agent.run(mock_state)
            
            assert result is not None
            assert result.analysis_results is not None
            assert "structure" in result.analysis_results
            assert "dependencies" in result.analysis_results
            assert "patterns" in result.analysis_results
    
    @pytest.mark.asyncio
    async def test_run_analyzer_agent_error(self, analyzer_agent, mock_state):
        """Test analyzer agent error handling."""
        with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = Exception("Tool error")
            
            result = await analyzer_agent.run(mock_state)
            
            assert result is not None
            assert result.error is not None
            assert "Tool error" in result.error
    
    def test_get_available_tools(self, analyzer_agent):
        """Test available tools retrieval."""
        tools = analyzer_agent.get_available_tools()
        assert isinstance(tools, list)
        assert len(tools) > 0
    
    @pytest.mark.asyncio
    async def test_use_tool(self, analyzer_agent):
        """Test tool usage."""
        with patch.object(analyzer_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            result = await analyzer_agent.use_tool("test_tool", param1="value1")
            
            assert result is not None
            assert result["result"] == "test"
    
    def test_log_activity(self, analyzer_agent):
        """Test activity logging."""
        # This test would verify that logging works correctly
        # For now, just ensure the method exists and can be called
        analyzer_agent.log_activity("test activity", {"key": "value"})
        assert True  # If we get here, no exception was raised
