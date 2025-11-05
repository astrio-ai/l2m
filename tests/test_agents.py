"""Tests for agent functionality."""

import pytest
from src.agents.analyzer_agent import AnalyzerAgent
from src.agents.translator_agent import TranslatorAgent


@pytest.mark.asyncio
async def test_analyzer_agent():
    """Test analyzer agent initialization and basic functionality."""
    agent = AnalyzerAgent()
    assert agent is not None
    assert agent.agent is not None


@pytest.mark.asyncio
async def test_translator_agent():
    """Test translator agent initialization."""
    agent = TranslatorAgent()
    assert agent is not None
    assert agent.agent is not None

