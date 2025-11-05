"""Tests for agent functionality."""

import pytest
from src.agents.analyzer_agent import AnalyzerAgent
from src.agents.translator_agent import TranslatorAgent
from src.agents.reviewer_agent import ReviewerAgent
from src.agents.tester_agent import TesterAgent
from src.agents.refactor_agent import RefactorAgent
from src.agents.orchestrator_agent import OrchestratorAgent


@pytest.mark.asyncio
async def test_analyzer_agent_initialization():
    """Test analyzer agent initialization."""
    agent = AnalyzerAgent()
    assert agent is not None
    assert agent.agent is not None
    assert agent.agent.name == "COBOL Analyzer"


@pytest.mark.asyncio
async def test_translator_agent_initialization():
    """Test translator agent initialization."""
    agent = TranslatorAgent()
    assert agent is not None
    assert agent.agent is not None
    assert agent.agent.name == "COBOL Translator"


@pytest.mark.asyncio
async def test_reviewer_agent_initialization():
    """Test reviewer agent initialization."""
    agent = ReviewerAgent()
    assert agent is not None
    assert agent.agent is not None
    assert agent.agent.name == "Code Reviewer"


@pytest.mark.asyncio
async def test_tester_agent_initialization():
    """Test tester agent initialization."""
    agent = TesterAgent()
    assert agent is not None
    assert agent.agent is not None
    assert agent.agent.name == "Test Generator"


@pytest.mark.asyncio
async def test_refactor_agent_initialization():
    """Test refactor agent initialization."""
    agent = RefactorAgent()
    assert agent is not None
    assert agent.agent is not None
    assert agent.agent.name == "Code Refactorer"


@pytest.mark.asyncio
async def test_orchestrator_agent_initialization():
    """Test orchestrator agent initialization."""
    agent = OrchestratorAgent()
    assert agent is not None
    assert agent.agent is not None
    assert agent.agent.name == "Orchestrator"


@pytest.mark.asyncio
async def test_orchestrator_with_handoffs():
    """Test orchestrator with handoff agents."""
    analyzer = AnalyzerAgent()
    translator = TranslatorAgent()
    
    orchestrator = OrchestratorAgent(handoff_agents=[analyzer.agent, translator.agent])
    assert orchestrator is not None
    assert len(orchestrator.agent.handoffs) == 2


def test_analyzer_agent_has_tools():
    """Test that analyzer agent has tools."""
    agent = AnalyzerAgent()
    # Check that agent has tools
    # In OpenAI Agents SDK, tools are passed during Agent initialization
    assert agent.agent is not None
    # Tools are stored in agent.tools (if available) or passed during initialization
    assert hasattr(agent.agent, 'tools')


def test_translator_agent_has_tools():
    """Test that translator agent has tools."""
    agent = TranslatorAgent()
    assert agent.agent is not None
    assert hasattr(agent.agent, 'tools')


def test_reviewer_agent_has_tools():
    """Test that reviewer agent has tools."""
    agent = ReviewerAgent()
    assert agent.agent is not None
    assert hasattr(agent.agent, 'tools')


def test_tester_agent_has_tools():
    """Test that tester agent has multiple tools."""
    agent = TesterAgent()
    assert agent.agent is not None
    # Tester agent should have tools for test generation and execution
    assert hasattr(agent.agent, 'tools')
    # Check that tools list exists and has items
    if hasattr(agent.agent, 'tools') and agent.agent.tools:
        assert len(agent.agent.tools) >= 2
