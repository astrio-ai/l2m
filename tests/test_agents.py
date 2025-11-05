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
    tools = agent.agent.get_all_tools()
    assert len(tools) > 0


def test_translator_agent_has_tools():
    """Test that translator agent has tools."""
    agent = TranslatorAgent()
    tools = agent.agent.get_all_tools()
    assert len(tools) > 0


def test_reviewer_agent_has_tools():
    """Test that reviewer agent has tools."""
    agent = ReviewerAgent()
    tools = agent.agent.get_all_tools()
    assert len(tools) > 0


def test_tester_agent_has_tools():
    """Test that tester agent has multiple tools."""
    agent = TesterAgent()
    tools = agent.agent.get_all_tools()
    assert len(tools) >= 2  # create_tests and execute_tests
