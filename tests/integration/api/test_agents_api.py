"""
Integration tests for agents API endpoints.

This module contains integration tests for the agents API
endpoints and functionality.
"""

import pytest
from fastapi.testclient import TestClient
from unittest.mock import Mock, patch
from src.api.server import create_app


class TestAgentsAPI:
    """Test cases for the agents API."""
    
    @pytest.fixture
    def app(self):
        """Create FastAPI app fixture."""
        return create_app()
    
    @pytest.fixture
    def client(self, app):
        """Create test client fixture."""
        return TestClient(app)
    
    def test_list_agents(self, client):
        """Test listing all agents."""
        response = client.get("/api/v1/agents/")
        
        assert response.status_code == 200
        data = response.json()
        assert "agents" in data
        assert len(data["agents"]) > 0
        
        # Verify agent structure
        for agent in data["agents"]:
            assert "id" in agent
            assert "name" in agent
            assert "description" in agent
    
    def test_run_agent_success(self, client):
        """Test successful agent execution."""
        with patch("src.api.routes.agents._get_agent_instance") as mock_get_agent:
            # Mock agent instance
            mock_agent = Mock()
            mock_agent.run.return_value = {"success": True, "result": "test"}
            mock_get_agent.return_value = mock_agent
            
            request_data = {
                "agent_type": "analyzer",
                "parameters": {
                    "codebase_path": "/test/cobol",
                    "analysis_type": "full"
                }
            }
            
            response = client.post("/api/v1/agents/run", json=request_data)
            
            assert response.status_code == 200
            data = response.json()
            assert data["agent_id"] == "analyzer"
            assert data["agent_type"] == "analyzer"
            assert data["status"] == "completed"
            assert data["result"]["success"] is True
    
    def test_run_agent_not_found(self, client):
        """Test agent not found error."""
        with patch("src.api.routes.agents._get_agent_instance") as mock_get_agent:
            mock_get_agent.return_value = None
            
            request_data = {
                "agent_type": "nonexistent",
                "parameters": {}
            }
            
            response = client.post("/api/v1/agents/run", json=request_data)
            
            assert response.status_code == 404
            data = response.json()
            assert "detail" in data
            assert "not found" in data["detail"].lower()
    
    def test_run_agent_error(self, client):
        """Test agent execution error."""
        with patch("src.api.routes.agents._get_agent_instance") as mock_get_agent:
            # Mock agent instance with error
            mock_agent = Mock()
            mock_agent.run.side_effect = Exception("Agent error")
            mock_get_agent.return_value = mock_agent
            
            request_data = {
                "agent_type": "analyzer",
                "parameters": {}
            }
            
            response = client.post("/api/v1/agents/run", json=request_data)
            
            assert response.status_code == 500
            data = response.json()
            assert "detail" in data
            assert "Agent error" in data["detail"]
    
    def test_get_agent_status(self, client):
        """Test getting agent status."""
        response = client.get("/api/v1/agents/analyzer/status")
        
        assert response.status_code == 200
        data = response.json()
        assert "agent_id" in data
        assert "status" in data
        assert data["agent_id"] == "analyzer"
    
    def test_get_agent_tools(self, client):
        """Test getting agent tools."""
        with patch("src.api.routes.agents._get_agent_instance") as mock_get_agent:
            # Mock agent instance
            mock_agent = Mock()
            mock_agent.get_available_tools.return_value = ["tool1", "tool2"]
            mock_get_agent.return_value = mock_agent
            
            response = client.get("/api/v1/agents/analyzer/tools")
            
            assert response.status_code == 200
            data = response.json()
            assert "agent_id" in data
            assert "tools" in data
            assert data["agent_id"] == "analyzer"
            assert len(data["tools"]) == 2
    
    def test_get_agent_tools_not_found(self, client):
        """Test getting tools for non-existent agent."""
        with patch("src.api.routes.agents._get_agent_instance") as mock_get_agent:
            mock_get_agent.return_value = None
            
            response = client.get("/api/v1/agents/nonexistent/tools")
            
            assert response.status_code == 404
            data = response.json()
            assert "detail" in data
            assert "not found" in data["detail"].lower()
    
    def test_get_agent_tools_error(self, client):
        """Test getting agent tools error."""
        with patch("src.api.routes.agents._get_agent_instance") as mock_get_agent:
            # Mock agent instance with error
            mock_agent = Mock()
            mock_agent.get_available_tools.side_effect = Exception("Tools error")
            mock_get_agent.return_value = mock_agent
            
            response = client.get("/api/v1/agents/analyzer/tools")
            
            assert response.status_code == 500
            data = response.json()
            assert "detail" in data
            assert "Tools error" in data["detail"]
