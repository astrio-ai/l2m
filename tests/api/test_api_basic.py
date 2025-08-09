"""
Basic tests for the Legacy2Modern API

These tests verify the basic structure and functionality of the API.
"""

import pytest
import sys
import os
from pathlib import Path

# Add the project root to sys.path
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from api.models import (
    FrameworkType, TranspilationRequest, WebsiteModernizationRequest,
    CobolTranspilationRequest, AnalysisRequest
)
from api.services import Legacy2ModernService


class TestAPIModels:
    """Test the Pydantic models."""
    
    def test_framework_type_enum(self):
        """Test FrameworkType enum values."""
        assert FrameworkType.REACT == "react"
        assert FrameworkType.NEXTJS == "nextjs"
        assert FrameworkType.ASTRO == "astro"
    
    def test_transpilation_request(self):
        """Test TranspilationRequest model."""
        request = TranspilationRequest(
            source_code="IDENTIFICATION DIVISION...",
            target_framework=FrameworkType.REACT
        )
        assert request.source_code == "IDENTIFICATION DIVISION..."
        assert request.target_framework == FrameworkType.REACT
        assert request.analyze_only is False
    
    def test_website_modernization_request(self):
        """Test WebsiteModernizationRequest model."""
        request = WebsiteModernizationRequest(
            input_path="/path/to/input.html",
            output_dir="/path/to/output",
            target_framework=FrameworkType.NEXTJS
        )
        assert request.input_path == "/path/to/input.html"
        assert request.output_dir == "/path/to/output"
        assert request.target_framework == FrameworkType.NEXTJS
        assert request.analyze_only is False
    
    def test_cobol_transpilation_request(self):
        """Test CobolTranspilationRequest model."""
        request = CobolTranspilationRequest(
            source_code="IDENTIFICATION DIVISION...",
            output_file="/path/to/output.py"
        )
        assert request.source_code == "IDENTIFICATION DIVISION..."
        assert request.output_file == "/path/to/output.py"
    
    def test_analysis_request(self):
        """Test AnalysisRequest model."""
        request = AnalysisRequest(
            source_code="def hello(): print('Hello')",
            target_code="const hello = () => console.log('Hello')"
        )
        assert request.source_code == "def hello(): print('Hello')"
        assert request.target_code == "const hello = () => console.log('Hello')"


class TestAPIService:
    """Test the API service layer."""
    
    @pytest.fixture
    def service(self):
        """Create a service instance for testing."""
        try:
            return Legacy2ModernService()
        except Exception as e:
            pytest.skip(f"Service initialization failed: {e}")
    
    def test_service_initialization(self, service):
        """Test that the service initializes correctly."""
        assert service is not None
        assert hasattr(service, 'cobol_transpiler')
        assert hasattr(service, 'website_transpiler')
        assert hasattr(service, 'llm_agent')
    
    def test_health_status(self, service):
        """Test health status functionality."""
        health = service.get_health_status()
        assert 'status' in health
        assert 'components' in health
        assert 'timestamp' in health
        assert isinstance(health['components'], dict)
    
    def test_code_analysis(self, service):
        """Test code analysis functionality."""
        source_code = "def hello(): print('Hello')"
        result = service.analyze_code(source_code)
        
        assert result['success'] is True
        assert 'analysis' in result
        assert result['analysis']['source_code_lines'] == 1
        assert result['analysis']['source_code_length'] == len(source_code)
    
    def test_code_complexity_calculation(self, service):
        """Test code complexity calculation."""
        source_code = """# This is a comment
def hello():
    print('Hello')  # Another comment
    return True"""
        
        complexity = service._calculate_complexity(source_code)
        assert complexity['total_lines'] == 4
        assert complexity['non_empty_lines'] == 3
        assert complexity['comment_lines'] == 2
        assert complexity['avg_line_length'] > 0


class TestAPIEndpoints:
    """Test the API endpoints (requires running server)."""
    
    def test_health_endpoint_available(self):
        """Test that health endpoint documentation is available."""
        # This test verifies the endpoint structure without requiring a running server
        from api.main import app
        
        # Check if the health endpoint is registered
        routes = [route.path for route in app.routes]
        assert "/health" in routes
        
        # Check if the health endpoint is a GET request
        health_route = next(route for route in app.routes if route.path == "/health")
        assert "GET" in [method for method in health_route.methods]
    
    def test_transpile_endpoints_available(self):
        """Test that transpilation endpoints are available."""
        from api.main import app
        
        routes = [route.path for route in app.routes]
        assert "/transpile/cobol" in routes
        assert "/transpile/cobol/file" in routes
        assert "/modernize/website" in routes
        assert "/modernize/website/file" in routes
    
    def test_analysis_endpoints_available(self):
        """Test that analysis endpoints are available."""
        from api.main import app
        
        routes = [route.path for route in app.routes]
        assert "/analyze/website" in routes
        assert "/analyze/code" in routes
        assert "/frameworks" in routes


if __name__ == "__main__":
    pytest.main([__file__]) 