"""
Unit tests for the COBOL parser.

This module contains unit tests for the COBOL parser
functionality and behavior.
"""

import pytest
from unittest.mock import Mock, patch
from src.language_parsers.cobol.ast_visitor import CobolASTVisitor
from src.language_parsers.base_parser import BaseParser


class TestCobolASTVisitor:
    """Test cases for the COBOL AST visitor."""
    
    @pytest.fixture
    def ast_visitor(self):
        """Create AST visitor fixture."""
        return CobolASTVisitor()
    
    def test_ast_visitor_initialization(self, ast_visitor):
        """Test AST visitor initialization."""
        assert ast_visitor is not None
        assert ast_visitor.analysis_results is not None
        assert "programs" in ast_visitor.analysis_results
        assert "procedures" in ast_visitor.analysis_results
        assert "data_items" in ast_visitor.analysis_results
    
    def test_get_analysis_results(self, ast_visitor):
        """Test analysis results retrieval."""
        results = ast_visitor.get_analysis_results()
        assert isinstance(results, dict)
        assert "programs" in results
        assert "procedures" in results
        assert "data_items" in results
        assert "file_operations" in results
        assert "control_structures" in results
    
    def test_visit_program(self, ast_visitor):
        """Test program visiting."""
        # Mock context object
        mock_ctx = Mock()
        mock_ctx.start.line = 1
        
        result = ast_visitor.visitProgram(mock_ctx)
        
        assert result is not None
        assert result["type"] == "PROGRAM"
        assert result["line_number"] == 1
    
    def test_visit_procedure(self, ast_visitor):
        """Test procedure visiting."""
        # Mock context object
        mock_ctx = Mock()
        mock_ctx.start.line = 10
        
        result = ast_visitor.visitProcedure(mock_ctx)
        
        assert result is not None
        assert result["type"] == "PROCEDURE"
        assert result["line_number"] == 10
    
    def test_visit_data_item(self, ast_visitor):
        """Test data item visiting."""
        # Mock context object
        mock_ctx = Mock()
        mock_ctx.start.line = 20
        
        result = ast_visitor.visitDataItem(mock_ctx)
        
        assert result is not None
        assert result["type"] == "DATA_ITEM"
        assert result["line_number"] == 20
    
    def test_visit_file_operation(self, ast_visitor):
        """Test file operation visiting."""
        # Mock context object
        mock_ctx = Mock()
        mock_ctx.start.line = 30
        
        result = ast_visitor.visitFileOperation(mock_ctx)
        
        assert result is not None
        assert result["line_number"] == 30
    
    def test_visit_control_structure(self, ast_visitor):
        """Test control structure visiting."""
        # Mock context object
        mock_ctx = Mock()
        mock_ctx.start.line = 40
        
        result = ast_visitor.visitControlStructure(mock_ctx)
        
        assert result is not None
        assert result["line_number"] == 40
    
    def test_extract_program_name(self, ast_visitor):
        """Test program name extraction."""
        mock_ctx = Mock()
        result = ast_visitor._extract_program_name(mock_ctx)
        assert result == "UNKNOWN_PROGRAM"
    
    def test_extract_procedure_name(self, ast_visitor):
        """Test procedure name extraction."""
        mock_ctx = Mock()
        result = ast_visitor._extract_procedure_name(mock_ctx)
        assert result == "UNKNOWN_PROCEDURE"
    
    def test_extract_data_item_name(self, ast_visitor):
        """Test data item name extraction."""
        mock_ctx = Mock()
        result = ast_visitor._extract_data_item_name(mock_ctx)
        assert result == "UNKNOWN_DATA_ITEM"
    
    def test_extract_data_type(self, ast_visitor):
        """Test data type extraction."""
        mock_ctx = Mock()
        result = ast_visitor._extract_data_type(mock_ctx)
        assert result == "UNKNOWN_TYPE"
    
    def test_extract_level(self, ast_visitor):
        """Test level extraction."""
        mock_ctx = Mock()
        result = ast_visitor._extract_level(mock_ctx)
        assert result == 0
    
    def test_extract_operation_type(self, ast_visitor):
        """Test operation type extraction."""
        mock_ctx = Mock()
        result = ast_visitor._extract_operation_type(mock_ctx)
        assert result == "UNKNOWN_OPERATION"
    
    def test_extract_file_name(self, ast_visitor):
        """Test file name extraction."""
        mock_ctx = Mock()
        result = ast_visitor._extract_file_name(mock_ctx)
        assert result == "UNKNOWN_FILE"
    
    def test_extract_control_type(self, ast_visitor):
        """Test control type extraction."""
        mock_ctx = Mock()
        result = ast_visitor._extract_control_type(mock_ctx)
        assert result == "UNKNOWN_CONTROL"
    
    def test_extract_condition(self, ast_visitor):
        """Test condition extraction."""
        mock_ctx = Mock()
        result = ast_visitor._extract_condition(mock_ctx)
        assert result == "UNKNOWN_CONDITION"


class TestBaseParser:
    """Test cases for the base parser."""
    
    def test_base_parser_initialization(self):
        """Test base parser initialization."""
        parser = BaseParser("cobol")
        assert parser.language == "cobol"
        assert parser.logger is not None
    
    def test_validate_file(self):
        """Test file validation."""
        parser = BaseParser("cobol")
        
        # Test with valid extension
        assert parser.validate_file("test.cobol")
        assert parser.validate_file("test.cbl")
        assert parser.validate_file("test.cob")
        
        # Test with invalid extension
        assert not parser.validate_file("test.txt")
        assert not parser.validate_file("test.py")
    
    def test_log_parsing_activity(self):
        """Test parsing activity logging."""
        parser = BaseParser("cobol")
        
        # This test would verify that logging works correctly
        # For now, just ensure the method exists and can be called
        parser.log_parsing_activity("test activity", {"key": "value"})
        assert True  # If we get here, no exception was raised
