"""
Tests for Edge Case Detection

This module tests the edge case detection functionality for identifying
complex COBOL constructs that need AI assistance.
"""

import os
import sys
import pytest
import tempfile
from unittest.mock import Mock

# Add the project root to sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from packages.transpiler.engine.edge_case_detector import EdgeCaseDetector
from packages.transpiler.engine.parser.cobol_lst import LosslessNode


class TestEdgeCaseDetector:
    """Test the EdgeCaseDetector functionality."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.detector = EdgeCaseDetector()
    
    def test_initialization(self):
        """Test that detector initializes correctly."""
        assert hasattr(self.detector, 'known_patterns')
        assert hasattr(self.detector, 'complex_constructs')
        assert hasattr(self.detector, 'edge_cases')
        assert len(self.detector.known_patterns) > 0
        assert len(self.detector.complex_constructs) > 0
    
    def test_detect_unknown_tokens(self):
        """Test detection of unknown tokens."""
        # Create a node with unknown tokens
        node = Mock(spec=LosslessNode)
        node.rule_name = "StatementContext"
        node.children = []
        
        tokens = []
        for text in ["UNKNOWN", "TOKEN", "HERE"]:
            token = Mock()
            token.text = text
            tokens.append(token)
        
        node.get_tokens.return_value = tokens
        
        edge_cases = self.detector.detect_edge_cases(node)
        
        assert len(edge_cases) > 0
        assert any(ec['type'] == 'unknown_tokens' for ec in edge_cases)
        
        # Check that unknown tokens are identified
        unknown_case = next(ec for ec in edge_cases if ec['type'] == 'unknown_tokens')
        assert 'UNKNOWN' in unknown_case['unknown_tokens']
        assert 'TOKEN' in unknown_case['unknown_tokens']
        assert 'HERE' in unknown_case['unknown_tokens']
    
    def test_detect_complex_constructs(self):
        """Test detection of complex constructs."""
        # Create a node with complex tokens
        node = Mock(spec=LosslessNode)
        node.rule_name = "StatementContext"
        node.children = []
        
        tokens = []
        for text in ["COMPUTE", "RESULT", "=", "A", "+", "B"]:
            token = Mock()
            token.text = text
            tokens.append(token)
        
        node.get_tokens.return_value = tokens
        
        edge_cases = self.detector.detect_edge_cases(node)
        
        assert len(edge_cases) > 0
        assert any(ec['type'] == 'complex_construct' for ec in edge_cases)
        
        # Check that complex tokens are identified
        complex_case = next(ec for ec in edge_cases if ec['type'] == 'complex_construct')
        assert 'COMPUTE' in complex_case['complex_tokens']
    
    def test_detect_nested_structure(self):
        """Test detection of complex nested structures."""
        # Create a node with many children
        node = Mock(spec=LosslessNode)
        node.rule_name = "ComplexStatementContext"
        node.children = [Mock() for _ in range(5)]  # More than 3 children
        
        tokens = []
        for text in ["COMPLEX", "NESTED", "STRUCTURE"]:
            token = Mock()
            token.text = text
            tokens.append(token)
        
        node.get_tokens.return_value = tokens
        
        # Mock get_tokens for children
        for child in node.children:
            child.get_tokens.return_value = []
            child.children = []
        
        edge_cases = self.detector.detect_edge_cases(node)
        
        assert len(edge_cases) > 0
        assert any(ec['type'] == 'complex_nested_structure' for ec in edge_cases)
        
        # Check nested structure details
        nested_case = next(ec for ec in edge_cases if ec['type'] == 'complex_nested_structure')
        assert nested_case['child_count'] == 5
        assert nested_case['token_count'] == 3
    
    def test_detect_long_statement(self):
        """Test detection of very long statements."""
        # Create a node with many tokens
        node = Mock(spec=LosslessNode)
        node.rule_name = "StatementContext"
        node.children = []
        
        tokens = []
        for i in range(25):  # More than 20 tokens
            token = Mock()
            token.text = f"TOKEN{i}"
            tokens.append(token)
        
        node.get_tokens.return_value = tokens
        
        edge_cases = self.detector.detect_edge_cases(node)
        
        assert len(edge_cases) > 0
        assert any(ec['type'] == 'long_statement' for ec in edge_cases)
        
        # Check long statement details
        long_case = next(ec for ec in edge_cases if ec['type'] == 'long_statement')
        assert long_case['token_count'] == 25
        assert len(long_case['tokens']) == 15  # Should be capped at 15
    
    def test_log_edge_cases(self):
        """Test logging of edge cases."""
        # Create some edge cases
        edge_cases = [
            {
                'type': 'unknown_tokens',
                'node_type': 'StatementContext',
                'context': 'test',
                'unknown_tokens': ['UNKNOWN1', 'UNKNOWN2'],
                'all_tokens': ['UNKNOWN1', 'UNKNOWN2', 'KNOWN'],
                'severity': 'high'
            },
            {
                'type': 'complex_construct',
                'node_type': 'StatementContext',
                'context': 'test',
                'complex_tokens': ['COMPUTE'],
                'all_tokens': ['COMPUTE', 'A', 'B'],
                'severity': 'high'
            }
        ]
        
        # Test logging (should not raise exceptions)
        self.detector.log_edge_cases(edge_cases, "test.cobol")
        
        # Check that edge cases are stored
        assert len(self.detector.edge_cases) == 2
    
    def test_generate_edge_case_report(self):
        """Test generation of edge case report."""
        # Create some edge cases
        edge_cases = [
            {
                'type': 'unknown_tokens',
                'node_type': 'StatementContext',
                'context': 'test',
                'unknown_tokens': ['UNKNOWN1', 'UNKNOWN2'],
                'all_tokens': ['UNKNOWN1', 'UNKNOWN2', 'KNOWN'],
                'severity': 'high'
            },
            {
                'type': 'complex_construct',
                'node_type': 'StatementContext',
                'context': 'test',
                'complex_tokens': ['COMPUTE'],
                'all_tokens': ['COMPUTE', 'A', 'B'],
                'severity': 'high'
            }
        ]
        
        self.detector.edge_cases = edge_cases
        
        # Generate report to temporary file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as temp_file:
            report_path = temp_file.name
        
        try:
            self.detector.generate_edge_case_report(report_path)
            
            # Check that report was generated
            assert os.path.exists(report_path)
            
            with open(report_path, 'r') as f:
                content = f.read()
                assert "COBOL Edge Cases Report" in content
                assert "UNKNOWN_TOKENS" in content
                assert "COMPLEX_CONSTRUCT" in content
                assert "UNKNOWN1" in content
                assert "COMPUTE" in content
        
        finally:
            # Clean up
            if os.path.exists(report_path):
                os.unlink(report_path)
    
    def test_get_ai_processing_candidates(self):
        """Test getting AI processing candidates."""
        # Create edge cases with different severities
        edge_cases = [
            {
                'type': 'unknown_tokens',
                'severity': 'high'
            },
            {
                'type': 'complex_construct',
                'severity': 'high'
            },
            {
                'type': 'long_statement',
                'severity': 'medium'
            }
        ]
        
        self.detector.edge_cases = edge_cases
        
        candidates = self.detector.get_ai_processing_candidates()
        
        # Should return high severity cases and complex constructs
        assert len(candidates) == 2
        assert all(ec['severity'] == 'high' or ec['type'] in ['complex_construct', 'unknown_tokens'] 
                  for ec in candidates)
    
    def test_clear_edge_cases(self):
        """Test clearing of edge cases."""
        # Add some edge cases
        self.detector.edge_cases = [{'type': 'test', 'severity': 'low'}]
        assert len(self.detector.edge_cases) == 1
        
        # Clear them
        self.detector.clear_edge_cases()
        assert len(self.detector.edge_cases) == 0


class TestEdgeCaseDetectorIntegration:
    """Test integration of edge case detector with real COBOL files."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.detector = EdgeCaseDetector()
    
    def test_detect_edge_cases_in_complex_cobol(self):
        """Test edge case detection in complex COBOL code."""
        # Create a complex COBOL-like structure
        root_node = Mock(spec=LosslessNode)
        root_node.rule_name = "CompilationUnitContext"
        
        # Create a complex statement with unknown tokens
        complex_node = Mock(spec=LosslessNode)
        complex_node.rule_name = "StatementContext"
        complex_node.children = []
        
        tokens = []
        for text in ["COMPUTE", "RESULT", "=", "A", "*", "B", "+", "C", "/", "D"]:
            token = Mock()
            token.text = text
            tokens.append(token)
        
        complex_node.get_tokens.return_value = tokens
        
        root_node.children = [complex_node]
        root_node.get_tokens.return_value = []
        
        edge_cases = self.detector.detect_edge_cases(root_node)
        
        # Should detect complex construct
        assert len(edge_cases) > 0
        assert any(ec['type'] == 'complex_construct' for ec in edge_cases)
        
        # Log the edge cases
        self.detector.log_edge_cases(edge_cases, "complex_test.cobol")
        
        # Check that candidates are identified for AI processing
        candidates = self.detector.get_ai_processing_candidates()
        assert len(candidates) > 0


if __name__ == "__main__":
    pytest.main([__file__]) 