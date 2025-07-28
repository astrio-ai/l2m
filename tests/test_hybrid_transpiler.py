"""
Tests for Hybrid Transpiler

This module tests the hybrid transpiler that combines rule-based
and AI-assisted translation.
"""

import os
import sys
import pytest
from unittest.mock import Mock, patch

# Add the project root to sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from packages.transpiler.engine.hybrid_transpiler import HybridTranspiler
from packages.transpiler.engine.llm_augmentor import LLMConfig


class TestHybridTranspiler:
    """Test the HybridTranspiler class."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.llm_config = LLMConfig(
            api_key='test_key',
            model='gpt-4',
            temperature=0.1
        )
        self.transpiler = HybridTranspiler(self.llm_config)
    
    def test_initialization(self):
        """Test that hybrid transpiler initializes correctly."""
        assert hasattr(self.transpiler, 'rule_based_transpiler')
        assert hasattr(self.transpiler, 'edge_case_detector')
        assert hasattr(self.transpiler, 'llm_augmentor')
        assert hasattr(self.transpiler, 'ai_translations')
        assert hasattr(self.transpiler, 'edge_cases')
    
    def test_detect_edge_cases(self):
        """Test edge case detection."""
        cobol_source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           TEST-PROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  COUNTER                    PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       100-MAIN.
           DISPLAY 'HELLO WORLD'
           GOBACK.
        """
        
        edge_cases = self.transpiler.detect_edge_cases(cobol_source)
        
        # Should detect some edge cases (unknown tokens)
        assert isinstance(edge_cases, list)
        assert len(edge_cases) > 0
    
    def test_apply_rule_based_translation(self):
        """Test rule-based translation."""
        cobol_source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           TEST-PROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  COUNTER                    PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       100-MAIN.
           DISPLAY 'HELLO WORLD'
           GOBACK.
        """
        
        result = self.transpiler.apply_rule_based_translation(cobol_source, "test.cobol")
        
        assert isinstance(result, str)
        assert "def main():" in result
        assert "display" in result.lower()
    
    @patch('packages.transpiler.engine.llm_augmentor.openai.ChatCompletion.create')
    def test_apply_ai_translation(self, mock_create):
        """Test AI-assisted translation."""
        # Mock successful API response
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message.content = "result = a + b"
        mock_create.return_value = mock_response
        
        edge_cases = [
            {
                'type': 'complex_construct',
                'severity': 'high',
                'all_tokens': ['COMPUTE', 'RESULT', '=', 'A', '+', 'B']
            },
            {
                'type': 'unknown_tokens',
                'severity': 'high',
                'all_tokens': ['UNKNOWN', 'TOKEN']
            }
        ]
        
        translations = self.transpiler.apply_ai_translation(edge_cases)
        
        assert len(translations) == 2
        assert 'edge_case_0_complex_construct' in translations
        assert 'edge_case_1_unknown_tokens' in translations
    
    def test_apply_ai_translation_no_api_key(self):
        """Test AI translation without API key."""
        config = LLMConfig(api_key='', model='gpt-4', temperature=0.1)
        transpiler = HybridTranspiler(config)
        
        edge_cases = [
            {
                'type': 'complex_construct',
                'severity': 'high',
                'all_tokens': ['COMPUTE', 'RESULT', '=', 'A', '+', 'B']
            }
        ]
        
        translations = transpiler.apply_ai_translation(edge_cases)
        
        assert translations == {}
    
    def test_apply_ai_translation_no_high_priority_cases(self):
        """Test AI translation with no high-priority cases."""
        edge_cases = [
            {
                'type': 'long_statement',
                'severity': 'medium',
                'all_tokens': ['SIMPLE', 'STATEMENT']
            }
        ]
        
        translations = self.transpiler.apply_ai_translation(edge_cases)
        
        assert translations == {}
    
    def test_integrate_translations(self):
        """Test integration of rule-based and AI translations."""
        rule_based_code = """
# Generated Python code from COBOL

# Variable declarations
counter = ''

def para_100_main():
    global counter
    print('HELLO WORLD')
    return

def main():
    para_100_main()

if __name__ == '__main__':
    main()
        """
        
        ai_translations = {
            'edge_case_0_complex_construct': 'result = a + b',
            'edge_case_1_unknown_tokens': 'unknown_token()'
        }
        
        result = self.transpiler.integrate_translations(rule_based_code, ai_translations)
        
        assert '# AI-Generated Code Snippets' in result
        assert '# ========================' in result
        assert 'edge_case_0_complex_construct' in result
        assert 'edge_case_1_unknown_tokens' in result
        assert 'result = a + b' in result
        assert 'unknown_token()' in result
        assert 'def main():' in result
    
    def test_integrate_translations_no_ai_translations(self):
        """Test integration with no AI translations."""
        rule_based_code = """
# Generated Python code from COBOL

def main():
    print('HELLO WORLD')
        """
        
        result = self.transpiler.integrate_translations(rule_based_code, {})
        
        assert result == rule_based_code
    
    def test_integrate_translations_no_main_function(self):
        """Test integration when no main function is found."""
        rule_based_code = """
# Generated Python code from COBOL

print('HELLO WORLD')
        """
        
        ai_translations = {
            'edge_case_0_complex_construct': 'result = a + b'
        }
        
        result = self.transpiler.integrate_translations(rule_based_code, ai_translations)
        
        assert '# AI-Generated Code Snippets' in result
        assert 'result = a + b' in result
    
    def test_get_translation_stats(self):
        """Test getting translation statistics."""
        # Set up some edge cases and translations
        self.transpiler.edge_cases = [
            {'type': 'complex_construct', 'severity': 'high'},
            {'type': 'unknown_tokens', 'severity': 'high'},
            {'type': 'long_statement', 'severity': 'medium'}
        ]
        self.transpiler.ai_translations = {
            'edge_case_0_complex_construct': 'result = a + b',
            'edge_case_1_unknown_tokens': 'unknown_token()'
        }
        
        stats = self.transpiler.get_translation_stats()
        
        assert stats['total_edge_cases'] == 3
        assert stats['ai_translations'] == 2
        assert stats['llm_available'] == True
        assert 'complex_construct' in stats['edge_case_types']
        assert 'unknown_tokens' in stats['edge_case_types']
        assert 'long_statement' in stats['edge_case_types']
        assert stats['severity_distribution']['high'] == 2
        assert stats['severity_distribution']['medium'] == 1
    
    def test_generate_translation_report(self, tmp_path):
        """Test generation of translation report."""
        # Set up some edge cases and translations
        self.transpiler.edge_cases = [
            {'type': 'complex_construct', 'severity': 'high'},
            {'type': 'unknown_tokens', 'severity': 'high'}
        ]
        self.transpiler.ai_translations = {
            'edge_case_0_complex_construct': 'result = a + b',
            'edge_case_1_unknown_tokens': 'unknown_token()'
        }
        
        report_file = tmp_path / "translation_report.txt"
        self.transpiler.generate_translation_report(str(report_file))
        
        assert report_file.exists()
        
        with open(report_file, 'r') as f:
            content = f.read()
            assert "COBOL to Python Translation Report" in content
            assert "Total edge cases: 2" in content
            assert "Total AI translations: 2" in content
            assert "complex_construct" in content
            assert "unknown_tokens" in content
            assert "result = a + b" in content
            assert "unknown_token()" in content


class TestHybridTranspilerIntegration:
    """Test integration of hybrid transpiler with real COBOL files."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.llm_config = LLMConfig(
            api_key='test_key',
            model='gpt-4',
            temperature=0.1
        )
        self.transpiler = HybridTranspiler(self.llm_config)
    
    @patch('packages.transpiler.engine.llm_augmentor.openai.ChatCompletion.create')
    def test_transpile_source_with_ai_assistance(self, mock_create):
        """Test transpiling source with AI assistance."""
        # Mock successful API response
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message.content = "result = a + b"
        mock_create.return_value = mock_response
        
        cobol_source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           TEST-PROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  COUNTER                    PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       100-MAIN.
           DISPLAY 'HELLO WORLD'
           GOBACK.
        """
        
        result = self.transpiler.transpile_source(cobol_source, "test.cobol")
        
        assert isinstance(result, str)
        assert "def main():" in result
        assert "display" in result.lower()
        # Should include AI-generated code if edge cases were detected
        if self.transpiler.ai_translations:
            assert "# AI-Generated Code Snippets" in result
    
    def test_transpile_source_without_ai_assistance(self):
        """Test transpiling source without AI assistance."""
        config = LLMConfig(api_key='', model='gpt-4', temperature=0.1)
        transpiler = HybridTranspiler(config)
        
        cobol_source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           TEST-PROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  COUNTER                    PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       100-MAIN.
           DISPLAY 'HELLO WORLD'
           GOBACK.
        """
        
        result = transpiler.transpile_source(cobol_source, "test.cobol")
        
        assert isinstance(result, str)
        assert "def main():" in result
        assert "display" in result.lower()
        # Should not include AI-generated code
        assert "# AI-Generated Code Snippets" not in result


if __name__ == "__main__":
    pytest.main([__file__]) 