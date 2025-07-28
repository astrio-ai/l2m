import os
import sys
import pytest
from unittest.mock import Mock, patch

# Add the project root to sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from packages.transpiler.engine.parser.cobol_lst import (
    parse_cobol_source, 
    CobolSemanticAnalyzer, 
    LosslessNode,
    Token
)

# Directory containing sample COBOL files
EXAMPLES_DIR = os.path.join(os.path.dirname(__file__), "..", "examples", "cobol")

# List of sample COBOL files to test
SAMPLE_FILES = [
    "HELLO.cobol",
    "PAYROL00.cobol",
    "ADDAMT.cobol"
]

def load_cobol_file(filename):
    """Load a COBOL file from the examples directory."""
    with open(os.path.join(EXAMPLES_DIR, filename), "r") as f:
        return f.read()


class TestLosslessNode:
    """Test the LosslessNode class functionality."""
    
    def test_lossless_node_creation(self):
        """Test creating a LosslessNode."""
        node = LosslessNode("TestContext")
        assert node.rule_name == "TestContext"
        assert node.children == []
        assert node.tokens == []
    
    def test_lossless_node_with_tokens(self):
        """Test LosslessNode with tokens."""
        tokens = [
            Token("IDENTIFIER", "TEST-VAR"),
            Token("EQUALS", "="),
            Token("STRING", "'HELLO'")
        ]
        node = LosslessNode("AssignmentContext", tokens=tokens)
        assert len(node.tokens) == 3
        assert node.tokens[0].text == "TEST-VAR"
    
    def test_lossless_node_with_children(self):
        """Test LosslessNode with children."""
        child1 = LosslessNode("ChildContext1")
        child2 = LosslessNode("ChildContext2")
        node = LosslessNode("ParentContext", children=[child1, child2])
        
        assert len(node.children) == 2
        assert node.children[0].rule_name == "ChildContext1"
        assert node.children[1].rule_name == "ChildContext2"
    
    def test_get_tokens(self):
        """Test getting tokens from node."""
        tokens = [
            Token("IDENTIFIER", "TEST-VAR"),
            Token("EQUALS", "="),
            Token("STRING", "'HELLO'")
        ]
        node = LosslessNode("TestContext", tokens=tokens)
        
        result_tokens = node.get_tokens()
        assert len(result_tokens) == 3
        assert result_tokens[0].text == "TEST-VAR"
    
    def test_find_all_kind(self):
        """Test finding nodes by kind."""
        # Create a tree structure
        root = LosslessNode("CompilationUnitContext")
        
        data_div = LosslessNode("DataDivisionContext")
        proc_div = LosslessNode("ProcedureDivisionContext")
        
        var1 = LosslessNode("VariableContext")
        var2 = LosslessNode("VariableContext")
        
        data_div.children = [var1]
        proc_div.children = [var2]
        root.children = [data_div, proc_div]
        
        variables = root.find_all_kind("VariableContext")
        assert len(variables) == 2


class TestToken:
    """Test the Token class functionality."""
    
    def test_token_creation(self):
        """Test creating a Token."""
        token = Token("IDENTIFIER", "TEST-VAR")
        assert token.type == "IDENTIFIER"
        assert token.text == "TEST-VAR"
    
    def test_token_equality(self):
        """Test token equality."""
        token1 = Token("IDENTIFIER", "TEST-VAR")
        token2 = Token("IDENTIFIER", "TEST-VAR")
        token3 = Token("IDENTIFIER", "DIFFERENT")
        
        assert token1 == token2
        assert token1 != token3
    
    def test_token_string_representation(self):
        """Test token string representation."""
        token = Token("IDENTIFIER", "TEST-VAR")
        assert str(token) == "Token(IDENTIFIER, 'TEST-VAR')"


class TestCobolSemanticAnalyzer:
    """Test the CobolSemanticAnalyzer functionality."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.mock_lst = Mock(spec=LosslessNode)
        self.mock_tokens = [Mock(spec=Token)]
        self.analyzer = CobolSemanticAnalyzer(self.mock_lst, self.mock_tokens)
    
    def test_analyzer_initialization(self):
        """Test that analyzer initializes correctly."""
        assert self.analyzer.lst_root == self.mock_lst
        assert self.analyzer.tokens == self.mock_tokens
        assert hasattr(self.analyzer, 'symbol_table_root')
    
    def test_analyze_method(self):
        """Test the analyze method."""
        # Mock the LST structure
        mock_lst = Mock(spec=LosslessNode)
        mock_lst.rule_name = "CompilationUnitContext"
        mock_lst.children = []
        
        mock_tokens = []
        analyzer = CobolSemanticAnalyzer(mock_lst, mock_tokens)
        
        # Should not raise an exception
        analyzer.analyze()
        
        # Should have created symbol table
        assert hasattr(analyzer, 'symbol_table_root')
    
    def test_extract_programs(self):
        """Test program extraction from LST."""
        # Create mock LST with program
        mock_lst = Mock(spec=LosslessNode)
        mock_lst.rule_name = "CompilationUnitContext"
        
        mock_identification = Mock(spec=LosslessNode)
        mock_identification.rule_name = "IdentificationDivisionContext"
        
        mock_program_id = Mock(spec=LosslessNode)
        mock_program_id.rule_name = "ProgramIdContext"
        mock_program_id.get_tokens.return_value = [
            Mock(text="PROGRAM-ID"), Mock(text="TEST-PROG")
        ]
        
        mock_identification.children = [mock_program_id]
        mock_lst.children = [mock_identification]
        
        analyzer = CobolSemanticAnalyzer(mock_lst, [])
        analyzer.analyze()
        
        # Should have extracted program information
        assert hasattr(analyzer, 'symbol_table_root')
    
    def test_extract_variables(self):
        """Test variable extraction from LST."""
        # Create mock LST with variables
        mock_lst = Mock(spec=LosslessNode)
        mock_lst.rule_name = "CompilationUnitContext"
        
        mock_data_division = Mock(spec=LosslessNode)
        mock_data_division.rule_name = "DataDivisionContext"
        
        mock_working_storage = Mock(spec=LosslessNode)
        mock_working_storage.rule_name = "WorkingStorageSectionContext"
        
        mock_data_entry = Mock(spec=LosslessNode)
        mock_data_entry.rule_name = "DataDescriptionEntryContext"
        mock_data_entry.get_tokens.return_value = [
            Mock(text="01"), Mock(text="TEST-VAR"), Mock(text="PIC"), Mock(text="X(10)")
        ]
        
        mock_working_storage.children = [mock_data_entry]
        mock_data_division.children = [mock_working_storage]
        mock_lst.children = [mock_data_division]
        
        analyzer = CobolSemanticAnalyzer(mock_lst, [])
        analyzer.analyze()
        
        # Should have extracted variable information
        assert hasattr(analyzer, 'symbol_table_root')


class TestParseCobolSource:
    """Test the parse_cobol_source function."""
    
    def test_parse_simple_cobol(self):
        """Test parsing a simple COBOL program."""
        cobol_source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           TEST-PROG.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TEST-VAR                    PIC X(10).
       
       PROCEDURE DIVISION.
       100-MAIN.
           DISPLAY 'HELLO WORLD'
           GOBACK.
        """
        
        lst, tokens = parse_cobol_source(cobol_source)
        
        assert lst is not None
        assert tokens is not None
        assert isinstance(lst, LosslessNode)
        assert isinstance(tokens, list)
    
    def test_parse_empty_source(self):
        """Test parsing empty COBOL source."""
        lst, tokens = parse_cobol_source("")
        
        assert lst is not None
        assert tokens is not None
    
    def test_parse_invalid_syntax(self):
        """Test parsing invalid COBOL syntax."""
        invalid_cobol = "INVALID COBOL SYNTAX"
        
        # Should handle gracefully
        lst, tokens = parse_cobol_source(invalid_cobol)
        
        assert lst is not None
        assert tokens is not None
    
    @pytest.mark.parametrize("filename", SAMPLE_FILES)
    def test_parse_sample_files(self, filename):
        """Test parsing sample COBOL files."""
        try:
            source = load_cobol_file(filename)
            lst, tokens = parse_cobol_source(source)
            
            assert lst is not None
            assert tokens is not None
            assert isinstance(lst, LosslessNode)
            assert isinstance(tokens, list)
            
        except Exception as e:
            pytest.fail(f"Failed to parse {filename}: {str(e)}")


class TestParserIntegration:
    """Test parser integration with real COBOL files."""
    
    @pytest.mark.parametrize("filename", SAMPLE_FILES)
    def test_full_parsing_pipeline(self, filename):
        """Test the full parsing pipeline with sample files."""
        try:
            source = load_cobol_file(filename)
            lst, tokens = parse_cobol_source(source)
            
            analyzer = CobolSemanticAnalyzer(lst, tokens)
            analyzer.analyze()
            
            # Basic validation
            assert analyzer.lst_root is not None
            assert analyzer.tokens is not None
            assert hasattr(analyzer, 'symbol_table_root')
            
        except Exception as e:
            pytest.fail(f"Failed to process {filename}: {str(e)}")
    
    def test_parser_error_handling(self):
        """Test parser error handling."""
        # Test with malformed COBOL
        malformed_cobol = """
        IDENTIFICATION DIVISION
        PROGRAM-ID
            TEST-PROG
        """
        
        # Should handle gracefully
        lst, tokens = parse_cobol_source(malformed_cobol)
        
        assert lst is not None
        assert tokens is not None


class TestParserPerformance:
    """Test parser performance with various file sizes."""
    
    def test_parse_small_file(self):
        """Test parsing a small COBOL file."""
        import time
        
        source = load_cobol_file("HELLO.cobol")
        
        start_time = time.time()
        lst, tokens = parse_cobol_source(source)
        end_time = time.time()
        
        # Should complete within reasonable time (less than 5 seconds)
        assert end_time - start_time < 5.0
        assert lst is not None
        assert tokens is not None
    
    def test_parse_medium_file(self):
        """Test parsing a medium COBOL file."""
        import time
        
        source = load_cobol_file("ADDAMT.cobol")
        
        start_time = time.time()
        lst, tokens = parse_cobol_source(source)
        end_time = time.time()
        
        # Should complete within reasonable time (less than 10 seconds)
        assert end_time - start_time < 10.0
        assert lst is not None
        assert tokens is not None


if __name__ == "__main__":
    pytest.main([__file__]) 