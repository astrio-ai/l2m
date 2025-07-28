import os
import sys
import pytest
import tempfile
from unittest.mock import Mock, patch

# Add the project root to sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from packages.transpiler.engine.cobol_transpiler import CobolTranspiler, transpile_cobol_file
from packages.transpiler.engine.parser.cobol_lst import CobolSemanticAnalyzer, LosslessNode

# Directory containing sample COBOL files
EXAMPLES_DIR = os.path.join(os.path.dirname(__file__), "..", "examples", "cobol")

# List of sample COBOL files to test
SAMPLE_FILES = [
    "HELLO.cobol",
    "PAYROL00.cobol",
    "ADDAMT.cobol",
    "IF_TEST.cobol",
    "PERFORM_TEST.cobol"
]

def load_cobol_file(filename):
    """Load a COBOL file from the examples directory."""
    with open(os.path.join(EXAMPLES_DIR, filename), "r") as f:
        return f.read()


class TestCobolTranspiler:
    """Test the COBOL transpiler functionality."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.transpiler = CobolTranspiler()
    
    def test_transpiler_initialization(self):
        """Test that transpiler initializes correctly."""
        assert self.transpiler.variables == {}
        assert self.transpiler.generated_code == []
        assert self.transpiler.indent_level == 0
        assert self.transpiler.loop_counter == 0
        assert hasattr(self.transpiler, 'rule_engine')
    
    def test_sanitize_python_name(self):
        """Test COBOL to Python name sanitization."""
        # Test basic hyphen replacement
        assert self.transpiler.sanitize_python_name("TEST-VAR") == "test_var"
        
        # Test digit prefix handling
        assert self.transpiler.sanitize_python_name("100-MAIN") == "para_100_main"
        
        # Test lowercase conversion
        assert self.transpiler.sanitize_python_name("COUNTER") == "counter"
        
        # Test mixed case
        assert self.transpiler.sanitize_python_name("My-Var-Name") == "my_var_name"
    
    def test_convert_cobol_condition(self):
        """Test COBOL condition to Python conversion."""
        # Test equality
        assert self.transpiler.convert_cobol_condition("A = B") == "a == b"
        
        # Test inequality
        assert self.transpiler.convert_cobol_condition("A NOT = B") == "a != b"
        
        # Test string literals
        assert self.transpiler.convert_cobol_condition("VAR = 'YES'") == "var == 'YES'"
        
        # Test string normalization
        assert self.transpiler.convert_cobol_condition("VAR = 'NO '") == "var == 'NO'"
        assert self.transpiler.convert_cobol_condition("VAR = 'YES '") == "var == 'YES'"
    
    def test_add_line(self):
        """Test adding lines with proper indentation."""
        self.transpiler.indent_level = 2
        self.transpiler.add_line("test line")
        
        assert len(self.transpiler.generated_code) == 1
        assert self.transpiler.generated_code[0] == "        test line"
    
    def test_extract_variables_from_lst(self):
        """Test variable extraction from LST."""
        # Create a mock LST node with data entries
        mock_lst_root = Mock(spec=LosslessNode)
        mock_lst_root.rule_name = "CompilationUnitContext"
        
        # Mock data division
        mock_data_division = Mock(spec=LosslessNode)
        mock_data_division.rule_name = "DataDivisionContext"
        
        # Mock working storage section
        mock_working_storage = Mock(spec=LosslessNode)
        mock_working_storage.rule_name = "WorkingStorageSectionContext"
        
        # Mock data entries
        mock_data_entry1 = Mock(spec=LosslessNode)
        mock_data_entry1.rule_name = "DataDescriptionEntryContext"
        mock_data_entry1.get_tokens.return_value = [
            Mock(text="01"), Mock(text="TEST-VAR"), Mock(text="PIC"), Mock(text="X(10)")
        ]
        
        mock_data_entry2 = Mock(spec=LosslessNode)
        mock_data_entry2.rule_name = "DataDescriptionEntryContext"
        mock_data_entry2.get_tokens.return_value = [
            Mock(text="01"), Mock(text="COUNTER"), Mock(text="PIC"), Mock(text="9(3)")
        ]
        
        mock_working_storage.children = [mock_data_entry1, mock_data_entry2]
        mock_data_division.children = [mock_working_storage]
        mock_lst_root.children = [mock_data_division]
        
        self.transpiler.extract_variables_from_lst(mock_lst_root)
        
        assert "TEST-VAR" in self.transpiler.variables
        assert "COUNTER" in self.transpiler.variables
    
    def test_generate_variable_declarations(self):
        """Test variable declaration generation."""
        self.transpiler.variables = {
            "TEST-VAR": {"type": "string", "pic": "X(10)"},
            "COUNTER": {"type": "number", "pic": "9(3)"},
            "AMOUNT": {"type": "number", "pic": "9(5)V99"}
        }
        
        self.transpiler.generate_variable_declarations()
        
        generated_code = '\n'.join(self.transpiler.generated_code)
        assert "test_var = ''" in generated_code
        assert "counter = 0" in generated_code
        assert "amount = 0" in generated_code
    
    def test_find_first_paragraph(self):
        """Test finding the first paragraph in LST."""
        # Create mock LST with paragraphs
        mock_lst_root = Mock(spec=LosslessNode)
        mock_lst_root.rule_name = "CompilationUnitContext"
        
        mock_procedure_division = Mock(spec=LosslessNode)
        mock_procedure_division.rule_name = "ProcedureDivisionContext"
        
        mock_paragraph1 = Mock(spec=LosslessNode)
        mock_paragraph1.rule_name = "ParagraphContext"
        mock_paragraph1.get_tokens.return_value = [Mock(text="100-MAIN")]
        
        mock_paragraph2 = Mock(spec=LosslessNode)
        mock_paragraph2.rule_name = "ParagraphContext"
        mock_paragraph2.get_tokens.return_value = [Mock(text="200-SUB")]
        
        mock_procedure_division.children = [mock_paragraph1, mock_paragraph2]
        mock_lst_root.children = [mock_procedure_division]
        
        first_paragraph = self.transpiler.find_first_paragraph(mock_lst_root)
        assert first_paragraph == "100-MAIN"
    
    def test_find_all_paragraphs(self):
        """Test finding all paragraphs in LST."""
        # Create mock LST with paragraphs
        mock_lst_root = Mock(spec=LosslessNode)
        mock_lst_root.rule_name = "CompilationUnitContext"
        
        mock_procedure_division = Mock(spec=LosslessNode)
        mock_procedure_division.rule_name = "ProcedureDivisionContext"
        
        mock_paragraph1 = Mock(spec=LosslessNode)
        mock_paragraph1.rule_name = "ParagraphContext"
        mock_paragraph1.get_tokens.return_value = [Mock(text="100-MAIN")]
        
        mock_paragraph2 = Mock(spec=LosslessNode)
        mock_paragraph2.rule_name = "ParagraphContext"
        mock_paragraph2.get_tokens.return_value = [Mock(text="200-SUB")]
        
        mock_procedure_division.children = [mock_paragraph1, mock_paragraph2]
        mock_lst_root.children = [mock_procedure_division]
        
        paragraphs = self.transpiler.find_all_paragraphs(mock_lst_root)
        assert "100-MAIN" in paragraphs
        assert "200-SUB" in paragraphs
        assert len(paragraphs) == 2


class TestTranspilerIntegration:
    """Test transpiler integration with real COBOL files."""
    
    @pytest.mark.parametrize("filename", SAMPLE_FILES)
    def test_transpile_file(self, filename):
        """Test that COBOL files can be transpiled to Python."""
        try:
            result = self.transpiler.transpile_file(os.path.join(EXAMPLES_DIR, filename))
            
            # Basic validation of generated Python code
            assert result is not None
            assert isinstance(result, str)
            assert len(result) > 0
            
            # Check for expected Python constructs
            assert "# Generated Python code from COBOL" in result
            assert "def main():" in result
            assert "if __name__ == '__main__':" in result
            assert "main()" in result
            
            # Check for variable declarations
            assert "# Variable declarations" in result
            
        except Exception as e:
            pytest.fail(f"Failed to transpile {filename}: {str(e)}")
    
    def test_transpile_source(self):
        """Test transpiling COBOL source code directly."""
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
        
        result = self.transpiler.transpile_source(cobol_source)
        
        assert result is not None
        assert isinstance(result, str)
        assert len(result) > 0
        assert "def main():" in result
        assert "test_var = ''" in result
    
    def test_transpile_file_with_output(self):
        """Test transpiling to output file."""
        input_file = os.path.join(EXAMPLES_DIR, "HELLO.cobol")
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as temp_file:
            output_file = temp_file.name
        
        try:
            transpile_cobol_file(input_file, output_file)
            
            # Check that output file was created
            assert os.path.exists(output_file)
            
            # Check that output file contains Python code
            with open(output_file, 'r') as f:
                content = f.read()
                assert "def main():" in content
                assert "if __name__ == '__main__':" in content
        
        finally:
            # Clean up
            if os.path.exists(output_file):
                os.unlink(output_file)


class TestTranspilerErrorHandling:
    """Test error handling in the transpiler."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.transpiler = CobolTranspiler()
    
    def test_transpile_nonexistent_file(self):
        """Test handling of nonexistent input file."""
        with pytest.raises(FileNotFoundError):
            self.transpiler.transpile_file("nonexistent.cobol")
    
    def test_transpile_invalid_cobol(self):
        """Test handling of invalid COBOL syntax."""
        invalid_cobol = "INVALID COBOL SYNTAX"
        
        # Should not raise an exception, but should handle gracefully
        result = self.transpiler.transpile_source(invalid_cobol)
        
        # Should still generate some Python code structure
        assert result is not None
        assert isinstance(result, str)
    
    def test_transpile_empty_source(self):
        """Test handling of empty COBOL source."""
        result = self.transpiler.transpile_source("")
        
        # Should generate basic Python structure
        assert result is not None
        assert isinstance(result, str)
        assert "def main():" in result


class TestTranspilerPerformance:
    """Test transpiler performance with various file sizes."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.transpiler = CobolTranspiler()
    
    def test_transpile_small_file(self):
        """Test transpiling a small COBOL file."""
        import time
        
        start_time = time.time()
        result = self.transpiler.transpile_file(os.path.join(EXAMPLES_DIR, "HELLO.cobol"))
        end_time = time.time()
        
        # Should complete within reasonable time (less than 5 seconds)
        assert end_time - start_time < 5.0
        assert result is not None
    
    def test_transpile_medium_file(self):
        """Test transpiling a medium COBOL file."""
        import time
        
        start_time = time.time()
        result = self.transpiler.transpile_file(os.path.join(EXAMPLES_DIR, "ADDAMT.cobol"))
        end_time = time.time()
        
        # Should complete within reasonable time (less than 10 seconds)
        assert end_time - start_time < 10.0
        assert result is not None


if __name__ == "__main__":
    pytest.main([__file__]) 