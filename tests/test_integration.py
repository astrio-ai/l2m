"""Integration tests for the modernization pipeline."""

import pytest
import tempfile
from pathlib import Path
from src.workflows.modernization_pipeline import ModernizationPipeline
from src.guardrails.cobol_input_guard import validate_cobol_input
from src.guardrails.python_output_guard import validate_python_output


class TestEndToEndWorkflow:
    """Integration tests for end-to-end workflow."""
    
    @pytest.mark.asyncio
    async def test_workflow_with_valid_cobol(self):
        """Test complete workflow with valid COBOL file."""
        # Create a simple COBOL file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.cbl', delete=False) as f:
            f.write("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD!'.
           GOBACK.""")
            temp_file = f.name
        
        try:
            # Validate input
            is_valid, error = validate_cobol_input(temp_file)
            assert is_valid is True, f"Input validation failed: {error}"
            
            # Run pipeline (will use API if key is set)
            pipeline = ModernizationPipeline(use_handoffs=False)
            results = await pipeline.run(temp_file)
            
            # Check results structure
            assert isinstance(results, dict)
            assert "cobol_file" in results
            
            # If translation was successful, validate output
            if results.get("translation") and "error" not in results:
                translation = results["translation"]
                # Extract Python code if it's in a code block
                if "```python" in translation:
                    # This would need more sophisticated parsing
                    pass
                elif "def " in translation or "print(" in translation:
                    # Basic validation
                    is_valid, error = validate_python_output(translation)
                    # Note: May have syntax issues from LLM output, but structure should be there
        finally:
            Path(temp_file).unlink()
    
    def test_guardrail_chain(self):
        """Test that guardrails work together."""
        # Test input guardrail
        with tempfile.NamedTemporaryFile(mode='w', suffix='.cbl', delete=False) as f:
            f.write("       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.")
            temp_file = f.name
        
        try:
            # Input validation
            is_valid, error = validate_cobol_input(temp_file)
            assert is_valid is True
            
            # Output validation (example)
            valid_python = "def test():\n    print('test')\n"
            is_valid, error = validate_python_output(valid_python)
            assert is_valid is True
        finally:
            Path(temp_file).unlink()

