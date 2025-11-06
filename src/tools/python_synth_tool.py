"""
Python Synthesis Tool - Python code synthesis helpers.

Provides utilities for generating and validating Python code.
"""

import ast
from typing import Dict, Any, Optional
from src.utils.logger import get_logger

logger = get_logger(__name__)


def generate_python_code(cobol_analysis: str, target_structure: str) -> str:
    """Generate Python code from COBOL analysis.
    
    NOTE: This is a placeholder function. The actual code generation should be done
    by the LLM agent directly. This function should not be used for validation.
    
    Args:
        cobol_analysis: Analysis results from analyzer
        target_structure: Desired Python structure
        
    Returns:
        Placeholder message (not actual Python code)
    """
    # This is a placeholder - actual code generation is done by the LLM agent
    # Do not use this for syntax validation as it doesn't generate valid Python
    logger.warning("generate_python_code called - this is a placeholder, not actual code generation")
    return f"# Placeholder - actual code generation should be done by LLM agent\n# Analysis: {cobol_analysis[:100]}..."


def validate_python_syntax(code: str) -> bool:
    """Validate that Python code has correct syntax.
    
    Args:
        code: Python code string to validate (may contain markdown code blocks)
        
    Returns:
        True if syntax is valid, False otherwise
    """
    if not code or not code.strip():
        return False
    
    # Extract Python code from markdown code blocks if present
    import re
    python_code = code
    
    # Look for Python code blocks
    code_block_patterns = [
        r'```python\s*\n(.*?)```',
        r'```\s*\n(.*?)```',
    ]
    
    for pattern in code_block_patterns:
        matches = re.findall(pattern, code, re.DOTALL)
        if matches:
            # Use the longest match (likely the main code)
            python_code = max(matches, key=len).strip()
            break
    
    # If no code blocks found, use the code as-is
    if not python_code or not python_code.strip():
        return False
    
    try:
        ast.parse(python_code)
        return True
    except SyntaxError as e:
        logger.error(f"Syntax error in Python code at line {e.lineno}: {e.msg}")
        if e.text:
            logger.error(f"Problematic line: {e.text.strip()}")
        return False
    except Exception as e:
        logger.error(f"Error validating Python code: {e}")
        return False

