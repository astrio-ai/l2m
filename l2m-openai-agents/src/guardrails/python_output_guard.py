"""
Python Output Guard - Ensures Python code safety.
"""

import ast
from typing import Optional
from src.utils.logger import get_logger

logger = get_logger(__name__)


def validate_python_output(python_code: str) -> tuple[bool, Optional[str]]:
    """Validate Python output code.
    
    Args:
        python_code: Python code to validate
        
    Returns:
        Tuple of (is_valid, error_message)
    """
    if not python_code or not python_code.strip():
        return False, "Python code is empty"
    
    # Check syntax
    try:
        ast.parse(python_code)
    except SyntaxError as e:
        return False, f"Syntax error: {str(e)}"
    
    # TODO: Add additional safety checks
    # - No dangerous imports (os.system, eval, exec, etc.)
    # - No hardcoded credentials
    # - No unsafe file operations
    
    return True, None

