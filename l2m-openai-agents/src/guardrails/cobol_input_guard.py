"""
COBOL Input Guard - Validates COBOL input.
"""

from pathlib import Path
from typing import Optional
from src.utils.logger import get_logger

logger = get_logger(__name__)


def validate_cobol_input(file_path: str) -> tuple[bool, Optional[str]]:
    """Validate COBOL input file.
    
    Args:
        file_path: Path to COBOL file
        
    Returns:
        Tuple of (is_valid, error_message)
    """
    path = Path(file_path)
    
    if not path.exists():
        return False, f"File not found: {file_path}"
    
    if not path.is_file():
        return False, f"Not a file: {file_path}"
    
    # Check file extension
    if path.suffix.lower() not in ['.cbl', '.cob', '.cobol']:
        logger.warning(f"File extension {path.suffix} is not standard for COBOL")
    
    # Try to read file
    try:
        content = path.read_text(encoding='utf-8', errors='ignore')
        if len(content.strip()) == 0:
            return False, "File is empty"
    except Exception as e:
        return False, f"Error reading file: {str(e)}"
    
    return True, None

