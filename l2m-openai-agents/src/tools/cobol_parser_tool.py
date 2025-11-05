"""
COBOL Parser Tool - Parsing helper for COBOL code.

Provides regex-based and LLM-assisted parsing of COBOL source files.
"""

import re
from pathlib import Path
from typing import Dict, List, Any
from src.utils.logger import get_logger

logger = get_logger(__name__)


def parse_cobol_file(file_path: str) -> str:
    """Parse a COBOL file and extract basic structure.
    
    Args:
        file_path: Path to the COBOL file
        
    Returns:
        Parsed COBOL structure as string
    """
    try:
        path = Path(file_path)
        if not path.exists():
            return f"Error: File not found: {file_path}"
        
        content = path.read_text(encoding="utf-8", errors="ignore")
        
        # Extract PROGRAM-ID
        program_id_match = re.search(r'PROGRAM-ID\.\s+(\w+)', content, re.IGNORECASE)
        program_id = program_id_match.group(1) if program_id_match else "UNKNOWN"
        
        # Extract WORKING-STORAGE variables
        working_storage = re.findall(
            r'(\d{2})\s+(\w+)\s+(PIC|PICTURE)\s+([^\n]+)',
            content,
            re.IGNORECASE
        )
        
        # Extract PROCEDURE DIVISION procedures
        procedures = re.findall(
            r'(\w+)\s+SECTION\.',
            content,
            re.IGNORECASE
        )
        
        result = f"""
PROGRAM-ID: {program_id}
WORKING-STORAGE variables: {len(working_storage)}
PROCEDURES: {len(procedures)}
"""
        return result.strip()
    
    except Exception as e:
        logger.error(f"Error parsing COBOL file: {e}")
        return f"Error: {str(e)}"


def analyze_cobol_structure(file_path: str) -> Dict[str, Any]:
    """Analyze COBOL file structure and return structured data.
    
    Args:
        file_path: Path to the COBOL file
        
    Returns:
        Dictionary with analysis results
    """
    try:
        path = Path(file_path)
        if not path.exists():
            return {"error": f"File not found: {file_path}"}
        
        content = path.read_text(encoding="utf-8", errors="ignore")
        
        # Extract PROGRAM-ID
        program_id_match = re.search(r'PROGRAM-ID\.\s+(\w+)', content, re.IGNORECASE)
        program_id = program_id_match.group(1) if program_id_match else "UNKNOWN"
        
        # Extract WORKING-STORAGE
        working_storage = []
        ws_match = re.search(r'WORKING-STORAGE\s+SECTION\.(.*?)PROCEDURE\s+DIVISION', content, re.DOTALL | re.IGNORECASE)
        if ws_match:
            ws_content = ws_match.group(1)
            variables = re.findall(
                r'(\d{2})\s+(\w+)\s+(PIC|PICTURE)\s+([^\n]+)',
                ws_content,
                re.IGNORECASE
            )
            for var in variables:
                working_storage.append({
                    "level": var[0],
                    "name": var[1],
                    "type": var[2],
                    "definition": var[3].strip()
                })
        
        # Extract procedures
        procedures = re.findall(
            r'(\w+)\s+SECTION\.',
            content,
            re.IGNORECASE
        )
        
        # Extract DISPLAY statements
        displays = re.findall(r'DISPLAY\s+([^\n]+)', content, re.IGNORECASE)
        
        return {
            "program_id": program_id,
            "working_storage": working_storage,
            "procedures": procedures,
            "display_statements": displays,
            "file_path": file_path
        }
    
    except Exception as e:
        logger.error(f"Error analyzing COBOL structure: {e}")
        return {"error": str(e)}

