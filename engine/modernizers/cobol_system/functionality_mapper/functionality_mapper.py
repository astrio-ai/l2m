"""
COBOL to Python Functionality Mapper

This module provides specialized functionality mapping for COBOL to Python
modernization, including COBOL-specific features like PIC clauses, level numbers,
and file I/O operations.
"""

import logging
from typing import Dict, Any, List, Optional, Tuple
from dataclasses import dataclass, field
from enum import Enum
import re
from datetime import datetime

from engine.functionality_mapper import (
    FunctionalityMapper, FunctionalityMapping, FunctionalityType,
    InputOutputMapping, BusinessLogicMapping, EquivalenceLevel
)


class COBOLDataType(Enum):
    """COBOL data types."""
    ALPHANUMERIC = "X"  # PIC X
    NUMERIC = "9"  # PIC 9
    DECIMAL = "V9"  # PIC 9V9
    COMP = "COMP"  # COMP-3, COMP-4, etc.
    FLOAT = "FLOAT"  # Floating point
    DATE = "DATE"  # Date fields
    TIME = "TIME"  # Time fields


@dataclass
class COBOLFieldMapping:
    """Mapping of a COBOL field to Python."""
    cobol_name: str
    python_name: str
    cobol_type: str  # PIC clause
    python_type: str
    level_number: int
    length: int
    precision: Optional[int] = None
    scale: Optional[int] = None
    is_signed: bool = False
    is_comp: bool = False
    default_value: Optional[str] = None
    validation_rules: List[str] = field(default_factory=list)


@dataclass
class COBOLProgramMapping:
    """Mapping of a COBOL program to Python."""
    program_name: str
    python_module_name: str
    division_mappings: Dict[str, str] = field(default_factory=dict)
    paragraph_mappings: Dict[str, str] = field(default_factory=dict)
    file_mappings: Dict[str, str] = field(default_factory=dict)
    field_mappings: List[COBOLFieldMapping] = field(default_factory=list)
    working_storage: Dict[str, Any] = field(default_factory=dict)
    linkage_section: Dict[str, Any] = field(default_factory=dict)


class COBOLFunctionalityMapper(FunctionalityMapper):
    """
    Specialized functionality mapper for COBOL to Python modernization.
    
    This class extends the base FunctionalityMapper with COBOL-specific
    features like PIC clause parsing, level number handling, and
    COBOL program structure mapping.
    """
    
    def __init__(self):
        super().__init__()
        self.logger = logging.getLogger(__name__)
        self.cobol_programs: Dict[str, COBOLProgramMapping] = {}
        
    def create_cobol_program_mapping(
        self,
        program_name: str,
        python_module_name: str,
        source_code: Optional[str] = None
    ) -> Tuple[FunctionalityMapping, COBOLProgramMapping]:
        """
        Create a mapping for a COBOL program to Python.
        
        Args:
            program_name: Name of the COBOL program
            python_module_name: Name of the Python module
            source_code: COBOL source code (optional)
            
        Returns:
            Tuple of (FunctionalityMapping, COBOLProgramMapping)
        """
        # Create base functionality mapping
        functionality_mapping = self.create_functionality_mapping(
            functionality_type=FunctionalityType.PROGRAM,
            source_name=program_name,
            target_name=python_module_name,
            source_language="cobol",
            target_language="python",
            source_code=source_code,
            custom_id=f"PROG-{program_name.upper()}"
        )
        
        # Create COBOL-specific mapping
        cobol_mapping = COBOLProgramMapping(
            program_name=program_name,
            python_module_name=python_module_name
        )
        
        # Store COBOL mapping
        self.cobol_programs[functionality_mapping.functionality_id] = cobol_mapping
        
        return functionality_mapping, cobol_mapping
    
    def map_cobol_fields(
        self,
        functionality_id: str,
        field_definitions: List[Dict[str, Any]]
    ) -> List[COBOLFieldMapping]:
        """
        Map COBOL field definitions to Python.
        
        Args:
            functionality_id: ID of the functionality mapping
            field_definitions: List of COBOL field definitions
            
        Returns:
            List of COBOLFieldMapping objects
        """
        if functionality_id not in self.cobol_programs:
            raise ValueError(f"COBOL program mapping {functionality_id} not found")
        
        cobol_mapping = self.cobol_programs[functionality_id]
        field_mappings = []
        
        for field_def in field_definitions:
            field_mapping = self._create_field_mapping(field_def)
            field_mappings.append(field_mapping)
        
        # Update COBOL mapping
        cobol_mapping.field_mappings = field_mappings
        
        # Update base mapping
        mapping = self.mappings[functionality_id]
        mapping.updated_at = datetime.now()
        
        self.logger.info(f"Mapped {len(field_mappings)} COBOL fields for {functionality_id}")
        return field_mappings
    
    def map_cobol_paragraphs(
        self,
        functionality_id: str,
        paragraph_mappings: Dict[str, str]
    ) -> Dict[str, str]:
        """
        Map COBOL paragraphs to Python functions.
        
        Args:
            functionality_id: ID of the functionality mapping
            paragraph_mappings: Dictionary mapping paragraph names to Python function names
            
        Returns:
            Updated paragraph mappings
        """
        if functionality_id not in self.cobol_programs:
            raise ValueError(f"COBOL program mapping {functionality_id} not found")
        
        cobol_mapping = self.cobol_programs[functionality_id]
        cobol_mapping.paragraph_mappings = paragraph_mappings
        
        # Update base mapping
        mapping = self.mappings[functionality_id]
        mapping.updated_at = datetime.now()
        
        self.logger.info(f"Mapped {len(paragraph_mappings)} COBOL paragraphs for {functionality_id}")
        return paragraph_mappings
    
    def map_cobol_files(
        self,
        functionality_id: str,
        file_mappings: Dict[str, str]
    ) -> Dict[str, str]:
        """
        Map COBOL file operations to Python file handling.
        
        Args:
            functionality_id: ID of the functionality mapping
            file_mappings: Dictionary mapping COBOL file names to Python file paths
            
        Returns:
            Updated file mappings
        """
        if functionality_id not in self.cobol_programs:
            raise ValueError(f"COBOL program mapping {functionality_id} not found")
        
        cobol_mapping = self.cobol_programs[functionality_id]
        cobol_mapping.file_mappings = file_mappings
        
        # Update base mapping
        mapping = self.mappings[functionality_id]
        mapping.updated_at = datetime.now()
        
        self.logger.info(f"Mapped {len(file_mappings)} COBOL files for {functionality_id}")
        return file_mappings
    
    def analyze_cobol_structure(
        self,
        functionality_id: str,
        source_code: str
    ) -> Dict[str, Any]:
        """
        Analyze COBOL program structure and extract mapping information.
        
        Args:
            functionality_id: ID of the functionality mapping
            source_code: COBOL source code
            
        Returns:
            Analysis result with extracted structure information
        """
        if functionality_id not in self.cobol_programs:
            raise ValueError(f"COBOL program mapping {functionality_id} not found")
        
        analysis_result = {
            "divisions": {},
            "paragraphs": [],
            "files": [],
            "fields": [],
            "working_storage": {},
            "linkage_section": {}
        }
        
        # Parse COBOL divisions
        divisions = self._parse_cobol_divisions(source_code)
        analysis_result["divisions"] = divisions
        
        # Extract paragraphs
        paragraphs = self._extract_paragraphs(source_code)
        analysis_result["paragraphs"] = paragraphs
        
        # Extract file definitions
        files = self._extract_file_definitions(source_code)
        analysis_result["files"] = files
        
        # Extract field definitions
        fields = self._extract_field_definitions(source_code)
        analysis_result["fields"] = fields
        
        # Extract working storage
        working_storage = self._extract_working_storage(source_code)
        analysis_result["working_storage"] = working_storage
        
        # Extract linkage section
        linkage_section = self._extract_linkage_section(source_code)
        analysis_result["linkage_section"] = linkage_section
        
        return analysis_result
    
    def generate_python_equivalence_tests(
        self,
        functionality_id: str
    ) -> List[Dict[str, Any]]:
        """
        Generate test cases to validate COBOL to Python equivalence.
        
        Args:
            functionality_id: ID of the functionality mapping
            
        Returns:
            List of test cases
        """
        if functionality_id not in self.cobol_programs:
            raise ValueError(f"COBOL program mapping {functionality_id} not found")
        
        cobol_mapping = self.cobol_programs[functionality_id]
        test_cases = []
        
        # Generate test cases based on field mappings
        for field_mapping in cobol_mapping.field_mappings:
            test_case = self._generate_field_test_case(field_mapping)
            test_cases.append(test_case)
        
        # Generate test cases based on paragraph mappings
        for cobol_paragraph, python_function in cobol_mapping.paragraph_mappings.items():
            test_case = self._generate_paragraph_test_case(cobol_paragraph, python_function)
            test_cases.append(test_case)
        
        return test_cases
    
    def _create_field_mapping(self, field_def: Dict[str, Any]) -> COBOLFieldMapping:
        """Create a COBOL field mapping from field definition."""
        cobol_name = field_def.get("name", "")
        python_name = self._convert_to_snake_case(cobol_name)
        
        # Parse PIC clause
        pic_clause = field_def.get("pic", "")
        cobol_type, length, precision, scale, is_signed = self._parse_pic_clause(pic_clause)
        
        # Determine Python type
        python_type = self._map_cobol_type_to_python(cobol_type, length, precision, scale)
        
        return COBOLFieldMapping(
            cobol_name=cobol_name,
            python_name=python_name,
            cobol_type=cobol_type,
            python_type=python_type,
            level_number=field_def.get("level", 0),
            length=length,
            precision=precision,
            scale=scale,
            is_signed=is_signed,
            is_comp=field_def.get("is_comp", False),
            default_value=field_def.get("default_value"),
            validation_rules=field_def.get("validation_rules", [])
        )
    
    def _parse_pic_clause(self, pic_clause: str) -> Tuple[str, int, Optional[int], Optional[int], bool]:
        """Parse COBOL PIC clause."""
        if not pic_clause:
            return "X", 1, None, None, False
        
        # Remove PIC and parentheses
        pic_clean = re.sub(r'PIC\s*', '', pic_clause.upper())
        
        # Check for signed fields
        is_signed = "S" in pic_clean
        
        # Parse alphanumeric fields (X)
        if "X" in pic_clean:
            # Extract length from X(10) format
            x_match = re.search(r'X\((\d+)\)', pic_clean)
            if x_match:
                length = int(x_match.group(1))
            else:
                # Count X characters if no parentheses
                length = pic_clean.count("X")
            return "X", length, None, None, False
        
        # Parse numeric fields (9)
        elif "9" in pic_clean:
            # Handle decimal places (V)
            if "V" in pic_clean:
                parts = pic_clean.split("V")
                # Extract length from 9(8) format
                nine_match = re.search(r'9\((\d+)\)', parts[0])
                if nine_match:
                    integer_part = int(nine_match.group(1))
                else:
                    integer_part = parts[0].count("9")
                decimal_part = parts[1].count("9")
                return "9", integer_part + decimal_part, integer_part + decimal_part, decimal_part, is_signed
            else:
                # Extract length from 9(5) format
                nine_match = re.search(r'9\((\d+)\)', pic_clean)
                if nine_match:
                    length = int(nine_match.group(1))
                else:
                    length = pic_clean.count("9")
                return "9", length, length, None, is_signed
        
        # Default to alphanumeric
        return "X", 1, None, None, False
    
    def _map_cobol_type_to_python(self, cobol_type: str, length: int, precision: Optional[int], scale: Optional[int]) -> str:
        """Map COBOL type to Python type."""
        if cobol_type == "9":
            if scale and scale > 0:
                return "decimal.Decimal"
            elif length <= 9:
                return "int"
            else:
                return "str"  # Large numbers as string to preserve precision
        elif cobol_type == "X":
            return "str"
        elif cobol_type == "COMP":
            return "int"
        else:
            return "str"
    
    def _convert_to_snake_case(self, name: str) -> str:
        """Convert COBOL field name to Python snake_case."""
        # Remove common COBOL prefixes/suffixes
        name = re.sub(r'^(WS-|LS-|FD-)', '', name)
        
        # Convert to snake_case
        name = re.sub(r'[^a-zA-Z0-9]', '_', name)
        name = re.sub(r'_+', '_', name)
        name = name.strip('_').lower()
        
        return name
    
    def _parse_cobol_divisions(self, source_code: str) -> Dict[str, str]:
        """Parse COBOL divisions from source code."""
        divisions = {}
        
        # Find division headers
        division_pattern = r'^\s*(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION'
        
        for line in source_code.split('\n'):
            match = re.match(division_pattern, line, re.IGNORECASE)
            if match:
                division_name = match.group(1).upper()
                divisions[division_name] = line.strip()
        
        return divisions
    
    def _extract_paragraphs(self, source_code: str) -> List[str]:
        """Extract paragraph names from COBOL source code."""
        paragraphs = []
        
        # Find paragraph definitions
        paragraph_pattern = r'^\s*([A-Z][A-Z0-9-]*)\s*\.'
        
        for line in source_code.split('\n'):
            match = re.match(paragraph_pattern, line, re.IGNORECASE)
            if match:
                paragraph_name = match.group(1).upper()
                paragraphs.append(paragraph_name)
        
        return paragraphs
    
    def _extract_file_definitions(self, source_code: str) -> List[str]:
        """Extract file definitions from COBOL source code."""
        files = []
        
        # Find file definitions
        file_pattern = r'^\s*SELECT\s+([A-Z][A-Z0-9-]*)'
        
        for line in source_code.split('\n'):
            match = re.search(file_pattern, line, re.IGNORECASE)
            if match:
                file_name = match.group(1).upper()
                files.append(file_name)
        
        return files
    
    def _extract_field_definitions(self, source_code: str) -> List[Dict[str, Any]]:
        """Extract field definitions from COBOL source code."""
        fields = []
        
        # Find field definitions (level numbers)
        field_pattern = r'^\s*(\d{2})\s+([A-Z][A-Z0-9-]*)\s+PIC\s+([^\.]+)'
        
        for line in source_code.split('\n'):
            match = re.match(field_pattern, line, re.IGNORECASE)
            if match:
                level = int(match.group(1))
                name = match.group(2).upper()
                pic_clause = match.group(3).strip()
                
                fields.append({
                    "level": level,
                    "name": name,
                    "pic": pic_clause
                })
        
        return fields
    
    def _extract_working_storage(self, source_code: str) -> Dict[str, Any]:
        """Extract working storage section from COBOL source code."""
        working_storage = {}
        
        # Find working storage section
        in_working_storage = False
        for line in source_code.split('\n'):
            if re.match(r'^\s*WORKING-STORAGE\s+SECTION', line, re.IGNORECASE):
                in_working_storage = True
                continue
            elif re.match(r'^\s*(PROCEDURE|LINKAGE)\s+DIVISION', line, re.IGNORECASE):
                in_working_storage = False
                continue
            
            if in_working_storage:
                # Parse field definitions in working storage
                field_match = re.match(r'^\s*(\d{2})\s+([A-Z][A-Z0-9-]*)\s+PIC\s+([^\.]+)', line, re.IGNORECASE)
                if field_match:
                    level = int(field_match.group(1))
                    name = field_match.group(2).upper()
                    pic_clause = field_match.group(3).strip()
                    
                    working_storage[name] = {
                        "level": level,
                        "pic": pic_clause
                    }
        
        return working_storage
    
    def _extract_linkage_section(self, source_code: str) -> Dict[str, Any]:
        """Extract linkage section from COBOL source code."""
        linkage_section = {}
        
        # Find linkage section
        in_linkage_section = False
        for line in source_code.split('\n'):
            if re.match(r'^\s*LINKAGE\s+SECTION', line, re.IGNORECASE):
                in_linkage_section = True
                continue
            elif re.match(r'^\s*PROCEDURE\s+DIVISION', line, re.IGNORECASE):
                in_linkage_section = False
                continue
            
            if in_linkage_section:
                # Parse field definitions in linkage section
                field_match = re.match(r'^\s*(\d{2})\s+([A-Z][A-Z0-9-]*)\s+PIC\s+([^\.]+)', line, re.IGNORECASE)
                if field_match:
                    level = int(field_match.group(1))
                    name = field_match.group(2).upper()
                    pic_clause = field_match.group(3).strip()
                    
                    linkage_section[name] = {
                        "level": level,
                        "pic": pic_clause
                    }
        
        return linkage_section
    
    def _generate_field_test_case(self, field_mapping: COBOLFieldMapping) -> Dict[str, Any]:
        """Generate test case for a field mapping."""
        return {
            "test_type": "field_mapping",
            "cobol_field": field_mapping.cobol_name,
            "python_field": field_mapping.python_name,
            "cobol_type": field_mapping.cobol_type,
            "python_type": field_mapping.python_type,
            "test_inputs": {
                "cobol_value": self._generate_test_value(field_mapping.cobol_type, field_mapping.length),
                "expected_python_value": None  # To be filled based on transformation
            },
            "expected_outputs": {
                "python_value": None  # To be filled based on transformation
            }
        }
    
    def _generate_paragraph_test_case(self, cobol_paragraph: str, python_function: str) -> Dict[str, Any]:
        """Generate test case for a paragraph mapping."""
        return {
            "test_type": "paragraph_mapping",
            "cobol_paragraph": cobol_paragraph,
            "python_function": python_function,
            "test_inputs": {
                "paragraph_inputs": {}  # To be filled based on paragraph analysis
            },
            "expected_outputs": {
                "function_outputs": {}  # To be filled based on function analysis
            }
        }
    
    def _generate_test_value(self, cobol_type: str, length: int) -> str:
        """Generate test value for a COBOL field type."""
        if cobol_type == "9":
            return "1" * length
        elif cobol_type == "X":
            return "A" * length
        else:
            return "TEST" * (length // 4 + 1) 