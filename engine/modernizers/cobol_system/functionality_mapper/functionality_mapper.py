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
    source_name: str  # COBOL field name
    target_name: str  # Python field name
    source_type: str  # PIC clause
    target_type: str  # Python type
    level_number: int = 1
    length: int = 0
    precision: Optional[int] = None
    scale: Optional[int] = None
    is_signed: bool = False
    is_comp: bool = False
    default_value: Optional[str] = None
    validation_rules: List[str] = field(default_factory=list)
    
    @property
    def cobol_name(self) -> str:
        """Backward compatibility: return source_name."""
        return self.source_name
    
    @property
    def python_name(self) -> str:
        """Backward compatibility: return target_name."""
        return self.target_name
    
    @property
    def python_type(self) -> str:
        """Backward compatibility: return target_type."""
        return self.target_type


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
    ) -> FunctionalityMapping:
        """
        Create a mapping for a COBOL program to Python.
        
        Args:
            program_name: Name of the COBOL program
            python_module_name: Name of the Python module
            source_code: COBOL source code (optional)
            
        Returns:
            FunctionalityMapping object
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
        
        return functionality_mapping
    
    def map_cobol_fields(
        self,
        functionality_id: str,
        field_mappings: List[COBOLFieldMapping]
    ) -> List[COBOLFieldMapping]:
        """
        Map COBOL field definitions to Python.
        
        Args:
            functionality_id: ID of the functionality mapping
            field_mappings: List of COBOLFieldMapping objects
            
        Returns:
            List of COBOLFieldMapping objects
        """
        if functionality_id not in self.cobol_programs:
            raise ValueError(f"COBOL program mapping {functionality_id} not found")
        
        cobol_mapping = self.cobol_programs[functionality_id]
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
    
    def analyze_cobol_structure(self, source_code: str) -> Dict[str, Any]:
        """
        Analyze COBOL program structure.
        
        Args:
            functionality_id: ID of the functionality mapping
            source_code: COBOL source code
            
        Returns:
            Analysis results
        """
        analysis = {
            "program_name": "",
            "divisions": {},
            "paragraphs": [],
            "files": [],
            "data_structures": [],
            "working_storage": {},
            "linkage_section": {}
        }
        
        # Extract program name
        program_match = re.search(r'PROGRAM-ID\.\s+([A-Z0-9-]+)', source_code, re.IGNORECASE)
        if program_match:
            analysis["program_name"] = program_match.group(1)
        
        # Extract divisions
        divisions = self._parse_cobol_divisions(source_code)
        analysis["divisions"] = divisions
        # Also provide list format for backward compatibility
        analysis["division_list"] = list(divisions.keys())
        
        # Extract paragraphs
        paragraphs = self._extract_paragraphs(source_code)
        analysis["paragraphs"] = paragraphs
        
        # Extract file definitions
        files = self._extract_file_definitions(source_code)
        analysis["files"] = files
        
        # Extract data structures
        data_structures = self._extract_field_definitions(source_code)
        analysis["data_structures"] = data_structures
        analysis["fields"] = data_structures  # Backward compatibility
        
        # Extract working storage
        working_storage = self._extract_working_storage(source_code)
        analysis["working_storage"] = working_storage
        
        # Extract linkage section
        linkage_section = self._extract_linkage_section(source_code)
        analysis["linkage_section"] = linkage_section
        
        return analysis
    
    def parse_pic_clause(self, pic_clause: str) -> str:
        """
        Parse COBOL PIC clause and return Python type.
        
        Args:
            pic_clause: COBOL PIC clause (e.g., "PIC 9(6)", "PIC X(30)")
            
        Returns:
            Python type string
        """
        # Remove PIC keyword and spaces
        pic_clean = pic_clause.replace("PIC", "").strip()
        
        # Handle alphanumeric (X)
        if "X" in pic_clean:
            return "str"
        
        # Handle numeric (9)
        if "9" in pic_clean:
            # Check for decimal places
            if "V" in pic_clean:
                return "float"
            else:
                return "int"
        
        # Handle other types
        if "COMP" in pic_clean:
            return "int"
        elif "FLOAT" in pic_clean:
            return "float"
        else:
            return "str"
    
    def convert_to_snake_case(self, name: str) -> str:
        """
        Convert COBOL name to Python snake_case.
        
        Args:
            name: COBOL name (e.g., "EMPLOYEE-ID")
            
        Returns:
            Python snake_case name (e.g., "employee_id")
        """
        # Replace hyphens with underscores and convert to lowercase
        snake_case = name.replace("-", "_").lower()
        return snake_case
    
    def generate_python_equivalence_tests(
        self,
        functionality_id: str
    ) -> List[Dict[str, Any]]:
        """
        Generate Python equivalence tests for COBOL program.
        
        Args:
            functionality_id: ID of the functionality mapping
            
        Returns:
            List of test cases
        """
        if functionality_id not in self.cobol_programs:
            raise ValueError(f"COBOL program mapping {functionality_id} not found")
        
        cobol_mapping = self.cobol_programs[functionality_id]
        tests = []
        
        # Generate field tests
        for field_mapping in cobol_mapping.field_mappings:
            test = self._generate_field_test_case(field_mapping)
            tests.append(test)
        
        # Generate paragraph tests
        for cobol_paragraph, python_function in cobol_mapping.paragraph_mappings.items():
            test = self._generate_paragraph_test_case(cobol_paragraph, python_function)
            tests.append(test)
        
        return tests
    
    def validate_equivalence(
        self,
        functionality_id: str,
        test_cases: Optional[List[Any]] = None,
        validation_strategies: Optional[List[Any]] = None
    ) -> Dict[str, Any]:
        """
        Validate the equivalence between COBOL and Python functionality.
        
        Args:
            functionality_id: ID of the functionality mapping
            test_cases: Test cases to validate equivalence
            validation_strategies: Specific validation strategies to use
            
        Returns:
            Validation result with confidence score, issues, and warnings
        """
        # Call the base method first
        result = super().validate_equivalence(functionality_id, test_cases, validation_strategies)
        
        # Add the missing keys that the test expects
        if "issues" not in result:
            result["issues"] = []
        if "warnings" not in result:
            result["warnings"] = []
        
        return result
    
    def _create_field_mapping(self, field_def: Dict[str, Any]) -> COBOLFieldMapping:
        """Create a COBOL field mapping from field definition."""
        cobol_name = field_def.get("name", "")
        python_name = self.convert_to_snake_case(cobol_name)
        cobol_type = field_def.get("pic", "")
        
        # Parse PIC clause for additional details
        pic_details = self._parse_pic_clause_details(cobol_type)
        
        # Determine Python type based on PIC clause
        if "X" in cobol_type:
            python_type = "str"
        elif "9" in cobol_type:
            if pic_details.get("scale"):
                python_type = "decimal.Decimal"
            else:
                python_type = "int"
        else:
            python_type = "str"
        
        return COBOLFieldMapping(
            source_name=cobol_name,
            target_name=python_name,
            source_type=cobol_type,
            target_type=python_type,
            level_number=field_def.get("level", 1),
            length=pic_details.get("length", 0),
            precision=pic_details.get("precision"),
            scale=pic_details.get("scale"),
            is_signed=pic_details.get("is_signed", False)
        )
    
    def _parse_pic_clause_details(self, pic_clause: str) -> Dict[str, Any]:
        """Parse PIC clause for detailed information."""
        details = {
            "length": 0,
            "precision": None,
            "scale": None,
            "is_signed": False
        }
        
        # Handle alphanumeric (X)
        x_match = re.search(r'X\((\d+)\)', pic_clause)
        if x_match:
            details["length"] = int(x_match.group(1))
            return details
        
        # Handle numeric (9)
        nine_match = re.search(r'9\((\d+)\)', pic_clause)
        if nine_match:
            details["length"] = int(nine_match.group(1))
            details["precision"] = details["length"]
            
            # Check for signed numbers (S before 9)
            if re.search(r'S9', pic_clause):
                details["is_signed"] = True
            
            # Check for decimal places - V followed by one or more digits
            v_match = re.search(r'V(\d+)', pic_clause)
            if v_match:
                scale_str = v_match.group(1)
                details["scale"] = len(scale_str)  # Count the digits after V
                # Total length is precision + scale
                details["length"] = details["precision"] + details["scale"]
                # Update precision to total length for decimal fields
                details["precision"] = details["length"]
            
            return details
        
        # Handle signed numbers (for other types)
        if "S" in pic_clause:
            details["is_signed"] = True
        
        return details
    
    def _parse_pic_clause(self, pic_clause: str) -> Tuple[str, int, Optional[int], Optional[int], bool]:
        """Parse PIC clause and return type information."""
        # This is a legacy method for backward compatibility
        details = self._parse_pic_clause_details(pic_clause)
        
        # Extract the COBOL type (first character after PIC)
        pic_clean = pic_clause.replace("PIC", "").strip()
        cobol_type = "9"  # default
        if "X" in pic_clean:
            cobol_type = "X"
        elif "9" in pic_clean:
            cobol_type = "9"
        
        return (
            cobol_type,
            details["length"],
            details["precision"],
            details["scale"],
            details["is_signed"]
        )
    
    def _map_cobol_type_to_python(self, cobol_type: str, length: int, precision: Optional[int], scale: Optional[int]) -> str:
        """Map COBOL type to Python type."""
        if "X" in cobol_type:
            return "str"
        elif "9" in cobol_type:
            if scale:
                return "decimal.Decimal"
            elif length > 9:  # Large numbers as string
                return "str"
            else:
                return "int"
        else:
            return "str"
    
    def _convert_to_snake_case(self, name: str) -> str:
        """Convert COBOL name to snake_case (legacy method)."""
        # Remove common prefixes like WS- (working storage)
        if name.startswith("WS-"):
            name = name[3:]  # Remove WS- prefix
        elif name.startswith("LS-"):
            name = name[3:]  # Remove LS- prefix (linkage section)
        
        return self.convert_to_snake_case(name)
    
    def _parse_cobol_divisions(self, source_code: str) -> Dict[str, List[str]]:
        """Parse COBOL divisions."""
        divisions = {}
        
        # Extract division names
        division_patterns = [
            r'IDENTIFICATION\s+DIVISION',
            r'ENVIRONMENT\s+DIVISION',
            r'DATA\s+DIVISION',
            r'PROCEDURE\s+DIVISION'
        ]
        
        for pattern in division_patterns:
            if re.search(pattern, source_code, re.IGNORECASE):
                # Extract just the division name (before "DIVISION")
                division_name = pattern.replace(r'\s+DIVISION', '').replace('\\', '')
                # Find the full division text
                full_match = re.search(pattern + r'\.', source_code, re.IGNORECASE)
                if full_match:
                    divisions[division_name] = [full_match.group(0)]
                else:
                    divisions[division_name] = []
        
        return divisions
    
    def _extract_paragraphs(self, source_code: str) -> List[str]:
        """Extract COBOL paragraphs."""
        paragraphs = []
        
        # Look for paragraph names (usually start at column 8)
        # Paragraphs are typically followed by a period
        paragraph_pattern = r'^\s*([A-Z0-9-]+)\s*\.'
        matches = re.findall(paragraph_pattern, source_code, re.MULTILINE | re.IGNORECASE)
        
        for match in matches:
            # Filter out common non-paragraph items
            if match not in ['PROGRAM-ID', 'AUTHOR', 'DATE-WRITTEN', 'DATE-COMPILED', 'ENVIRONMENT', 'DATA', 'PROCEDURE', 'WORKING-STORAGE', 'LINKAGE', 'FILE', 'INPUT-OUTPUT']:
                paragraphs.append(match)
        
        return paragraphs
    
    def _extract_file_definitions(self, source_code: str) -> List[str]:
        """Extract COBOL file definitions."""
        files = []
        
        # Look for SELECT statements
        select_pattern = r'SELECT\s+([A-Z0-9-]+)'
        matches = re.findall(select_pattern, source_code, re.IGNORECASE)
        
        for match in matches:
            # Add -FILE suffix if not already present
            if not match.endswith('-FILE'):
                match = match + '-FILE'
            files.append(match)
        
        return files
    
    def _extract_field_definitions(self, source_code: str) -> List[Dict[str, Any]]:
        """Extract COBOL field definitions."""
        fields = []
        
        # Look for field definitions (level numbers) - handle hyphens in names
        # Pattern: level number, field name, PIC clause
        field_pattern = r'(\d{2})\s+([A-Z0-9-]+)\s+PIC\s+([^\.\s]+)'
        matches = re.findall(field_pattern, source_code, re.IGNORECASE)
        
        for level, name, pic in matches:
            fields.append({
                "level": int(level),
                "name": name,
                "pic": pic.strip()
            })
        
        return fields
    
    def _extract_working_storage(self, source_code: str) -> Dict[str, Any]:
        """Extract working storage section."""
        working_storage = {}
        
        # Look for WORKING-STORAGE SECTION
        ws_pattern = r'WORKING-STORAGE\s+SECTION(.*?)(?=PROCEDURE\s+DIVISION|$)'
        ws_match = re.search(ws_pattern, source_code, re.IGNORECASE | re.DOTALL)
        
        if ws_match:
            ws_content = ws_match.group(1)
            # Extract field definitions from working storage
            fields = self._extract_field_definitions(ws_content)
            for field in fields:
                working_storage[field["name"]] = field
        
        return working_storage
    
    def _extract_linkage_section(self, source_code: str) -> Dict[str, Any]:
        """Extract linkage section."""
        linkage_section = {}
        
        # Look for LINKAGE SECTION
        linkage_pattern = r'LINKAGE\s+SECTION(.*?)(?=PROCEDURE\s+DIVISION|$)'
        linkage_match = re.search(linkage_pattern, source_code, re.IGNORECASE | re.DOTALL)
        
        if linkage_match:
            linkage_content = linkage_match.group(1)
            # Extract field definitions from linkage section
            fields = self._extract_field_definitions(linkage_content)
            for field in fields:
                linkage_section[field["name"]] = field
        
        return linkage_section
    
    def _generate_field_test_case(self, field_mapping: COBOLFieldMapping) -> Dict[str, Any]:
        """Generate test case for a field mapping."""
        return {
            "test_id": f"field_{field_mapping.source_name.lower()}",
            "test_type": "unit_test",
            "name": f"Test {field_mapping.source_name} field mapping",
            "description": f"Test mapping of {field_mapping.source_name} to {field_mapping.target_name}",
            "cobol_field": field_mapping.source_name,
            "inputs": {"cobol_value": self._generate_test_value(field_mapping.source_type, field_mapping.length)},
            "expected_outputs": {"python_value": "expected_value"}
        }
    
    def _generate_paragraph_test_case(self, cobol_paragraph: str, python_function: str) -> Dict[str, Any]:
        """Generate test case for a paragraph mapping."""
        return {
            "test_id": f"paragraph_{cobol_paragraph.lower()}",
            "test_type": "unit_test",
            "name": f"Test {cobol_paragraph} paragraph mapping",
            "description": f"Test mapping of {cobol_paragraph} to {python_function}",
            "cobol_paragraph": cobol_paragraph,
            "inputs": {},
            "expected_outputs": {}
        }
    
    def _generate_test_value(self, cobol_type: str, length: int) -> str:
        """Generate test value for COBOL type."""
        if "X" in cobol_type:
            return "A" * min(length, 10)
        elif "9" in cobol_type:
            return "1" * min(length, 5)
        else:
            return "test_value" 