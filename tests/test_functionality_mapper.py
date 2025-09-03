"""
Tests for COBOL Functionality Mapper

This module tests the COBOL-specific functionality mapping capabilities
for COBOL to Python modernization.
"""

import pytest
import json
from datetime import datetime
from unittest.mock import Mock, patch

from engine.functionality_mapper import (
    FunctionalityMapper, FunctionalityType, EquivalenceLevel
)
from engine.modernizers.cobol_system.functionality_mapper import (
    COBOLFunctionalityMapper, COBOLFieldMapping, COBOLProgramMapping,
    COBOLDataType
)


class TestCOBOLFunctionalityMapper:
    """Test the COBOL functionality mapper."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.cobol_mapper = COBOLFunctionalityMapper()
        
        # Sample COBOL source code for testing
        self.sample_cobol_source = """
        IDENTIFICATION DIVISION.
        PROGRAM-ID. PAYROLL-PROGRAM.
        
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT EMPLOYEE-FILE ASSIGN TO "employees.dat".
            SELECT PAYROLL-FILE ASSIGN TO "payroll.dat".
        
        DATA DIVISION.
        FILE SECTION.
        FD EMPLOYEE-FILE.
        01 EMPLOYEE-RECORD.
            05 EMPLOYEE-ID PIC 9(5).
            05 EMPLOYEE-NAME PIC X(30).
            05 EMPLOYEE-SALARY PIC 9(8)V99.
            05 EMPLOYEE-TAX-RATE PIC V999.
        
        WORKING-STORAGE SECTION.
        01 WS-TOTAL-SALARY PIC 9(10)V99 VALUE ZERO.
        01 WS-TOTAL-TAX PIC 9(10)V99 VALUE ZERO.
        
        PROCEDURE DIVISION.
        MAIN-LOGIC.
            PERFORM INITIALIZE-PROGRAM.
            PERFORM PROCESS-EMPLOYEES.
            PERFORM CALCULATE-TOTALS.
            PERFORM DISPLAY-RESULTS.
            STOP RUN.
        
        INITIALIZE-PROGRAM.
            OPEN INPUT EMPLOYEE-FILE.
            OPEN OUTPUT PAYROLL-FILE.
        
        PROCESS-EMPLOYEES.
            READ EMPLOYEE-FILE.
            PERFORM UNTIL EMPLOYEE-FILE-EOF
                PERFORM CALCULATE-EMPLOYEE-PAYROLL
                READ EMPLOYEE-FILE
            END-PERFORM.
        
        CALCULATE-EMPLOYEE-PAYROLL.
            COMPUTE EMPLOYEE-TAX = EMPLOYEE-SALARY * EMPLOYEE-TAX-RATE.
            COMPUTE EMPLOYEE-NET = EMPLOYEE-SALARY - EMPLOYEE-TAX.
            ADD EMPLOYEE-SALARY TO WS-TOTAL-SALARY.
            ADD EMPLOYEE-TAX TO WS-TOTAL-TAX.
            WRITE PAYROLL-RECORD FROM EMPLOYEE-RECORD.
        
        CALCULATE-TOTALS.
            DISPLAY "TOTAL SALARY: " WS-TOTAL-SALARY.
            DISPLAY "TOTAL TAX: " WS-TOTAL-TAX.
        
        DISPLAY-RESULTS.
            CLOSE EMPLOYEE-FILE.
            CLOSE PAYROLL-FILE.
        """
    
    def test_create_cobol_program_mapping(self):
        """Test creating a COBOL program mapping."""
        functionality_mapping = self.cobol_mapper.create_cobol_program_mapping(
            "PAYROLL-PROGRAM",
            "payroll_program",
            self.sample_cobol_source
        )
        
        assert functionality_mapping.functionality_id.startswith("PROG-")
        assert functionality_mapping.source_name == "PAYROLL-PROGRAM"
        assert functionality_mapping.target_name == "payroll_program"
        assert functionality_mapping.source_language == "cobol"
        assert functionality_mapping.target_language == "python"
        assert functionality_mapping.functionality_type == FunctionalityType.PROGRAM
    
    def test_map_cobol_fields(self):
        """Test mapping COBOL fields to Python."""
        functionality_mapping = self.cobol_mapper.create_cobol_program_mapping(
            "TEST-PROGRAM",
            "test_program"
        )
        
        field_mappings = [
            COBOLFieldMapping(
                source_name="EMPLOYEE-ID",
                target_name="employee_id",
                source_type="PIC 9(5)",
                target_type="int"
            ),
            COBOLFieldMapping(
                source_name="EMPLOYEE-NAME",
                target_name="employee_name",
                source_type="PIC X(30)",
                target_type="str"
            ),
            COBOLFieldMapping(
                source_name="EMPLOYEE-SALARY",
                target_name="employee_salary",
                source_type="PIC 9(8)V99",
                target_type="decimal.Decimal"
            )
        ]
        
        result = self.cobol_mapper.map_cobol_fields(
            functionality_mapping.functionality_id,
            field_mappings
        )
        
        assert len(result) == 3
        assert result[0].source_name == "EMPLOYEE-ID"
        assert result[0].target_name == "employee_id"
        assert result[0].target_type == "int"
        assert result[1].target_type == "str"
        assert result[2].target_type == "decimal.Decimal"
    
    def test_analyze_cobol_structure(self):
        """Test analyzing COBOL program structure."""
        functionality_mapping = self.cobol_mapper.create_cobol_program_mapping(
            "PAYROLL-PROGRAM",
            "payroll_program"
        )
        
        analysis = self.cobol_mapper.analyze_cobol_structure(
            self.sample_cobol_source
        )
        
        assert "divisions" in analysis
        assert "IDENTIFICATION" in analysis["divisions"]
        assert "ENVIRONMENT" in analysis["divisions"]
        assert "DATA" in analysis["divisions"]
        assert "PROCEDURE" in analysis["divisions"]
        
        assert "paragraphs" in analysis
        assert "MAIN-LOGIC" in analysis["paragraphs"]
        assert "INITIALIZE-PROGRAM" in analysis["paragraphs"]
        
        assert "files" in analysis
        assert "EMPLOYEE-FILE" in analysis["files"]
        assert "PAYROLL-FILE" in analysis["files"]
        
        assert "fields" in analysis
        assert len(analysis["fields"]) > 0
    
    def test_parse_pic_clause(self):
        """Test parsing PIC clauses."""
        # Test numeric fields
        result = self.cobol_mapper._parse_pic_clause("PIC 9(5)")
        assert result[0] == "9"  # type
        assert result[1] == 5    # length
        assert result[2] == 5    # precision (same as length for simple numeric)
        assert result[3] is None  # scale
        assert result[4] is False # is_signed
        
        # Test alphanumeric fields
        result = self.cobol_mapper._parse_pic_clause("PIC X(30)")
        assert result[0] == "X"  # type
        assert result[1] == 30   # length
        
        # Test decimal fields
        result = self.cobol_mapper._parse_pic_clause("PIC 9(8)V99")
        assert result[0] == "9"  # type
        assert result[1] == 10   # length (8 + 2)
        assert result[2] == 10   # precision (total length)
        assert result[3] == 2    # scale
        
        # Test signed fields
        result = self.cobol_mapper._parse_pic_clause("PIC S9(5)")
        assert result[0] == "9"  # type
        assert result[1] == 5    # length
        assert result[4] is True  # is_signed
    
    def test_convert_to_snake_case(self):
        """Test converting COBOL names to Python snake_case."""
        assert self.cobol_mapper._convert_to_snake_case("EMPLOYEE-ID") == "employee_id"
        assert self.cobol_mapper._convert_to_snake_case("WS-TOTAL-SALARY") == "total_salary"
        assert self.cobol_mapper._convert_to_snake_case("MAIN-LOGIC") == "main_logic"
        assert self.cobol_mapper._convert_to_snake_case("CALCULATE-EMPLOYEE-PAYROLL") == "calculate_employee_payroll"
    
    def test_map_cobol_paragraphs(self):
        """Test mapping COBOL paragraphs to Python functions."""
        functionality_mapping = self.cobol_mapper.create_cobol_program_mapping(
            "TEST-PROGRAM",
            "test_program"
        )
        
        paragraph_mappings = {
            "MAIN-LOGIC": "main_logic",
            "INITIALIZE-PROGRAM": "initialize_program",
            "CALCULATE-TAX": "calculate_tax"
        }
        
        result = self.cobol_mapper.map_cobol_paragraphs(
            functionality_mapping.functionality_id,
            paragraph_mappings
        )
        
        assert result["MAIN-LOGIC"] == "main_logic"
        assert result["INITIALIZE-PROGRAM"] == "initialize_program"
        assert result["CALCULATE-TAX"] == "calculate_tax"
    
    def test_map_cobol_files(self):
        """Test mapping COBOL files to Python file paths."""
        functionality_mapping = self.cobol_mapper.create_cobol_program_mapping(
            "TEST-PROGRAM",
            "test_program"
        )
        
        file_mappings = {
            "EMPLOYEE-FILE": "employees.dat",
            "PAYROLL-FILE": "payroll.dat",
            "OUTPUT-FILE": "output.dat"
        }
        
        result = self.cobol_mapper.map_cobol_files(
            functionality_mapping.functionality_id,
            file_mappings
        )
        
        assert result["EMPLOYEE-FILE"] == "employees.dat"
        assert result["PAYROLL-FILE"] == "payroll.dat"
        assert result["OUTPUT-FILE"] == "output.dat"
    
    def test_generate_python_equivalence_tests(self):
        """Test generating Python equivalence tests."""
        functionality_mapping = self.cobol_mapper.create_cobol_program_mapping(
            "TEST-PROGRAM",
            "test_program"
        )
        
        # Add some field mappings first
        field_mappings = [
            COBOLFieldMapping(
                source_name="EMPLOYEE-ID",
                target_name="employee_id",
                source_type="PIC 9(5)",
                target_type="int"
            )
        ]
        
        self.cobol_mapper.map_cobol_fields(
            functionality_mapping.functionality_id,
            field_mappings
        )
        
        test_cases = self.cobol_mapper.generate_python_equivalence_tests(
            functionality_mapping.functionality_id
        )
        
        assert len(test_cases) > 0
        assert all("test_type" in test_case for test_case in test_cases)
        assert all("cobol_field" in test_case or "cobol_paragraph" in test_case for test_case in test_cases)
    
    def test_extract_cobol_divisions(self):
        """Test extracting COBOL divisions."""
        divisions = self.cobol_mapper._parse_cobol_divisions(self.sample_cobol_source)
        
        assert "IDENTIFICATION" in divisions
        assert "ENVIRONMENT" in divisions
        assert "DATA" in divisions
        assert "PROCEDURE" in divisions
        
        assert "IDENTIFICATION DIVISION." in divisions["IDENTIFICATION"]
        assert "ENVIRONMENT DIVISION." in divisions["ENVIRONMENT"]
        assert "DATA DIVISION." in divisions["DATA"]
        assert "PROCEDURE DIVISION." in divisions["PROCEDURE"]
    
    def test_extract_paragraphs(self):
        """Test extracting COBOL paragraphs."""
        paragraphs = self.cobol_mapper._extract_paragraphs(self.sample_cobol_source)
        
        expected_paragraphs = [
            "MAIN-LOGIC", "INITIALIZE-PROGRAM", "PROCESS-EMPLOYEES",
            "CALCULATE-EMPLOYEE-PAYROLL", "CALCULATE-TOTALS", "DISPLAY-RESULTS"
        ]
        
        for paragraph in expected_paragraphs:
            assert paragraph in paragraphs
    
    def test_extract_file_definitions(self):
        """Test extracting COBOL file definitions."""
        files = self.cobol_mapper._extract_file_definitions(self.sample_cobol_source)
        
        assert "EMPLOYEE-FILE" in files
        assert "PAYROLL-FILE" in files
    
    def test_extract_field_definitions(self):
        """Test extracting COBOL field definitions."""
        fields = self.cobol_mapper._extract_field_definitions(self.sample_cobol_source)
        
        assert len(fields) > 0
        
        # Check for specific fields
        field_names = [field["name"] for field in fields]
        assert "EMPLOYEE-ID" in field_names
        assert "EMPLOYEE-NAME" in field_names
        assert "EMPLOYEE-SALARY" in field_names
        assert "WS-TOTAL-SALARY" in field_names
    
    def test_map_cobol_type_to_python(self):
        """Test mapping COBOL types to Python types."""
        # Test numeric types
        assert self.cobol_mapper._map_cobol_type_to_python("9", 5, 5, None) == "int"
        assert self.cobol_mapper._map_cobol_type_to_python("9", 10, 10, None) == "str"  # Large numbers as string
        
        # Test alphanumeric types
        assert self.cobol_mapper._map_cobol_type_to_python("X", 30, None, None) == "str"
        
        # Test decimal types
        assert self.cobol_mapper._map_cobol_type_to_python("9", 8, None, 2) == "decimal.Decimal"
        assert self.cobol_mapper._map_cobol_type_to_python("9", 10, None, 2) == "decimal.Decimal"
    
    def test_validate_equivalence(self):
        """Test validating COBOL to Python equivalence."""
        functionality_mapping = self.cobol_mapper.create_cobol_program_mapping(
            "TEST-PROGRAM",
            "test_program"
        )
        
        # Add some mappings
        self.cobol_mapper.map_inputs_outputs(
            functionality_mapping.functionality_id,
            source_inputs={"employee_data": "COBOL-RECORD"},
            target_inputs={"employee_data": "dict"},
            source_outputs={"result": "COBOL-RECORD"},
            target_outputs={"result": "dict"}
        )
        
        validation_result = self.cobol_mapper.validate_equivalence(
            functionality_mapping.functionality_id
        )
        
        assert "confidence_score" in validation_result
        assert "equivalence_level" in validation_result
        assert "issues" in validation_result
        assert isinstance(validation_result["confidence_score"], float)
        assert validation_result["confidence_score"] >= 0.0
        assert validation_result["confidence_score"] <= 1.0
    
    def test_export_import_mappings(self):
        """Test exporting and importing COBOL mappings."""
        # Create a mapping
        functionality_mapping = self.cobol_mapper.create_cobol_program_mapping(
            "EXPORT-TEST",
            "export_test"
        )
        
        # Export mappings
        exported_data = self.cobol_mapper.export_mappings("json")
        
        # Create new mapper and import
        new_mapper = COBOLFunctionalityMapper()
        imported_count = new_mapper.import_mappings(exported_data, "json")
        
        assert imported_count > 0
        
        # Verify the mapping was imported
        summary = new_mapper.get_mapping_summary()
        assert summary["total_mappings"] > 0 