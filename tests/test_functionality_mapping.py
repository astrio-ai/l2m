"""
Tests for Functionality Mapping System

This module tests the comprehensive functionality mapping system for
software modernization, including COBOL to Python and website modernization.
"""

import pytest
import json
from datetime import datetime
from unittest.mock import Mock, patch

from engine.functionality_mapper import (
    FunctionalityMapper, FunctionalityType, EquivalenceLevel,
    InputOutputMapping, BusinessLogicMapping
)
from engine.modernizers.cobol_system.functionality_mapper import (
    COBOLFunctionalityMapper, COBOLFieldMapping, COBOLProgramMapping
)
from engine.modernizers.static_site.functionality_mapper import (
    WebsiteFunctionalityMapper, WebsiteFramework, UIComponentType,
    UIComponentMapping, APIMapping
)


class TestFunctionalityMapper:
    """Test the base functionality mapper."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.mapper = FunctionalityMapper()
    
    def test_create_functionality_mapping(self):
        """Test creating a basic functionality mapping."""
        mapping = self.mapper.create_functionality_mapping(
            functionality_type=FunctionalityType.PROGRAM,
            source_name="LEGACY-PROG",
            target_name="modern_program",
            source_language="cobol",
            target_language="python"
        )
        
        assert mapping.functionality_id.startswith("PROG-")
        assert mapping.source_name == "LEGACY-PROG"
        assert mapping.target_name == "modern_program"
        assert mapping.source_language == "cobol"
        assert mapping.target_language == "python"
        assert mapping.functionality_type == FunctionalityType.PROGRAM
        assert mapping.equivalence_level == EquivalenceLevel.MEDIUM
    
    def test_map_inputs_outputs(self):
        """Test mapping inputs and outputs."""
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.FUNCTION,
            "ADD-NUMBERS",
            "add_numbers",
            "cobol",
            "python"
        )
        
        io_mapping = self.mapper.map_inputs_outputs(
            mapping.functionality_id,
            source_inputs={"num1": "PIC 9(5)", "num2": "PIC 9(5)"},
            target_inputs={"num1": "int", "num2": "int"},
            source_outputs={"result": "PIC 9(6)"},
            target_outputs={"result": "int"},
            data_transformations={"num1": "int(num1)", "num2": "int(num2)"}
        )
        
        assert io_mapping.source_inputs["num1"] == "PIC 9(5)"
        assert io_mapping.target_inputs["num1"] == "int"
        assert io_mapping.data_transformations["num1"] == "int(num1)"
    
    def test_map_business_logic(self):
        """Test mapping business logic."""
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.BUSINESS_RULE,
            "CALCULATE-TAX",
            "calculate_tax",
            "cobol",
            "python"
        )
        
        logic_mapping = self.mapper.map_business_logic(
            mapping.functionality_id,
            source_logic="IF AMOUNT > 10000 THEN TAX = AMOUNT * 0.15",
            target_logic="if amount > 10000: tax = amount * 0.15",
            business_rules=["Tax rate is 15% for amounts over 10000"],
            decision_points=["amount > 10000"]
        )
        
        assert logic_mapping.source_logic == "IF AMOUNT > 10000 THEN TAX = AMOUNT * 0.15"
        assert logic_mapping.target_logic == "if amount > 10000: tax = amount * 0.15"
        assert "Tax rate is 15% for amounts over 10000" in logic_mapping.business_rules
    
    def test_validate_equivalence(self):
        """Test equivalence validation."""
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.FUNCTION,
            "TEST-FUNC",
            "test_func",
            "cobol",
            "python"
        )
        
        # Add input/output mapping
        self.mapper.map_inputs_outputs(
            mapping.functionality_id,
            source_inputs={"input": "PIC X(10)"},
            target_inputs={"input": "str"},
            source_outputs={"output": "PIC X(10)"},
            target_outputs={"output": "str"}
        )
        
        # Add business logic mapping
        self.mapper.map_business_logic(
            mapping.functionality_id,
            source_logic="MOVE INPUT TO OUTPUT",
            target_logic="output = input",
            business_rules=["Simple pass-through"]
        )
        
        # Validate equivalence
        result = self.mapper.validate_equivalence(mapping.functionality_id)
        
        assert "functionality_id" in result
        assert "confidence_score" in result
        assert "io_validation" in result
        assert "logic_validation" in result
        assert result["confidence_score"] > 0
    
    def test_get_mapping_summary(self):
        """Test getting mapping summary."""
        # Create multiple mappings
        self.mapper.create_functionality_mapping(
            FunctionalityType.PROGRAM, "PROG1", "prog1", "cobol", "python"
        )
        self.mapper.create_functionality_mapping(
            FunctionalityType.FUNCTION, "FUNC1", "func1", "cobol", "python"
        )
        
        summary = self.mapper.get_mapping_summary()
        
        assert summary["total_mappings"] == 2
        assert "program" in summary["type_counts"]
        assert "function" in summary["type_counts"]
        assert "cobol→python" in summary["language_pairs"]
    
    def test_export_import_mappings(self):
        """Test exporting and importing mappings."""
        # Create a mapping
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.PROGRAM, "TEST-PROG", "test_prog", "cobol", "python"
        )
        
        # Export mappings
        exported = self.mapper.export_mappings("json")
        exported_data = json.loads(exported)
        
        assert "mappings" in exported_data
        assert len(exported_data["mappings"]) == 1
        assert exported_data["mappings"][0]["source_name"] == "TEST-PROG"
        
        # Create new mapper and import
        new_mapper = FunctionalityMapper()
        imported_count = new_mapper.import_mappings(exported, "json")
        
        assert imported_count == 1
        assert len(new_mapper.mappings) == 1


class TestCOBOLFunctionalityMapper:
    """Test the COBOL-specific functionality mapper."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.cobol_mapper = COBOLFunctionalityMapper()
    
    def test_create_cobol_program_mapping(self):
        """Test creating a COBOL program mapping."""
        functionality_mapping, cobol_mapping = self.cobol_mapper.create_cobol_program_mapping(
            "PAYROLL-PROG",
            "payroll_program",
            source_code="IDENTIFICATION DIVISION.\nPROGRAM-ID. PAYROLL-PROG."
        )
        
        assert functionality_mapping.functionality_id.startswith("PROG-PAYROLL-PROG")
        assert cobol_mapping.program_name == "PAYROLL-PROG"
        assert cobol_mapping.python_module_name == "payroll_program"
    
    def test_map_cobol_fields(self):
        """Test mapping COBOL fields."""
        functionality_mapping, cobol_mapping = self.cobol_mapper.create_cobol_program_mapping(
            "TEST-PROG", "test_prog"
        )
        
        field_definitions = [
            {"name": "WS-EMPLOYEE-NAME", "level": 1, "pic": "PIC X(30)"},
            {"name": "WS-SALARY", "level": 1, "pic": "PIC 9(8)V99"},
            {"name": "WS-TAX-RATE", "level": 1, "pic": "PIC V999"}
        ]
        
        field_mappings = self.cobol_mapper.map_cobol_fields(
            functionality_mapping.functionality_id,
            field_definitions
        )
        
        assert len(field_mappings) == 3
        assert field_mappings[0].cobol_name == "WS-EMPLOYEE-NAME"
        assert field_mappings[0].python_name == "employee_name"
        assert field_mappings[0].python_type == "str"
        assert field_mappings[1].python_type == "decimal.Decimal"
    
    def test_analyze_cobol_structure(self):
        """Test analyzing COBOL program structure."""
        functionality_mapping, cobol_mapping = self.cobol_mapper.create_cobol_program_mapping(
            "TEST-PROG", "test_prog"
        )
        
        cobol_source = """
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-PROG.
        
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT INPUT-FILE ASSIGN TO "input.dat".
        
        DATA DIVISION.
        FILE SECTION.
        FD INPUT-FILE.
        01 INPUT-RECORD.
            05 EMPLOYEE-NAME PIC X(30).
            05 EMPLOYEE-SALARY PIC 9(8)V99.
        
        WORKING-STORAGE SECTION.
        01 WS-TAX-RATE PIC V999 VALUE 0.15.
        
        PROCEDURE DIVISION.
        MAIN-LOGIC.
            PERFORM READ-INPUT.
            PERFORM CALCULATE-TAX.
            STOP RUN.
        
        READ-INPUT.
            READ INPUT-FILE.
        
        CALCULATE-TAX.
            COMPUTE TAX = EMPLOYEE-SALARY * WS-TAX-RATE.
        """
        
        analysis = self.cobol_mapper.analyze_cobol_structure(
            functionality_mapping.functionality_id,
            cobol_source
        )
        
        assert "IDENTIFICATION" in analysis["divisions"]
        assert "ENVIRONMENT" in analysis["divisions"]
        assert "DATA" in analysis["divisions"]
        assert "PROCEDURE" in analysis["divisions"]
        assert "MAIN-LOGIC" in analysis["paragraphs"]
        assert "READ-INPUT" in analysis["paragraphs"]
        assert "CALCULATE-TAX" in analysis["paragraphs"]
        assert "INPUT-FILE" in analysis["files"]
        assert len(analysis["fields"]) > 0
    
    def test_parse_pic_clause(self):
        """Test parsing COBOL PIC clauses."""
        mapper = self.cobol_mapper
        
        # Test alphanumeric
        cobol_type, length, precision, scale, is_signed = mapper._parse_pic_clause("PIC X(10)")
        assert cobol_type == "X"
        assert length == 10
        assert precision is None
        assert scale is None
        assert not is_signed
        
        # Test numeric
        cobol_type, length, precision, scale, is_signed = mapper._parse_pic_clause("PIC 9(5)")
        assert cobol_type == "9"
        assert length == 5
        assert precision == 5
        assert scale is None
        assert not is_signed
        
        # Test decimal
        cobol_type, length, precision, scale, is_signed = mapper._parse_pic_clause("PIC 9(8)V99")
        assert cobol_type == "9"
        assert length == 10
        assert precision == 10
        assert scale == 2
        assert not is_signed
        
        # Test signed
        cobol_type, length, precision, scale, is_signed = mapper._parse_pic_clause("PIC S9(5)")
        assert cobol_type == "9"
        assert is_signed
    
    def test_convert_to_snake_case(self):
        """Test converting COBOL names to Python snake_case."""
        mapper = self.cobol_mapper
        
        assert mapper._convert_to_snake_case("WS-EMPLOYEE-NAME") == "employee_name"
        assert mapper._convert_to_snake_case("LS-TAX-RATE") == "tax_rate"
        assert mapper._convert_to_snake_case("FD-INPUT-FILE") == "input_file"
        assert mapper._convert_to_snake_case("EMPLOYEE_SALARY") == "employee_salary"


class TestWebsiteFunctionalityMapper:
    """Test the website-specific functionality mapper."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.website_mapper = WebsiteFunctionalityMapper()
    
    def test_create_website_mapping(self):
        """Test creating a website mapping."""
        functionality_mapping, website_mapping = self.website_mapper.create_website_mapping(
            "https://legacy-site.com",
            "https://modern-site.com",
            WebsiteFramework.REACT
        )
        
        assert functionality_mapping.functionality_type == FunctionalityType.COMPONENT
        assert website_mapping.legacy_url == "https://legacy-site.com"
        assert website_mapping.modern_url == "https://modern-site.com"
        assert website_mapping.target_framework == WebsiteFramework.REACT
    
    def test_map_ui_components(self):
        """Test mapping UI components."""
        functionality_mapping, website_mapping = self.website_mapper.create_website_mapping(
            "https://test.com", "https://modern.com", WebsiteFramework.REACT
        )
        
        component_mappings = [
            {
                "legacy_selector": "#navigation",
                "modern_component": "Navigation",
                "component_type": "navigation",
                "props_mapping": {"title": "siteTitle"},
                "event_handlers": {"onClick": "handleNavClick"}
            },
            {
                "legacy_selector": ".contact-form",
                "modern_component": "ContactForm",
                "component_type": "form",
                "props_mapping": {"action": "submitUrl"},
                "event_handlers": {"onSubmit": "handleSubmit"}
            }
        ]
        
        ui_mappings = self.website_mapper.map_ui_components(
            functionality_mapping.functionality_id,
            component_mappings
        )
        
        assert len(ui_mappings) == 2
        assert ui_mappings[0].legacy_selector == "#navigation"
        assert ui_mappings[0].modern_component == "Navigation"
        assert ui_mappings[0].component_type == UIComponentType.NAVIGATION
        assert ui_mappings[1].component_type == UIComponentType.FORM
    
    def test_map_api_endpoints(self):
        """Test mapping API endpoints."""
        functionality_mapping, website_mapping = self.website_mapper.create_website_mapping(
            "https://test.com", "https://modern.com", WebsiteFramework.NEXTJS
        )
        
        api_mappings = [
            {
                "legacy_endpoint": "/api/users",
                "modern_endpoint": "/api/users",
                "http_method": "GET",
                "request_mapping": {"id": "userId"},
                "response_mapping": {"user_data": "userData"}
            },
            {
                "legacy_endpoint": "/api/users",
                "modern_endpoint": "/api/users",
                "http_method": "POST",
                "request_mapping": {"name": "userName", "email": "userEmail"},
                "response_mapping": {"success": "isSuccess"}
            }
        ]
        
        api_mappings_list = self.website_mapper.map_api_endpoints(
            functionality_mapping.functionality_id,
            api_mappings
        )
        
        assert len(api_mappings_list) == 2
        assert api_mappings_list[0].legacy_endpoint == "/api/users"
        assert api_mappings_list[0].http_method == "GET"
        assert api_mappings_list[1].http_method == "POST"
    
    def test_analyze_legacy_website(self):
        """Test analyzing legacy website structure."""
        functionality_mapping, website_mapping = self.website_mapper.create_website_mapping(
            "https://test.com", "https://modern.com", WebsiteFramework.REACT
        )
        
        html_content = """
        <!DOCTYPE html>
        <html>
        <head>
            <title>Test Site</title>
            <link rel="stylesheet" href="styles.css">
        </head>
        <body>
            <header>
                <nav>
                    <a href="/">Home</a>
                    <a href="/about">About</a>
                </nav>
            </header>
            
            <main>
                <form action="/submit" method="post">
                    <input type="text" name="name" placeholder="Name">
                    <input type="email" name="email" placeholder="Email">
                    <button type="submit">Submit</button>
                </form>
                
                <table>
                    <tr><th>Name</th><th>Email</th></tr>
                    <tr><td>John</td><td>john@example.com</td></tr>
                </table>
            </main>
            
            <footer>
                <p>&copy; 2024 Test Site</p>
            </footer>
            
            <script src="script.js"></script>
        </body>
        </html>
        """
        
        analysis = self.website_mapper.analyze_legacy_website(
            functionality_mapping.functionality_id,
            html_content
        )
        
        assert len(analysis["components"]) > 0
        assert len(analysis["forms"]) > 0
        assert len(analysis["tables"]) > 0
        assert len(analysis["navigation"]) > 0
        assert len(analysis["scripts"]) > 0
        assert len(analysis["styles"]) > 0
        assert len(analysis["links"]) > 0
    
    def test_generate_modernization_plan(self):
        """Test generating modernization plan."""
        functionality_mapping, website_mapping = self.website_mapper.create_website_mapping(
            "https://test.com", "https://modern.com", WebsiteFramework.REACT
        )
        
        # Add some component mappings
        component_mappings = [
            {
                "legacy_selector": "#nav",
                "modern_component": "Navigation",
                "component_type": "navigation"
            }
        ]
        
        self.website_mapper.map_ui_components(
            functionality_mapping.functionality_id,
            component_mappings
        )
        
        plan = self.website_mapper.generate_modernization_plan(
            functionality_mapping.functionality_id
        )
        
        assert plan["target_framework"] == "react"
        assert plan["components_to_create"] == 1
        assert len(plan["steps"]) > 0
        assert len(plan["recommendations"]) > 0
    
    def test_generate_component_id(self):
        """Test generating component ID from URL."""
        mapper = self.website_mapper
        
        assert mapper._generate_component_id("https://example.com") == "EXAMPLE_COM_HOME"
        assert mapper._generate_component_id("https://test.com/about") == "TEST_COM_ABOUT"
        assert mapper._generate_component_id("https://site.com/products/123") == "SITE_COM_PRODUCTS_123"


if __name__ == "__main__":
    pytest.main([__file__]) 