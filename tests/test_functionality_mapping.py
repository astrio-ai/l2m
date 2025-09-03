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
    InputOutputMapping, BusinessLogicMapping, ValidationStrategy,
    TestType, TestCase, ValidationResult, TestResult
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
    
    def test_validate_equivalence_basic(self):
        """Test basic equivalence validation."""
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.FUNCTION,
            "TEST-FUNC",
            "test_func",
            "cobol",
            "python"
        )
        
        # Add some business logic
        self.mapper.map_business_logic(
            mapping.functionality_id,
            source_logic="IF X > 0 THEN Y = X * 2",
            target_logic="if x > 0: y = x * 2"
        )
        
        result = self.mapper.validate_equivalence(mapping.functionality_id)
        
        assert "confidence_score" in result
        assert "validation_status" in result
        assert "validation_results" in result
        assert "test_results" in result
        assert "summary" in result
        assert isinstance(result["confidence_score"], float)
        assert result["confidence_score"] >= 0.0
        assert result["confidence_score"] <= 1.0
    
    def test_validate_equivalence_with_test_cases(self):
        """Test equivalence validation with test cases."""
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.FUNCTION,
            "ADD-FUNC",
            "add_func",
            "cobol",
            "python"
        )
        
        # Add business logic
        self.mapper.map_business_logic(
            mapping.functionality_id,
            source_logic="RESULT = A + B",
            target_logic="result = a + b"
        )
        
        # Create test cases
        test_cases = [
            TestCase(
                test_id="test_1",
                test_type=TestType.UNIT_TEST,
                name="Basic Addition Test",
                description="Test basic addition functionality",
                inputs={"a": 5, "b": 3},
                expected_outputs={"result": 8}
            ),
            TestCase(
                test_id="test_2",
                test_type=TestType.EDGE_CASE_TEST,
                name="Zero Addition Test",
                description="Test addition with zero",
                inputs={"a": 5, "b": 0},
                expected_outputs={"result": 5}
            )
        ]
        
        result = self.mapper.validate_equivalence(
            mapping.functionality_id,
            test_cases=test_cases
        )
        
        assert "test_results" in result
        assert len(result["test_results"]) == 2
        assert result["test_results"][0]["test_id"] == "test_1"
        assert result["test_results"][1]["test_id"] == "test_2"
    
    def test_validate_equivalence_with_specific_strategies(self):
        """Test equivalence validation with specific validation strategies."""
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.FUNCTION,
            "SECURE-FUNC",
            "secure_func",
            "cobol",
            "python"
        )
        
        # Add business logic with security considerations
        self.mapper.map_business_logic(
            mapping.functionality_id,
            source_logic="ACCEPT USER-INPUT",
            target_logic="user_input = input('Enter data: ')",
            error_handling={"invalid_input": "raise ValueError"}
        )
        
        # Test with specific validation strategies
        strategies = [
            ValidationStrategy.SECURITY_AUDIT,
            ValidationStrategy.CODE_QUALITY,
            ValidationStrategy.SYNTAX_CHECK
        ]
        
        result = self.mapper.validate_equivalence(
            mapping.functionality_id,
            validation_strategies=strategies
        )
        
        assert "validation_results" in result
        validation_results = result["validation_results"]
        
        # Check that only requested strategies were run
        assert len(validation_results) == 3
        assert "security_audit" in validation_results
        assert "code_quality" in validation_results
        assert "syntax_check" in validation_results
        assert "semantic_analysis" not in validation_results  # Not requested
    
    def test_get_mapping_summary(self):
        """Test getting mapping summary with enhanced statistics."""
        # Create multiple mappings
        mapping1 = self.mapper.create_functionality_mapping(
            FunctionalityType.FUNCTION,
            "FUNC-1",
            "func_1",
            "cobol",
            "python"
        )
        
        mapping2 = self.mapper.create_functionality_mapping(
            FunctionalityType.PROGRAM,
            "PROG-1",
            "prog_1",
            "cobol",
            "python"
        )
        
        # Validate mappings to populate statistics
        self.mapper.validate_equivalence(mapping1.functionality_id)
        self.mapper.validate_equivalence(mapping2.functionality_id)
        
        summary = self.mapper.get_mapping_summary()
        
        assert summary["total_mappings"] == 2
        assert summary["validated_count"] >= 0
        assert summary["failed_count"] >= 0
        assert summary["needs_review_count"] >= 0
        assert "validation_strategy_stats" in summary
        assert isinstance(summary["average_confidence"], float)
    
    def test_export_import_mappings(self):
        """Test exporting and importing mappings."""
        # Create a mapping
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.FUNCTION,
            "EXPORT-TEST",
            "export_test",
            "cobol",
            "python"
        )
        
        # Add some data
        self.mapper.map_business_logic(
            mapping.functionality_id,
            source_logic="TEST LOGIC",
            target_logic="test_logic"
        )
        
        # Export
        exported_data = self.mapper.export_mappings("json")
        exported_dict = json.loads(exported_data)
        
        assert len(exported_dict) == 1
        assert exported_dict[0]["source_name"] == "EXPORT-TEST"
        
        # Create new mapper and import
        new_mapper = FunctionalityMapper()
        imported_count = new_mapper.import_mappings(exported_data, "json")
        
        assert imported_count == 1
        assert len(new_mapper.mappings) == 1
        
        # Verify imported mapping
        imported_mapping = list(new_mapper.mappings.values())[0]
        assert imported_mapping.source_name == "EXPORT-TEST"
        assert imported_mapping.business_logic_mapping.source_logic == "TEST LOGIC"


class TestValidationEngine:
    """Test the advanced validation engine."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.mapper = FunctionalityMapper()
        self.validation_engine = self.mapper.validation_engine
    
    def test_syntax_validation(self):
        """Test syntax validation strategy."""
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.PROGRAM,
            "SYNTAX-TEST",
            "syntax_test",
            "cobol",
            "python"
        )
        
        # Add COBOL code with PROGRAM-ID
        self.mapper.map_business_logic(
            mapping.functionality_id,
            source_logic="       IDENTIFICATION DIVISION.\n       PROGRAM-ID. SYNTAX-TEST.",
            target_logic="def syntax_test():\n    pass"
        )
        
        result = self.validation_engine.validate_mapping(
            mapping, 
            strategies=[ValidationStrategy.SYNTAX_CHECK]
        )
        
        assert "syntax_check" in result
        syntax_result = result["syntax_check"]
        assert isinstance(syntax_result, ValidationResult)
        assert syntax_result.strategy == ValidationStrategy.SYNTAX_CHECK
        assert isinstance(syntax_result.score, float)
        assert syntax_result.score >= 0.0
        assert syntax_result.score <= 1.0
    
    def test_semantic_validation(self):
        """Test semantic validation strategy."""
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.FUNCTION,
            "SEMANTIC-TEST",
            "semantic_test",
            "cobol",
            "python"
        )
        
        # Add logic with conditional statements
        self.mapper.map_business_logic(
            mapping.functionality_id,
            source_logic="IF CONDITION THEN ACTION",
            target_logic="if condition:\n    action"
        )
        
        result = self.validation_engine.validate_mapping(
            mapping,
            strategies=[ValidationStrategy.SEMANTIC_ANALYSIS]
        )
        
        assert "semantic_analysis" in result
        semantic_result = result["semantic_analysis"]
        assert semantic_result.strategy == ValidationStrategy.SEMANTIC_ANALYSIS
        assert isinstance(semantic_result.score, float)
    
    def test_security_validation(self):
        """Test security validation strategy."""
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.FUNCTION,
            "SECURITY-TEST",
            "security_test",
            "cobol",
            "python"
        )
        
        # Add code with potential security issues
        self.mapper.map_business_logic(
            mapping.functionality_id,
            source_logic="ACCEPT USER-INPUT",
            target_logic="result = eval(input('Enter code: '))"  # Dangerous!
        )
        
        result = self.validation_engine.validate_mapping(
            mapping,
            strategies=[ValidationStrategy.SECURITY_AUDIT]
        )
        
        assert "security_audit" in result
        security_result = result["security_audit"]
        assert security_result.strategy == ValidationStrategy.SECURITY_AUDIT
        assert len(security_result.issues) > 0  # Should detect eval() usage
    
    def test_performance_validation(self):
        """Test performance validation strategy."""
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.FUNCTION,
            "PERF-TEST",
            "perf_test",
            "cobol",
            "python"
        )
        
        result = self.validation_engine.validate_mapping(
            mapping,
            strategies=[ValidationStrategy.PERFORMANCE_BENCHMARK]
        )
        
        assert "performance_benchmark" in result
        perf_result = result["performance_benchmark"]
        assert perf_result.strategy == ValidationStrategy.PERFORMANCE_BENCHMARK
        assert "metrics" in perf_result.__dict__
    
    def test_code_quality_validation(self):
        """Test code quality validation strategy."""
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.FUNCTION,
            "QUALITY-TEST",
            "quality_test",
            "cobol",
            "python"
        )
        
        # Add complex code
        complex_code = "if x > 0:\n" + "    if y > 0:\n" * 10 + "        pass"
        self.mapper.map_business_logic(
            mapping.functionality_id,
            source_logic="SIMPLE LOGIC",
            target_logic=complex_code
        )
        
        result = self.validation_engine.validate_mapping(
            mapping,
            strategies=[ValidationStrategy.CODE_QUALITY]
        )
        
        assert "code_quality" in result
        quality_result = result["code_quality"]
        assert quality_result.strategy == ValidationStrategy.CODE_QUALITY
        assert len(quality_result.warnings) > 0  # Should detect complexity


class TestTestEngine:
    """Test the advanced test execution engine."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.mapper = FunctionalityMapper()
        self.test_engine = self.mapper.test_engine
    
    def test_unit_test_execution(self):
        """Test unit test execution."""
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.FUNCTION,
            "UNIT-TEST",
            "unit_test",
            "cobol",
            "python"
        )
        
        test_case = TestCase(
            test_id="unit_1",
            test_type=TestType.UNIT_TEST,
            name="Basic Unit Test",
            description="Test basic functionality",
            inputs={"x": 5, "y": 3},
            expected_outputs={"result": 8}
        )
        
        results = self.test_engine.execute_test_suite(mapping, [test_case])
        
        assert len(results) == 1
        result = results[0]
        assert isinstance(result, TestResult)
        assert result.test_case.test_id == "unit_1"
        assert result.test_case.test_type == TestType.UNIT_TEST
        assert isinstance(result.passed, bool)
        assert isinstance(result.execution_time, float)
    
    def test_stress_test_execution(self):
        """Test stress test execution."""
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.FUNCTION,
            "STRESS-TEST",
            "stress_test",
            "cobol",
            "python"
        )
        
        test_case = TestCase(
            test_id="stress_1",
            test_type=TestType.STRESS_TEST,
            name="Performance Stress Test",
            description="Test performance under load",
            inputs={"load": "high"},
            expected_outputs={}
        )
        
        results = self.test_engine.execute_test_suite(mapping, [test_case])
        
        assert len(results) == 1
        result = results[0]
        assert result.test_case.test_type == TestType.STRESS_TEST
        assert "performance_metrics" in result.__dict__
        assert "memory_usage" in result.__dict__
        assert "cpu_usage" in result.__dict__
    
    def test_edge_case_test_execution(self):
        """Test edge case test execution."""
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.FUNCTION,
            "EDGE-TEST",
            "edge_test",
            "cobol",
            "python"
        )
        
        test_case = TestCase(
            test_id="edge_1",
            test_type=TestType.EDGE_CASE_TEST,
            name="Boundary Test",
            description="Test boundary conditions",
            inputs={"edge_case": True},
            expected_outputs={"handled": True}
        )
        
        results = self.test_engine.execute_test_suite(mapping, [test_case])
        
        assert len(results) == 1
        result = results[0]
        assert result.test_case.test_type == TestType.EDGE_CASE_TEST
        assert len(result.warnings) > 0  # Should detect edge case
    
    def test_security_test_execution(self):
        """Test security test execution."""
        mapping = self.mapper.create_functionality_mapping(
            FunctionalityType.FUNCTION,
            "SEC-TEST",
            "sec_test",
            "cobol",
            "python"
        )
        
        test_case = TestCase(
            test_id="sec_1",
            test_type=TestType.SECURITY_TEST,
            name="Security Vulnerability Test",
            description="Test for security vulnerabilities",
            inputs={"malicious_input": True},
            expected_outputs={"secure": True}
        )
        
        results = self.test_engine.execute_test_suite(mapping, [test_case])
        
        assert len(results) == 1
        result = results[0]
        assert result.test_case.test_type == TestType.SECURITY_TEST
        assert len(result.warnings) > 0  # Should detect security issue


class TestCOBOLFunctionalityMapper:
    """Test COBOL-specific functionality mapper."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.cobol_mapper = COBOLFunctionalityMapper()
    
    def test_create_cobol_program_mapping(self):
        """Test creating COBOL program mapping."""
        mapping = self.cobol_mapper.create_cobol_program_mapping(
            "PAYROLL-PROG",
            "payroll_program",
            source_code="       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PAYROLL-PROG."
        )
        
        assert mapping.functionality_id.startswith("PROG-")
        assert mapping.source_name == "PAYROLL-PROG"
        assert mapping.target_name == "payroll_program"
        assert mapping.source_language == "cobol"
        assert mapping.target_language == "python"
    
    def test_map_cobol_fields(self):
        """Test mapping COBOL fields."""
        mapping = self.cobol_mapper.create_cobol_program_mapping(
            "FIELD-TEST",
            "field_test"
        )
        
        field_mappings = [
            COBOLFieldMapping(
                source_name="EMPLOYEE-ID",
                target_name="employee_id",
                source_type="PIC 9(6)",
                target_type="int"
            ),
            COBOLFieldMapping(
                source_name="EMPLOYEE-NAME",
                target_name="employee_name",
                source_type="PIC X(30)",
                target_type="str"
            ),
            COBOLFieldMapping(
                source_name="SALARY",
                target_name="salary",
                source_type="PIC 9(8)V99",
                target_type="float"
            )
        ]
        
        result = self.cobol_mapper.map_cobol_fields(
            mapping.functionality_id,
            field_mappings
        )
        
        assert len(result) == 3
        assert result[0].source_name == "EMPLOYEE-ID"
        assert result[0].target_name == "employee_id"
        assert result[0].source_type == "PIC 9(6)"
        assert result[0].target_type == "int"
    
    def test_analyze_cobol_structure(self):
        """Test COBOL structure analysis."""
        cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 EMPLOYEE-RECORD.
               05 EMPLOYEE-ID PIC 9(6).
               05 EMPLOYEE-NAME PIC X(30).
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD'.
           STOP RUN.
        """
        
        analysis = self.cobol_mapper.analyze_cobol_structure(cobol_code)
        
        assert "program_name" in analysis
        assert "data_structures" in analysis
        assert "paragraphs" in analysis  # Changed from "procedures" to "paragraphs"
        assert analysis["program_name"] == "TEST-PROG"
        assert len(analysis["data_structures"]) > 0
    
    def test_parse_pic_clause(self):
        """Test PIC clause parsing."""
        pic_tests = [
            ("PIC 9(6)", "int"),
            ("PIC X(30)", "str"),
            ("PIC 9(8)V99", "float"),
            ("PIC 9(3)", "int"),
            ("PIC X(10)", "str")
        ]
        
        for pic_clause, expected_type in pic_tests:
            parsed_type = self.cobol_mapper.parse_pic_clause(pic_clause)
            assert parsed_type == expected_type
    
    def test_convert_to_snake_case(self):
        """Test COBOL to snake_case conversion."""
        conversions = [
            ("EMPLOYEE-ID", "employee_id"),
            ("CUSTOMER-NAME", "customer_name"),
            ("SALARY-AMOUNT", "salary_amount"),
            ("PAYROLL-PROGRAM", "payroll_program")
        ]
        
        for cobol_name, expected_snake in conversions:
            converted = self.cobol_mapper.convert_to_snake_case(cobol_name)
            assert converted == expected_snake


class TestWebsiteFunctionalityMapper:
    """Test website functionality mapper."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.website_mapper = WebsiteFunctionalityMapper()
    
    def test_create_website_mapping(self):
        """Test creating website mapping."""
        mapping = self.website_mapper.create_website_mapping(
            "legacy-site.html",
            WebsiteFramework.REACT,
            "modern-app"
        )
        
        assert mapping.functionality_id.startswith("COMP-")
        assert mapping.source_name == "legacy-site.html"
        assert mapping.target_name == "modern-app"
        assert mapping.source_language == "html"
        assert mapping.target_language == "react"
    
    def test_map_ui_components(self):
        """Test mapping UI components."""
        mapping = self.website_mapper.create_website_mapping(
            "test-site.html",
            WebsiteFramework.REACT,
            "test-app"
        )
        
        components = [
            UIComponentMapping("header", UIComponentType.HEADER, "Header", "Header"),
            UIComponentMapping("nav", UIComponentType.NAVIGATION, "Nav", "Navigation"),
            UIComponentMapping("main", UIComponentType.CONTENT, "Main", "MainContent")
        ]
        
        result = self.website_mapper.map_ui_components(
            mapping.functionality_id,
            components
        )
        
        assert len(result) == 3
        assert result[0].component_id == "header"
        assert result[0].component_type == UIComponentType.HEADER
        assert result[0].source_name == "Header"
        assert result[0].target_name == "Header"
    
    def test_map_api_endpoints(self):
        """Test mapping API endpoints."""
        mapping = self.website_mapper.create_website_mapping(
            "api-site.html",
            WebsiteFramework.NEXTJS,
            "api-app"
        )
        
        endpoints = [
            APIMapping("/api/users", "GET", "get_users", "getUsers"),
            APIMapping("/api/users", "POST", "create_user", "createUser")
        ]
        
        result = self.website_mapper.map_api_endpoints(
            mapping.functionality_id,
            endpoints
        )
        
        assert len(result) == 2
        assert result[0].endpoint_path == "/api/users"
        assert result[0].http_method == "GET"
        assert result[0].source_name == "get_users"
        assert result[0].target_name == "getUsers"
    
    def test_analyze_legacy_website(self):
        """Test legacy website analysis."""
        html_code = """
        <!DOCTYPE html>
        <html>
        <head>
            <title>Legacy Site</title>
            <link href="bootstrap.css" rel="stylesheet">
        </head>
        <body>
            <div class="container">
                <h1>Welcome</h1>
                <button onclick="showAlert()">Click me</button>
            </div>
            <script src="jquery.js"></script>
            <script>
                function showAlert() {
                    alert('Hello!');
                }
            </script>
        </body>
        </html>
        """
        
        analysis = self.website_mapper.analyze_legacy_website(html_code)
        
        assert "framework_dependencies" in analysis
        assert "components" in analysis
        assert "javascript_functions" in analysis
        assert "bootstrap" in analysis["framework_dependencies"]
        assert "jquery" in analysis["framework_dependencies"]
    
    def test_generate_modernization_plan(self):
        """Test modernization plan generation."""
        mapping = self.website_mapper.create_website_mapping(
            "plan-test.html",
            WebsiteFramework.REACT,
            "plan-app"
        )
        
        plan = self.website_mapper.generate_modernization_plan(mapping.functionality_id)
        
        assert "target_framework" in plan
        assert "components_to_create" in plan
        assert "api_endpoints_to_migrate" in plan
        assert "migration_steps" in plan
        assert plan["target_framework"] == WebsiteFramework.REACT.value
    
    def test_generate_component_id(self):
        """Test component ID generation."""
        component_ids = [
            ("header", "header"),
            ("main-navigation", "main_navigation"),
            ("user-profile-card", "user_profile_card"),
            ("contact-form", "contact_form")
        ]
        
        for input_id, expected_id in component_ids:
            generated_id = self.website_mapper._generate_component_id(input_id)
            assert generated_id == expected_id


if __name__ == "__main__":
    pytest.main([__file__]) 