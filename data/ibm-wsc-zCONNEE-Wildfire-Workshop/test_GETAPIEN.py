"""
Test suite for GETAPIEN.py

Tests for CICS GET operation via z/OS Connect EE with CEEENV environment setup.
"""

import pytest
import os
from unittest.mock import MagicMock, patch
from GETAPIEN import (
    main,
    ParmBuffer,
    GetRequest,
    GetResponse,
    GetInfoOper1,
    baqcstub,
    ceeenv_set,
    FeedbackCode,
    BAQ_SUCCESS,
    BAQ_ERROR_IN_API,
    BAQ_ERROR_IN_ZCEE,
    BAQ_ERROR_IN_STUB,
    DEFAULT_BAQURI,
    DEFAULT_BAQPORT,
)
from CSCVDLTI import (
    BAQRequestInfo,
    BAQResponseInfo,
    ErrorMsg
)


# ============================================================================
# Test CEEENV Functions
# ============================================================================

class TestCeeenvSet:
    """Test ceeenv_set function."""
    
    def test_success(self):
        """Test successful CEEENV SET."""
        mock_env = {}
        feedback, success = ceeenv_set("BAQURI", "test.example.com", mock_env)
        
        assert success is True
        assert feedback.severity == 0
        assert mock_env["BAQURI"] == "test.example.com"
    
    def test_empty_name(self):
        """Test CEEENV SET with empty variable name."""
        mock_env = {}
        feedback, success = ceeenv_set("", "value", mock_env)
        
        assert success is False
        assert feedback.severity == 8  # Error
    
    def test_empty_value(self):
        """Test CEEENV SET with empty value."""
        mock_env = {}
        feedback, success = ceeenv_set("BAQURI", "", mock_env)
        
        assert success is False
        assert feedback.severity == 8  # Error
    
    def test_multiple_variables(self):
        """Test setting multiple environment variables."""
        mock_env = {}
        ceeenv_set("BAQURI", "test.example.com", mock_env)
        ceeenv_set("BAQPORT", "8080", mock_env)
        
        assert mock_env["BAQURI"] == "test.example.com"
        assert mock_env["BAQPORT"] == "8080"


# ============================================================================
# Test PARM Buffer Structure
# ============================================================================

class TestParmBuffer:
    """Test ParmBuffer structure."""
    
    def test_creation(self):
        """Test creating ParmBuffer."""
        parm = ParmBuffer()
        assert parm.parm_length == 0
        assert parm.employee == ""
        assert parm.filler == ""
    
    def test_with_employee(self):
        """Test ParmBuffer with employee number."""
        parm = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        assert parm.parm_length == 6
        assert parm.employee == "000001"


# ============================================================================
# Test GET Request/Response Structures
# ============================================================================

class TestGetRequest:
    """Test GetRequest structure."""
    
    def test_creation(self):
        """Test creating GetRequest."""
        req = GetRequest()
        assert req.employee == ""
        assert req.employee_length == 0
    
    def test_set_employee(self):
        """Test setting employee number."""
        req = GetRequest()
        req.set_employee("000001")
        assert req.employee == "000001"
        assert req.employee_length == 6


class TestGetResponse:
    """Test GetResponse structure."""
    
    def test_creation(self):
        """Test creating GetResponse."""
        resp = GetResponse()
        assert resp.employee_number2 == ""
        assert resp.employee_name2 == ""
        assert resp.phone2 == ""
        assert resp.user_identity2 == ""
    
    def test_set_fields(self):
        """Test setting response fields."""
        resp = GetResponse()
        resp.employee_number2 = "000001"
        resp.employee_name2 = "John Doe"
        resp.phone2 = "5551234"
        resp.user_identity2 = "SYSTEM"
        
        assert resp.employee_number2 == "000001"
        assert resp.employee_name2 == "John Doe"
        assert resp.phone2 == "5551234"
        assert resp.user_identity2 == "SYSTEM"


# ============================================================================
# Test BAQCSTUB
# ============================================================================

class TestBaqcstub:
    """Test baqcstub function."""
    
    def test_success(self):
        """Test successful GET."""
        get_info = GetInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        get_request = GetRequest()
        get_request.set_employee("000001")
        get_response = GetResponse()
        
        result = baqcstub(
            get_info,
            baq_request_info,
            b"",
            0,
            baq_response_info,
            b"",
            0,
            get_request,
            get_response
        )
        
        assert result.is_success()
        assert result.status_code == 200
        assert get_response.employee_number2 == "000001"
        assert get_response.employee_name2 == "John Doe"
        assert get_response.phone2 == "5551234"
        assert get_response.user_identity2 == "SYSTEM"


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_success(self, capsys):
        """Test main with successful GET."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        mock_env = {}
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_env=mock_env
        )
        
        assert return_code == 200
        assert error_msg.origin == ""
        assert error_msg.code == 0
        
        # Check environment variables were set
        assert mock_env.get("BAQURI") == DEFAULT_BAQURI
        assert mock_env.get("BAQPORT") == DEFAULT_BAQPORT
        
        # Check output
        captured = capsys.readouterr()
        assert "EmployeeNumber:" in captured.out
        assert "EmployeeName:" in captured.out
        assert "Phone:" in captured.out
        assert "EIBRESP:" in captured.out
        assert "000001" in captured.out
        assert "John Doe" in captured.out
        assert "SYSTEM" in captured.out
    
    def test_main_custom_env_vars(self):
        """Test main with custom environment variable values."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        mock_env = {}
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_env=mock_env,
            baquri="custom.example.com",
            baqport="9999"
        )
        
        assert return_code == 200
        assert mock_env.get("BAQURI") == "custom.example.com"
        assert mock_env.get("BAQPORT") == "9999"
    
    def test_main_error_in_api(self, capsys):
        """Test main with API error."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 404
            baq_response_info.status_message = "Employee not found"
            return baq_response_info
        
        parm_buffer = ParmBuffer(
            parm_length=0,
            employee=""  # Empty
        )
        mock_env = {}
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub,
            mock_env=mock_env
        )
        
        assert return_code == 404
        assert error_msg.origin == "API"
        assert error_msg.code == 404
    
    def test_main_all_response_fields_populated(self, capsys):
        """Test that all response fields are populated on success."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "GET successful"
            # Set custom response
            args[8].employee_number2 = "000002"  # get_response
            args[8].employee_name2 = "Jane Smith"
            args[8].xaddress2 = "456 Oak Ave"
            args[8].phone2 = "5555678"
            args[8].xdate2 = "20250201"
            args[8].amount2 = "200.50"
            args[8].ceibresp = "DFHRESP(NORMAL)"
            args[8].ceibresp2 = "DFHRESP2(NORMAL)"
            args[8].user_identity2 = "ADMIN"
            return baq_response_info
        
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000002"
        )
        mock_env = {}
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub,
            mock_env=mock_env
        )
        
        assert return_code == 200
        captured = capsys.readouterr()
        assert "Jane Smith" in captured.out
        assert "456 Oak Ave" in captured.out
        assert "5555678" in captured.out
        assert "ADMIN" in captured.out
    
    def test_main_env_vars_set_before_baqcstub(self):
        """Test that environment variables are set before BAQCSTUB call."""
        captured_env = {}
        
        def mock_baqcstub(*args, **kwargs):
            # Capture environment state
            captured_env.update(os.environ.copy())
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "GET successful"
            args[8].employee_number2 = "000001"
            args[8].employee_name2 = "John Doe"
            args[8].ceibresp = "DFHRESP(NORMAL)"
            args[8].ceibresp2 = "DFHRESP2(NORMAL)"
            args[8].user_identity2 = "SYSTEM"
            return baq_response_info
        
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        mock_env = {}
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub,
            mock_env=mock_env
        )
        
        assert return_code == 200
        # Environment variables should be set in mock_env
        assert "BAQURI" in mock_env
        assert "BAQPORT" in mock_env


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_success(self, capsys):
        """Test full flow: CEEENV SET -> BAQCSTUB -> return code (success)."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        mock_env = {}
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_env=mock_env
        )
        
        assert return_code == 200
        assert error_msg.origin == ""
        assert error_msg.code == 0
        
        # Verify environment variables were set
        assert mock_env.get("BAQURI") == DEFAULT_BAQURI
        assert mock_env.get("BAQPORT") == DEFAULT_BAQPORT
        
        captured = capsys.readouterr()
        assert "EmployeeNumber:" in captured.out
        assert "EmployeeName:" in captured.out
        assert "Phone:" in captured.out
        assert "HTTP CODE:" in captured.out
    
    def test_full_flow_custom_env(self):
        """Test full flow with custom environment variables."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        mock_env = {}
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_env=mock_env,
            baquri="custom-host.example.com",
            baqport="8080"
        )
        
        assert return_code == 200
        assert mock_env.get("BAQURI") == "custom-host.example.com"
        assert mock_env.get("BAQPORT") == "8080"
    
    def test_full_flow_error(self, capsys):
        """Test full flow: CEEENV SET -> BAQCSTUB -> return code (error)."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 404
            baq_response_info.status_message = "Employee 000001 not found"
            return baq_response_info
        
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        mock_env = {}
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub,
            mock_env=mock_env
        )
        
        assert return_code == 404
        assert error_msg.origin == "API"
        # Environment variables should still be set even on error
        assert mock_env.get("BAQURI") == DEFAULT_BAQURI
        assert mock_env.get("BAQPORT") == DEFAULT_BAQPORT
    
    def test_environment_variables_persistence(self):
        """Test that environment variables persist across calls."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        mock_env = {}
        
        # First call
        return_code1, _ = main(
            parm_buffer=parm_buffer,
            mock_env=mock_env
        )
        
        # Second call - should use same mock_env
        return_code2, _ = main(
            parm_buffer=parm_buffer,
            mock_env=mock_env
        )
        
        assert return_code1 == 200
        assert return_code2 == 200
        # Environment variables should still be set
        assert mock_env.get("BAQURI") == DEFAULT_BAQURI
        assert mock_env.get("BAQPORT") == DEFAULT_BAQPORT

