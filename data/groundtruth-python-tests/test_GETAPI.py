"""
Test suite for GETAPI.py

Tests for CICS GET operation via z/OS Connect EE (PARM-BUFFER variant).
"""

import pytest
from unittest.mock import MagicMock, patch
from GETAPI import (
    main,
    ParmBuffer,
    GetRequest,
    GetResponse,
    GetInfoOper1,
    baqcstub,
    BAQ_SUCCESS,
    BAQ_ERROR_IN_API,
    BAQ_ERROR_IN_ZCEE,
    BAQ_ERROR_IN_STUB
)
from CSCVDLTI import (
    BAQRequestInfo,
    BAQResponseInfo,
    ErrorMsg
)


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
    
    def test_empty_string(self):
        """Test setting empty string."""
        req = GetRequest()
        req.set_employee("")
        assert req.employee == ""
        assert req.employee_length == 0


class TestGetResponse:
    """Test GetResponse structure."""
    
    def test_creation(self):
        """Test creating GetResponse."""
        resp = GetResponse()
        assert resp.employee_number2 == ""
        assert resp.name2 == ""
        assert resp.xaddress2 == ""
        assert resp.phone_number2 == ""
        assert resp.xdate2 == ""
        assert resp.amount2 == ""
        assert resp.ceibresp == ""
        assert resp.ceibresp2 == ""
        assert resp.userid2 == ""
    
    def test_set_fields(self):
        """Test setting response fields."""
        resp = GetResponse()
        resp.employee_number2 = "000001"
        resp.name2 = "John Doe"
        resp.xaddress2 = "123 Main St"
        resp.phone_number2 = "5551234"
        resp.xdate2 = "20250101"
        resp.amount2 = "100.00"
        resp.ceibresp = "DFHRESP(NORMAL)"
        resp.ceibresp2 = "DFHRESP2(NORMAL)"
        resp.userid2 = "SYSTEM"
        
        assert resp.employee_number2 == "000001"
        assert resp.name2 == "John Doe"
        assert resp.ceibresp == "DFHRESP(NORMAL)"


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
        assert get_response.name2 == "John Doe"
        assert get_response.xaddress2 == "123 Main St"
        assert get_response.phone_number2 == "5551234"
        assert get_response.xdate2 == "20250101"
        assert get_response.amount2 == "100.00"
        assert get_response.ceibresp == "DFHRESP(NORMAL)"
        assert get_response.ceibresp2 == "DFHRESP2(NORMAL)"
        assert get_response.userid2 == "SYSTEM"
    
    def test_empty_employee(self):
        """Test GET with empty employee number."""
        get_info = GetInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        get_request = GetRequest()
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
        
        assert not result.is_success()
        assert result.return_code == BAQ_ERROR_IN_API
        assert result.status_code == 400
        assert "required" in result.status_message.lower()


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
        
        return_code, error_msg = main(parm_buffer=parm_buffer)
        
        assert return_code == 200
        assert error_msg.origin == ""
        assert error_msg.code == 0
        
        # Check output
        captured = capsys.readouterr()
        assert "EmployeeNumber:" in captured.out
        assert "EmployeeName:" in captured.out
        assert "Address:" in captured.out
        assert "Phone:" in captured.out
        assert "Date:" in captured.out
        assert "Amount:" in captured.out
        assert "EIBRESP:" in captured.out
        assert "EIBRESP2:" in captured.out
        assert "USERID:" in captured.out
        assert "HTTP CODE:" in captured.out
        assert "000001" in captured.out
        assert "John Doe" in captured.out
        assert "SYSTEM" in captured.out
    
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
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert return_code == 404
        assert error_msg.origin == "API"
        assert error_msg.code == 404
        assert "not found" in error_msg.detail.lower()
        
        # Check error output
        captured = capsys.readouterr()
        assert "Error code:" in captured.out
        assert "Error msg:" in captured.out
        assert "Error origin:" in captured.out
        assert "API" in captured.out
    
    def test_main_error_in_zcee(self, capsys):
        """Test main with zCEE error."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_ZCEE
            baq_response_info.status_code = 500
            baq_response_info.status_message = "zCEE server error"
            return baq_response_info
        
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert return_code == 500
        assert error_msg.origin == "ZCEE"
        assert error_msg.code == 500
    
    def test_main_error_in_stub(self, capsys):
        """Test main with stub error."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_STUB
            baq_response_info.status_code = 503
            baq_response_info.status_message = "Stub communication error"
            return baq_response_info
        
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert return_code == 503
        assert error_msg.origin == "STUB"
        assert error_msg.code == 503
    
    def test_main_all_response_fields_populated(self, capsys):
        """Test that all response fields are populated on success."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "GET successful"
            # Set custom response
            args[8].employee_number2 = "000002"  # get_response is 9th argument (0-indexed)
            args[8].name2 = "Jane Smith"
            args[8].xaddress2 = "456 Oak Ave"
            args[8].phone_number2 = "5555678"
            args[8].xdate2 = "20250201"
            args[8].amount2 = "200.50"
            args[8].ceibresp = "DFHRESP(NORMAL)"
            args[8].ceibresp2 = "DFHRESP2(NORMAL)"
            args[8].userid2 = "ADMIN"
            return baq_response_info
        
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000002"
        )
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert return_code == 200
        captured = capsys.readouterr()
        assert "Jane Smith" in captured.out
        assert "456 Oak Ave" in captured.out
        assert "5555678" in captured.out
        assert "20250201" in captured.out
        assert "200.50" in captured.out
        assert "ADMIN" in captured.out
    
    def test_main_empty_parm_buffer(self):
        """Test main with empty PARM buffer."""
        parm_buffer = ParmBuffer(
            parm_length=0,
            employee=""
        )
        
        return_code, error_msg = main(parm_buffer=parm_buffer)
        
        # Should return error (400)
        assert return_code == 400
        assert error_msg.origin == "API"
    
    def test_main_request_fields_set(self):
        """Test that GET request fields are populated."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        
        # Track the GET request
        captured_request = None
        
        def mock_baqcstub(*args, **kwargs):
            nonlocal captured_request
            captured_request = args[7]  # get_request is 8th argument (0-indexed)
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "GET successful"
            # Set response
            args[8].employee_number2 = "000001"  # get_response
            args[8].name2 = "John Doe"
            args[8].ceibresp = "DFHRESP(NORMAL)"
            args[8].ceibresp2 = "DFHRESP2(NORMAL)"
            args[8].userid2 = "SYSTEM"
            return baq_response_info
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert return_code == 200
        assert captured_request is not None
        assert captured_request.employee == "000001"
        assert captured_request.employee_length == 6


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_success(self, capsys):
        """Test full flow: PARM-BUFFER -> BAQCSTUB -> return code (success)."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        
        return_code, error_msg = main(parm_buffer=parm_buffer)
        
        assert return_code == 200
        assert error_msg.origin == ""
        assert error_msg.code == 0
        
        captured = capsys.readouterr()
        assert "EmployeeNumber:" in captured.out
        assert "EmployeeName:" in captured.out
        assert "Address:" in captured.out
        assert "Phone:" in captured.out
        assert "Date:" in captured.out
        assert "Amount:" in captured.out
        assert "EIBRESP:" in captured.out
        assert "EIBRESP2:" in captured.out
        assert "USERID:" in captured.out
        assert "HTTP CODE:" in captured.out
        assert "200" in captured.out
    
    def test_full_flow_error(self, capsys):
        """Test full flow: PARM-BUFFER -> BAQCSTUB -> return code (error)."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 404
            baq_response_info.status_message = "Employee 000001 not found in database"
            return baq_response_info
        
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert return_code == 404
        assert error_msg.origin == "API"
        assert error_msg.code == 404
        assert "not found" in error_msg.detail.lower()
        
        captured = capsys.readouterr()
        assert "Error code:" in captured.out
        assert "Error msg:" in captured.out
        assert "Error origin: API" in captured.out
    
    def test_return_code_mapping(self):
        """Test that return code is correctly mapped from status code."""
        test_cases = [
            (200, BAQ_SUCCESS),
            (404, BAQ_ERROR_IN_API),
            (500, BAQ_ERROR_IN_ZCEE),
            (503, BAQ_ERROR_IN_STUB),
        ]
        
        for expected_status_code, return_code_type in test_cases:
            def mock_baqcstub(*args, **kwargs):
                baq_response_info = BAQResponseInfo()
                baq_response_info.return_code = return_code_type
                baq_response_info.status_code = expected_status_code
                baq_response_info.status_message = f"Status {expected_status_code}"
                if return_code_type == BAQ_SUCCESS:
                    args[8].employee_number2 = "000001"
                    args[8].name2 = "John Doe"
                    args[8].ceibresp = "DFHRESP(NORMAL)"
                    args[8].ceibresp2 = "DFHRESP2(NORMAL)"
                    args[8].userid2 = "SYSTEM"
                return baq_response_info
            
            parm_buffer = ParmBuffer(
                parm_length=6,
                employee="000001"
            )
            
            return_code, error_msg = main(
                parm_buffer=parm_buffer,
                mock_baqcstub=mock_baqcstub
            )
            
            assert return_code == expected_status_code, \
                f"Expected return code {expected_status_code}, got {return_code}"

