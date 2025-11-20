"""
Test suite for DELTAPI.py

Tests for CICS DELETE operation via z/OS Connect EE (PARM-BUFFER variant).
"""

import pytest
from unittest.mock import MagicMock, patch
from DELTAPI import (
    main,
    ParmBuffer,
    DeleteRequest,
    DeleteResponse,
    DeleteInfoOper1,
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
        assert parm.employee_number == ""
        assert parm.filler == ""
    
    def test_with_employee(self):
        """Test ParmBuffer with employee number."""
        parm = ParmBuffer(
            parm_length=6,
            employee_number="000001"
        )
        assert parm.parm_length == 6
        assert parm.employee_number == "000001"


# ============================================================================
# Test DELETE Request/Response Structures
# ============================================================================

class TestDeleteRequest:
    """Test DeleteRequest structure."""
    
    def test_creation(self):
        """Test creating DeleteRequest."""
        req = DeleteRequest()
        assert req.employee == ""
        assert req.employee_length == 0
        assert req.cscvinc_delete_service_op_num == 1
    
    def test_set_employee(self):
        """Test setting employee number."""
        req = DeleteRequest()
        req.set_employee("000001")
        assert req.employee == "000001"
        assert req.employee_length == 6
    
    def test_empty_string(self):
        """Test setting empty string."""
        req = DeleteRequest()
        req.set_employee("")
        assert req.employee == ""
        assert req.employee_length == 0


class TestDeleteResponse:
    """Test DeleteResponse structure."""
    
    def test_creation(self):
        """Test creating DeleteResponse."""
        resp = DeleteResponse()
        assert resp.ceibresp == ""
        assert resp.ceibresp2 == ""
        assert resp.userid2 == ""
    
    def test_set_fields(self):
        """Test setting response fields."""
        resp = DeleteResponse()
        resp.ceibresp = "DFHRESP(NORMAL)"
        resp.ceibresp2 = "DFHRESP2(NORMAL)"
        resp.userid2 = "SYSTEM"
        assert resp.ceibresp == "DFHRESP(NORMAL)"
        assert resp.ceibresp2 == "DFHRESP2(NORMAL)"
        assert resp.userid2 == "SYSTEM"


# ============================================================================
# Test BAQCSTUB
# ============================================================================

class TestBaqcstub:
    """Test baqcstub function."""
    
    def test_success(self):
        """Test successful DELETE."""
        delete_info = DeleteInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        delete_request = DeleteRequest()
        delete_request.set_employee("000001")
        delete_response = DeleteResponse()
        
        result = baqcstub(
            delete_info,
            baq_request_info,
            b"",
            0,
            baq_response_info,
            b"",
            0,
            delete_request,
            delete_response
        )
        
        assert result.is_success()
        assert result.status_code == 200  # OK (deleted)
        assert delete_response.ceibresp == "DFHRESP(NORMAL)"
        assert delete_response.ceibresp2 == "DFHRESP2(NORMAL)"
        assert delete_response.userid2 == "SYSTEM"
    
    def test_empty_employee(self):
        """Test DELETE with empty employee number."""
        delete_info = DeleteInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        delete_request = DeleteRequest()
        delete_response = DeleteResponse()
        
        result = baqcstub(
            delete_info,
            baq_request_info,
            b"",
            0,
            baq_response_info,
            b"",
            0,
            delete_request,
            delete_response
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
        """Test main with successful DELETE."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee_number="000001"
        )
        
        return_code, error_msg = main(parm_buffer=parm_buffer)
        
        assert return_code == 200
        assert error_msg.origin == ""
        assert error_msg.code == 0
        
        # Check output
        captured = capsys.readouterr()
        assert "EIBRESP:" in captured.out
        assert "EIBRESP2:" in captured.out
        assert "USERID:" in captured.out
        assert "HTTP CODE:" in captured.out
        assert "DFHRESP(NORMAL)" in captured.out
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
            employee_number=""  # Empty
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
            employee_number="000001"
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
            employee_number="000001"
        )
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert return_code == 503
        assert error_msg.origin == "STUB"
        assert error_msg.code == 503
    
    def test_main_all_fields_set(self):
        """Test that all DELETE request fields are populated."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee_number="000001"
        )
        
        # Track the DELETE request
        captured_request = None
        
        def mock_baqcstub(*args, **kwargs):
            nonlocal captured_request
            captured_request = args[7]  # delete_request is 8th argument (0-indexed)
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "Deleted"
            # Set response
            args[8].ceibresp = "DFHRESP(NORMAL)"  # delete_response is 9th argument
            args[8].ceibresp2 = "DFHRESP2(NORMAL)"
            args[8].userid2 = "ADMIN"
            return baq_response_info
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert return_code == 200
        assert captured_request is not None
        assert captured_request.employee == "000001"
        assert captured_request.employee_length == 6
        # Check array index fields are set to 1
        assert captured_request.cscvinc_delete_service_op_num == 1
        assert captured_request.request_container2_num == 1
        assert captured_request.file_area2_num == 1
        assert captured_request.employee_number_num == 1
    
    def test_main_custom_response(self, capsys):
        """Test main with custom response fields."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "Deleted"
            # Set custom response
            args[8].ceibresp = "DFHRESP(NORMAL)"  # delete_response
            args[8].ceibresp2 = "DFHRESP2(NORMAL)"
            args[8].userid2 = "CUSTOMUSER"
            return baq_response_info
        
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee_number="000001"
        )
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert return_code == 200
        captured = capsys.readouterr()
        assert "CUSTOMUSER" in captured.out
    
    def test_main_empty_parm_buffer(self):
        """Test main with empty PARM buffer."""
        parm_buffer = ParmBuffer(
            parm_length=0,
            employee_number=""
        )
        
        return_code, error_msg = main(parm_buffer=parm_buffer)
        
        # Should return error (400)
        assert return_code == 400
        assert error_msg.origin == "API"


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_success(self, capsys):
        """Test full flow: PARM-BUFFER -> BAQCSTUB -> return code (success)."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee_number="000001"
        )
        
        return_code, error_msg = main(parm_buffer=parm_buffer)
        
        assert return_code == 200
        assert error_msg.origin == ""
        assert error_msg.code == 0
        
        captured = capsys.readouterr()
        assert "EIBRESP:" in captured.out
        assert "EIBRESP2:" in captured.out
        assert "USERID:" in captured.out
        assert "HTTP CODE:  200" in captured.out
    
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
            employee_number="000001"
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
                    args[8].ceibresp = "DFHRESP(NORMAL)"
                    args[8].ceibresp2 = "DFHRESP2(NORMAL)"
                    args[8].userid2 = "SYSTEM"
                return baq_response_info
            
            parm_buffer = ParmBuffer(
                parm_length=6,
                employee_number="000001"
            )
            
            return_code, error_msg = main(
                parm_buffer=parm_buffer,
                mock_baqcstub=mock_baqcstub
            )
            
            assert return_code == expected_status_code, \
                f"Expected return code {expected_status_code}, got {return_code}"

