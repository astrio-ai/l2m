"""
Test suite for POSTAPI.py

Tests for CICS POST operation via z/OS Connect EE.
"""

import pytest
from unittest.mock import MagicMock
from POSTAPI import (
    main,
    ParmBuffer,
    PostRequest,
    PostResponse,
    PostInfoOper1,
    baqcstub,
    BAQ_SUCCESS,
    BAQ_ERROR_IN_API,
    BAQ_ERROR_IN_ZCEE,
    BAQ_ERROR_IN_STUB,
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
    
    def test_with_employee_number(self):
        """Test ParmBuffer with employee number."""
        parm = ParmBuffer(
            parm_length=6,
            employee_number="000001"
        )
        assert parm.parm_length == 6
        assert parm.employee_number == "000001"


# ============================================================================
# Test POST Request/Response Structures
# ============================================================================

class TestPostRequest:
    """Test PostRequest structure."""
    
    def test_creation(self):
        """Test creating PostRequest."""
        req = PostRequest()
        assert req.employee_number2 == ""
        assert req.employee_number2_length == 0
        assert req.name2 == ""
        assert req.cscvinc_insert_service_op_num == 1
    
    def test_set_employee_number2(self):
        """Test setting employee number."""
        req = PostRequest()
        req.set_employee_number2("000001")
        assert req.employee_number2 == "000001"
        assert req.employee_number2_length == 6
    
    def test_set_all_fields(self):
        """Test setting all fields."""
        req = PostRequest()
        req.set_employee_number2("000001")
        req.set_name2("John")
        req.set_xaddress2("Apex")
        req.set_phone_number2("0065")
        req.set_xdate2("11 22 65")
        req.set_amount2("$1000.65")
        
        assert req.employee_number2 == "000001"
        assert req.name2 == "John"
        assert req.xaddress2 == "Apex"
        assert req.phone_number2 == "0065"
        assert req.xdate2 == "11 22 65"
        assert req.amount2 == "$1000.65"
        assert req.employee_number2_length == 6
        assert req.name2_length == 4
        assert req.xaddress2_length == 4
        assert req.phone_number2_length == 4
        assert req.xdate2_length == 8  # "11 22 65" is 8 characters
        assert req.amount2_length == 8


class TestPostResponse:
    """Test PostResponse structure."""
    
    def test_creation(self):
        """Test creating PostResponse."""
        resp = PostResponse()
        assert resp.ceibresp == ""
        assert resp.ceibresp2 == ""
        assert resp.userid2 == ""
    
    def test_with_data(self):
        """Test PostResponse with data."""
        resp = PostResponse(
            ceibresp="DFHRESP(NORMAL)",
            ceibresp2="DFHRESP2(NORMAL)",
            userid2="SYSTEM"
        )
        assert resp.ceibresp == "DFHRESP(NORMAL)"
        assert resp.ceibresp2 == "DFHRESP2(NORMAL)"
        assert resp.userid2 == "SYSTEM"


# ============================================================================
# Test BAQCSTUB
# ============================================================================

class TestBaqcstub:
    """Test baqcstub function."""
    
    def test_success(self):
        """Test successful POST."""
        post_info = PostInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        post_request = PostRequest()
        post_request.set_employee_number2("000001")
        post_response = PostResponse()
        
        result = baqcstub(
            post_info,
            baq_request_info,
            b"",
            0,
            baq_response_info,
            b"",
            0,
            post_request,
            post_response
        )
        
        assert result.is_success()
        assert result.status_code == 200
        assert post_response.ceibresp == "DFHRESP(NORMAL)"
        assert post_response.ceibresp2 == "DFHRESP2(NORMAL)"
        assert post_response.userid2 == "SYSTEM"
    
    def test_error_missing_employee_number(self):
        """Test POST with missing employee number."""
        post_info = PostInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        post_request = PostRequest()
        post_response = PostResponse()
        
        result = baqcstub(
            post_info,
            baq_request_info,
            b"",
            0,
            baq_response_info,
            b"",
            0,
            post_request,
            post_response
        )
        
        assert not result.is_success()
        assert result.status_code == 400
        assert "required" in result.status_message.lower()


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_success(self, capsys):
        """Test main with successful POST."""
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
        assert "200" in captured.out
    
    def test_main_with_default_data(self):
        """Test main with default data values."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee_number="000001"
        )
        
        captured_request = None
        
        def mock_baqcstub(post_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         post_request, post_response):
            nonlocal captured_request
            captured_request = post_request
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "POST successful"
            post_response.ceibresp = "DFHRESP(NORMAL)"
            post_response.ceibresp2 = "DFHRESP2(NORMAL)"
            post_response.userid2 = "SYSTEM"
            return baq_response_info
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert return_code == 200
        assert captured_request is not None
        assert captured_request.employee_number2 == "000001"
        assert captured_request.name2 == "John"
        assert captured_request.xaddress2 == "Apex"
        assert captured_request.phone_number2 == "0065"
        assert captured_request.xdate2 == "11 22 65"
        assert captured_request.amount2 == "$1000.65"
        assert captured_request.cscvinc_insert_service_op_num == 1
        assert captured_request.request2_num == 1
    
    def test_main_with_custom_data(self):
        """Test main with custom data."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee_number="000002"
        )
        custom_data = {
            "name2": "Jane Smith",
            "xaddress2": "New York",
            "phone_number2": "1234",
            "xdate2": "12 31 24",
            "amount2": "$5000.00"
        }
        
        captured_request = None
        
        def mock_baqcstub(post_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         post_request, post_response):
            nonlocal captured_request
            captured_request = post_request
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "POST successful"
            post_response.ceibresp = "DFHRESP(NORMAL)"
            post_response.ceibresp2 = "DFHRESP2(NORMAL)"
            post_response.userid2 = "SYSTEM"
            return baq_response_info
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub,
            custom_data=custom_data
        )
        
        assert return_code == 200
        assert captured_request is not None
        assert captured_request.employee_number2 == "000002"
        assert captured_request.name2 == "Jane Smith"
        assert captured_request.xaddress2 == "New York"
        assert captured_request.phone_number2 == "1234"
        assert captured_request.xdate2 == "12 31 24"
        assert captured_request.amount2 == "$5000.00"
    
    def test_main_error_in_api(self, capsys):
        """Test main with API error."""
        def mock_baqcstub(post_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         post_request, post_response):
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 400
            baq_response_info.status_message = "Invalid employee number"
            return baq_response_info
        
        parm_buffer = ParmBuffer(
            parm_length=0,
            employee_number=""
        )
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert return_code == 400
        assert error_msg.origin == "API"
        assert error_msg.code == 400
        assert "Invalid employee number" in error_msg.detail
        
        captured = capsys.readouterr()
        assert "Error code: 400" in captured.out
        assert "Error msg:" in captured.out
        assert "Error origin: API" in captured.out
    
    def test_main_error_in_zcee(self, capsys):
        """Test main with ZCEE error."""
        def mock_baqcstub(post_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         post_request, post_response):
            baq_response_info.return_code = BAQ_ERROR_IN_ZCEE
            baq_response_info.status_code = 500
            baq_response_info.status_message = "z/OS Connect EE server error"
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
        
        captured = capsys.readouterr()
        assert "Error origin: ZCEE" in captured.out
    
    def test_main_error_in_stub(self, capsys):
        """Test main with STUB error."""
        def mock_baqcstub(post_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         post_request, post_response):
            baq_response_info.return_code = BAQ_ERROR_IN_STUB
            baq_response_info.status_code = 1
            baq_response_info.status_message = "Communication stub error"
            return baq_response_info
        
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee_number="000001"
        )
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert return_code == 1
        assert error_msg.origin == "STUB"
        assert error_msg.code == 1
        
        captured = capsys.readouterr()
        assert "Error origin: STUB" in captured.out
    
    def test_main_all_array_indices_set(self):
        """Test that all array indices are set to 1."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee_number="000001"
        )
        
        captured_request = None
        
        def mock_baqcstub(post_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         post_request, post_response):
            nonlocal captured_request
            captured_request = post_request
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "POST successful"
            post_response.ceibresp = "DFHRESP(NORMAL)"
            post_response.ceibresp2 = "DFHRESP2(NORMAL)"
            post_response.userid2 = "SYSTEM"
            return baq_response_info
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert return_code == 200
        assert captured_request is not None
        assert captured_request.cscvinc_insert_service_op_num == 1
        assert captured_request.request2_num == 1
        assert captured_request.filea2_num == 1
        assert captured_request.employee_number_num == 1
        assert captured_request.name_num == 1
        assert captured_request.xaddress_num == 1
        assert captured_request.phone_number_num == 1
        assert captured_request.xdate_num == 1
        assert captured_request.amount_num == 1


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_success(self, capsys):
        """Test full flow: PARM buffer -> BAQCSTUB -> success -> display fields."""
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
        assert "USERID:" in captured.out
        assert "HTTP CODE:" in captured.out
        assert "200" in captured.out
    
    def test_full_flow_error(self, capsys):
        """Test full flow: PARM buffer -> BAQCSTUB -> error -> error handling."""
        def mock_baqcstub(post_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         post_request, post_response):
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 500
            baq_response_info.status_message = "Internal server error"
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
        assert error_msg.origin == "API"
        assert error_msg.code == 500
    
    def test_empty_parm_buffer(self):
        """Test with empty PARM buffer (employee number not provided)."""
        parm_buffer = ParmBuffer(
            parm_length=0,
            employee_number=""
        )
        
        # Should fail with error
        return_code, error_msg = main(parm_buffer=parm_buffer)
        
        assert return_code == 400
        assert error_msg.origin == "API"
        assert "required" in error_msg.detail.lower()
    
    def test_response_fields_populated(self, capsys):
        """Test that all response fields are populated on success."""
        def mock_baqcstub(post_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         post_request, post_response):
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "POST successful"
            # Set custom response
            post_response.ceibresp = "DFHRESP(NORMAL)"
            post_response.ceibresp2 = "DFHRESP2(NORMAL)"
            post_response.userid2 = "ADMIN"
            return baq_response_info
        
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee_number="000002"
        )
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert return_code == 200
        captured = capsys.readouterr()
        assert "ADMIN" in captured.out
        assert "DFHRESP(NORMAL)" in captured.out
        assert "DFHRESP2(NORMAL)" in captured.out

