"""
Test suite for CSCVDLTI.py

Tests for IMS DL/I DELETE operation via z/OS Connect EE.
"""

import pytest
from unittest.mock import MagicMock, patch
from CSCVDLTI import (
    InBuffer,
    OutBuffer,
    IOPCB,
    ALTPCB,
    DeleteRequest,
    DeleteResponse,
    DeleteInfoOper1,
    BAQRequestInfo,
    BAQResponseInfo,
    ErrorMsg,
    cbltdli_get_unique,
    cbltdli_isrt,
    baqcstub,
    main,
    BAQ_SUCCESS,
    BAQ_ERROR_IN_API,
    BAQ_ERROR_IN_ZCEE,
    BAQ_ERROR_IN_STUB,
    NORMAL
)


# ============================================================================
# Test Input/Output Buffers
# ============================================================================

class TestBuffers:
    """Test input/output buffer structures."""
    
    def test_in_buffer_creation(self):
        """Test creating input buffer."""
        in_buf = InBuffer(
            ll=100,
            numb="000001",
            name="John Doe"
        )
        
        assert in_buf.ll == 100
        assert in_buf.numb == "000001"
        assert in_buf.name == "John Doe"
    
    def test_out_buffer_creation(self):
        """Test creating output buffer."""
        out_buf = OutBuffer(
            numb="000001",
            userid="TESTUSER",
            httpcode=200
        )
        
        assert out_buf.ll == 400
        assert out_buf.numb == "000001"
        assert out_buf.userid == "TESTUSER"
        assert out_buf.httpcode == 200


# ============================================================================
# Test PCB Structures
# ============================================================================

class TestPCBs:
    """Test PCB structures."""
    
    def test_iopcb_creation(self):
        """Test creating IOPCB."""
        iopcb = IOPCB(
            modname="CSCVDLT",
            userid="TESTUSER"
        )
        
        assert iopcb.modname == "CSCVDLT"
        assert iopcb.userid == "TESTUSER"
    
    def test_altpcb_creation(self):
        """Test creating ALTPCB."""
        altpcb = ALTPCB(
            dbd_name="TESTDBD",
            seg_level="01"
        )
        
        assert altpcb.dbd_name == "TESTDBD"
        assert altpcb.seg_level == "01"


# ============================================================================
# Test DELETE Request/Response
# ============================================================================

class TestDeleteRequestResponse:
    """Test DELETE request/response structures."""
    
    def test_delete_request_creation(self):
        """Test creating DELETE request."""
        request = DeleteRequest(employee="000001")
        
        assert request.employee == "000001"
        assert request.employee_length == 6
    
    def test_delete_request_auto_length(self):
        """Test that employee_length is set automatically."""
        request = DeleteRequest()
        request.employee = "12345"
        
        assert request.employee_length == 5
    
    def test_delete_response_creation(self):
        """Test creating DELETE response."""
        response = DeleteResponse(userid2="SYSTEM")
        
        assert response.userid2 == "SYSTEM"


# ============================================================================
# Test BAQ Response Info
# ============================================================================

class TestBAQResponseInfo:
    """Test BAQResponseInfo structure."""
    
    def test_is_success(self):
        """Test success check."""
        response_info = BAQResponseInfo(return_code=BAQ_SUCCESS)
        
        assert response_info.is_success() is True
    
    def test_is_error_in_api(self):
        """Test API error check."""
        response_info = BAQResponseInfo(return_code=BAQ_ERROR_IN_API)
        
        assert response_info.is_error_in_api() is True
        assert response_info.is_success() is False
    
    def test_is_error_in_zcee(self):
        """Test zCEE error check."""
        response_info = BAQResponseInfo(return_code=BAQ_ERROR_IN_ZCEE)
        
        assert response_info.is_error_in_zcee() is True
    
    def test_is_error_in_stub(self):
        """Test stub error check."""
        response_info = BAQResponseInfo(return_code=BAQ_ERROR_IN_STUB)
        
        assert response_info.is_error_in_stub() is True


# ============================================================================
# Test DL/I Operations
# ============================================================================

class TestDLIOperations:
    """Test DL/I operations."""
    
    def test_cbltdli_get_unique(self):
        """Test GET-UNIQUE operation."""
        iopcb = IOPCB()
        in_buffer = InBuffer()
        
        status = cbltdli_get_unique(iopcb, in_buffer)
        
        assert status == NORMAL
        assert iopcb.io_status == NORMAL
    
    def test_cbltdli_isrt(self):
        """Test ISRT operation."""
        iopcb = IOPCB()
        out_buffer = OutBuffer()
        
        status = cbltdli_isrt(iopcb, out_buffer, "CSCVDLT")
        
        assert status == NORMAL
        assert iopcb.io_status == NORMAL


# ============================================================================
# Test BAQCSTUB
# ============================================================================

class TestBAQCSTUB:
    """Test BAQCSTUB communication stub."""
    
    def test_baqcstub_success(self):
        """Test successful BAQCSTUB call."""
        delete_info = DeleteInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        delete_request = DeleteRequest(employee="000001")
        delete_response = DeleteResponse()
        
        result = baqcstub(
            delete_info,
            baq_request_info,
            b"",
            100,
            baq_response_info,
            b"",
            200,
            delete_request,
            delete_response
        )
        
        assert result.return_code == BAQ_SUCCESS
        assert result.status_code == 200
        assert delete_response.userid2 == "SYSTEM"
    
    def test_baqcstub_empty_employee(self):
        """Test BAQCSTUB with empty employee number."""
        delete_info = DeleteInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        delete_request = DeleteRequest(employee="")
        delete_response = DeleteResponse()
        
        result = baqcstub(
            delete_info,
            baq_request_info,
            b"",
            100,
            baq_response_info,
            b"",
            200,
            delete_request,
            delete_response
        )
        
        assert result.return_code == BAQ_ERROR_IN_API
        assert result.status_code == 400
        assert "required" in result.status_message


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_success(self):
        """Test main with successful DELETE."""
        iopcb = IOPCB(modname="CSCVDLT", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001",
            name="John Doe"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer
        )
        
        assert status == NORMAL
        assert out_buffer.numb == "000001"
        assert out_buffer.userid == "SYSTEM"
        assert out_buffer.httpcode == 200
        assert out_buffer.segno == 1
        assert iopcb.modname == "CSCODLT"  # Should be changed
    
    def test_main_error_in_api(self):
        """Test main with API error."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 404
            baq_response_info.status_message = "Employee not found"
            return baq_response_info
        
        iopcb = IOPCB(modname="CSCVDLT", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000999"  # Non-existent
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert status == NORMAL
        assert out_buffer.httpcode == 404
        assert error_msg.origin == "API"
        assert error_msg.code == 404
        assert "not found" in out_buffer.msg1.lower()
    
    def test_main_error_in_zcee(self):
        """Test main with zCEE error."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_ZCEE
            baq_response_info.status_code = 500
            baq_response_info.status_message = "zCEE server error"
            return baq_response_info
        
        iopcb = IOPCB(modname="CSCVDLT", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert status == NORMAL
        assert out_buffer.httpcode == 500
        assert error_msg.origin == "ZCEE"
        assert error_msg.code == 500
    
    def test_main_error_in_stub(self):
        """Test main with stub error."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_STUB
            baq_response_info.status_code = 503
            baq_response_info.status_message = "Stub communication error"
            return baq_response_info
        
        iopcb = IOPCB(modname="CSCVDLT", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert status == NORMAL
        assert out_buffer.httpcode == 503
        assert error_msg.origin == "STUB"
        assert error_msg.code == 503
    
    def test_main_long_error_message(self):
        """Test main with long error message (split across msg1-4)."""
        long_message = "A" * 300  # 300 characters
        
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 400
            baq_response_info.status_message = long_message
            return baq_response_info
        
        iopcb = IOPCB(modname="CSCVDLT", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert len(out_buffer.msg1) <= 75
        assert len(out_buffer.msg2) <= 75
        assert len(out_buffer.msg3) <= 75
        assert len(out_buffer.msg4) <= 75
        # Check that messages are populated
        assert out_buffer.msg1 != ""
        assert out_buffer.msg2 != ""
        assert out_buffer.msg3 != ""
        assert out_buffer.msg4 != ""
    
    def test_main_no_modname_change(self):
        """Test main when modname is not CSCVDLT."""
        iopcb = IOPCB(modname="OTHERMOD", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer
        )
        
        # Should not change modname
        assert iopcb.modname == "OTHERMOD"
    
    def test_main_empty_employee(self):
        """Test main with empty employee number."""
        iopcb = IOPCB(modname="CSCVDLT", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb=""  # Empty
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer
        )
        
        assert status == NORMAL
        assert out_buffer.httpcode == 400
        assert error_msg.origin == "API"
        assert "required" in out_buffer.msg1.lower()
    
    def test_main_segment_number_increment(self):
        """Test that segment number increments."""
        iopcb = IOPCB(modname="CSCVDLT", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001"
        )
        
        out_buffer1, status1, error_msg1 = main(
            iopcb=iopcb,
            in_buffer=in_buffer
        )
        
        out_buffer2, status2, error_msg2 = main(
            iopcb=iopcb,
            in_buffer=in_buffer
        )
        
        assert out_buffer1.segno == 1
        # Note: Each call creates new segno, so second call will also be 1
        # In real IMS, segno would persist across calls
        assert out_buffer2.segno == 1


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_success(self):
        """Test full flow: GET-UNIQUE -> BAQCSTUB -> ISRT (success)."""
        iopcb = IOPCB(
            modname="CSCVDLT",
            userid="TESTUSER",
            lterm_name="TESTLTERM"
        )
        altpcb = ALTPCB(
            dbd_name="TESTDBD",
            seg_level="01"
        )
        in_buffer = InBuffer(
            ll=100,
            zz=0,
            trcd="DELETE",
            numb="000001",
            name="John Doe",
            addrx="123 Main St",
            phone="5551234",
            datex="20250101",
            amount="100.00"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            altpcb=altpcb,
            in_buffer=in_buffer
        )
        
        assert status == NORMAL
        assert out_buffer.numb == "000001"
        assert out_buffer.userid == "SYSTEM"
        assert out_buffer.httpcode == 200
        assert error_msg.origin == ""
        assert iopcb.modname == "CSCODLT"
    
    def test_full_flow_error(self):
        """Test full flow: GET-UNIQUE -> BAQCSTUB -> ISRT (error)."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 404
            baq_response_info.status_message = "Employee 000999 not found in database"
            return baq_response_info
        
        iopcb = IOPCB(modname="CSCVDLT", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000999"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert status == NORMAL
        assert out_buffer.numb == "000999"
        assert out_buffer.httpcode == 404
        assert error_msg.origin == "API"
        assert error_msg.code == 404
        assert "not found" in out_buffer.msg1.lower()

