"""
Test suite for CSCVGETI.py

Tests for IMS DL/I GET operation via z/OS Connect EE.
"""

import pytest
from unittest.mock import MagicMock
from CSCVGETI import (
    GetRequest,
    GetResponse,
    GetInfoOper1,
    baqcstub,
    main,
    BAQ_SUCCESS,
    BAQ_ERROR_IN_API,
    BAQ_ERROR_IN_ZCEE,
    BAQ_ERROR_IN_STUB
)
from CSCVDLTI import (
    InBuffer,
    OutBuffer,
    IOPCB,
    ALTPCB,
    BAQRequestInfo,
    BAQResponseInfo,
    NORMAL
)


# ============================================================================
# Test GET Request/Response
# ============================================================================

class TestGetRequestResponse:
    """Test GET request/response structures."""
    
    def test_get_request_creation(self):
        """Test creating GET request."""
        request = GetRequest(employee="000001")
        
        assert request.employee == "000001"
        assert request.employee_length == 6
    
    def test_get_request_auto_length(self):
        """Test that employee_length is set automatically."""
        request = GetRequest()
        request.employee = "12345"
        
        assert request.employee_length == 5
    
    def test_get_response_creation(self):
        """Test creating GET response."""
        response = GetResponse(
            name2="John Doe",
            address2="123 Main St",
            phoneNumber2="5551234",
            date2="20250101",
            amount2="100.00",
            userid2="SYSTEM"
        )
        
        assert response.name2 == "John Doe"
        assert response.address2 == "123 Main St"
        assert response.phoneNumber2 == "5551234"
        assert response.userid2 == "SYSTEM"


# ============================================================================
# Test BAQCSTUB
# ============================================================================

class TestBAQCSTUB:
    """Test BAQCSTUB communication stub for GET."""
    
    def test_baqcstub_success(self):
        """Test successful BAQCSTUB call."""
        get_info = GetInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        get_request = GetRequest(employee="000001")
        get_response = GetResponse()
        
        result = baqcstub(
            get_info,
            baq_request_info,
            b"",
            100,
            baq_response_info,
            b"",
            200,
            get_request,
            get_response
        )
        
        assert result.return_code == BAQ_SUCCESS
        assert result.status_code == 200
        assert get_response.name2 == "John Doe"
        assert get_response.address2 == "123 Main St"
        assert get_response.userid2 == "SYSTEM"
    
    def test_baqcstub_empty_employee(self):
        """Test BAQCSTUB with empty employee number."""
        get_info = GetInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        get_request = GetRequest(employee="")
        get_response = GetResponse()
        
        result = baqcstub(
            get_info,
            baq_request_info,
            b"",
            100,
            baq_response_info,
            b"",
            200,
            get_request,
            get_response
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
        """Test main with successful GET."""
        iopcb = IOPCB(modname="CSCVGET", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer
        )
        
        assert status == NORMAL
        assert out_buffer.numb == "000001"
        assert out_buffer.name == "John Doe"
        assert out_buffer.addrx == "123 Main St"
        assert out_buffer.phone == "5551234"
        assert out_buffer.datex == "20250101"
        assert out_buffer.amount == "100.00"
        assert out_buffer.userid == "SYSTEM"
        assert out_buffer.httpcode == 200
        assert out_buffer.segno == 1
        assert iopcb.modname == "CSCOGET"  # Should be changed
    
    def test_main_error_in_api(self):
        """Test main with API error."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 404
            baq_response_info.status_message = "Employee not found"
            return baq_response_info
        
        iopcb = IOPCB(modname="CSCVGET", userid="TESTUSER")
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
        
        iopcb = IOPCB(modname="CSCVGET", userid="TESTUSER")
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
        
        iopcb = IOPCB(modname="CSCVGET", userid="TESTUSER")
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
        
        iopcb = IOPCB(modname="CSCVGET", userid="TESTUSER")
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
        """Test main when modname is not CSCVGET."""
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
        iopcb = IOPCB(modname="CSCVGET", userid="TESTUSER")
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
    
    def test_main_response_fields_populated(self):
        """Test that all response fields are populated on success."""
        iopcb = IOPCB(modname="CSCVGET", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer
        )
        
        # All fields should be populated from response
        assert out_buffer.name != ""
        assert out_buffer.addrx != ""
        assert out_buffer.phone != ""
        assert out_buffer.datex != ""
        assert out_buffer.amount != ""
        assert out_buffer.userid != ""


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_success(self):
        """Test full flow: GET-UNIQUE -> BAQCSTUB -> ISRT (success)."""
        iopcb = IOPCB(
            modname="CSCVGET",
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
            trcd="GET",
            numb="000001"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            altpcb=altpcb,
            in_buffer=in_buffer
        )
        
        assert status == NORMAL
        assert out_buffer.numb == "000001"
        assert out_buffer.name == "John Doe"
        assert out_buffer.addrx == "123 Main St"
        assert out_buffer.phone == "5551234"
        assert out_buffer.datex == "20250101"
        assert out_buffer.amount == "100.00"
        assert out_buffer.userid == "SYSTEM"
        assert out_buffer.httpcode == 200
        assert error_msg.origin == ""
        assert iopcb.modname == "CSCOGET"
    
    def test_full_flow_error(self):
        """Test full flow: GET-UNIQUE -> BAQCSTUB -> ISRT (error)."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 404
            baq_response_info.status_message = "Employee 000999 not found in database"
            return baq_response_info
        
        iopcb = IOPCB(modname="CSCVGET", userid="TESTUSER")
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
        # Response fields should be empty on error
        assert out_buffer.name == ""
        assert out_buffer.addrx == ""
    
    def test_custom_response_data(self):
        """Test with custom response data."""
        def mock_baqcstub(*args, **kwargs):
            # Get the response object from args
            get_response = args[8]  # get_response is 9th argument (0-indexed)
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "GET successful"
            
            # Set custom response data
            get_response.name2 = "Jane Smith"
            get_response.address2 = "456 Oak Ave"
            get_response.phoneNumber2 = "5555678"
            get_response.date2 = "20250201"
            get_response.amount2 = "200.50"
            get_response.userid2 = "ADMIN"
            
            return baq_response_info
        
        iopcb = IOPCB(modname="CSCVGET", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000002"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert status == NORMAL
        assert out_buffer.name == "Jane Smith"
        assert out_buffer.addrx == "456 Oak Ave"
        assert out_buffer.phone == "5555678"
        assert out_buffer.datex == "20250201"
        assert out_buffer.amount == "200.50"
        assert out_buffer.userid == "ADMIN"
        assert out_buffer.httpcode == 200

