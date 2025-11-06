"""
Test suite for CSCVPSTI.py

Tests for IMS DL/I POST operation via z/OS Connect EE (CSCVPST modname variant).
"""

import pytest
from unittest.mock import MagicMock
from CSCVPSTI import (
    main,
    PostRequest,
    PostResponse,
    PostInfoOper1,
    baqcstub,
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
        assert req.name2_length == 0
        assert req.cscvinc_insert_service_op_num == 1
    
    def test_set_employee_number2(self):
        """Test setting employee number."""
        req = PostRequest()
        req.set_employee_number2("000001")
        assert req.employee_number2 == "000001"
        assert req.employee_number2_length == 6
    
    def test_set_name2(self):
        """Test setting name."""
        req = PostRequest()
        req.set_name2("John Doe")
        assert req.name2 == "John Doe"
        assert req.name2_length == 8
    
    def test_set_xaddress2(self):
        """Test setting address."""
        req = PostRequest()
        req.set_xaddress2("123 Main St")
        assert req.xaddress2 == "123 Main St"
        assert req.xaddress2_length == 11
    
    def test_set_phone_number2(self):
        """Test setting phone number."""
        req = PostRequest()
        req.set_phone_number2("5551234")
        assert req.phone_number2 == "5551234"
        assert req.phone_number2_length == 7
    
    def test_set_xdate2(self):
        """Test setting date."""
        req = PostRequest()
        req.set_xdate2("20250101")
        assert req.xdate2 == "20250101"
        assert req.xdate2_length == 8
    
    def test_set_amount2(self):
        """Test setting amount."""
        req = PostRequest()
        req.set_amount2("100.00")
        assert req.amount2 == "100.00"
        assert req.amount2_length == 6
    
    def test_empty_strings(self):
        """Test setting empty strings."""
        req = PostRequest()
        req.set_employee_number2("")
        assert req.employee_number2 == ""
        assert req.employee_number2_length == 0


class TestPostResponse:
    """Test PostResponse structure."""
    
    def test_creation(self):
        """Test creating PostResponse."""
        resp = PostResponse()
        assert resp.userid2 == ""
    
    def test_set_userid2(self):
        """Test setting user ID."""
        resp = PostResponse()
        resp.userid2 = "SYSTEM"
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
        post_request.set_name2("John Doe")
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
        assert result.status_code == 201
        assert post_response.userid2 == "SYSTEM"
    
    def test_empty_employee(self):
        """Test POST with empty employee number."""
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
        assert result.return_code == BAQ_ERROR_IN_API
        assert result.status_code == 400
        assert "required" in result.status_message.lower()


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_success(self):
        """Test main with successful POST."""
        iopcb = IOPCB(modname="CSCVPST", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001",
            name="John Doe",
            addrx="123 Main St",
            phone="5551234",
            datex="20250101",
            amount="100.00"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer
        )
        
        assert status == NORMAL
        assert out_buffer.numb == "000001"
        assert out_buffer.userid == "SYSTEM"
        assert out_buffer.httpcode == 201
        assert out_buffer.segno == 1
        assert iopcb.modname == "CSCOPST"  # Should be changed from CSCVPST
    
    def test_main_error_in_api(self):
        """Test main with API error."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 400
            baq_response_info.status_message = "Employee number is required"
            return baq_response_info
        
        iopcb = IOPCB(modname="CSCVPST", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb=""  # Empty
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert status == NORMAL
        assert out_buffer.httpcode == 400
        assert error_msg.origin == "API"
        assert error_msg.code == 400
        assert "required" in out_buffer.msg1.lower()
    
    def test_main_error_in_zcee(self):
        """Test main with zCEE error."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_ZCEE
            baq_response_info.status_code = 500
            baq_response_info.status_message = "zCEE server error"
            return baq_response_info
        
        iopcb = IOPCB(modname="CSCVPST", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001",
            name="John Doe"
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
        
        iopcb = IOPCB(modname="CSCVPST", userid="TESTUSER")
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
        
        iopcb = IOPCB(modname="CSCVPST", userid="TESTUSER")
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
        """Test main when modname is not CSCVPST."""
        iopcb = IOPCB(modname="OTHERMOD", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001",
            name="John Doe"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer
        )
        
        # Should not change modname
        assert iopcb.modname == "OTHERMOD"
    
    def test_main_all_fields_populated(self):
        """Test that all POST request fields are populated."""
        iopcb = IOPCB(modname="CSCVPST", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001",
            name="Jane Smith",
            addrx="456 Oak Ave",
            phone="5555678",
            datex="20250201",
            amount="200.50"
        )
        
        # Track the POST request
        captured_request = None
        
        def mock_baqcstub(*args, **kwargs):
            nonlocal captured_request
            captured_request = args[7]  # post_request is 8th argument (0-indexed)
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 201
            baq_response_info.status_message = "Created"
            # Set response
            args[8].userid2 = "ADMIN"  # post_response is 9th argument
            return baq_response_info
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert status == NORMAL
        assert captured_request is not None
        assert captured_request.employee_number2 == "000001"
        assert captured_request.name2 == "Jane Smith"
        assert captured_request.xaddress2 == "456 Oak Ave"
        assert captured_request.phone_number2 == "5555678"
        assert captured_request.xdate2 == "20250201"
        assert captured_request.amount2 == "200.50"
        # Check lengths are set
        assert captured_request.employee_number2_length == 6
        assert captured_request.name2_length == 10
        assert captured_request.xaddress2_length == 11
        assert captured_request.phone_number2_length == 7
        assert captured_request.xdate2_length == 8
        assert captured_request.amount2_length == 6
        # Check array index fields are set to 1
        assert captured_request.cscvinc_insert_service_op_num == 1
        assert captured_request.request2_num == 1
        assert captured_request.filea2_num == 1
        assert out_buffer.userid == "ADMIN"
    
    def test_main_response_userid_only(self):
        """Test that only USERID is copied to output on success."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 201
            baq_response_info.status_message = "Created"
            # Set response
            args[8].userid2 = "CUSTOMUSER"  # post_response
            return baq_response_info
        
        iopcb = IOPCB(modname="CSCVPST", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001",
            name="John Doe"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert status == NORMAL
        assert out_buffer.userid == "CUSTOMUSER"
        # Other fields should remain empty (unlike GET which copies all fields)
        assert out_buffer.name == ""
        assert out_buffer.addrx == ""


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_success(self):
        """Test full flow: GET-UNIQUE -> BAQCSTUB -> ISRT (success)."""
        iopcb = IOPCB(
            modname="CSCVPST",
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
            trcd="POST",
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
        assert out_buffer.httpcode == 201
        assert error_msg.origin == ""
        assert iopcb.modname == "CSCOPST"  # CSCVPST -> CSCOPST
    
    def test_full_flow_error(self):
        """Test full flow: GET-UNIQUE -> BAQCSTUB -> ISRT (error)."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 409
            baq_response_info.status_message = "Employee 000001 already exists in database"
            return baq_response_info
        
        iopcb = IOPCB(modname="CSCVPST", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001",
            name="John Doe"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer,
            mock_baqcstub=mock_baqcstub
        )
        
        assert status == NORMAL
        assert out_buffer.numb == "000001"
        assert out_buffer.httpcode == 409
        assert error_msg.origin == "API"
        assert error_msg.code == 409
        assert "already exists" in out_buffer.msg1.lower()
        # USERID should be empty on error
        assert out_buffer.userid == ""
    
    def test_modname_change_cscvpst(self):
        """Test that CSCVPST modname is changed to CSCOPST."""
        iopcb = IOPCB(modname="CSCVPST", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001",
            name="John Doe"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer
        )
        
        assert iopcb.modname == "CSCOPST"
    
    def test_modname_no_change_other(self):
        """Test that other modnames are not changed."""
        iopcb = IOPCB(modname="OTHERMOD", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001",
            name="John Doe"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer
        )
        
        # Should not change (only CSCVPST changes)
        assert iopcb.modname == "OTHERMOD"

