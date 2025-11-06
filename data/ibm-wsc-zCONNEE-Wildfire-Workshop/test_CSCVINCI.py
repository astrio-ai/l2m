"""
Test suite for CSCVINCI.py

Tests for IMS DL/I GET operation via z/OS Connect EE (CSCRGET modname variant).
"""

import pytest
from unittest.mock import MagicMock
from CSCVINCI import (
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
from CSCVGETI import (
    GetRequest,
    GetResponse,
    GetInfoOper1
)


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_success(self):
        """Test main with successful GET."""
        iopcb = IOPCB(modname="CSCRGET", userid="TESTUSER")
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
        assert iopcb.modname == "CSCOGET"  # Should be changed from CSCRGET
    
    def test_main_error_in_api(self):
        """Test main with API error."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 404
            baq_response_info.status_message = "Employee not found"
            return baq_response_info
        
        iopcb = IOPCB(modname="CSCRGET", userid="TESTUSER")
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
        
        iopcb = IOPCB(modname="CSCRGET", userid="TESTUSER")
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
        
        iopcb = IOPCB(modname="CSCRGET", userid="TESTUSER")
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
        
        iopcb = IOPCB(modname="CSCRGET", userid="TESTUSER")
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
        """Test main when modname is not CSCRGET."""
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
        iopcb = IOPCB(modname="CSCRGET", userid="TESTUSER")
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
        iopcb = IOPCB(modname="CSCRGET", userid="TESTUSER")
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
            modname="CSCRGET",
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
        assert iopcb.modname == "CSCOGET"  # CSCRGET -> CSCOGET
    
    def test_full_flow_error(self):
        """Test full flow: GET-UNIQUE -> BAQCSTUB -> ISRT (error)."""
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 404
            baq_response_info.status_message = "Employee 000999 not found in database"
            return baq_response_info
        
        iopcb = IOPCB(modname="CSCRGET", userid="TESTUSER")
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
        
        iopcb = IOPCB(modname="CSCRGET", userid="TESTUSER")
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
        assert iopcb.modname == "CSCOGET"
    
    def test_modname_change_cscrget(self):
        """Test that CSCRGET modname is changed to CSCOGET."""
        iopcb = IOPCB(modname="CSCRGET", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer
        )
        
        assert iopcb.modname == "CSCOGET"
    
    def test_modname_no_change_other(self):
        """Test that other modnames are not changed."""
        iopcb = IOPCB(modname="CSCVGET", userid="TESTUSER")
        in_buffer = InBuffer(
            ll=100,
            numb="000001"
        )
        
        out_buffer, status, error_msg = main(
            iopcb=iopcb,
            in_buffer=in_buffer
        )
        
        # Should not change (only CSCRGET changes)
        assert iopcb.modname == "CSCVGET"

