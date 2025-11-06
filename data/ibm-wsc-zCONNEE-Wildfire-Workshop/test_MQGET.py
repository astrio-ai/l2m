"""
Test suite for MQGET.py

Tests for CICS MQ GET operation via z/OS Connect EE.
"""

import pytest
from unittest.mock import MagicMock
from MQGET import (
    main,
    GetRequest,
    GetResponse,
    GetInfoOper1,
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
# Test GET Request/Response Structures
# ============================================================================

class TestGetRequest:
    """Test GetRequest structure."""
    
    def test_creation(self):
        """Test creating GetRequest."""
        req = GetRequest()
        assert req.filler == ""
    
    def test_filler(self):
        """Test filler field."""
        req = GetRequest(filler="X")
        assert req.filler == "X"


class TestGetResponse:
    """Test GetResponse structure."""
    
    def test_creation(self):
        """Test creating GetResponse."""
        resp = GetResponse()
        assert resp.numb == ""
        assert resp.name == ""
        assert resp.addrx == ""
        assert resp.phone == ""
        assert resp.datex == ""
        assert resp.amount == ""
    
    def test_with_data(self):
        """Test GetResponse with data."""
        resp = GetResponse(
            numb="000001",
            name="John Doe",
            addrx="123 Main St",
            phone="5551234",
            datex="20250101",
            amount="100.00"
        )
        assert resp.numb == "000001"
        assert resp.name == "John Doe"
        assert resp.addrx == "123 Main St"
        assert resp.phone == "5551234"
        assert resp.datex == "20250101"
        assert resp.amount == "100.00"


# ============================================================================
# Test BAQCSTUB
# ============================================================================

class TestBaqcstub:
    """Test baqcstub function."""
    
    def test_success(self):
        """Test successful MQ GET."""
        get_info = GetInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        get_request = GetRequest()
        get_response = GetResponse()
        
        result = baqcstub(
            get_info,
            baq_request_info,
            b"",
            1,
            baq_response_info,
            b"",
            0,
            get_request,
            get_response
        )
        
        assert result.is_success()
        assert result.status_code == 200
        assert get_response.numb == "000001"
        assert get_response.name == "John Doe"
        assert get_response.addrx == "123 Main St"
        assert get_response.phone == "5551234"
        assert get_response.datex == "20250101"
        assert get_response.amount == "100.00"


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_success(self, capsys):
        """Test main with successful MQ GET."""
        return_code, error_msg = main()
        
        assert return_code == 200
        assert error_msg.origin == ""
        assert error_msg.code == 0
        
        # Check output
        captured = capsys.readouterr()
        assert "NUMB:" in captured.out
        assert "NAME:" in captured.out
        assert "ADDRX:" in captured.out
        assert "PHONE:" in captured.out
        assert "DATEX:" in captured.out
        assert "AMOUNT:" in captured.out
        assert "HTTP CODE:" in captured.out
        assert "000001" in captured.out
        assert "John Doe" in captured.out
    
    def test_main_with_custom_response(self, capsys):
        """Test main with custom response data."""
        def mock_baqcstub(get_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         get_request, get_response):
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "MQ GET successful"
            # Set custom response
            get_response.numb = "000002"
            get_response.name = "Jane Smith"
            get_response.addrx = "456 Oak Ave"
            get_response.phone = "5555678"
            get_response.datex = "20250201"
            get_response.amount = "200.50"
            return baq_response_info
        
        return_code, error_msg = main(mock_baqcstub=mock_baqcstub)
        
        assert return_code == 200
        captured = capsys.readouterr()
        assert "Jane Smith" in captured.out
        assert "456 Oak Ave" in captured.out
        assert "5555678" in captured.out
        assert "200.50" in captured.out
    
    def test_main_error_in_api(self, capsys):
        """Test main with API error."""
        def mock_baqcstub(get_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         get_request, get_response):
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 404
            baq_response_info.status_message = "No messages available in queue"
            return baq_response_info
        
        return_code, error_msg = main(mock_baqcstub=mock_baqcstub)
        
        assert return_code == 404
        assert error_msg.origin == "API"
        assert error_msg.code == 404
        assert "No messages available" in error_msg.detail
        
        captured = capsys.readouterr()
        assert "Error code: 404" in captured.out
        assert "Error msg:" in captured.out
        assert "Error origin: API" in captured.out
    
    def test_main_error_in_zcee(self, capsys):
        """Test main with ZCEE error."""
        def mock_baqcstub(get_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         get_request, get_response):
            baq_response_info.return_code = BAQ_ERROR_IN_ZCEE
            baq_response_info.status_code = 500
            baq_response_info.status_message = "z/OS Connect EE server error"
            return baq_response_info
        
        return_code, error_msg = main(mock_baqcstub=mock_baqcstub)
        
        assert return_code == 500
        assert error_msg.origin == "ZCEE"
        assert error_msg.code == 500
        
        captured = capsys.readouterr()
        assert "Error origin: ZCEE" in captured.out
    
    def test_main_error_in_stub(self, capsys):
        """Test main with STUB error."""
        def mock_baqcstub(get_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         get_request, get_response):
            baq_response_info.return_code = BAQ_ERROR_IN_STUB
            baq_response_info.status_code = 1
            baq_response_info.status_message = "Communication stub error"
            return baq_response_info
        
        return_code, error_msg = main(mock_baqcstub=mock_baqcstub)
        
        assert return_code == 1
        assert error_msg.origin == "STUB"
        assert error_msg.code == 1
        
        captured = capsys.readouterr()
        assert "Error origin: STUB" in captured.out
    
    def test_main_all_response_fields_populated(self, capsys):
        """Test that all response fields are populated on success."""
        def mock_baqcstub(get_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         get_request, get_response):
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "MQ GET successful"
            # Set all fields
            get_response.numb = "000003"
            get_response.name = "Bob Johnson"
            get_response.addrx = "789 Elm Street"
            get_response.phone = "5559012"
            get_response.datex = "20250301"
            get_response.amount = "300.75"
            return baq_response_info
        
        return_code, error_msg = main(mock_baqcstub=mock_baqcstub)
        
        assert return_code == 200
        captured = capsys.readouterr()
        assert "NUMB:" in captured.out
        assert "000003" in captured.out
        assert "Bob Johnson" in captured.out
        assert "789 Elm Street" in captured.out
        assert "5559012" in captured.out
        assert "20250301" in captured.out
        assert "300.75" in captured.out


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_success(self, capsys):
        """Test full flow: BAQCSTUB -> success -> display fields."""
        return_code, error_msg = main()
        
        assert return_code == 200
        assert error_msg.origin == ""
        assert error_msg.code == 0
        
        captured = capsys.readouterr()
        assert "NUMB:" in captured.out
        assert "NAME:" in captured.out
        assert "HTTP CODE:" in captured.out
        assert "200" in captured.out
    
    def test_full_flow_error(self, capsys):
        """Test full flow: BAQCSTUB -> error -> error handling."""
        def mock_baqcstub(get_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         get_request, get_response):
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 500
            baq_response_info.status_message = "Internal server error"
            return baq_response_info
        
        return_code, error_msg = main(mock_baqcstub=mock_baqcstub)
        
        assert return_code == 500
        assert error_msg.origin == "API"
        assert error_msg.code == 500
    
    def test_request_structure(self):
        """Test that GET request is properly initialized."""
        def mock_baqcstub(get_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         get_request, get_response):
            # Verify request is empty (just filler)
            assert get_request.filler == ""
            assert baq_request_len == 1  # Length of filler
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "MQ GET successful"
            return baq_response_info
        
        return_code, error_msg = main(mock_baqcstub=mock_baqcstub)
        
        assert return_code == 200
    
    def test_multiple_mq_get_operations(self, capsys):
        """Test multiple MQ GET operations in sequence."""
        call_count = 0
        
        def mock_baqcstub(get_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         get_request, get_response):
            nonlocal call_count
            call_count += 1
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "MQ GET successful"
            # Return different data for each call
            get_response.numb = f"00000{call_count}"
            get_response.name = f"User {call_count}"
            get_response.amount = f"{call_count * 100}.00"
            return baq_response_info
        
        # First GET
        return_code1, error_msg1 = main(mock_baqcstub=mock_baqcstub)
        assert return_code1 == 200
        
        # Second GET
        return_code2, error_msg2 = main(mock_baqcstub=mock_baqcstub)
        assert return_code2 == 200
        
        # Third GET
        return_code3, error_msg3 = main(mock_baqcstub=mock_baqcstub)
        assert return_code3 == 200
        
        assert call_count == 3
        
        captured = capsys.readouterr()
        assert "User 3" in captured.out

