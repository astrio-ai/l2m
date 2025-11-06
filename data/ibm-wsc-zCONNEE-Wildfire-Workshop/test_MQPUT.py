"""
Test suite for MQPUT.py

Tests for CICS MQ PUT operation via z/OS Connect EE.
"""

import pytest
from unittest.mock import MagicMock
from MQPUT import (
    main,
    PutRequest,
    PutResponse,
    PutInfoOper1,
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
# Test PUT Request/Response Structures
# ============================================================================

class TestPutRequest:
    """Test PutRequest structure."""
    
    def test_creation(self):
        """Test creating PutRequest."""
        req = PutRequest()
        assert req.mqmessage2_num == 1
        assert req.numb2 == ""
        assert req.numb2_length == 0
        assert req.name2 == ""
        assert req.name2_length == 0
    
    def test_set_numb2(self):
        """Test setting numb2."""
        req = PutRequest()
        req.set_numb2("837367")
        assert req.numb2 == "837367"
        assert req.numb2_length == 6
    
    def test_set_name2(self):
        """Test setting name2."""
        req = PutRequest()
        req.set_name2("John")
        assert req.name2 == "John"
        assert req.name2_length == 4
    
    def test_set_addrx2(self):
        """Test setting addrx2."""
        req = PutRequest()
        req.set_addrx2("Apex")
        assert req.addrx2 == "Apex"
        assert req.addrx2_length == 4
    
    def test_set_phone2(self):
        """Test setting phone2."""
        req = PutRequest()
        req.set_phone2("0065")
        assert req.phone2 == "0065"
        assert req.phone2_length == 4
    
    def test_set_datex2(self):
        """Test setting datex2."""
        req = PutRequest()
        req.set_datex2("11 22 65")
        assert req.datex2 == "11 22 65"
        assert req.datex2_length == 8  # "11 22 65" is 8 characters (including spaces)
    
    def test_set_amount2(self):
        """Test setting amount2."""
        req = PutRequest()
        req.set_amount2("$1000.65")
        assert req.amount2 == "$1000.65"
        assert req.amount2_length == 8
    
    def test_set_all_fields(self):
        """Test setting all fields."""
        req = PutRequest()
        req.set_numb2("837367")
        req.set_name2("John")
        req.set_addrx2("Apex")
        req.set_phone2("0065")
        req.set_datex2("11 22 65")
        req.set_amount2("$1000.65")
        
        assert req.numb2 == "837367"
        assert req.name2 == "John"
        assert req.addrx2 == "Apex"
        assert req.phone2 == "0065"
        assert req.datex2 == "11 22 65"
        assert req.amount2 == "$1000.65"
        assert req.numb2_length == 6
        assert req.name2_length == 4
        assert req.addrx2_length == 4
        assert req.phone2_length == 4
        assert req.datex2_length == 8  # "11 22 65" is 8 characters (including spaces)
        assert req.amount2_length == 8
    
    def test_set_empty_values(self):
        """Test setting empty values."""
        req = PutRequest()
        req.set_numb2("")
        req.set_name2("")
        assert req.numb2 == ""
        assert req.numb2_length == 0
        assert req.name2 == ""
        assert req.name2_length == 0


class TestPutResponse:
    """Test PutResponse structure."""
    
    def test_creation(self):
        """Test creating PutResponse."""
        resp = PutResponse()
        assert resp.filler == ""
    
    def test_filler(self):
        """Test filler field."""
        resp = PutResponse(filler="X")
        assert resp.filler == "X"


# ============================================================================
# Test BAQCSTUB
# ============================================================================

class TestBaqcstub:
    """Test baqcstub function."""
    
    def test_success(self):
        """Test successful MQ PUT."""
        put_info = PutInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        put_request = PutRequest()
        put_request.set_numb2("837367")
        put_request.set_name2("John")
        put_request.set_addrx2("Apex")
        put_request.set_phone2("0065")
        put_request.set_datex2("11 22 65")
        put_request.set_amount2("$1000.65")
        put_response = PutResponse()
        
        result = baqcstub(
            put_info,
            baq_request_info,
            b"",
            0,
            baq_response_info,
            b"",
            1,
            put_request,
            put_response
        )
        
        assert result.is_success()
        assert result.status_code == 200
        assert "successful" in result.status_message.lower()
    
    def test_error_missing_numb2(self):
        """Test PUT with missing numb2."""
        put_info = PutInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        put_request = PutRequest()
        put_request.set_name2("John")
        put_response = PutResponse()
        
        result = baqcstub(
            put_info,
            baq_request_info,
            b"",
            0,
            baq_response_info,
            b"",
            1,
            put_request,
            put_response
        )
        
        assert not result.is_success()
        assert result.status_code == 400
        assert "required" in result.status_message.lower()
    
    def test_error_missing_name2(self):
        """Test PUT with missing name2."""
        put_info = PutInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        put_request = PutRequest()
        put_request.set_numb2("837367")
        put_response = PutResponse()
        
        result = baqcstub(
            put_info,
            baq_request_info,
            b"",
            0,
            baq_response_info,
            b"",
            1,
            put_request,
            put_response
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
        """Test main with successful MQ PUT."""
        return_code, error_msg = main()
        
        assert return_code == 200
        assert error_msg.origin == ""
        assert error_msg.code == 0
        
        # Check output
        captured = capsys.readouterr()
        assert "HTTP CODE:" in captured.out
        assert "200" in captured.out
    
    def test_main_with_default_data(self):
        """Test main with default data values."""
        captured_request = None
        
        def mock_baqcstub(put_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         put_request, put_response):
            nonlocal captured_request
            captured_request = put_request
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "MQ PUT successful"
            return baq_response_info
        
        return_code, error_msg = main(mock_baqcstub=mock_baqcstub)
        
        assert return_code == 200
        assert captured_request is not None
        assert captured_request.numb2 == "837367"
        assert captured_request.name2 == "John"
        assert captured_request.addrx2 == "Apex"
        assert captured_request.phone2 == "0065"
        assert captured_request.datex2 == "11 22 65"
        assert captured_request.amount2 == "$1000.65"
        assert captured_request.mqmessage2_num == 1
    
    def test_main_with_custom_data(self):
        """Test main with custom data."""
        custom_data = {
            "numb2": "999999",
            "name2": "Jane Smith",
            "addrx2": "New York",
            "phone2": "1234",
            "datex2": "12 31 24",
            "amount2": "$5000.00"
        }
        
        captured_request = None
        
        def mock_baqcstub(put_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         put_request, put_response):
            nonlocal captured_request
            captured_request = put_request
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "MQ PUT successful"
            return baq_response_info
        
        return_code, error_msg = main(mock_baqcstub=mock_baqcstub, custom_data=custom_data)
        
        assert return_code == 200
        assert captured_request is not None
        assert captured_request.numb2 == "999999"
        assert captured_request.name2 == "Jane Smith"
        assert captured_request.addrx2 == "New York"
        assert captured_request.phone2 == "1234"
        assert captured_request.datex2 == "12 31 24"
        assert captured_request.amount2 == "$5000.00"
    
    def test_main_error_in_api(self, capsys):
        """Test main with API error."""
        def mock_baqcstub(put_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         put_request, put_response):
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 400
            baq_response_info.status_message = "Invalid message format"
            return baq_response_info
        
        return_code, error_msg = main(mock_baqcstub=mock_baqcstub)
        
        assert return_code == 400
        assert error_msg.origin == "API"
        assert error_msg.code == 400
        assert "Invalid message format" in error_msg.detail
        
        captured = capsys.readouterr()
        assert "Error code: 400" in captured.out
        assert "Error msg:" in captured.out
        assert "Error origin: API" in captured.out
    
    def test_main_error_in_zcee(self, capsys):
        """Test main with ZCEE error."""
        def mock_baqcstub(put_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         put_request, put_response):
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
        def mock_baqcstub(put_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         put_request, put_response):
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
    
    def test_main_length_fields_auto_updated(self):
        """Test that length fields are auto-updated when setting values."""
        captured_request = None
        
        def mock_baqcstub(put_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         put_request, put_response):
            nonlocal captured_request
            captured_request = put_request
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "MQ PUT successful"
            return baq_response_info
        
        custom_data = {
            "numb2": "123456",
            "name2": "Test Name",
            "addrx2": "Test Address",
            "phone2": "12345678",
            "datex2": "01 01 01",
            "amount2": "$100.00"
        }
        
        return_code, error_msg = main(mock_baqcstub=mock_baqcstub, custom_data=custom_data)
        
        assert return_code == 200
        assert captured_request is not None
        assert captured_request.numb2_length == 6
        assert captured_request.name2_length == 9
        assert captured_request.addrx2_length == 12
        assert captured_request.phone2_length == 8
        assert captured_request.datex2_length == 8  # "01 01 01" is 8 characters (including spaces)
        assert captured_request.amount2_length == 7


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_success(self, capsys):
        """Test full flow: setup data -> BAQCSTUB -> success -> display HTTP CODE."""
        return_code, error_msg = main()
        
        assert return_code == 200
        assert error_msg.origin == ""
        assert error_msg.code == 0
        
        captured = capsys.readouterr()
        assert "HTTP CODE:" in captured.out
        assert "200" in captured.out
    
    def test_full_flow_error(self, capsys):
        """Test full flow: setup data -> BAQCSTUB -> error -> error handling."""
        def mock_baqcstub(put_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         put_request, put_response):
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 500
            baq_response_info.status_message = "Queue full"
            return baq_response_info
        
        return_code, error_msg = main(mock_baqcstub=mock_baqcstub)
        
        assert return_code == 500
        assert error_msg.origin == "API"
        assert error_msg.code == 500
    
    def test_response_structure(self):
        """Test that PUT response is properly initialized (empty)."""
        captured_response = None
        
        def mock_baqcstub(put_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         put_request, put_response):
            nonlocal captured_response
            captured_response = put_response
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "MQ PUT successful"
            return baq_response_info
        
        return_code, error_msg = main(mock_baqcstub=mock_baqcstub)
        
        assert return_code == 200
        assert captured_response is not None
        assert captured_response.filler == ""
    
    def test_multiple_mq_put_operations(self, capsys):
        """Test multiple MQ PUT operations in sequence."""
        call_count = 0
        
        def mock_baqcstub(put_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         put_request, put_response):
            nonlocal call_count
            call_count += 1
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "MQ PUT successful"
            return baq_response_info
        
        # First PUT
        return_code1, error_msg1 = main(mock_baqcstub=mock_baqcstub)
        assert return_code1 == 200
        
        # Second PUT
        custom_data2 = {
            "numb2": "111111",
            "name2": "User 2"
        }
        return_code2, error_msg2 = main(mock_baqcstub=mock_baqcstub, custom_data=custom_data2)
        assert return_code2 == 200
        
        # Third PUT
        custom_data3 = {
            "numb2": "222222",
            "name2": "User 3"
        }
        return_code3, error_msg3 = main(mock_baqcstub=mock_baqcstub, custom_data=custom_data3)
        assert return_code3 == 200
        
        assert call_count == 3

