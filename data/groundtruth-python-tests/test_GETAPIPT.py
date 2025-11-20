"""
Test suite for GETAPIPT.py

Tests for CICS GET operation via z/OS Connect EE with Pass Ticket and OAuth.
"""

import pytest
import os
from unittest.mock import MagicMock, patch
from GETAPIPT import (
    main,
    ParmBuffer,
    GetRequest,
    GetResponse,
    GetInfoOper1,
    baqcstub,
    ceeenv_get,
    FeedbackCode,
    BAQ_SUCCESS,
    BAQ_ERROR_IN_API,
    BAQ_ERROR_IN_ZCEE,
    BAQ_ERROR_IN_STUB,
    OAUTH_CLIENT_ID,
    OAUTH_CLIENT_SECRET,
    OAUTH_SCOPE,
)
from CSCVDLTI import (
    BAQRequestInfo,
    BAQResponseInfo,
    ErrorMsg
)


# ============================================================================
# Test CEEENV GET Functions
# ============================================================================

class TestCeeenvGet:
    """Test ceeenv_get function."""
    
    def test_success(self):
        """Test successful CEEENV GET."""
        mock_env = {"ATSOAUTHUSERNAME": "testuser"}
        value, length, feedback = ceeenv_get("ATSOAUTHUSERNAME", mock_env)
        
        assert value == "testuser"
        assert length == 8
        assert feedback.severity == 0
    
    def test_not_found(self):
        """Test CEEENV GET with non-existent variable."""
        mock_env = {}
        value, length, feedback = ceeenv_get("NONEXISTENT", mock_env)
        
        assert value == ""
        assert length == 0
        assert feedback.severity == 0
    
    def test_empty_name(self):
        """Test CEEENV GET with empty variable name."""
        mock_env = {}
        value, length, feedback = ceeenv_get("", mock_env)
        
        assert value == ""
        assert length == 0
        assert feedback.severity == 8  # Error
    
    def test_multiple_variables(self):
        """Test getting multiple environment variables."""
        mock_env = {
            "ATSOAUTHUSERNAME": "testuser",
            "ATSOAUTHPASSWORD": "testpass"
        }
        username, username_len, _ = ceeenv_get("ATSOAUTHUSERNAME", mock_env)
        password, password_len, _ = ceeenv_get("ATSOAUTHPASSWORD", mock_env)
        
        assert username == "testuser"
        assert username_len == 8
        assert password == "testpass"
        assert password_len == 8


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
        assert req.baq_oauth_username == ""
        assert req.baq_oauth_clientid == ""
    
    def test_set_employee(self):
        """Test setting employee number."""
        req = GetRequest()
        req.set_employee("000001")
        assert req.employee == "000001"
        assert req.employee_length == 6
    
    def test_oauth_fields(self):
        """Test OAuth fields."""
        req = GetRequest()
        req.baq_oauth_username = "testuser"
        req.baq_oauth_username_len = 8
        req.baq_oauth_clientid = OAUTH_CLIENT_ID
        req.baq_oauth_clientid_len = len(OAUTH_CLIENT_ID)
        
        assert req.baq_oauth_username == "testuser"
        assert req.baq_oauth_clientid == OAUTH_CLIENT_ID


class TestGetResponse:
    """Test GetResponse structure."""
    
    def test_creation(self):
        """Test creating GetResponse."""
        resp = GetResponse()
        assert resp.employee_number2 == ""
        assert resp.name2 == ""
        assert resp.phone_number2 == ""
        assert resp.userid2 == ""
    
    def test_set_fields(self):
        """Test setting response fields."""
        resp = GetResponse()
        resp.employee_number2 = "000001"
        resp.name2 = "John Doe"
        resp.phone_number2 = "5551234"
        resp.userid2 = "SYSTEM"
        
        assert resp.employee_number2 == "000001"
        assert resp.name2 == "John Doe"
        assert resp.phone_number2 == "5551234"
        assert resp.userid2 == "SYSTEM"


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
        assert get_response.phone_number2 == "5551234"
        assert get_response.userid2 == "SYSTEM"


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
        
        # Check output
        captured = capsys.readouterr()
        assert "EmployeeNumber:" in captured.out
        assert "EmployeeName:" in captured.out
        assert "Phone:" in captured.out
        assert "EIBRESP:" in captured.out
        assert "000001" in captured.out
        assert "John Doe" in captured.out
        assert "SYSTEM" in captured.out
        assert "BAQ-OAUTH-USERNAME: Not found" in captured.out
        assert "BAQ-OAUTH-PASSWORD: Not found" in captured.out
    
    def test_main_with_oauth_credentials(self, capsys):
        """Test main with OAuth credentials from environment."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        mock_env = {
            "ATSOAUTHUSERNAME": "testuser",
            "ATSOAUTHPASSWORD": "testpass"
        }
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_env=mock_env
        )
        
        assert return_code == 200
        
        # Check output
        captured = capsys.readouterr()
        assert "BAQ-OAUTH-USERNAME:  testuser" in captured.out
        assert "BAQ-OAUTH-PASSWORD:  testpass" in captured.out
    
    def test_main_oauth_fields_set(self):
        """Test that OAuth fields are set correctly."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        mock_env = {
            "ATSOAUTHUSERNAME": "testuser",
            "ATSOAUTHPASSWORD": "testpass"
        }
        
        # Track the GET request
        captured_request = None
        
        def mock_baqcstub(*args, **kwargs):
            nonlocal captured_request
            captured_request = args[7]  # get_request is 8th argument (0-indexed)
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "GET successful"
            args[8].employee_number2 = "000001"  # get_response
            args[8].name2 = "John Doe"
            args[8].ceibresp = "DFHRESP(NORMAL)"
            args[8].ceibresp2 = "DFHRESP2(NORMAL)"
            args[8].userid2 = "SYSTEM"
            return baq_response_info
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub,
            mock_env=mock_env
        )
        
        assert return_code == 200
        assert captured_request is not None
        assert captured_request.employee == "000001"
        assert captured_request.baq_oauth_username == "testuser"
        assert captured_request.baq_oauth_username_len == 8
        assert captured_request.baq_oauth_password == "testpass"
        assert captured_request.baq_oauth_password_len == 8
        assert captured_request.baq_oauth_clientid == OAUTH_CLIENT_ID
        assert captured_request.baq_oauth_clientid_len == len(OAUTH_CLIENT_ID)
        assert captured_request.baq_oauth_client_secret == OAUTH_CLIENT_SECRET
        assert captured_request.baq_oauth_client_secret_len == len(OAUTH_CLIENT_SECRET)
        assert captured_request.baq_oauth_scope == OAUTH_SCOPE
        assert captured_request.baq_oauth_scope_len == len(OAUTH_SCOPE)
    
    def test_main_atsptktc_called(self):
        """Test that ATSPTKTC is called."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        mock_env = {}
        atsptktc_called = False
        
        def mock_atsptktc():
            nonlocal atsptktc_called
            atsptktc_called = True
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_env=mock_env,
            mock_atsptktc=mock_atsptktc
        )
        
        assert atsptktc_called is True
        assert return_code == 200
    
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


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_success(self, capsys):
        """Test full flow: ATSPTKTC -> CEEENV GET -> BAQCSTUB -> return code."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        mock_env = {
            "ATSOAUTHUSERNAME": "testuser",
            "ATSOAUTHPASSWORD": "testpass"
        }
        atsptktc_called = False
        
        def mock_atsptktc():
            nonlocal atsptktc_called
            atsptktc_called = True
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_env=mock_env,
            mock_atsptktc=mock_atsptktc
        )
        
        assert return_code == 200
        assert error_msg.origin == ""
        assert atsptktc_called is True
        
        captured = capsys.readouterr()
        assert "EmployeeNumber:" in captured.out
        assert "BAQ-OAUTH-USERNAME:  testuser" in captured.out
        assert "BAQ-OAUTH-PASSWORD:  testpass" in captured.out
    
    def test_full_flow_without_oauth(self, capsys):
        """Test full flow without OAuth credentials."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        mock_env = {}  # No OAuth credentials
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_env=mock_env
        )
        
        assert return_code == 200
        captured = capsys.readouterr()
        assert "BAQ-OAUTH-USERNAME: Not found" in captured.out
        assert "BAQ-OAUTH-PASSWORD: Not found" in captured.out
    
    def test_full_flow_error(self, capsys):
        """Test full flow: ATSPTKTC -> CEEENV GET -> BAQCSTUB -> error."""
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
        assert error_msg.code == 404
    
    def test_oauth_credentials_order(self):
        """Test that OAuth credentials are retrieved before BAQCSTUB call."""
        parm_buffer = ParmBuffer(
            parm_length=6,
            employee="000001"
        )
        mock_env = {
            "ATSOAUTHUSERNAME": "testuser",
            "ATSOAUTHPASSWORD": "testpass"
        }
        
        call_order = []
        
        def mock_atsptktc():
            call_order.append("ATSPTKTC")
        
        def mock_baqcstub(*args, **kwargs):
            call_order.append("BAQCSTUB")
            # Verify OAuth credentials are set
            get_request = args[7]
            assert get_request.baq_oauth_username == "testuser"
            assert get_request.baq_oauth_password == "testpass"
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "GET successful"
            args[8].employee_number2 = "000001"
            args[8].name2 = "John Doe"
            args[8].ceibresp = "DFHRESP(NORMAL)"
            args[8].ceibresp2 = "DFHRESP2(NORMAL)"
            args[8].userid2 = "SYSTEM"
            return baq_response_info
        
        return_code, error_msg = main(
            parm_buffer=parm_buffer,
            mock_baqcstub=mock_baqcstub,
            mock_env=mock_env,
            mock_atsptktc=mock_atsptktc
        )
        
        assert return_code == 200
        # ATSPTKTC should be called first
        assert call_order[0] == "ATSPTKTC"
        assert call_order[1] == "BAQCSTUB"

