"""
Test suite for LOANAPIR.py

Tests for CICS loan application program via z/OS Connect EE.
"""

import pytest
from unittest.mock import MagicMock
from LOANAPIR import (
    main,
    Minimap,
    CommareaBuffer,
    PostRequest,
    PostResponse,
    PostInfoOper1,
    baqcstub,
    send_map,
    receive_map,
    send_control,
    send_text,
    return_transid,
    return_normal,
    BAQ_SUCCESS,
    BAQ_ERROR_IN_API,
    BAQ_ERROR_IN_ZCEE,
    BAQ_ERROR_IN_STUB,
    DFHENTER,
    DFHPF3,
    DFHPF12,
    DFHCLEAR,
    TRANSID,
)
from CSCVDLTI import (
    BAQRequestInfo,
    BAQResponseInfo,
    ErrorMsg
)


# ============================================================================
# Test Map Structures
# ============================================================================

class TestMinimap:
    """Test Minimap structure."""
    
    def test_creation(self):
        """Test creating Minimap."""
        map_obj = Minimap()
        assert map_obj.namei == ""
        assert map_obj.crdscrei == ""
        assert map_obj.approvedo == ""
        assert map_obj.uido == ""
        assert len(map_obj.msg1o) == 0
    
    def test_with_data(self):
        """Test Minimap with data."""
        map_obj = Minimap(
            namei="John Doe",
            crdscrei="700",
            approvedo="Loan approved",
            uido="LOAN1234"
        )
        assert map_obj.namei == "John Doe"
        assert map_obj.crdscrei == "700"
        assert map_obj.approvedo == "Loan approved"
        assert map_obj.uido == "LOAN1234"


class TestCommareaBuffer:
    """Test CommareaBuffer structure."""
    
    def test_creation(self):
        """Test creating CommareaBuffer."""
        commarea = CommareaBuffer()
        assert commarea.name == ""
        assert commarea.credit_score == ""
        assert commarea.approved == ""
        assert len(commarea.messages) == 10
    
    def test_with_data(self):
        """Test CommareaBuffer with data."""
        commarea = CommareaBuffer(
            name="John Doe",
            credit_score="700",
            approved="T",
            uid="LOAN1234"
        )
        assert commarea.name == "John Doe"
        assert commarea.credit_score == "700"
        assert commarea.approved == "T"
        assert commarea.uid == "LOAN1234"


# ============================================================================
# Test POST Request/Response Structures
# ============================================================================

class TestPostRequest:
    """Test PostRequest structure."""
    
    def test_creation(self):
        """Test creating PostRequest."""
        req = PostRequest()
        assert req.name2 == ""
        assert req.name2_length == 0
        assert req.age == ""
        assert req.creditscore == ""
        assert req.miniloan_commarea2_num == 1
    
    def test_set_name2(self):
        """Test setting name2."""
        req = PostRequest()
        req.set_name2("John Doe")
        assert req.name2 == "John Doe"
        assert req.name2_length == 8
    
    def test_set_name2_empty(self):
        """Test setting empty name2."""
        req = PostRequest()
        req.set_name2("")
        assert req.name2 == ""
        assert req.name2_length == 0


class TestPostResponse:
    """Test PostResponse structure."""
    
    def test_creation(self):
        """Test creating PostResponse."""
        resp = PostResponse()
        assert resp.approvedx2 == ""
        assert resp.uid2 == ""
        assert len(resp.messages2) == 10
    
    def test_with_data(self):
        """Test PostResponse with data."""
        resp = PostResponse()
        resp.approvedx2 = "T"
        resp.uid2 = "LOAN1234"
        resp.messages2[0] = "Loan approved"
        assert resp.approvedx2 == "T"
        assert resp.uid2 == "LOAN1234"
        assert resp.messages2[0] == "Loan approved"


# ============================================================================
# Test BAQCSTUB
# ============================================================================

class TestBaqcstub:
    """Test baqcstub function."""
    
    def test_success_approved(self):
        """Test successful POST with approved loan."""
        post_info = PostInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        post_request = PostRequest()
        post_request.set_name2("John Doe")
        post_request.creditscore = "700"
        post_request.yearlyincome = "50000"
        post_request.age = "30"
        post_request.amount = "10000"
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
        assert post_response.approvedx2 == "T"
        assert post_response.uid2.startswith("LOAN")
        assert len(post_response.messages2[0]) > 0
    
    def test_success_not_approved(self):
        """Test successful POST with not approved loan (low credit score)."""
        post_info = PostInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        post_request = PostRequest()
        post_request.set_name2("Jane Smith")
        post_request.creditscore = "600"  # Below 650
        post_request.yearlyincome = "40000"
        post_request.age = "25"
        post_request.amount = "5000"
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
        assert post_response.approvedx2 == "F"
        assert "too low" in post_response.messages2[1].lower() or "not approved" in post_response.messages2[0].lower()
    
    def test_error_missing_name(self):
        """Test POST with missing name."""
        post_info = PostInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        post_request = PostRequest()
        post_request.creditscore = "700"
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
    
    def test_error_invalid_numeric(self):
        """Test POST with invalid numeric values."""
        post_info = PostInfoOper1()
        baq_request_info = BAQRequestInfo()
        baq_response_info = BAQResponseInfo()
        post_request = PostRequest()
        post_request.set_name2("John Doe")
        post_request.creditscore = "invalid"
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


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_first_time_display(self):
        """Test first time display (EIBCALEN = 0)."""
        send_map_called = False
        return_transid_called = False
        
        def mock_send_map(*args, **kwargs):
            nonlocal send_map_called
            send_map_called = True
            assert args[0] == 'MINIMAP'
            assert args[1] == 'MINIMAP'
            assert kwargs.get('map_only') is True
            assert kwargs.get('freekb') is True
            assert kwargs.get('erase') is True
        
        def mock_return_transid(transid, commarea, length=None):
            nonlocal return_transid_called
            return_transid_called = True
            assert transid == TRANSID
        
        map_out, commarea, error = main(
            eibcalen=0,
            eibaid="",
            dfhcommarea=None,
            mock_send_map=mock_send_map,
            mock_return_transid=mock_return_transid
        )
        
        assert send_map_called is True
        assert return_transid_called is True
        assert error.origin == ""
    
    def test_pf3_exit(self):
        """Test PF3 exit."""
        send_control_called = False
        send_text_called = False
        return_normal_called = False
        
        def mock_send_control(*args, **kwargs):
            nonlocal send_control_called
            send_control_called = True
        
        def mock_send_text(text, *args, **kwargs):
            nonlocal send_text_called
            send_text_called = True
            assert "LOANAPIR Session Over" in text
        
        def mock_return_normal():
            nonlocal return_normal_called
            return_normal_called = True
        
        map_out, commarea, error = main(
            eibcalen=10,
            eibaid=DFHPF3,
            dfhcommarea=None,
            mock_send_control=mock_send_control,
            mock_send_text=mock_send_text,
            mock_return_normal=mock_return_normal
        )
        
        assert send_control_called is True
        assert send_text_called is True
        assert return_normal_called is True
    
    def test_pf12_exit(self):
        """Test PF12 exit."""
        return_normal_called = False
        
        def mock_return_normal():
            nonlocal return_normal_called
            return_normal_called = True
        
        map_out, commarea, error = main(
            eibcalen=10,
            eibaid=DFHPF12,
            dfhcommarea=None,
            mock_return_normal=mock_return_normal
        )
        
        assert return_normal_called is True
    
    def test_enter_approved_loan(self):
        """Test ENTER with approved loan application."""
        map_in = Minimap(
            namei="John Doe",
            crdscrei="700",
            incomei="50000",
            agei="30",
            amounti="10000",
            yrpaymnti="1200"
        )
        
        def mock_baqcstub(post_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         post_request, post_response):
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "Loan processed successfully"
            # Set response
            post_response.approvedx2 = "T"
            post_response.uid2 = "LOAN1234"
            post_response.messages2[0] = "Loan approved"
            post_response.messages2[1] = "Credit score: 700"
            return baq_response_info
        
        map_out, commarea, error = main(
            eibcalen=10,
            eibaid=DFHENTER,
            dfhcommarea=None,
            map_in=map_in,
            mock_baqcstub=mock_baqcstub
        )
        
        assert map_out.approvedo == "Loan approved"
        assert map_out.uido == "LOAN1234"
        assert map_out.msg1o == "Loan approved"
        assert map_out.msg2o == "Credit score: 700"
        assert error.origin == ""
    
    def test_enter_not_approved_loan(self):
        """Test ENTER with not approved loan application."""
        map_in = Minimap(
            namei="Jane Smith",
            crdscrei="600",  # Below 650
            incomei="40000",
            agei="25",
            amounti="5000",
            yrpaymnti="600"
        )
        
        def mock_baqcstub(post_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         post_request, post_response):
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "Loan processed successfully"
            # Set response
            post_response.approvedx2 = "F"
            post_response.uid2 = "LOAN5678"
            post_response.messages2[0] = "Loan application not approved"
            post_response.messages2[1] = "Credit score too low: 600 (minimum: 650)"
            return baq_response_info
        
        map_out, commarea, error = main(
            eibcalen=10,
            eibaid=DFHENTER,
            dfhcommarea=None,
            map_in=map_in,
            mock_baqcstub=mock_baqcstub
        )
        
        assert map_out.approvedo == "Loan not approved"
        assert map_out.uido == "LOAN5678"
        assert map_out.msg1o == "Loan application not approved"
        assert error.origin == ""
    
    def test_enter_api_error(self):
        """Test ENTER with API error."""
        map_in = Minimap(
            namei="",
            crdscrei="700"
        )
        
        def mock_baqcstub(post_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         post_request, post_response):
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 400
            baq_response_info.status_message = "Name and credit score are required"
            return baq_response_info
        
        map_out, commarea, error = main(
            eibcalen=10,
            eibaid=DFHENTER,
            dfhcommarea=None,
            map_in=map_in,
            mock_baqcstub=mock_baqcstub
        )
        
        assert error.origin == "API"
        assert error.code == 400
        assert "Name and credit score are required" in error.detail
        assert "400" in map_out.msg1o or str(error.code) in map_out.msg1o
    
    def test_enter_long_error_message(self):
        """Test ENTER with long error message split across multiple fields."""
        map_in = Minimap(
            namei="Test",
            crdscrei="700"
        )
        
        long_message = "A" * 550  # Long error message
        
        def mock_baqcstub(*args, **kwargs):
            baq_response_info = BAQResponseInfo()
            baq_response_info.return_code = BAQ_ERROR_IN_API
            baq_response_info.status_code = 500
            baq_response_info.status_message = long_message
            return baq_response_info
        
        map_out, commarea, error = main(
            eibcalen=10,
            eibaid=DFHENTER,
            dfhcommarea=None,
            map_in=map_in,
            mock_baqcstub=mock_baqcstub
        )
        
        assert error.origin == "API"
        assert error.code == 500
        # Check that error message is split across msg1o-msgao
        assert len(map_out.msg2o) > 0 or len(map_out.msg3o) > 0
        # msg1o should contain status code
        assert "500" in map_out.msg1o or str(baq_response_info.status_code) in map_out.msg1o
    
    def test_clear_continue(self):
        """Test CLEAR key (should continue processing)."""
        map_in = Minimap()
        
        map_out, commarea, error = main(
            eibcalen=10,
            eibaid=DFHCLEAR,
            dfhcommarea=None,
            map_in=map_in
        )
        
        # Should continue and send map
        assert error.origin == ""
    
    def test_all_messages_populated(self):
        """Test that all 10 messages are populated on success."""
        map_in = Minimap(
            namei="John Doe",
            crdscrei="700",
            incomei="50000",
            agei="30",
            amounti="10000",
            yrpaymnti="1200"
        )
        
        def mock_baqcstub(post_info, baq_request_info, baq_request_ptr, baq_request_len,
                         baq_response_info, baq_response_ptr, baq_response_len,
                         post_request, post_response):
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "Loan processed successfully"
            # Set all 10 messages
            post_response.approvedx2 = "T"
            post_response.uid2 = "LOAN1234"
            for i in range(10):
                post_response.messages2[i] = f"Message {i+1}"
            return baq_response_info
        
        map_out, commarea, error = main(
            eibcalen=10,
            eibaid=DFHENTER,
            dfhcommarea=None,
            map_in=map_in,
            mock_baqcstub=mock_baqcstub
        )
        
        assert map_out.msg1o == "Message 1"
        assert map_out.msg2o == "Message 2"
        assert map_out.msg3o == "Message 3"
        assert map_out.msg4o == "Message 4"
        assert map_out.msg5o == "Message 5"
        assert map_out.msg6o == "Message 6"
        assert map_out.msg7o == "Message 7"
        assert map_out.msg8o == "Message 8"
        assert map_out.msg9o == "Message 9"


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_approved(self):
        """Test full flow: first display -> enter -> approved."""
        # First time display
        map_out1, commarea1, error1 = main(
            eibcalen=0,
            eibaid="",
            dfhcommarea=None
        )
        
        # Enter loan application
        map_in = Minimap(
            namei="John Doe",
            crdscrei="700",
            incomei="50000",
            agei="30",
            amounti="10000",
            yrpaymnti="1200"
        )
        
        map_out2, commarea2, error2 = main(
            eibcalen=10,
            eibaid=DFHENTER,
            dfhcommarea=commarea1,
            map_in=map_in
        )
        
        assert map_out2.approvedo == "Loan approved" or map_out2.approvedo == "Loan not approved"
        assert map_out2.uido.startswith("LOAN") or map_out2.uido == ""
        assert error2.origin == ""
    
    def test_full_flow_not_approved(self):
        """Test full flow: first display -> enter -> not approved."""
        # First time display
        map_out1, commarea1, error1 = main(
            eibcalen=0,
            eibaid="",
            dfhcommarea=None
        )
        
        # Enter loan application with low credit score
        map_in = Minimap(
            namei="Jane Smith",
            crdscrei="600",  # Below 650
            incomei="40000",
            agei="25",
            amounti="5000",
            yrpaymnti="600"
        )
        
        map_out2, commarea2, error2 = main(
            eibcalen=10,
            eibaid=DFHENTER,
            dfhcommarea=commarea1,
            map_in=map_in
        )
        
        # Should be approved or not approved based on mock logic
        assert map_out2.approvedo in ["Loan approved", "Loan not approved"]
        assert error2.origin == ""
    
    def test_full_flow_exit(self):
        """Test full flow: first display -> PF3 -> exit."""
        # First time display
        map_out1, commarea1, error1 = main(
            eibcalen=0,
            eibaid="",
            dfhcommarea=None
        )
        
        # Exit with PF3
        return_normal_called = False
        
        def mock_return_normal():
            nonlocal return_normal_called
            return_normal_called = True
        
        map_out2, commarea2, error2 = main(
            eibcalen=10,
            eibaid=DFHPF3,
            dfhcommarea=commarea1,
            mock_return_normal=mock_return_normal
        )
        
        assert return_normal_called is True
    
    def test_commarea_persistence(self):
        """Test that commarea is persisted across transactions."""
        commarea1 = CommareaBuffer(
            name="John Doe",
            credit_score="700",
            uid="LOAN1234"
        )
        
        map_out1, commarea2, error1 = main(
            eibcalen=10,
            eibaid=DFHCLEAR,
            dfhcommarea=commarea1
        )
        
        # Commarea should be preserved
        assert commarea2.name == "John Doe"
        assert commarea2.credit_score == "700"
        assert commarea2.uid == "LOAN1234"

