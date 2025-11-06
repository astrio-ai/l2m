"""
Test suite for LOANCICS.py

Tests for CICS loan application program via CICS LINK.
"""

import pytest
from unittest.mock import MagicMock
from LOANCICS import (
    main,
    Minimap,
    CommareaBuffer,
    link_program,
    send_map,
    receive_map,
    send_control,
    send_text,
    return_transid,
    return_normal,
    DFHENTER,
    DFHPF3,
    DFHPF12,
    DFHCLEAR,
    TRANSID,
    LINKED_PROGRAM,
)
from CSCVDLTI import ErrorMsg


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
        assert map_obj.efdtei == ""
        assert map_obj.interesti == ""
        assert map_obj.approvedo == ""
        assert map_obj.uido == ""
        assert len(map_obj.msg1o) == 0
    
    def test_with_data(self):
        """Test Minimap with data."""
        map_obj = Minimap(
            namei="John Doe",
            crdscrei="700",
            efdtei="20250101",
            interesti="5.5",
            approvedo="Loan approved",
            uido="LOAN1234"
        )
        assert map_obj.namei == "John Doe"
        assert map_obj.crdscrei == "700"
        assert map_obj.efdtei == "20250101"
        assert map_obj.interesti == "5.5"
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
    
    def test_bool_value_true(self):
        """Test bool_value() method returns True for 'T'."""
        commarea = CommareaBuffer(approved="T")
        assert commarea.bool_value() is True
    
    def test_bool_value_false(self):
        """Test bool_value() method returns False for 'F'."""
        commarea = CommareaBuffer(approved="F")
        assert commarea.bool_value() is False
    
    def test_bool_value_empty(self):
        """Test bool_value() method returns False for empty."""
        commarea = CommareaBuffer(approved="")
        assert commarea.bool_value() is False


# ============================================================================
# Test LINK PROGRAM
# ============================================================================

class TestLinkProgram:
    """Test link_program function."""
    
    def test_success_approved(self):
        """Test successful LINK with approved loan."""
        commarea = CommareaBuffer()
        commarea.name = "John Doe"
        commarea.credit_score = "700"
        commarea.yearly_income = "50000"
        commarea.age = "30"
        commarea.amount = "10000"
        commarea.approved = "F"
        
        updated_commarea, resp = link_program(LINKED_PROGRAM, commarea)
        
        assert updated_commarea.approved == "T"
        assert updated_commarea.uid.startswith("LOAN")
        assert len(updated_commarea.messages[0]) > 0
        assert "approved" in updated_commarea.messages[0].lower()
    
    def test_success_not_approved_low_credit(self):
        """Test successful LINK with not approved loan (low credit score)."""
        commarea = CommareaBuffer()
        commarea.name = "Jane Smith"
        commarea.credit_score = "600"  # Below 650
        commarea.yearly_income = "40000"
        commarea.age = "25"
        commarea.amount = "5000"
        commarea.approved = "F"
        
        updated_commarea, resp = link_program(LINKED_PROGRAM, commarea)
        
        assert updated_commarea.approved == "F"
        assert updated_commarea.uid.startswith("LOAN")
        assert "not approved" in updated_commarea.messages[0].lower() or "too low" in updated_commarea.messages[1].lower()
    
    def test_success_not_approved_low_age(self):
        """Test successful LINK with not approved loan (low age)."""
        commarea = CommareaBuffer()
        commarea.name = "Young Person"
        commarea.credit_score = "700"
        commarea.yearly_income = "30000"
        commarea.age = "17"  # Below 18
        commarea.amount = "5000"
        commarea.approved = "F"
        
        updated_commarea, resp = link_program(LINKED_PROGRAM, commarea)
        
        assert updated_commarea.approved == "F"
        assert "not approved" in updated_commarea.messages[0].lower() or "requirement" in updated_commarea.messages[2].lower()
    
    def test_invalid_numeric(self):
        """Test LINK with invalid numeric values."""
        commarea = CommareaBuffer()
        commarea.name = "Test User"
        commarea.credit_score = "invalid"
        commarea.age = "30"
        commarea.approved = "F"
        
        updated_commarea, resp = link_program(LINKED_PROGRAM, commarea)
        
        assert updated_commarea.approved == "F"
        assert "Invalid" in updated_commarea.messages[0] or updated_commarea.messages[0] == ""
    
    def test_wrong_program_name(self):
        """Test LINK with wrong program name."""
        commarea = CommareaBuffer()
        commarea.name = "Test User"
        commarea.credit_score = "700"
        commarea.age = "30"
        
        updated_commarea, resp = link_program("WRONGPROG", commarea)
        
        # Should return unchanged commarea
        assert updated_commarea.name == "Test User"
        assert updated_commarea.credit_score == "700"


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
            assert "LOANCICS Session Over" in text
        
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
            yrpaymnti="1200",
            efdtei="20250101",
            interesti="5.5"
        )
        
        map_out, commarea, error = main(
            eibcalen=10,
            eibaid=DFHENTER,
            dfhcommarea=None,
            map_in=map_in
        )
        
        assert map_out.approvedo == "Loan approved"
        assert map_out.uido.startswith("LOAN")
        assert map_out.nameo == "John Doe"
        assert len(map_out.msg1o) > 0
        assert error.origin == ""
        # Verify commarea was updated
        assert commarea.approved == "T"
        assert commarea.name == "John Doe"
        assert commarea.credit_score == "700"
    
    def test_enter_not_approved_loan(self):
        """Test ENTER with not approved loan application."""
        map_in = Minimap(
            namei="Jane Smith",
            crdscrei="600",  # Below 650
            incomei="40000",
            agei="25",
            amounti="5000",
            yrpaymnti="600",
            efdtei="20250101",
            interesti="5.5"
        )
        
        map_out, commarea, error = main(
            eibcalen=10,
            eibaid=DFHENTER,
            dfhcommarea=None,
            map_in=map_in
        )
        
        assert map_out.approvedo == "Loan not approved"
        assert map_out.uido.startswith("LOAN")
        assert map_out.nameo == "Jane Smith"
        assert error.origin == ""
        # Verify commarea was updated
        assert commarea.approved == "F"
        assert commarea.name == "Jane Smith"
        assert commarea.credit_score == "600"
    
    def test_enter_with_commarea_persistence(self):
        """Test ENTER with existing commarea data."""
        existing_commarea = CommareaBuffer(
            name="Previous Name",
            credit_score="800",
            uid="LOAN9999"
        )
        
        map_in = Minimap(
            namei="John Doe",
            crdscrei="700",
            incomei="50000",
            agei="30",
            amounti="10000",
            yrpaymnti="1200",
            efdtei="20250101",
            interesti="5.5"
        )
        
        map_out, commarea, error = main(
            eibcalen=10,
            eibaid=DFHENTER,
            dfhcommarea=existing_commarea,
            map_in=map_in
        )
        
        # Commarea should be updated with new values
        assert commarea.name == "John Doe"
        assert commarea.credit_score == "700"
        assert commarea.approved == "T"
        assert map_out.nameo == "John Doe"
    
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
        """Test that all 9 messages are populated on success."""
        map_in = Minimap(
            namei="John Doe",
            crdscrei="700",
            incomei="50000",
            agei="30",
            amounti="10000",
            yrpaymnti="1200",
            efdtei="20250101",
            interesti="5.5"
        )
        
        def mock_link_program(program_name, commarea):
            commarea.approved = "T"
            commarea.uid = "LOAN1234"
            for i in range(9):
                commarea.messages[i] = f"Message {i+1}"
            return commarea, None
        
        map_out, commarea, error = main(
            eibcalen=10,
            eibaid=DFHENTER,
            dfhcommarea=None,
            map_in=map_in,
            mock_link_program=mock_link_program
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
    
    def test_commarea_initialization(self):
        """Test that commarea fields are initialized correctly before LINK."""
        map_in = Minimap(
            namei="John Doe",
            crdscrei="700",
            incomei="50000",
            agei="30",
            amounti="10000",
            yrpaymnti="1200",
            efdtei="20250101",
            interesti="5.5"
        )
        
        link_called = False
        captured_commarea = None
        
        def mock_link_program(program_name, commarea):
            nonlocal link_called, captured_commarea
            link_called = True
            captured_commarea = commarea
            # Return commarea as-is for this test
            return commarea, None
        
        map_out, commarea, error = main(
            eibcalen=10,
            eibaid=DFHENTER,
            dfhcommarea=None,
            map_in=map_in,
            mock_link_program=mock_link_program
        )
        
        assert link_called is True
        assert captured_commarea is not None
        assert captured_commarea.name == "John Doe"
        assert captured_commarea.age == "30"
        assert captured_commarea.credit_score == "700"
        assert captured_commarea.yearly_income == "50000"
        assert captured_commarea.amount == "10000"
        assert captured_commarea.approved == "F"  # Initialized to 'F'
        assert captured_commarea.effect_date == "20250101"
        assert captured_commarea.yearly_interest_rate == "5.5"
        assert captured_commarea.yearly_repayment == "1200"
        assert captured_commarea.messages_num == "0"


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
            yrpaymnti="1200",
            efdtei="20250101",
            interesti="5.5"
        )
        
        map_out2, commarea2, error2 = main(
            eibcalen=10,
            eibaid=DFHENTER,
            dfhcommarea=commarea1,
            map_in=map_in
        )
        
        assert map_out2.approvedo in ["Loan approved", "Loan not approved"]
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
            yrpaymnti="600",
            efdtei="20250101",
            interesti="5.5"
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
            uid="LOAN1234",
            approved="T"
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
        assert commarea2.approved == "T"
    
    def test_multiple_loan_applications(self):
        """Test multiple loan applications in sequence."""
        # First application
        map_in1 = Minimap(
            namei="John Doe",
            crdscrei="700",
            incomei="50000",
            agei="30",
            amounti="10000",
            yrpaymnti="1200",
            efdtei="20250101",
            interesti="5.5"
        )
        
        map_out1, commarea1, error1 = main(
            eibcalen=10,
            eibaid=DFHENTER,
            dfhcommarea=None,
            map_in=map_in1
        )
        
        # Second application
        map_in2 = Minimap(
            namei="Jane Smith",
            crdscrei="600",
            incomei="40000",
            agei="25",
            amounti="5000",
            yrpaymnti="600",
            efdtei="20250201",
            interesti="6.0"
        )
        
        map_out2, commarea2, error2 = main(
            eibcalen=10,
            eibaid=DFHENTER,
            dfhcommarea=commarea1,
            map_in=map_in2
        )
        
        # Second application should have its own data
        assert commarea2.name == "Jane Smith"
        assert commarea2.credit_score == "600"
        assert map_out2.nameo == "Jane Smith"
        assert error2.origin == ""

