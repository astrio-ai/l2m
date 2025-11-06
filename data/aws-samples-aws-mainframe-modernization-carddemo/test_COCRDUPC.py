"""
Test suite for COCRDUPC.py

Tests for Credit Card Update transaction program.
"""

import pytest
import tempfile
import os
from decimal import Decimal
from unittest.mock import patch

from COCRDUPC import (
    ThisProgCommarea,
    CardDemoCommarea,
    CardUpdateMap,
    WorkingStorage,
    parse_card_record,
    format_card_record,
    send_map,
    receive_map,
    setup_screen_vars,
    setup_infomsg,
    edit_account,
    edit_card,
    edit_name,
    edit_cardstatus,
    edit_expiry_mon,
    edit_expiry_year,
    edit_map_inputs,
    process_inputs,
    getcard_byacctcard,
    read_data,
    write_processing,
    decide_action,
    main,
    CardFileHandler,
    LIT_THISPGM,
    LIT_THISTRANID,
    DFHENTER,
    DFHPF3,
    DFHPF5,
    DFHPF12,
    LIT_CCLISTPGM,
    LIT_MENUPGM,
    DFHRESP_NORMAL,
    DFHRESP_NOTFND
)
from COCRDSLC import CardRecord


# ============================================================================
# Test ThisProgCommarea
# ============================================================================

class TestThisProgCommarea:
    """Test ThisProgCommarea dataclass."""
    
    def test_details_not_fetched(self):
        """Test details_not_fetched property."""
        commarea = ThisProgCommarea()
        assert commarea.details_not_fetched is True
        
        commarea.change_action = "S"
        assert commarea.details_not_fetched is False
    
    def test_show_details(self):
        """Test show_details property."""
        commarea = ThisProgCommarea()
        commarea.change_action = "S"
        assert commarea.show_details is True
    
    def test_changes_made(self):
        """Test changes_made property."""
        commarea = ThisProgCommarea()
        commarea.change_action = "N"
        assert commarea.changes_made is True
        
        commarea.change_action = "C"
        assert commarea.changes_made is True
    
    def test_changes_ok_not_confirmed(self):
        """Test changes_ok_not_confirmed property."""
        commarea = ThisProgCommarea()
        commarea.change_action = "N"
        assert commarea.changes_ok_not_confirmed is True
    
    def test_changes_okayed_and_done(self):
        """Test changes_okayed_and_done property."""
        commarea = ThisProgCommarea()
        commarea.change_action = "C"
        assert commarea.changes_okayed_and_done is True


# ============================================================================
# Test CardFileHandler
# ============================================================================

class TestCardFileHandler:
    """Test CardFileHandler."""
    
    def test_read_for_update_success(self):
        """Test successful read for update."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            card_num = "1234567890123456"
            record = CardRecord(
                card_num=card_num,
                card_acct_id="12345678901",
                card_active_status="Y",
                card_embossed_name="JOHN DOE",
                card_expiration_date="2025-12-31"
            )
            f.write(format_card_record(record) + "\n")
            temp_file = f.name
        
        try:
            handler = CardFileHandler(temp_file)
            handler.open_file()
            
            record_line, resp_cd = handler.read_for_update(card_num)
            assert resp_cd == 0
            assert record_line is not None
            assert card_num in record_line
            # Check that record is locked
            assert card_num in handler._locked_records
        finally:
            os.unlink(temp_file)
    
    def test_rewrite_record_success(self):
        """Test successful rewrite."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            card_num = "1234567890123456"
            record = CardRecord(
                card_num=card_num,
                card_acct_id="12345678901",
                card_active_status="Y",
                card_embossed_name="JOHN DOE",
                card_expiration_date="2025-12-31"
            )
            f.write(format_card_record(record) + "\n")
            temp_file = f.name
        
        try:
            handler = CardFileHandler(temp_file)
            handler.open_file()
            
            # Read for update
            record_line, resp_cd = handler.read_for_update(card_num)
            assert resp_cd == 0
            
            # Update record
            new_record = CardRecord(
                card_num=card_num,
                card_acct_id="12345678901",
                card_active_status="N",
                card_embossed_name="JANE SMITH",
                card_expiration_date="2026-06-30"
            )
            new_record_line = format_card_record(new_record)
            
            resp_cd = handler.rewrite_record(card_num, new_record_line)
            assert resp_cd == 0
            
            # Verify update
            updated_line, _ = handler.read_record(card_num)
            assert "JANE SMITH" in updated_line
            assert "N" in updated_line
        finally:
            os.unlink(temp_file)
    
    def test_rewrite_record_changed(self):
        """Test rewrite when record was changed."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            card_num = "1234567890123456"
            record = CardRecord(
                card_num=card_num,
                card_acct_id="12345678901",
                card_active_status="Y",
                card_embossed_name="JOHN DOE",
                card_expiration_date="2025-12-31"
            )
            f.write(format_card_record(record) + "\n")
            temp_file = f.name
        
        try:
            handler = CardFileHandler(temp_file)
            handler.open_file()
            
            # Read for update
            record_line, resp_cd = handler.read_for_update(card_num)
            assert resp_cd == 0
            
            # Simulate record change by another user
            handler._data[card_num] = format_card_record(CardRecord(
                card_num=card_num,
                card_acct_id="12345678901",
                card_active_status="N",
                card_embossed_name="CHANGED",
                card_expiration_date="2025-12-31"
            ))
            
            # Try to rewrite - should fail
            new_record_line = format_card_record(CardRecord(
                card_num=card_num,
                card_acct_id="12345678901",
                card_active_status="Y",
                card_embossed_name="JANE SMITH",
                card_expiration_date="2026-06-30"
            ))
            
            resp_cd = handler.rewrite_record(card_num, new_record_line)
            assert resp_cd == 12  # Should fail
        finally:
            os.unlink(temp_file)


# ============================================================================
# Test Edit Functions
# ============================================================================

class TestEditFunctions:
    """Test edit functions."""
    
    def test_edit_account_valid(self):
        """Test editing valid account ID."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        prog_commarea = ThisProgCommarea()
        ws.cc_acct_id = "12345678901"
        
        edit_account(ws, commarea, prog_commarea)
        
        assert ws.acctfilter_isvalid is True
        assert commarea.acct_id == "12345678901"
        assert ws.input_error is False
    
    def test_edit_account_invalid(self):
        """Test editing invalid account ID."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        prog_commarea = ThisProgCommarea()
        ws.cc_acct_id = "ABC123"
        
        edit_account(ws, commarea, prog_commarea)
        
        assert ws.acctfilter_not_ok is True
        assert ws.input_error is True
        assert "MUST BE A 11 DIGIT NUMBER" in ws.return_msg
    
    def test_edit_card_valid(self):
        """Test editing valid card number."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        prog_commarea = ThisProgCommarea()
        ws.cc_card_num = "1234567890123456"
        
        edit_card(ws, commarea, prog_commarea)
        
        assert ws.cardfilter_isvalid is True
        assert commarea.card_num == "1234567890123456"
        assert ws.input_error is False
    
    def test_edit_name_valid(self):
        """Test editing valid card name."""
        ws = WorkingStorage()
        prog_commarea = ThisProgCommarea()
        prog_commarea.new_crdname = "JOHN DOE"
        
        edit_name(ws, prog_commarea)
        
        assert ws.cardname_isvalid is True
        assert ws.input_error is False
    
    def test_edit_name_invalid(self):
        """Test editing invalid card name."""
        ws = WorkingStorage()
        prog_commarea = ThisProgCommarea()
        prog_commarea.new_crdname = "JOHN123"
        
        edit_name(ws, prog_commarea)
        
        assert ws.cardname_not_ok is True
        assert ws.input_error is True
        assert "alphabets and spaces" in ws.return_msg
    
    def test_edit_cardstatus_valid(self):
        """Test editing valid card status."""
        ws = WorkingStorage()
        prog_commarea = ThisProgCommarea()
        prog_commarea.new_crdstcd = "Y"
        
        edit_cardstatus(ws, prog_commarea)
        
        assert ws.cardstatus_isvalid is True
        assert ws.input_error is False
    
    def test_edit_cardstatus_invalid(self):
        """Test editing invalid card status."""
        ws = WorkingStorage()
        prog_commarea = ThisProgCommarea()
        prog_commarea.new_crdstcd = "X"
        
        edit_cardstatus(ws, prog_commarea)
        
        assert ws.cardstatus_not_ok is True
        assert ws.input_error is True
        assert "Y or N" in ws.return_msg
    
    def test_edit_expiry_mon_valid(self):
        """Test editing valid expiry month."""
        ws = WorkingStorage()
        prog_commarea = ThisProgCommarea()
        prog_commarea.new_expmon = "06"
        
        edit_expiry_mon(ws, prog_commarea)
        
        assert ws.cardexpmon_isvalid is True
        assert ws.input_error is False
    
    def test_edit_expiry_mon_invalid(self):
        """Test editing invalid expiry month."""
        ws = WorkingStorage()
        prog_commarea = ThisProgCommarea()
        prog_commarea.new_expmon = "13"
        
        edit_expiry_mon(ws, prog_commarea)
        
        assert ws.cardexpmon_not_ok is True
        assert ws.input_error is True
        assert "1 and 12" in ws.return_msg
    
    def test_edit_expiry_year_valid(self):
        """Test editing valid expiry year."""
        ws = WorkingStorage()
        prog_commarea = ThisProgCommarea()
        prog_commarea.new_expyear = "2025"
        
        edit_expiry_year(ws, prog_commarea)
        
        assert ws.cardexpyear_isvalid is True
        assert ws.input_error is False
    
    def test_edit_expiry_year_invalid(self):
        """Test editing invalid expiry year."""
        ws = WorkingStorage()
        prog_commarea = ThisProgCommarea()
        prog_commarea.new_expyear = "1949"
        
        edit_expiry_year(ws, prog_commarea)
        
        assert ws.cardexpyear_not_ok is True
        assert ws.input_error is True
        assert "Invalid" in ws.return_msg


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_first_entry(self):
        """Test main on first entry."""
        commarea = CardDemoCommarea()
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            temp_file = f.name
        
        try:
            result_commarea, result_prog_commarea, result_map, xctl_prog = main(
                commarea=commarea,
                eibcalen=0,
                card_file=temp_file
            )
            
            assert result_map.title01o != ""
            assert "Please enter" in result_map.infomsgo
            assert xctl_prog is None
        finally:
            os.unlink(temp_file)
    
    def test_main_pf3_exit(self):
        """Test main with PF3."""
        commarea = CardDemoCommarea()
        commarea.from_program = ""
        commarea.from_tranid = ""
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            temp_file = f.name
        
        try:
            result_commarea, result_prog_commarea, result_map, xctl_prog = main(
                commarea=commarea,
                eibcalen=100,
                eibaid=DFHPF3,
                card_file=temp_file
            )
            
            assert xctl_prog == LIT_MENUPGM
            assert "Exiting" in result_commarea.error_msg
        finally:
            os.unlink(temp_file)
    
    def test_main_from_card_list(self):
        """Test main when coming from card list."""
        commarea = CardDemoCommarea()
        commarea.from_program = LIT_CCLISTPGM
        commarea.acct_id = "12345678901"
        commarea.card_num = "1234567890123456"
        commarea.pgm_enter = True
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            record = CardRecord(
                card_num="1234567890123456",
                card_acct_id="12345678901",
                card_active_status="Y",
                card_embossed_name="JOHN DOE",
                card_expiration_date="2025-12-31"
            )
            f.write(format_card_record(record) + "\n")
            temp_file = f.name
        
        try:
            result_commarea, result_prog_commarea, result_map, xctl_prog = main(
                commarea=commarea,
                eibcalen=100,
                card_file=temp_file
            )
            
            assert result_map.crdnameo != ""
            assert "Details of selected" in result_map.infomsgo
            assert result_prog_commarea.show_details is True
            assert xctl_prog is None
        finally:
            os.unlink(temp_file)
    
    def test_main_reenter_with_valid_input(self):
        """Test main on reenter with valid input."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = CardUpdateMap()
        map_in.acctsidi = "12345678901"
        map_in.cardsidi = "1234567890123456"
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            record = CardRecord(
                card_num="1234567890123456",
                card_acct_id="12345678901",
                card_active_status="Y",
                card_embossed_name="JOHN DOE",
                card_expiration_date="2025-12-31"
            )
            f.write(format_card_record(record) + "\n")
            temp_file = f.name
        
        try:
            result_commarea, result_prog_commarea, result_map, xctl_prog = main(
                commarea=commarea,
                eibcalen=100,
                eibaid=DFHENTER,
                map_input=map_in,
                card_file=temp_file
            )
            
            assert result_map.crdnameo != ""
            assert "Details of selected" in result_map.infomsgo
            assert result_prog_commarea.show_details is True
            assert xctl_prog is None
        finally:
            os.unlink(temp_file)
    
    def test_main_update_card(self):
        """Test main updating card."""
        commarea = CardDemoCommarea()
        commarea.from_program = LIT_CCLISTPGM
        commarea.acct_id = "12345678901"
        commarea.card_num = "1234567890123456"
        commarea.pgm_enter = True
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            record = CardRecord(
                card_num="1234567890123456",
                card_acct_id="12345678901",
                card_active_status="Y",
                card_embossed_name="JOHN DOE",
                card_expiration_date="2025-12-31"
            )
            f.write(format_card_record(record) + "\n")
            temp_file = f.name
        
        try:
            # First entry from card list
            result_commarea, result_prog_commarea, result_map, xctl_prog = main(
                commarea=commarea,
                eibcalen=100,
                card_file=temp_file
            )
            
            assert result_prog_commarea.show_details is True
            
            # Enter changes
            commarea2 = CardDemoCommarea()
            commarea2.from_program = LIT_CCLISTPGM
            commarea2.acct_id = "12345678901"
            commarea2.card_num = "1234567890123456"
            commarea2.pgm_reenter = True
            map_in2 = CardUpdateMap()
            map_in2.acctsidi = "12345678901"
            map_in2.cardsidi = "1234567890123456"
            map_in2.crdnamei = "JANE SMITH"
            map_in2.crdstcdi = "N"
            map_in2.expmoni = "06"
            map_in2.expyeari = "2026"
            
            result_commarea2, result_prog_commarea2, result_map2, xctl_prog2 = main(
                commarea=commarea2,
                eibcalen=100,
                eibaid=DFHENTER,
                map_input=map_in2,
                prog_commarea=result_prog_commarea,
                card_file=temp_file
            )
            
            assert result_prog_commarea2.changes_ok_not_confirmed is True
            assert "Press F5 to save" in result_map2.infomsgo
            
            # Confirm with PF5
            result_commarea3, result_prog_commarea3, result_map3, xctl_prog3 = main(
                commarea=result_commarea2,
                eibcalen=100,
                eibaid=DFHPF5,
                map_input=map_in2,
                prog_commarea=result_prog_commarea2,
                card_file=temp_file
            )
            
            assert result_prog_commarea3.changes_okayed_and_done is True
            assert "committed to database" in result_map3.infomsgo
            
            # Verify update
            handler = CardFileHandler(temp_file)
            handler.open_file()
            updated_line, _ = handler.read_record("1234567890123456")
            assert "JANE SMITH" in updated_line
            assert "N" in updated_line
        finally:
            os.unlink(temp_file)


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_from_card_list(self):
        """Test complete flow from card list."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            record = CardRecord(
                card_num="1234567890123456",
                card_acct_id="12345678901",
                card_active_status="Y",
                card_embossed_name="JOHN DOE",
                card_expiration_date="2025-12-31"
            )
            f.write(format_card_record(record) + "\n")
            temp_file = f.name
        
        try:
            # First entry from card list
            commarea = CardDemoCommarea()
            commarea.from_program = LIT_CCLISTPGM
            commarea.acct_id = "12345678901"
            commarea.card_num = "1234567890123456"
            commarea.pgm_enter = True
            
            result_commarea, result_prog_commarea, result_map, xctl_prog = main(
                commarea=commarea,
                eibcalen=100,
                card_file=temp_file
            )
            
            assert result_map.crdnameo == "JOHN DOE"
            assert result_prog_commarea.show_details is True
            
            # PF3 to exit
            result_commarea2, result_prog_commarea2, result_map2, xctl_prog2 = main(
                commarea=result_commarea,
                eibcalen=100,
                eibaid=DFHPF3,
                prog_commarea=result_prog_commarea,
                card_file=temp_file
            )
            
            assert xctl_prog2 == LIT_CCLISTPGM
        finally:
            os.unlink(temp_file)
    
    def test_full_flow_with_search_and_update(self):
        """Test complete flow with search and update."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            record = CardRecord(
                card_num="1234567890123456",
                card_acct_id="12345678901",
                card_active_status="Y",
                card_embossed_name="JOHN DOE",
                card_expiration_date="2025-12-31"
            )
            f.write(format_card_record(record) + "\n")
            temp_file = f.name
        
        try:
            # First entry
            commarea = CardDemoCommarea()
            result_commarea, result_prog_commarea, result_map, xctl_prog = main(
                commarea=commarea,
                eibcalen=0,
                card_file=temp_file
            )
            
            assert "Please enter" in result_map.infomsgo
            
            # Enter search criteria
            commarea2 = CardDemoCommarea()
            commarea2.pgm_reenter = True
            map_in = CardUpdateMap()
            map_in.acctsidi = "12345678901"
            map_in.cardsidi = "1234567890123456"
            
            result_commarea2, result_prog_commarea2, result_map2, xctl_prog2 = main(
                commarea=commarea2,
                eibcalen=100,
                eibaid=DFHENTER,
                map_input=map_in,
                card_file=temp_file
            )
            
            assert result_map2.crdnameo == "JOHN DOE"
            assert result_prog_commarea2.show_details is True
            
            # Update card
            commarea3 = CardDemoCommarea()
            commarea3.from_program = LIT_CCLISTPGM
            commarea3.acct_id = "12345678901"
            commarea3.card_num = "1234567890123456"
            commarea3.pgm_reenter = True
            map_in3 = CardUpdateMap()
            map_in3.acctsidi = "12345678901"
            map_in3.cardsidi = "1234567890123456"
            map_in3.crdnamei = "JANE SMITH"
            map_in3.crdstcdi = "N"
            map_in3.expmoni = "06"
            map_in3.expyeari = "2026"
            
            result_commarea3, result_prog_commarea3, result_map3, xctl_prog3 = main(
                commarea=commarea3,
                eibcalen=100,
                eibaid=DFHENTER,
                map_input=map_in3,
                prog_commarea=result_prog_commarea2,
                card_file=temp_file
            )
            
            assert result_prog_commarea3.changes_ok_not_confirmed is True
            
            # Confirm with PF5
            result_commarea4, result_prog_commarea4, result_map4, xctl_prog4 = main(
                commarea=result_commarea3,
                eibcalen=100,
                eibaid=DFHPF5,
                map_input=map_in3,
                prog_commarea=result_prog_commarea3,
                card_file=temp_file
            )
            
            assert result_prog_commarea4.changes_okayed_and_done is True
            
            # Verify update persisted
            handler = CardFileHandler(temp_file)
            handler.open_file()
            updated_line, _ = handler.read_record("1234567890123456")
            assert "JANE SMITH" in updated_line
        finally:
            os.unlink(temp_file)

