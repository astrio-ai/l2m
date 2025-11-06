"""
Test suite for COCRDLIC.py

Tests for Credit Card List transaction program.
"""

import pytest
import tempfile
import os
from decimal import Decimal
from unittest.mock import patch

from COCRDLIC import (
    CardRecord,
    CardDemoCommarea,
    CardListMap,
    WorkingStorage,
    ThisProgCommarea,
    parse_card_record,
    format_card_record,
    populate_header_info,
    send_map,
    receive_map,
    setup_screen_array,
    setup_array_attrs,
    setup_screen_attrs,
    setup_message,
    edit_account,
    edit_card,
    edit_array,
    edit_inputs,
    filter_records,
    read_forward,
    read_backwards,
    main,
    CardFileHandler,
    LIT_THISPGM,
    LIT_THISTRANID,
    DFHENTER,
    DFHPF3,
    DFHPF7,
    DFHPF8,
    WS_MAX_SCREEN_LINES
)


# ============================================================================
# Test CardRecord
# ============================================================================

class TestCardRecord:
    """Test CardRecord dataclass."""
    
    def test_creation(self):
        """Test creating a card record."""
        record = CardRecord()
        assert record.card_num == ""
        assert record.card_acct_id == ""
        assert record.card_active_status == ""
    
    def test_format_card_record(self):
        """Test formatting card record."""
        record = CardRecord(
            card_num="1234567890123456",
            card_acct_id="12345678901",
            card_active_status="Y"
        )
        formatted = format_card_record(record)
        assert len(formatted) >= 16
        assert record.card_num in formatted
    
    def test_parse_card_record(self):
        """Test parsing card record."""
        line = "1234567890123456" + "12345678901" + "Y" + " " * 120
        record = parse_card_record(line)
        assert record.card_num == "1234567890123456"
        assert record.card_acct_id == "12345678901"
        assert record.card_active_status == "Y"


# ============================================================================
# Test CardFileHandler
# ============================================================================

class TestCardFileHandler:
    """Test CardFileHandler."""
    
    def test_startbr_readnext_endbr(self):
        """Test browse operations."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            # Write some card records
            for i in range(1, 10):
                card_num = f"{i:016d}"
                acct_id = f"{i:011d}"
                record = CardRecord(card_num=card_num, card_acct_id=acct_id, card_active_status="Y")
                f.write(format_card_record(record) + "\n")
            temp_file = f.name
        
        try:
            handler = CardFileHandler(temp_file)
            handler.open_file()
            
            # Start browse
            resp_cd, reas_cd = handler.startbr("0000000000000001", gteq=True)
            assert resp_cd == 0
            
            # Read next
            record_line, resp_cd = handler.readnext()
            assert resp_cd == 0
            assert record_line is not None
            
            # End browse
            handler.endbr()
            assert handler._browse_active is False
        finally:
            os.unlink(temp_file)
    
    def test_readprev(self):
        """Test read previous."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            for i in range(1, 10):
                card_num = f"{i:016d}"
                acct_id = f"{i:011d}"
                record = CardRecord(card_num=card_num, card_acct_id=acct_id, card_active_status="Y")
                f.write(format_card_record(record) + "\n")
            temp_file = f.name
        
        try:
            handler = CardFileHandler(temp_file)
            handler.open_file()
            
            # Start browse at middle
            resp_cd, reas_cd = handler.startbr("0000000000000005", gteq=True)
            assert resp_cd == 0
            
            # Read previous
            record_line, resp_cd = handler.readprev()
            assert resp_cd == 0
            assert record_line is not None
            
            handler.endbr()
        finally:
            os.unlink(temp_file)


# ============================================================================
# Test Filter Functions
# ============================================================================

class TestFilterRecords:
    """Test filter_records function."""
    
    def test_filter_by_account_id(self):
        """Test filtering by account ID."""
        ws = WorkingStorage()
        ws.acctfilter_isvalid = True
        ws.cc_acct_id = "12345678901"
        
        card_record = CardRecord(
            card_num="1234567890123456",
            card_acct_id="12345678901",
            card_active_status="Y"
        )
        
        result = filter_records(ws, card_record)
        assert result is True
        
        # Test exclusion
        card_record2 = CardRecord(
            card_num="1234567890123456",
            card_acct_id="99999999999",
            card_active_status="Y"
        )
        result2 = filter_records(ws, card_record2)
        assert result2 is False
    
    def test_filter_by_card_num(self):
        """Test filtering by card number."""
        ws = WorkingStorage()
        ws.cardfilter_isvalid = True
        ws.cc_card_num_n = "1234567890123456"
        
        card_record = CardRecord(
            card_num="1234567890123456",
            card_acct_id="12345678901",
            card_active_status="Y"
        )
        
        result = filter_records(ws, card_record)
        assert result is True
        
        # Test exclusion
        card_record2 = CardRecord(
            card_num="9999999999999999",
            card_acct_id="12345678901",
            card_active_status="Y"
        )
        result2 = filter_records(ws, card_record2)
        assert result2 is False
    
    def test_no_filter(self):
        """Test with no filters."""
        ws = WorkingStorage()
        
        card_record = CardRecord(
            card_num="1234567890123456",
            card_acct_id="12345678901",
            card_active_status="Y"
        )
        
        result = filter_records(ws, card_record)
        assert result is True


# ============================================================================
# Test Edit Functions
# ============================================================================

class TestEditFunctions:
    """Test edit functions."""
    
    def test_edit_account_valid(self):
        """Test editing valid account ID."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        ws.cc_acct_id = "12345678901"
        
        edit_account(ws, commarea)
        
        assert ws.acctfilter_isvalid is True
        assert commarea.acct_id == "12345678901"
    
    def test_edit_account_invalid(self):
        """Test editing invalid account ID."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        ws.cc_acct_id = "ABC123"
        
        edit_account(ws, commarea)
        
        assert ws.acctfilter_not_ok is True
        assert ws.input_error is True
        assert "MUST BE A 11 DIGIT NUMBER" in ws.error_msg
    
    def test_edit_card_valid(self):
        """Test editing valid card number."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        ws.cc_card_num = "1234567890123456"
        
        edit_card(ws, commarea)
        
        assert ws.cardfilter_isvalid is True
        assert commarea.card_num == "1234567890123456"
    
    def test_edit_array_single_selection(self):
        """Test editing array with single selection."""
        ws = WorkingStorage()
        ws.edit_select_flags = ['', 'S', '', '', '', '', '']
        
        edit_array(ws)
        
        assert ws.i_selected == 2
        assert ws.input_error is False
    
    def test_edit_array_multiple_selections(self):
        """Test editing array with multiple selections."""
        ws = WorkingStorage()
        ws.edit_select_flags = ['S', 'U', '', '', '', '', '']
        
        edit_array(ws)
        
        assert ws.input_error is True
        assert "ONLY ONE RECORD" in ws.error_msg
    
    def test_edit_array_invalid_action(self):
        """Test editing array with invalid action code."""
        ws = WorkingStorage()
        ws.edit_select_flags = ['X', '', '', '', '', '', '']
        
        edit_array(ws)
        
        assert ws.input_error is True
        assert "INVALID ACTION CODE" in ws.error_msg


# ============================================================================
# Test Read Functions
# ============================================================================

class TestReadFunctions:
    """Test read functions."""
    
    def test_read_forward(self):
        """Test reading forward."""
        ws = WorkingStorage()
        prog_commarea = ThisProgCommarea()
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            for i in range(1, 10):
                card_num = f"{i:016d}"
                acct_id = f"{i:011d}"
                record = CardRecord(card_num=card_num, card_acct_id=acct_id, card_active_status="Y")
                f.write(format_card_record(record) + "\n")
            temp_file = f.name
        
        try:
            ws.card_handler = CardFileHandler(temp_file)
            ws.card_handler.open_file()
            ws.card_rid_cardnum = ""
            
            read_forward(ws, prog_commarea)
            
            assert len(ws.screen_rows) <= WS_MAX_SCREEN_LINES
            assert ws.scrn_counter <= WS_MAX_SCREEN_LINES
        finally:
            os.unlink(temp_file)
    
    def test_read_forward_with_filter(self):
        """Test reading forward with account filter."""
        ws = WorkingStorage()
        prog_commarea = ThisProgCommarea()
        ws.acctfilter_isvalid = True
        ws.cc_acct_id = "00000000001"
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            # Write records with different account IDs
            for i in range(1, 10):
                card_num = f"{i:016d}"
                acct_id = "00000000001" if i <= 5 else "00000000002"
                record = CardRecord(card_num=card_num, card_acct_id=acct_id, card_active_status="Y")
                f.write(format_card_record(record) + "\n")
            temp_file = f.name
        
        try:
            ws.card_handler = CardFileHandler(temp_file)
            ws.card_handler.open_file()
            ws.card_rid_cardnum = ""
            
            read_forward(ws, prog_commarea)
            
            # Should only have records with account ID 00000000001
            assert all(row[0] == "00000000001" for row in ws.screen_rows)
        finally:
            os.unlink(temp_file)


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_first_entry(self):
        """Test main on first entry."""
        commarea = CardDemoCommarea()
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            for i in range(1, 10):
                card_num = f"{i:016d}"
                acct_id = f"{i:011d}"
                record = CardRecord(card_num=card_num, card_acct_id=acct_id, card_active_status="Y")
                f.write(format_card_record(record) + "\n")
            temp_file = f.name
        
        try:
            result_commarea, result_prog_commarea, result_map, xctl_prog = main(
                commarea=commarea,
                eibcalen=0,
                card_file=temp_file
            )
            
            assert result_prog_commarea.screen_num == 1
            assert result_map.title01o != ""
            assert xctl_prog is None
        finally:
            os.unlink(temp_file)
    
    def test_main_pf3_exit(self):
        """Test main with PF3."""
        commarea = CardDemoCommarea()
        commarea.from_program = LIT_THISPGM
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            temp_file = f.name
        
        try:
            result_commarea, result_prog_commarea, result_map, xctl_prog = main(
                commarea=commarea,
                eibcalen=100,
                eibaid=DFHPF3,
                card_file=temp_file
            )
            
            assert xctl_prog == "COMEN01C"
            assert "EXITING" in result_commarea.error_msg
        finally:
            os.unlink(temp_file)
    
    def test_main_pf8_page_down(self):
        """Test main with PF8 (page down)."""
        commarea = CardDemoCommarea()
        commarea.from_program = LIT_THISPGM
        prog_commarea = ThisProgCommarea()
        prog_commarea.screen_num = 1
        prog_commarea.last_card_num = "0000000000000007"
        prog_commarea.first_card_num = "0000000000000001"
        prog_commarea.next_page_ind = "Y"
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            for i in range(1, 20):
                card_num = f"{i:016d}"
                acct_id = f"{i:011d}"
                record = CardRecord(card_num=card_num, card_acct_id=acct_id, card_active_status="Y")
                f.write(format_card_record(record) + "\n")
            temp_file = f.name
        
        try:
            result_commarea, result_prog_commarea, result_map, xctl_prog = main(
                commarea=commarea,
                eibcalen=100,
                eibaid=DFHPF8,
                prog_commarea=prog_commarea,
                card_file=temp_file
            )
            
            assert result_prog_commarea.screen_num == 2
            assert xctl_prog is None
        finally:
            os.unlink(temp_file)
    
    def test_main_select_view(self):
        """Test main with selection for view."""
        commarea = CardDemoCommarea()
        commarea.from_program = LIT_THISPGM
        commarea.pgm_reenter = True
        map_in = CardListMap()
        map_in.crdsel2i = "S"  # Select row 2
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            for i in range(1, 10):
                card_num = f"{i:016d}"
                acct_id = f"{i:011d}"
                record = CardRecord(card_num=card_num, card_acct_id=acct_id, card_active_status="Y")
                f.write(format_card_record(record) + "\n")
            temp_file = f.name
        
        try:
            # First call to populate screen rows
            commarea1 = CardDemoCommarea()
            commarea1.from_program = LIT_THISPGM
            result_commarea1, result_prog_commarea1, result_map1, xctl_prog1 = main(
                commarea=commarea1,
                eibcalen=100,
                eibaid=DFHENTER,
                card_file=temp_file
            )
            
            # Now try selection
            result_commarea, result_prog_commarea, result_map, xctl_prog = main(
                commarea=result_commarea1,
                eibcalen=100,
                eibaid=DFHENTER,
                map_input=map_in,
                prog_commarea=result_prog_commarea1,
                card_file=temp_file
            )
            
            # Should XCTL to detail program
            assert xctl_prog == "COCRDSLC"
            assert result_commarea.acct_id != ""
            assert result_commarea.card_num != ""
        finally:
            os.unlink(temp_file)


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_with_pagination(self):
        """Test complete flow with pagination."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            # Create 20 card records
            for i in range(1, 21):
                card_num = f"{i:016d}"
                acct_id = f"{i:011d}"
                record = CardRecord(card_num=card_num, card_acct_id=acct_id, card_active_status="Y")
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
            
            assert result_prog_commarea.screen_num == 1
            assert len(result_map.acctno1o) > 0  # Should have data
            
            # Page down
            commarea2 = CardDemoCommarea()
            commarea2.from_program = LIT_THISPGM
            result_commarea2, result_prog_commarea2, result_map2, xctl_prog2 = main(
                commarea=result_commarea,
                eibcalen=100,
                eibaid=DFHPF8,
                prog_commarea=result_prog_commarea,
                card_file=temp_file
            )
            
            assert result_prog_commarea2.screen_num == 2
        finally:
            os.unlink(temp_file)

