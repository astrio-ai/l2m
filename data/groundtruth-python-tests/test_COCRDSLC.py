"""
Test suite for COCRDSLC.py

Tests for Credit Card Detail transaction program.
"""

import pytest
import tempfile
import os
from decimal import Decimal
from unittest.mock import patch

from COCRDSLC import (
    CardRecord,
    CardDemoCommarea,
    CardDetailMap,
    WorkingStorage,
    ThisProgCommarea,
    parse_card_record,
    format_card_record,
    populate_header_info,
    send_map,
    receive_map,
    setup_screen_vars,
    setup_screen_attrs,
    edit_account,
    edit_card,
    edit_map_inputs,
    process_inputs,
    getcard_byacctcard,
    read_data,
    main,
    CardFileHandler,
    LIT_THISPGM,
    LIT_THISTRANID,
    DFHENTER,
    DFHPF3,
    LIT_CCLISTPGM,
    LIT_MENUPGM,
    DFHRESP_NORMAL,
    DFHRESP_NOTFND
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
        assert record.card_embossed_name == ""
        assert record.card_expiration_date == ""
    
    def test_expiry_properties(self):
        """Test expiry date properties."""
        record = CardRecord(card_expiration_date="2025-12-31")
        assert record.card_expiry_year == "2025"
        assert record.card_expiry_month == "12"
        assert record.card_expiry_day == "31"
    
    def test_format_card_record(self):
        """Test formatting card record."""
        record = CardRecord(
            card_num="1234567890123456",
            card_acct_id="12345678901",
            card_active_status="Y",
            card_embossed_name="JOHN DOE",
            card_expiration_date="2025-12-31"
        )
        formatted = format_card_record(record)
        assert len(formatted) >= 16
        assert record.card_num in formatted
        assert record.card_embossed_name in formatted
    
    def test_parse_card_record(self):
        """Test parsing card record."""
        line = ("1234567890123456" + "12345678901" + "Y" + 
                "JOHN DOE" + " " * 42 + "2025-12-31" + " " * 70)
        line = line.ljust(150)
        record = parse_card_record(line)
        assert record.card_num == "1234567890123456"
        assert record.card_acct_id == "12345678901"
        assert record.card_active_status == "Y"
        assert "JOHN" in record.card_embossed_name
        assert record.card_expiration_date == "2025-12-31"


# ============================================================================
# Test CardFileHandler
# ============================================================================

class TestCardFileHandler:
    """Test CardFileHandler."""
    
    def test_read_record_success(self):
        """Test successful read of card record."""
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
            
            record_line, resp_cd = handler.read_record(card_num)
            assert resp_cd == 0
            assert record_line is not None
            assert card_num in record_line
        finally:
            os.unlink(temp_file)
    
    def test_read_record_not_found(self):
        """Test read when card not found."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            temp_file = f.name
        
        try:
            handler = CardFileHandler(temp_file)
            handler.open_file()
            
            record_line, resp_cd = handler.read_record("9999999999999999")
            assert resp_cd == 12
            assert record_line is None
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
        ws.cc_acct_id = "12345678901"
        
        edit_account(ws, commarea)
        
        assert ws.acctfilter_isvalid is True
        assert commarea.acct_id == "12345678901"
        assert ws.input_error is False
    
    def test_edit_account_invalid(self):
        """Test editing invalid account ID."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        ws.cc_acct_id = "ABC123"
        
        edit_account(ws, commarea)
        
        assert ws.acctfilter_not_ok is True
        assert ws.input_error is True
        assert "MUST BE A 11 DIGIT NUMBER" in ws.return_msg
    
    def test_edit_account_blank(self):
        """Test editing blank account ID."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        ws.cc_acct_id = ""
        
        edit_account(ws, commarea)
        
        assert ws.acctfilter_blank is True
        assert ws.input_error is True
        assert "not provided" in ws.return_msg
    
    def test_edit_card_valid(self):
        """Test editing valid card number."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        ws.cc_card_num = "1234567890123456"
        
        edit_card(ws, commarea)
        
        assert ws.cardfilter_isvalid is True
        assert commarea.card_num == "1234567890123456"
        assert ws.input_error is False
    
    def test_edit_card_invalid(self):
        """Test editing invalid card number."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        ws.cc_card_num = "ABC123"
        
        edit_card(ws, commarea)
        
        assert ws.cardfilter_not_ok is True
        assert ws.input_error is True
        assert "MUST BE A 16 DIGIT NUMBER" in ws.return_msg
    
    def test_edit_map_inputs_no_criteria(self):
        """Test editing with no search criteria."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        ws.cc_acct_id = ""
        ws.cc_card_num = ""
        
        edit_map_inputs(ws, commarea)
        
        assert ws.input_error is True
        assert "No input received" in ws.return_msg


# ============================================================================
# Test Read Functions
# ============================================================================

class TestReadFunctions:
    """Test read functions."""
    
    def test_getcard_byacctcard_success(self):
        """Test successful card read."""
        ws = WorkingStorage()
        ws.cc_card_num = "1234567890123456"
        
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
            ws.card_handler = CardFileHandler(temp_file)
            ws.card_handler.open_file()
            
            getcard_byacctcard(ws)
            
            assert ws.found_cards_for_account is True
            assert ws.card_record is not None
            assert ws.card_record.card_num == "1234567890123456"
            assert "Displaying" in ws.info_msg
        finally:
            os.unlink(temp_file)
    
    def test_getcard_byacctcard_not_found(self):
        """Test card read when not found."""
        ws = WorkingStorage()
        ws.cc_card_num = "9999999999999999"
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            temp_file = f.name
        
        try:
            ws.card_handler = CardFileHandler(temp_file)
            ws.card_handler.open_file()
            
            getcard_byacctcard(ws)
            
            assert ws.found_cards_for_account is False
            assert ws.input_error is True
            assert "Did not find" in ws.return_msg
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
            assert "Displaying" in result_map.infomsgo
            assert xctl_prog is None
        finally:
            os.unlink(temp_file)
    
    def test_main_reenter_with_valid_input(self):
        """Test main on reenter with valid input."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = CardDetailMap()
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
            assert "Displaying" in result_map.infomsgo
            assert xctl_prog is None
        finally:
            os.unlink(temp_file)
    
    def test_main_reenter_with_invalid_input(self):
        """Test main on reenter with invalid input."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = CardDetailMap()
        map_in.acctsidi = "ABC123"
        map_in.cardsidi = "1234567890123456"
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            temp_file = f.name
        
        try:
            result_commarea, result_prog_commarea, result_map, xctl_prog = main(
                commarea=commarea,
                eibcalen=100,
                eibaid=DFHENTER,
                map_input=map_in,
                card_file=temp_file
            )
            
            assert "MUST BE A 11 DIGIT NUMBER" in result_map.errmsgo
            assert xctl_prog is None
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
            assert result_map.expmono == "12"
            assert result_map.expyearo == "2025"
            assert result_map.crdstcdo == "Y"
            assert "Displaying" in result_map.infomsgo
            
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
    
    def test_full_flow_with_search(self):
        """Test complete flow with search."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            record = CardRecord(
                card_num="1234567890123456",
                card_acct_id="12345678901",
                card_active_status="Y",
                card_embossed_name="JANE SMITH",
                card_expiration_date="2026-06-30"
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
            map_in = CardDetailMap()
            map_in.acctsidi = "12345678901"
            map_in.cardsidi = "1234567890123456"
            
            result_commarea2, result_prog_commarea2, result_map2, xctl_prog2 = main(
                commarea=commarea2,
                eibcalen=100,
                eibaid=DFHENTER,
                map_input=map_in,
                card_file=temp_file
            )
            
            assert result_map2.crdnameo == "JANE SMITH"
            assert result_map2.expmono == "06"
            assert result_map2.expyearo == "2026"
        finally:
            os.unlink(temp_file)

