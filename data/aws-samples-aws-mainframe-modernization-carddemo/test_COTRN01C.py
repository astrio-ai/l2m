"""
Test suite for COTRN01C.py

Tests for Transaction View transaction program.
"""

import pytest
from decimal import Decimal
from unittest.mock import patch

from COTRN01C import (
    TransactionViewMap,
    ThisProgCommarea,
    TranFileHandler,
    WorkingStorage,
    CardDemoCommarea,
    TranRecord,
    populate_header_info,
    send_trnview_screen,
    receive_trnview_screen,
    initialize_all_fields,
    read_transact_file,
    process_enter_key,
    clear_current_screen,
    return_to_prev_screen,
    main,
    WS_PGMNAME,
    WS_TRANID,
    DFHENTER,
    DFHPF3,
    DFHPF4,
    DFHPF5,
    COSGN00C,
    COMEN01C,
    COTRN00C,
    DFHRESP_NORMAL,
    DFHRESP_NOTFND
)


# ============================================================================
# Test ThisProgCommarea
# ============================================================================

class TestThisProgCommarea:
    """Test ThisProgCommarea dataclass."""
    
    def test_commarea_creation(self):
        """Test creating program-specific commarea."""
        commarea = ThisProgCommarea()
        assert commarea.trn_selected == ""
        assert commarea.page_num == 0
    
    def test_commarea_with_selection(self):
        """Test commarea with transaction selection."""
        commarea = ThisProgCommarea(
            trn_selected="TRAN000000000001"
        )
        assert commarea.trn_selected == "TRAN000000000001"


# ============================================================================
# Test TranFileHandler
# ============================================================================

class TestTranFileHandler:
    """Test TranFileHandler."""
    
    def test_read_record_found(self):
        """Test reading an existing record."""
        handler = TranFileHandler("test_transact.txt")
        test_record = TranRecord(
            tran_id="TRAN000000000001",
            tran_desc="Test Transaction",
            tran_amt=Decimal('100.50')
        )
        handler.add_record(test_record)
        handler.open_file()
        
        record = handler.read_record("TRAN000000000001")
        assert record is not None
        assert record.tran_id == "TRAN000000000001"
        assert handler.resp_cd == DFHRESP_NORMAL
    
    def test_read_record_not_found(self):
        """Test reading a non-existent record."""
        handler = TranFileHandler("test_transact.txt")
        handler.open_file()
        
        record = handler.read_record("NONEXIST")
        assert record is None
        assert handler.resp_cd == DFHRESP_NOTFND


# ============================================================================
# Test Map Functions
# ============================================================================

class TestMapFunctions:
    """Test map functions."""
    
    def test_populate_header_info(self):
        """Test populating header info."""
        ws = WorkingStorage()
        map_out = TransactionViewMap()
        
        populate_header_info(ws, map_out)
        
        assert map_out.title01o != ""
        assert map_out.trnnameo == WS_TRANID
        assert map_out.pgmnameo == WS_PGMNAME
        assert map_out.curdateo != ""
        assert map_out.curtimeo != ""
    
    def test_initialize_all_fields(self):
        """Test initializing all fields."""
        ws = WorkingStorage()
        map_in = TransactionViewMap()
        map_in.trnidini = "TEST"
        map_in.trnidi = "TEST123"
        ws.message = "Test message"
        
        initialize_all_fields(ws, map_in)
        
        assert map_in.trnidini == ""
        assert map_in.trnidi == ""
        assert ws.message == ""
        assert map_in.trnidinl == -1


# ============================================================================
# Test File Operations
# ============================================================================

class TestFileOperations:
    """Test file operations."""
    
    def test_read_transact_file_success(self):
        """Test successful read of transaction."""
        ws = WorkingStorage()
        handler = TranFileHandler("test_transact.txt")
        handler.add_record(TranRecord(
            tran_id="TRAN000000000001",
            tran_desc="Test",
            tran_amt=Decimal('100.00')
        ))
        handler.open_file()
        ws.tran_handler = handler
        
        record = read_transact_file(ws, "TRAN000000000001")
        
        assert record is not None
        assert record.tran_id == "TRAN000000000001"
        assert ws.err_flg is False
    
    def test_read_transact_file_not_found(self):
        """Test read of non-existent transaction."""
        ws = WorkingStorage()
        handler = TranFileHandler("test_transact.txt")
        handler.open_file()
        ws.tran_handler = handler
        
        record = read_transact_file(ws, "NONEXIST")
        
        assert record is None
        assert ws.err_flg is True
        assert "NOT found" in ws.message


# ============================================================================
# Test Process Enter Key
# ============================================================================

class TestProcessEnterKey:
    """Test process_enter_key function."""
    
    def test_missing_transaction_id(self):
        """Test processing with missing transaction ID."""
        ws = WorkingStorage()
        handler = TranFileHandler("test_transact.txt")
        handler.open_file()
        ws.tran_handler = handler
        map_in = TransactionViewMap()
        map_in.trnidini = ""
        map_out = TransactionViewMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "Tran ID" in ws.message
        assert map_out.trnidinl == -1
    
    def test_valid_transaction(self):
        """Test processing with valid transaction ID."""
        ws = WorkingStorage()
        handler = TranFileHandler("test_transact.txt")
        handler.add_record(TranRecord(
            tran_id="TRAN000000000001",
            tran_desc="Test Transaction",
            tran_amt=Decimal('100.50'),
            tran_card_num="1234567890123456",
            tran_type_cd="01",
            tran_cat_cd="0005",
            tran_source="POS",
            tran_orig_ts="2025-01-01-12.00.00.000000",
            tran_proc_ts="2025-01-01-12.05.00.000000",
            tran_merchant_id="MERCH001",
            tran_merchant_name="Test Merchant",
            tran_merchant_city="New York",
            tran_merchant_zip="10001"
        ))
        handler.open_file()
        ws.tran_handler = handler
        map_in = TransactionViewMap()
        map_in.trnidini = "TRAN000000000001"
        map_out = TransactionViewMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is True
        assert ws.err_flg is False
        assert map_in.trnidi == "TRAN000000000001"
        assert map_in.tdesci == "Test Transaction"
        assert "100.50" in map_in.trnamti
    
    def test_transaction_not_found(self):
        """Test processing with non-existent transaction ID."""
        ws = WorkingStorage()
        handler = TranFileHandler("test_transact.txt")
        handler.open_file()
        ws.tran_handler = handler
        map_in = TransactionViewMap()
        map_in.trnidini = "NONEXIST"
        map_out = TransactionViewMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "NOT found" in ws.message


# ============================================================================
# Test Clear Current Screen
# ============================================================================

class TestClearCurrentScreen:
    """Test clear_current_screen function."""
    
    def test_clear_screen(self):
        """Test clearing the screen."""
        ws = WorkingStorage()
        map_in = TransactionViewMap()
        map_in.trnidini = "TEST"
        map_in.trnidi = "TEST123"
        map_out = TransactionViewMap()
        
        clear_current_screen(ws, map_in, map_out)
        
        assert map_in.trnidini == ""
        assert map_in.trnidi == ""
        assert map_out.title01o != ""


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_first_entry(self):
        """Test main on first entry."""
        commarea = CardDemoCommarea()
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=0
        )
        
        assert xctl_prog == COSGN00C
    
    def test_main_first_screen_display(self):
        """Test main showing screen for first time."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100
        )
        
        assert result_commarea.pgm_reenter is True
        assert result_map.title01o != ""
        assert result_map.trnidinl == -1 or result_map.trnidinl == 0  # May be set by send_trnview_screen
        assert xctl_prog is None
    
    def test_main_with_selected_transaction(self):
        """Test main with selected transaction from list."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        handler = TranFileHandler("test_transact.txt")
        handler.add_record(TranRecord(
            tran_id="TRAN000000000001",
            tran_desc="Test Transaction",
            tran_amt=Decimal('100.00')
        ))
        handler.open_file()
        prog_commarea = ThisProgCommarea()
        prog_commarea.trn_selected = "TRAN000000000001"
        
        result_commarea, result_map, xctl_prog, prog_commarea2 = main(
            commarea=commarea,
            eibcalen=100,
            prog_commarea=prog_commarea,
            tran_handler=handler
        )
        
        assert result_map.title01o != ""
        assert xctl_prog is None
        # Transaction should be displayed
        assert result_map.trnidini == "TRAN000000000001" or result_map.trnidi != ""
    
    def test_main_pf3_exit_no_from_program(self):
        """Test main with PF3 when no from_program."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        commarea.from_program = ""
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF3
        )
        
        assert xctl_prog == COMEN01C
    
    def test_main_pf3_exit_with_from_program(self):
        """Test main with PF3 when from_program exists."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        commarea.from_program = "COTRN00C"
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF3
        )
        
        assert xctl_prog == "COTRN00C"
    
    def test_main_pf4_clear_screen(self):
        """Test main with PF4."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = TransactionViewMap()
        map_in.trnidini = "TEST"
        map_in.trnidi = "TEST123"
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF4,
            map_input=map_in
        )
        
        assert xctl_prog is None
        assert result_map.trnidini == ""
        assert result_map.trnidi == ""
    
    def test_main_pf5_return_to_list(self):
        """Test main with PF5."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF5
        )
        
        assert xctl_prog == COTRN00C
    
    def test_main_enter_valid_transaction(self):
        """Test main with enter key and valid transaction."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        handler = TranFileHandler("test_transact.txt")
        handler.add_record(TranRecord(
            tran_id="TRAN000000000001",
            tran_desc="Test Transaction",
            tran_amt=Decimal('100.50'),
            tran_card_num="1234567890123456"
        ))
        handler.open_file()
        map_in = TransactionViewMap()
        map_in.trnidini = "TRAN000000000001"
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            tran_handler=handler
        )
        
        assert xctl_prog is None
        assert result_map.trnidi == "TRAN000000000001"
        assert result_map.tdesci == "Test Transaction"
    
    def test_main_enter_invalid_transaction(self):
        """Test main with enter key and invalid transaction."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        handler = TranFileHandler("test_transact.txt")
        handler.open_file()
        map_in = TransactionViewMap()
        map_in.trnidini = "NONEXIST"
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            tran_handler=handler
        )
        
        assert xctl_prog is None
        assert "NOT found" in result_map.errmsgo


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_view_transaction(self):
        """Test full flow: view transaction from list."""
        # First entry - show screen with selected transaction
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        handler = TranFileHandler("test_transact.txt")
        handler.add_record(TranRecord(
            tran_id="TRAN000000000001",
            tran_desc="Test Transaction",
            tran_amt=Decimal('100.50'),
            tran_card_num="1234567890123456",
            tran_type_cd="01",
            tran_merchant_name="Test Merchant"
        ))
        handler.open_file()
        prog_commarea = ThisProgCommarea()
        prog_commarea.trn_selected = "TRAN000000000001"
        
        result_commarea, result_map, xctl_prog, prog_commarea2 = main(
            commarea=commarea,
            eibcalen=100,
            prog_commarea=prog_commarea,
            tran_handler=handler
        )
        
        assert result_map.title01o != ""
        assert result_map.trnidini == "TRAN000000000001" or result_map.trnidi != ""
        
        # PF5 to return to list
        commarea2 = CardDemoCommarea()
        commarea2.pgm_reenter = True
        
        result_commarea2, result_map2, xctl_prog2, prog_commarea3 = main(
            commarea=commarea2,
            eibcalen=100,
            eibaid=DFHPF5
        )
        
        assert xctl_prog2 == COTRN00C
    
    def test_full_flow_enter_transaction_id(self):
        """Test full flow: enter transaction ID manually."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        handler = TranFileHandler("test_transact.txt")
        handler.add_record(TranRecord(
            tran_id="TRAN000000000001",
            tran_desc="Test Transaction",
            tran_amt=Decimal('100.50')
        ))
        handler.open_file()
        map_in = TransactionViewMap()
        map_in.trnidini = "TRAN000000000001"
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            tran_handler=handler
        )
        
        assert xctl_prog is None
        assert result_map.trnidi == "TRAN000000000001"
        assert result_map.tdesci == "Test Transaction"
    
    def test_full_flow_pf4_clear(self):
        """Test full flow: view transaction and clear screen."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        handler = TranFileHandler("test_transact.txt")
        handler.add_record(TranRecord(
            tran_id="TRAN000000000001",
            tran_desc="Test Transaction",
            tran_amt=Decimal('100.50')
        ))
        handler.open_file()
        map_in = TransactionViewMap()
        map_in.trnidini = "TRAN000000000001"
        
        # First, view transaction
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            tran_handler=handler
        )
        
        assert result_map.trnidi != ""
        
        # Then, clear screen
        commarea2 = CardDemoCommarea()
        commarea2.pgm_reenter = True
        
        result_commarea2, result_map2, xctl_prog2, prog_commarea2 = main(
            commarea=commarea2,
            eibcalen=100,
            eibaid=DFHPF4,
            map_input=result_map,
            tran_handler=handler
        )
        
        assert xctl_prog2 is None
        assert result_map2.trnidini == ""
        assert result_map2.trnidi == ""

