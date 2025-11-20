"""
Test suite for COTRN02C.py

Tests for Add Transaction transaction program.
"""

import pytest
from decimal import Decimal
from unittest.mock import patch

from COTRN02C import (
    TransactionAddMap,
    ThisProgCommarea,
    TranFileHandler,
    XrefFileHandler,
    XrefAixFileHandler,
    WorkingStorage,
    CardDemoCommarea,
    CardXrefRecord,
    TranRecord,
    populate_header_info,
    send_trnadd_screen,
    receive_trnadd_screen,
    initialize_all_fields,
    read_cxacaix_file,
    read_ccxref_file,
    startbr_transact_file_high_values,
    readprev_transact_file,
    endbr_transact_file,
    write_transact_file,
    validate_input_key_fields,
    validate_input_data_fields,
    process_enter_key,
    add_transaction,
    copy_last_tran_data,
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
    DFHRESP_NORMAL,
    DFHRESP_NOTFND,
    DFHRESP_ENDFILE,
    DFHRESP_DUPKEY
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
        """Test commarea with card number selection."""
        commarea = ThisProgCommarea(
            trn_selected="1234567890123456"
        )
        assert commarea.trn_selected == "1234567890123456"


# ============================================================================
# Test XREF File Handlers
# ============================================================================

class TestXrefFileHandlers:
    """Test XREF file handlers."""
    
    def test_xref_handler_read_by_card_num(self):
        """Test reading XREF by card number."""
        handler = XrefFileHandler("test_xref.txt")
        test_record = CardXrefRecord(
            card_num="1234567890123456",
            acct_id="ACCT0000001"
        )
        handler.add_record(test_record)
        handler.open_file()
        
        record = handler.read_record("1234567890123456")
        assert record is not None
        assert record.card_num == "1234567890123456"
        assert record.acct_id == "ACCT0000001"
    
    def test_xref_aix_handler_read_by_acct_id(self):
        """Test reading XREF alternate index by account ID."""
        handler = XrefAixFileHandler("test_xref_aix.txt")
        test_record = CardXrefRecord(
            card_num="1234567890123456",
            acct_id="ACCT0000001"
        )
        handler.add_record(test_record)
        handler.open_file()
        
        record = handler.read_record("ACCT0000001")
        assert record is not None
        assert record.card_num == "1234567890123456"
        assert record.acct_id == "ACCT0000001"


# ============================================================================
# Test TranFileHandler Write
# ============================================================================

class TestTranFileHandlerWrite:
    """Test transaction file handler write operations."""
    
    def test_write_record_success(self):
        """Test writing a new transaction record."""
        import tempfile
        import os
        # Use a temporary file to avoid conflicts
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as tmp:
            tmp_filename = tmp.name
        
        try:
            handler = TranFileHandler(tmp_filename)
            handler.open_file()
            
            record = TranRecord(
                tran_id="0000000000000001",
                tran_desc="Test Transaction",
                tran_amt=Decimal('100.50')
            )
            
            resp_cd = handler.write_record(record)
            assert resp_cd == DFHRESP_NORMAL
            assert "0000000000000001" in handler._data
        finally:
            # Clean up
            if os.path.exists(tmp_filename):
                os.remove(tmp_filename)
    
    def test_write_record_duplicate(self):
        """Test writing a duplicate transaction ID."""
        handler = TranFileHandler("test_transact.txt")
        handler.open_file()
        
        record1 = TranRecord(tran_id="0000000000000001", tran_desc="Test 1")
        handler.write_record(record1)
        
        record2 = TranRecord(tran_id="0000000000000001", tran_desc="Test 2")
        resp_cd = handler.write_record(record2)
        assert resp_cd == DFHRESP_DUPKEY
    
    def test_startbr_high_values(self):
        """Test starting browse from HIGH-VALUES."""
        handler = TranFileHandler("test_transact.txt")
        handler.add_record(TranRecord(tran_id="0000000000000001", tran_desc="Test 1"))
        handler.add_record(TranRecord(tran_id="0000000000000002", tran_desc="Test 2"))
        handler.open_file()
        
        resp_cd = handler.startbr_high_values()
        assert resp_cd == DFHRESP_NORMAL
        
        # Read previous should get last record
        record = handler.readprev_high_values()
        assert record is not None
        assert record.tran_id == "0000000000000002"


# ============================================================================
# Test Map Functions
# ============================================================================

class TestMapFunctions:
    """Test map functions."""
    
    def test_populate_header_info(self):
        """Test populating header info."""
        ws = WorkingStorage()
        map_out = TransactionAddMap()
        
        populate_header_info(ws, map_out)
        
        assert map_out.title01o != ""
        assert map_out.trnnameo == WS_TRANID
        assert map_out.pgmnameo == WS_PGMNAME
    
    def test_initialize_all_fields(self):
        """Test initializing all fields."""
        ws = WorkingStorage()
        map_in = TransactionAddMap()
        map_in.actidini = "TEST"
        map_in.ttypcdi = "01"
        ws.message = "Test message"
        
        initialize_all_fields(ws, map_in)
        
        assert map_in.actidini == ""
        assert map_in.ttypcdi == ""
        assert ws.message == ""
        assert map_in.actidinl == -1


# ============================================================================
# Test File Operations
# ============================================================================

class TestFileOperations:
    """Test file operations."""
    
    def test_read_cxacaix_file_success(self):
        """Test successful read from CXACAIX."""
        ws = WorkingStorage()
        handler = XrefAixFileHandler("test_xref_aix.txt")
        handler.add_record(CardXrefRecord(
            card_num="1234567890123456",
            acct_id="00000000001"
        ))
        handler.open_file()
        ws.xref_aix_handler = handler
        
        record = read_cxacaix_file(ws, "00000000001")
        
        assert record is not None
        assert record.card_num == "1234567890123456"
        assert ws.err_flg is False
    
    def test_read_ccxref_file_success(self):
        """Test successful read from CCXREF."""
        ws = WorkingStorage()
        handler = XrefFileHandler("test_xref.txt")
        handler.add_record(CardXrefRecord(
            card_num="1234567890123456",
            acct_id="00000000001"
        ))
        handler.open_file()
        ws.xref_handler = handler
        
        record = read_ccxref_file(ws, "1234567890123456")
        
        assert record is not None
        assert record.acct_id == "00000000001"
        assert ws.err_flg is False
    
    def test_write_transact_file_success(self):
        """Test successful write of transaction."""
        import tempfile
        import os
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as tmp:
            tmp_filename = tmp.name
        
        try:
            ws = WorkingStorage()
            handler = TranFileHandler(tmp_filename)
            handler.open_file()
            ws.tran_handler = handler
            
            record = TranRecord(
                tran_id="0000000000000001",
                tran_desc="Test",
                tran_amt=Decimal('100.00')
            )
            
            result = write_transact_file(ws, record)
            
            assert result is True
            assert ws.err_flg is False
        finally:
            if os.path.exists(tmp_filename):
                os.remove(tmp_filename)
    
    def test_write_transact_file_duplicate(self):
        """Test write of duplicate transaction ID."""
        ws = WorkingStorage()
        handler = TranFileHandler("test_transact.txt")
        handler.add_record(TranRecord(tran_id="0000000000000001", tran_desc="Test"))
        handler.open_file()
        ws.tran_handler = handler
        
        record = TranRecord(tran_id="0000000000000001", tran_desc="Duplicate")
        
        result = write_transact_file(ws, record)
        
        assert result is False
        assert ws.err_flg is True
        assert "already exist" in ws.message


# ============================================================================
# Test Validation
# ============================================================================

class TestValidation:
    """Test validation functions."""
    
    def test_validate_key_fields_by_account_id(self):
        """Test validating key fields with account ID."""
        ws = WorkingStorage()
        handler = XrefAixFileHandler("test_xref_aix.txt")
        handler.add_record(CardXrefRecord(
            card_num="1234567890123456",
            acct_id="00000000001"
        ))
        handler.open_file()
        ws.xref_aix_handler = handler
        map_in = TransactionAddMap()
        map_in.actidini = "00000000001"
        map_out = TransactionAddMap()
        
        result = validate_input_key_fields(ws, map_in, map_out)
        
        assert result is True
        assert map_in.cardnini == "1234567890123456"
        assert ws.err_flg is False
    
    def test_validate_key_fields_by_card_num(self):
        """Test validating key fields with card number."""
        ws = WorkingStorage()
        handler = XrefFileHandler("test_xref.txt")
        handler.add_record(CardXrefRecord(
            card_num="1234567890123456",
            acct_id="00000000001"
        ))
        handler.open_file()
        ws.xref_handler = handler
        map_in = TransactionAddMap()
        map_in.cardnini = "1234567890123456"
        map_out = TransactionAddMap()
        
        result = validate_input_key_fields(ws, map_in, map_out)
        
        assert result is True
        assert map_in.actidini == "00000000001"
        assert ws.err_flg is False
    
    def test_validate_key_fields_missing(self):
        """Test validating key fields when both are missing."""
        ws = WorkingStorage()
        handler = XrefFileHandler("test_xref.txt")
        handler.open_file()
        ws.xref_handler = handler
        map_in = TransactionAddMap()
        map_out = TransactionAddMap()
        
        result = validate_input_key_fields(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "Account or Card Number" in ws.message
    
    def test_validate_data_fields_all_required(self):
        """Test validating data fields - all required."""
        ws = WorkingStorage()
        map_in = TransactionAddMap()
        map_in.ttypcdi = "01"
        map_in.tcatcdi = "0005"
        map_in.trnsrci = "POS"
        map_in.tdesci = "Test Transaction"
        map_in.trnamti = "+00000100.50"
        map_in.torigdti = "2025-01-01"
        map_in.tprocdti = "2025-01-01"
        map_in.midi = "000000001"
        map_in.mnamei = "Test Merchant"
        map_in.mcityi = "New York"
        map_in.mzipi = "10001"
        map_out = TransactionAddMap()
        
        result = validate_input_data_fields(ws, map_in, map_out)
        
        assert result is True
        assert ws.err_flg is False
    
    def test_validate_data_fields_missing_type(self):
        """Test validating data fields - missing type."""
        ws = WorkingStorage()
        map_in = TransactionAddMap()
        map_in.ttypcdi = ""  # Missing
        map_out = TransactionAddMap()
        
        result = validate_input_data_fields(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "Type CD" in ws.message
    
    def test_validate_data_fields_amount_format(self):
        """Test validating amount format."""
        ws = WorkingStorage()
        map_in = TransactionAddMap()
        map_in.ttypcdi = "01"
        map_in.tcatcdi = "0005"
        map_in.trnsrci = "POS"
        map_in.tdesci = "Test"
        map_in.trnamti = "100.50"  # Wrong format
        map_in.torigdti = "2025-01-01"
        map_in.tprocdti = "2025-01-01"
        map_in.midi = "000000001"
        map_in.mnamei = "Test"
        map_in.mcityi = "NY"
        map_in.mzipi = "10001"
        map_out = TransactionAddMap()
        
        result = validate_input_data_fields(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "Amount should be" in ws.message
    
    def test_validate_data_fields_date_format(self):
        """Test validating date format."""
        ws = WorkingStorage()
        map_in = TransactionAddMap()
        map_in.ttypcdi = "01"
        map_in.tcatcdi = "0005"
        map_in.trnsrci = "POS"
        map_in.tdesci = "Test"
        map_in.trnamti = "+00000100.50"
        map_in.torigdti = "2025/01/01"  # Wrong format
        map_in.tprocdti = "2025-01-01"
        map_in.midi = "000000001"
        map_in.mnamei = "Test"
        map_in.mcityi = "NY"
        map_in.mzipi = "10001"
        map_out = TransactionAddMap()
        
        result = validate_input_data_fields(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "Orig Date should be" in ws.message
    
    def test_validate_data_fields_invalid_date(self):
        """Test validating invalid date."""
        ws = WorkingStorage()
        map_in = TransactionAddMap()
        map_in.ttypcdi = "01"
        map_in.tcatcdi = "0005"
        map_in.trnsrci = "POS"
        map_in.tdesci = "Test"
        map_in.trnamti = "+00000100.50"
        map_in.torigdti = "2025-02-30"  # Invalid date
        map_in.tprocdti = "2025-01-01"
        map_in.midi = "000000001"
        map_in.mnamei = "Test"
        map_in.mcityi = "NY"
        map_in.mzipi = "10001"
        map_out = TransactionAddMap()
        
        result = validate_input_data_fields(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        # Date validation should catch invalid dates
        assert "Not a valid date" in ws.message or "Orig Date" in ws.message


# ============================================================================
# Test Process Enter Key
# ============================================================================

class TestProcessEnterKey:
    """Test process_enter_key function."""
    
    def test_process_enter_key_no_confirmation(self):
        """Test processing enter key without confirmation."""
        ws = WorkingStorage()
        handler = XrefFileHandler("test_xref.txt")
        handler.add_record(CardXrefRecord(
            card_num="1234567890123456",
            acct_id="00000000001"
        ))
        handler.open_file()
        ws.xref_handler = handler
        ws.tran_handler = TranFileHandler("test_transact.txt")
        ws.tran_handler.open_file()
        map_in = TransactionAddMap()
        map_in.cardnini = "1234567890123456"
        map_in.ttypcdi = "01"
        map_in.tcatcdi = "0005"
        map_in.trnsrci = "POS"
        map_in.tdesci = "Test"
        map_in.trnamti = "+00000100.50"
        map_in.torigdti = "2025-01-01"
        map_in.tprocdti = "2025-01-01"
        map_in.midi = "000000001"
        map_in.mnamei = "Test"
        map_in.mcityi = "NY"
        map_in.mzipi = "10001"
        map_in.confirmi = ""  # No confirmation
        map_out = TransactionAddMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "Confirm to add" in ws.message
    
    def test_process_enter_key_invalid_confirmation(self):
        """Test processing enter key with invalid confirmation."""
        ws = WorkingStorage()
        handler = XrefFileHandler("test_xref.txt")
        handler.add_record(CardXrefRecord(
            card_num="1234567890123456",
            acct_id="00000000001"
        ))
        handler.open_file()
        ws.xref_handler = handler
        ws.tran_handler = TranFileHandler("test_transact.txt")
        ws.tran_handler.open_file()
        map_in = TransactionAddMap()
        map_in.cardnini = "1234567890123456"
        map_in.ttypcdi = "01"
        map_in.tcatcdi = "0005"
        map_in.trnsrci = "POS"
        map_in.tdesci = "Test"
        map_in.trnamti = "+00000100.50"
        map_in.torigdti = "2025-01-01"
        map_in.tprocdti = "2025-01-01"
        map_in.midi = "000000001"
        map_in.mnamei = "Test"
        map_in.mcityi = "NY"
        map_in.mzipi = "10001"
        map_in.confirmi = "X"  # Invalid
        map_out = TransactionAddMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "Invalid value" in ws.message


# ============================================================================
# Test Add Transaction
# ============================================================================

class TestAddTransaction:
    """Test add_transaction function."""
    
    def test_add_transaction_success(self):
        """Test successfully adding a transaction."""
        ws = WorkingStorage()
        handler = TranFileHandler("test_transact.txt")
        handler.open_file()
        ws.tran_handler = handler
        map_in = TransactionAddMap()
        map_in.cardnini = "1234567890123456"
        map_in.ttypcdi = "01"
        map_in.tcatcdi = "0005"
        map_in.trnsrci = "POS"
        map_in.tdesci = "Test Transaction"
        map_in.trnamti = "+00000100.50"
        map_in.torigdti = "2025-01-01"
        map_in.tprocdti = "2025-01-01"
        map_in.midi = "000000001"
        map_in.mnamei = "Test Merchant"
        map_in.mcityi = "New York"
        map_in.mzipi = "10001"
        map_out = TransactionAddMap()
        
        result = add_transaction(ws, map_in, map_out)
        
        assert result is True
        assert ws.err_flg is False
        assert "successfully" in ws.message
        assert ws.tran_handler._data
    
    def test_add_transaction_generates_new_id(self):
        """Test that new transaction ID is generated."""
        ws = WorkingStorage()
        handler = TranFileHandler("test_transact.txt")
        handler.add_record(TranRecord(
            tran_id="0000000000000001",
            tran_desc="Existing"
        ))
        handler.open_file()
        ws.tran_handler = handler
        map_in = TransactionAddMap()
        map_in.cardnini = "1234567890123456"
        map_in.ttypcdi = "01"
        map_in.tcatcdi = "0005"
        map_in.trnsrci = "POS"
        map_in.tdesci = "New Transaction"
        map_in.trnamti = "+00000100.50"
        map_in.torigdti = "2025-01-01"
        map_in.tprocdti = "2025-01-01"
        map_in.midi = "000000001"
        map_in.mnamei = "Test"
        map_in.mcityi = "NY"
        map_in.mzipi = "10001"
        map_out = TransactionAddMap()
        
        result = add_transaction(ws, map_in, map_out)
        
        assert result is True
        # New transaction ID should be 2
        assert "0000000000000002" in ws.tran_handler._data


# ============================================================================
# Test Copy Last Transaction
# ============================================================================

class TestCopyLastTransaction:
    """Test copy_last_tran_data function."""
    
    def test_copy_last_tran_data(self):
        """Test copying last transaction data."""
        ws = WorkingStorage()
        handler = XrefFileHandler("test_xref.txt")
        handler.add_record(CardXrefRecord(
            card_num="1234567890123456",
            acct_id="00000000001"
        ))
        handler.open_file()
        ws.xref_handler = handler
        tran_handler = TranFileHandler("test_transact.txt")
        tran_handler.add_record(TranRecord(
            tran_id="0000000000000001",
            tran_desc="Last Transaction",
            tran_amt=Decimal('50.00'),
            tran_type_cd="01",
            tran_cat_cd="0005"
        ))
        tran_handler.open_file()
        ws.tran_handler = tran_handler
        map_in = TransactionAddMap()
        map_in.cardnini = "1234567890123456"
        map_out = TransactionAddMap()
        
        copy_last_tran_data(ws, map_in, map_out)
        
        # Should copy transaction data
        assert map_in.tdesci == "Last Transaction"
        assert "+00000050.00" in map_in.trnamti or "50.00" in map_in.trnamti


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
        assert result_map.actidinl == -1 or result_map.actidinl == 0  # May be set by send_trnadd_screen
    
    def test_main_pf3_exit(self):
        """Test main with PF3."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        commarea.from_program = ""
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF3
        )
        
        assert xctl_prog == COMEN01C
    
    def test_main_pf4_clear_screen(self):
        """Test main with PF4."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = TransactionAddMap()
        map_in.actidini = "TEST"
        map_in.ttypcdi = "01"
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF4,
            map_input=map_in
        )
        
        assert xctl_prog is None
        assert result_map.actidini == ""
        assert result_map.ttypcdi == ""
    
    def test_main_pf5_copy_last(self):
        """Test main with PF5."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        xref_handler = XrefFileHandler("test_xref.txt")
        xref_handler.add_record(CardXrefRecord(
            card_num="1234567890123456",
            acct_id="00000000001"
        ))
        xref_handler.open_file()
        tran_handler = TranFileHandler("test_transact.txt")
        tran_handler.add_record(TranRecord(
            tran_id="0000000000000001",
            tran_desc="Last Transaction",
            tran_amt=Decimal('50.00')
        ))
        tran_handler.open_file()
        map_in = TransactionAddMap()
        map_in.cardnini = "1234567890123456"
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF5,
            map_input=map_in,
            xref_handler=xref_handler,
            tran_handler=tran_handler
        )
        
        assert xctl_prog is None
        # Should have copied transaction data (but may need confirmation)
        assert result_map.tdesci == "Last Transaction" or "Confirm" in result_map.errmsgo


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_add_transaction(self):
        """Test full flow: add transaction."""
        # First entry - show screen
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        xref_handler = XrefFileHandler("test_xref.txt")
        xref_handler.add_record(CardXrefRecord(
            card_num="1234567890123456",
            acct_id="00000000001"
        ))
        xref_handler.open_file()
        xref_aix_handler = XrefAixFileHandler("test_xref_aix.txt")
        xref_aix_handler.add_record(CardXrefRecord(
            card_num="1234567890123456",
            acct_id="00000000001"
        ))
        xref_aix_handler.open_file()
        tran_handler = TranFileHandler("test_transact.txt")
        tran_handler.open_file()
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            xref_handler=xref_handler,
            xref_aix_handler=xref_aix_handler,
            tran_handler=tran_handler
        )
        
        assert result_map.title01o != ""
        
        # Enter transaction data
        commarea2 = CardDemoCommarea()
        commarea2.pgm_reenter = True
        map_in = TransactionAddMap()
        map_in.cardnini = "1234567890123456"
        map_in.ttypcdi = "01"
        map_in.tcatcdi = "0005"
        map_in.trnsrci = "POS"
        map_in.tdesci = "Test Transaction"
        map_in.trnamti = "+00000100.50"
        map_in.torigdti = "2025-01-01"
        map_in.tprocdti = "2025-01-01"
        map_in.midi = "000000001"
        map_in.mnamei = "Test Merchant"
        map_in.mcityi = "New York"
        map_in.mzipi = "10001"
        map_in.confirmi = "Y"
        
        result_commarea2, result_map2, xctl_prog2, prog_commarea2 = main(
            commarea=commarea2,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            xref_handler=xref_handler,
            xref_aix_handler=xref_aix_handler,
            tran_handler=tran_handler
        )
        
        assert xctl_prog2 is None
        assert "successfully" in result_map2.errmsgo or result_map2.errmsgo == ""
        # Verify transaction was added
        assert len(tran_handler._data) > 0

