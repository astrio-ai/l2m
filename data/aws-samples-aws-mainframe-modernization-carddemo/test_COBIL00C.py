"""
Test suite for COBIL00C.py

Tests for Bill Payment transaction program.
"""

import pytest
import tempfile
import os
from decimal import Decimal
from unittest.mock import patch

from COBIL00C import (
    TranRecord,
    CardDemoCommarea,
    BillPayMap,
    WorkingStorage,
    format_tran_record,
    parse_tran_record,
    get_current_timestamp,
    populate_header_info,
    send_billpay_screen,
    receive_billpay_screen,
    initialize_all_fields,
    read_acctdat_file,
    update_acctdat_file,
    read_cxacaix_file,
    startbr_transact_file,
    readprev_transact_file,
    endbr_transact_file,
    write_transact_file,
    process_enter_key,
    clear_current_screen,
    return_to_prev_screen,
    main,
    TranFileHandler,
    WS_PGMNAME,
    WS_TRANID,
    DFHENTER,
    DFHPF3,
    DFHPF4,
    COSGN00C,
    COMEN01C,
    DFHRESP_NORMAL,
    DFHRESP_NOTFND,
    DFHRESP_ENDFILE
)
from COACTUPC import (
    AccountRecord,
    CardXrefRecord,
    format_account_record,
    format_customer_record,
    AcctFileHandler,
    XrefFileHandler
)


# ============================================================================
# Test TranRecord
# ============================================================================

class TestTranRecord:
    """Test TranRecord dataclass."""
    
    def test_creation(self):
        """Test creating a transaction record."""
        record = TranRecord()
        assert record.tran_id == ""
        assert record.tran_amt == Decimal('0')
    
    def test_format_tran_record(self):
        """Test formatting transaction record."""
        record = TranRecord(
            tran_id="0000000000000001",
            tran_type_cd="02",
            tran_cat_cd="0002",
            tran_source="POS TERM",
            tran_desc="BILL PAYMENT - ONLINE",
            tran_amt=Decimal('1000.50'),
            tran_card_num="1234567890123456",
            tran_merchant_id="999999999",
            tran_merchant_name="BILL PAYMENT",
            tran_merchant_city="N/A",
            tran_merchant_zip="N/A",
            tran_orig_ts="2024-01-15-12.00.00.000000",
            tran_proc_ts="2024-01-15-12.00.00.000000"
        )
        formatted = format_tran_record(record)
        assert len(formatted) >= 16
        assert record.tran_id in formatted
    
    def test_parse_tran_record(self):
        """Test parsing transaction record."""
        line = "0000000000000001" + "02" + "0002" + "POS TERM" + " " * 20 + "BILL PAYMENT - ONLINE" + " " * 10
        line += f"{Decimal('1000.50'):>12.2f}" + "1234567890123456" + "999999999" + "BILL PAYMENT" + " " * 20
        line += "N/A" + " " * 27 + "N/A" + " " * 27
        line += "2024-01-15-12.00.00.000000" + "2024-01-15-12.00.00.000000"
        line = line.ljust(300)
        
        record = parse_tran_record(line)
        assert record.tran_id == "0000000000000001"
        assert record.tran_type_cd == "02"


# ============================================================================
# Test TranFileHandler
# ============================================================================

class TestTranFileHandler:
    """Test TranFileHandler."""
    
    def test_startbr_readprev_endbr(self):
        """Test browse operations."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            # Write some transaction records
            for i in range(1, 5):
                tran_id = f"{i:016d}"
                record = TranRecord(tran_id=tran_id, tran_type_cd="01")
                f.write(format_tran_record(record) + "\n")
            temp_file = f.name
        
        try:
            handler = TranFileHandler(temp_file)
            handler.open_file()
            
            # Start browse with high value
            high_value = "9999999999999999"
            resp_cd, reas_cd = handler.startbr(high_value)
            assert resp_cd == 0
            
            # Read previous (should get highest key)
            record_line, resp_cd = handler.readprev()
            assert resp_cd == DFHRESP_NORMAL
            assert record_line is not None
            
            # End browse
            handler.endbr()
            assert handler._browse_active is False
        finally:
            os.unlink(temp_file)
    
    def test_write_record(self):
        """Test writing transaction record."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            temp_file = f.name
        
        try:
            handler = TranFileHandler(temp_file)
            handler.open_file()
            
            record = TranRecord(tran_id="0000000000000001")
            record_line = format_tran_record(record)
            resp_cd, reas_cd = handler.write_record("0000000000000001", record_line)
            assert resp_cd == 0
            
            # Try duplicate - should fail
            resp_cd2, reas_cd2 = handler.write_record("0000000000000001", record_line)
            assert resp_cd2 == 15  # DUPKEY
        finally:
            os.unlink(temp_file)


# ============================================================================
# Test File Operations
# ============================================================================

class TestFileOperations:
    """Test file operations."""
    
    def test_read_acctdat_file_success(self):
        """Test successful read of account file."""
        ws = WorkingStorage()
        acct_record = AccountRecord(
            acct_id="12345678901",
            active_status="Y",
            curr_bal=Decimal('1000.50')
        )
        acct_line = format_account_record(acct_record)
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            f.write(acct_line + "\n")
            temp_file = f.name
        
        try:
            ws.acct_handler = AcctFileHandler(temp_file)
            ws.acct_handler.open_file()
            
            result = read_acctdat_file(ws, "12345678901")
            assert result is True
            assert ws.acct_record is not None
            assert ws.acct_record.acct_id == "12345678901"
        finally:
            os.unlink(temp_file)
    
    def test_read_acctdat_file_not_found(self):
        """Test read when account not found."""
        ws = WorkingStorage()
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            temp_file = f.name
        
        try:
            ws.acct_handler = AcctFileHandler(temp_file)
            ws.acct_handler.open_file()
            
            result = read_acctdat_file(ws, "99999999999")
            assert result is False
            assert ws.err_flg is True
            assert "NOT found" in ws.message
        finally:
            os.unlink(temp_file)
    
    def test_read_cxacaix_file_success(self):
        """Test successful read of xref file."""
        ws = WorkingStorage()
        xref_line = "1234567890123456" + "123456789" + "12345678901" + " " * 20
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            f.write(xref_line + "\n")
            temp_file = f.name
        
        try:
            ws.xref_handler = XrefFileHandler(temp_file)
            ws.xref_handler.open_file()
            
            result = read_cxacaix_file(ws, "12345678901")
            assert result is True
            assert ws.xref_record is not None
            assert ws.xref_record.acct_id == "12345678901"
        finally:
            os.unlink(temp_file)
    
    def test_write_transact_file_success(self):
        """Test successful write of transaction file."""
        ws = WorkingStorage()
        tran_record = TranRecord(
            tran_id="0000000000000001",
            tran_type_cd="02",
            tran_amt=Decimal('1000.50')
        )
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            temp_file = f.name
        
        try:
            ws.tran_handler = TranFileHandler(temp_file)
            ws.tran_handler.open_file()
            
            result = write_transact_file(ws, tran_record)
            assert result is True
            assert ws.resp_cd == 0
        finally:
            os.unlink(temp_file)


# ============================================================================
# Test Process Enter Key
# ============================================================================

class TestProcessEnterKey:
    """Test process_enter_key function."""
    
    def test_empty_account_id(self):
        """Test processing with empty account ID."""
        ws = WorkingStorage()
        map_in = BillPayMap()
        map_in.actidini = ""
        map_out = BillPayMap()
        commarea = CardDemoCommarea()
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            temp_file = f.name
        
        try:
            result = process_enter_key(ws, map_in, map_out, commarea, temp_file, temp_file, temp_file)
            assert result is False
            assert ws.err_flg is True
            assert "empty" in ws.message.lower()
        finally:
            os.unlink(temp_file)
    
    def test_confirm_y_payment_success(self):
        """Test successful payment with Y confirmation."""
        ws = WorkingStorage()
        map_in = BillPayMap()
        map_in.actidini = "12345678901"
        map_in.confirmi = "Y"
        map_out = BillPayMap()
        commarea = CardDemoCommarea()
        
        # Create test files
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_acct:
            acct_record = AccountRecord(
                acct_id="12345678901",
                active_status="Y",
                curr_bal=Decimal('1000.50')
            )
            f_acct.write(format_account_record(acct_record) + "\n")
            acct_file = f_acct.name
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_xref:
            xref_line = "1234567890123456" + "123456789" + "12345678901" + " " * 20
            f_xref.write(xref_line + "\n")
            xref_file = f_xref.name
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_tran:
            # Write one existing transaction
            existing_tran = TranRecord(tran_id="0000000000000001", tran_type_cd="01")
            f_tran.write(format_tran_record(existing_tran) + "\n")
            tran_file = f_tran.name
        
        try:
            # Initialize handlers
            ws.acct_handler = AcctFileHandler(acct_file)
            ws.xref_handler = XrefFileHandler(xref_file)
            ws.tran_handler = TranFileHandler(tran_file)
            ws.acct_handler.open_file()
            ws.xref_handler.open_file()
            ws.tran_handler.open_file()
            
            result = process_enter_key(ws, map_in, map_out, commarea, acct_file, xref_file, tran_file)
            assert result is True
            assert "successful" in ws.message.lower()
            assert ws.acct_record.curr_bal == Decimal('0.00')  # Balance should be zero after payment
        finally:
            os.unlink(acct_file)
            os.unlink(xref_file)
            os.unlink(tran_file)
    
    def test_zero_balance(self):
        """Test payment with zero balance."""
        ws = WorkingStorage()
        map_in = BillPayMap()
        map_in.actidini = "12345678901"
        map_in.confirmi = "Y"
        map_out = BillPayMap()
        commarea = CardDemoCommarea()
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_acct:
            acct_record = AccountRecord(
                acct_id="12345678901",
                active_status="Y",
                curr_bal=Decimal('0.00')
            )
            f_acct.write(format_account_record(acct_record) + "\n")
            acct_file = f_acct.name
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            temp_file = f.name
        
        try:
            # Initialize handlers
            ws.acct_handler = AcctFileHandler(acct_file)
            ws.xref_handler = XrefFileHandler(temp_file)
            ws.tran_handler = TranFileHandler(temp_file)
            ws.acct_handler.open_file()
            ws.xref_handler.open_file()
            ws.tran_handler.open_file()
            
            result = process_enter_key(ws, map_in, map_out, commarea, acct_file, temp_file, temp_file)
            assert result is False
            assert ws.err_flg is True
            assert "nothing to pay" in ws.message.lower()
        finally:
            os.unlink(acct_file)
            os.unlink(temp_file)
    
    def test_invalid_confirm(self):
        """Test with invalid confirmation value."""
        ws = WorkingStorage()
        map_in = BillPayMap()
        map_in.actidini = "12345678901"
        map_in.confirmi = "X"
        map_out = BillPayMap()
        commarea = CardDemoCommarea()
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_acct:
            acct_record = AccountRecord(
                acct_id="12345678901",
                active_status="Y",
                curr_bal=Decimal('1000.50')
            )
            f_acct.write(format_account_record(acct_record) + "\n")
            acct_file = f_acct.name
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            temp_file = f.name
        
        try:
            result = process_enter_key(ws, map_in, map_out, commarea, acct_file, temp_file, temp_file)
            assert result is False
            assert ws.err_flg is True
            assert "Invalid value" in ws.message
        finally:
            os.unlink(acct_file)
            os.unlink(temp_file)


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_eibcalen_zero(self):
        """Test main with eibcalen=0."""
        commarea = CardDemoCommarea()
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=0
        )
        
        assert xctl_prog == COSGN00C
    
    def test_main_first_entry(self):
        """Test main on first entry."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100
        )
        
        assert result_commarea.pgm_reenter is True
        assert result_map.title01o != ""
        assert xctl_prog is None
    
    def test_main_pf3_exit(self):
        """Test main with PF3."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        commarea.from_program = ""
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF3
        )
        
        assert xctl_prog == COMEN01C
    
    def test_main_pf4_clear(self):
        """Test main with PF4 (clear)."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = BillPayMap()
        map_in.actidini = "12345678901"
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF4,
            map_input=map_in
        )
        
        assert xctl_prog is None


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_payment_flow(self):
        """Test complete payment flow."""
        # Create test files
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_acct:
            acct_record = AccountRecord(
                acct_id="12345678901",
                active_status="Y",
                curr_bal=Decimal('1500.75')
            )
            f_acct.write(format_account_record(acct_record) + "\n")
            acct_file = f_acct.name
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_xref:
            xref_line = "1234567890123456" + "123456789" + "12345678901" + " " * 20
            f_xref.write(xref_line + "\n")
            xref_file = f_xref.name
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_tran:
            tran_file = f_tran.name
        
        try:
            # First entry
            commarea = CardDemoCommarea()
            commarea.pgm_reenter = False
            
            result_commarea, result_map, xctl_prog = main(
                commarea=commarea,
                eibcalen=100,
                acct_file=acct_file,
                xref_file=xref_file,
                tran_file=tran_file
            )
            assert result_commarea.pgm_reenter is True
            
            # Process payment
            commarea2 = CardDemoCommarea()
            commarea2.pgm_reenter = True
            map_in = BillPayMap()
            map_in.actidini = "12345678901"
            map_in.confirmi = "Y"
            
            result_commarea2, result_map2, xctl_prog2 = main(
                commarea=commarea2,
                eibcalen=100,
                eibaid=DFHENTER,
                map_input=map_in,
                acct_file=acct_file,
                xref_file=xref_file,
                tran_file=tran_file
            )
            
            assert "successful" in result_map2.errmsgo.lower()
            assert result_map2.errmsgc == "DFHGREEN"
        finally:
            os.unlink(acct_file)
            os.unlink(xref_file)
            os.unlink(tran_file)

