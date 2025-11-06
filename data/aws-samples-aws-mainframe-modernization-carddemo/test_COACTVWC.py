"""
Test suite for COACTVWC.py

Tests for Account View transaction program.
"""

import pytest
import tempfile
import os
from decimal import Decimal
from unittest.mock import patch, MagicMock

from COACTVWC import (
    CardDemoCommarea,
    WorkingStorage,
    read_cardxref_by_acct,
    read_acctdata_by_acct,
    read_custdata_by_cust,
    read_account,
    edit_account_input,
    process_inputs,
    setup_screen_vars,
    setup_screen_attrs,
    send_map,
    main,
    abend_program,
    LIT_THISPGM,
    LIT_THISTRANID,
    LIT_MENUPGM,
    DFHRESP_NORMAL,
    DFHRESP_NOTFND
)
from COACTUPC import (
    CardXrefRecord,
    AccountRecord,
    CustomerRecord,
    XrefFileHandler,
    AcctFileHandler,
    CustFileHandler,
    parse_xref_record,
    parse_account_record,
    parse_customer_record,
    format_account_record,
    format_customer_record
)


# ============================================================================
# Test CardDemoCommarea
# ============================================================================

class TestCardDemoCommarea:
    """Test CardDemoCommarea dataclass."""
    
    def test_creation(self):
        """Test creating a commarea."""
        commarea = CardDemoCommarea()
        assert commarea.from_program == ""
        assert commarea.acct_id == ""
        assert commarea.pgm_enter is False
    
    def test_assignment(self):
        """Test assigning values to commarea."""
        commarea = CardDemoCommarea()
        commarea.acct_id = "12345678901"
        commarea.pgm_enter = True
        assert commarea.acct_id == "12345678901"
        assert commarea.pgm_enter is True


# ============================================================================
# Test WorkingStorage
# ============================================================================

class TestWorkingStorage:
    """Test WorkingStorage class."""
    
    def test_creation(self):
        """Test creating working storage."""
        ws = WorkingStorage()
        assert ws.input_ok is True
        assert ws.input_error is False
        assert ws.tranid == "CAVW"
    
    def test_flags(self):
        """Test flag operations."""
        ws = WorkingStorage()
        ws.input_error = True
        ws.acctfilter_not_ok = True
        assert ws.input_error is True
        assert ws.acctfilter_not_ok is True


# ============================================================================
# Test File Operations
# ============================================================================

class TestReadCardxrefByAcct:
    """Test read_cardxref_by_acct function."""
    
    def test_read_success(self):
        """Test successful read by account ID."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        
        # Create temp file
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            # Write a record: card_num(16) + cust_id(9) + acct_id(11)
            xref_line = "1234567890123456" + "123456789" + "12345678901" + " " * 20
            f.write(xref_line + "\n")
            temp_file = f.name
        
        try:
            ws.xref_handler = XrefFileHandler(temp_file)
            ws.xref_handler.open_file()
            ws.card_rid_acct_id_x = "12345678901"
            
            result = read_cardxref_by_acct(ws, commarea)
            assert result is True
            assert ws.resp_cd == DFHRESP_NORMAL
            assert commarea.cust_id == "123456789"
            assert commarea.card_num == "1234567890123456"
        finally:
            os.unlink(temp_file)
    
    def test_read_not_found(self):
        """Test read when account not found."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            f.write("1234567890123456" + "123456789" + "12345678901" + " " * 20 + "\n")
            temp_file = f.name
        
        try:
            ws.xref_handler = XrefFileHandler(temp_file)
            ws.xref_handler.open_file()
            ws.card_rid_acct_id_x = "99999999999"  # Non-existent account
            
            result = read_cardxref_by_acct(ws, commarea)
            assert result is False
            assert ws.input_error is True
            assert ws.acctfilter_not_ok is True
            assert "not found" in ws.return_msg
        finally:
            os.unlink(temp_file)


class TestReadAcctdataByAcct:
    """Test read_acctdata_by_acct function."""
    
    def test_read_success(self):
        """Test successful read of account data."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        
        acct_record = AccountRecord(
            acct_id="12345678901",
            active_status="Y",
            curr_bal=Decimal('1000.50'),
            credit_limit=Decimal('5000.00')
        )
        acct_line = format_account_record(acct_record)
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            f.write(acct_line + "\n")
            temp_file = f.name
        
        try:
            ws.acct_handler = AcctFileHandler(temp_file)
            ws.acct_handler.open_file()
            ws.card_rid_acct_id_x = "12345678901"
            
            result = read_acctdata_by_acct(ws, commarea)
            assert result is True
            assert ws.resp_cd == DFHRESP_NORMAL
            assert ws.found_acct_in_master is True
            assert ws.acct_record is not None
            assert ws.acct_record.acct_id == "12345678901"
        finally:
            os.unlink(temp_file)
    
    def test_read_not_found(self):
        """Test read when account not found."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            acct_line = format_account_record(
                AccountRecord(acct_id="12345678901")
            )
            f.write(acct_line + "\n")
            temp_file = f.name
        
        try:
            ws.acct_handler = AcctFileHandler(temp_file)
            ws.acct_handler.open_file()
            ws.card_rid_acct_id_x = "99999999999"
            
            result = read_acctdata_by_acct(ws, commarea)
            assert result is False
            assert ws.input_error is True
            assert "not found" in ws.return_msg
        finally:
            os.unlink(temp_file)


class TestReadCustdataByCust:
    """Test read_custdata_by_cust function."""
    
    def test_read_success(self):
        """Test successful read of customer data."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        
        cust_record = CustomerRecord(
            cust_id="123456789",
            first_name="John",
            last_name="Doe"
        )
        cust_line = format_customer_record(cust_record)
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            f.write(cust_line + "\n")
            temp_file = f.name
        
        try:
            ws.cust_handler = CustFileHandler(temp_file)
            ws.cust_handler.open_file()
            ws.card_rid_cust_id_x = "123456789"
            
            result = read_custdata_by_cust(ws, commarea)
            assert result is True
            assert ws.resp_cd == DFHRESP_NORMAL
            assert ws.found_cust_in_master is True
            assert ws.cust_record is not None
            assert ws.cust_record.cust_id == "123456789"
        finally:
            os.unlink(temp_file)


# ============================================================================
# Test Account Validation
# ============================================================================

class TestEditAccountInput:
    """Test edit_account_input function."""
    
    def test_valid_account_id(self):
        """Test valid account ID."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        commarea.acctsidi = "12345678901"
        
        edit_account_input(ws, commarea)
        assert ws.input_error is False
        assert ws.acctfilter_isvalid is True
        assert commarea.acct_id == "12345678901"
    
    def test_blank_account_id(self):
        """Test blank account ID."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        commarea.acctsidi = ""
        
        edit_account_input(ws, commarea)
        assert ws.input_error is True
        assert ws.acctfilter_blank is True
        assert "not provided" in ws.return_msg
    
    def test_star_account_id(self):
        """Test * as account ID."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        commarea.acctsidi = "*"
        
        edit_account_input(ws, commarea)
        assert ws.input_error is True
        assert ws.acctfilter_blank is True
    
    def test_non_numeric_account_id(self):
        """Test non-numeric account ID."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        commarea.acctsidi = "ABC12345678"
        
        edit_account_input(ws, commarea)
        assert ws.input_error is True
        assert ws.acctfilter_not_ok is True
        assert "non-zero 11 digit number" in ws.return_msg
    
    def test_short_account_id(self):
        """Test account ID that's too short."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        commarea.acctsidi = "123456789"
        
        edit_account_input(ws, commarea)
        assert ws.input_error is True
        assert ws.acctfilter_not_ok is True
    
    def test_zero_account_id(self):
        """Test account ID that's all zeros."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        commarea.acctsidi = "00000000000"
        
        edit_account_input(ws, commarea)
        assert ws.input_error is True
        assert ws.acctfilter_not_ok is True


# ============================================================================
# Test Process Inputs
# ============================================================================

class TestProcessInputs:
    """Test process_inputs function."""
    
    def test_valid_input(self):
        """Test processing valid input."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        commarea.acctsidi = "12345678901"
        
        process_inputs(ws, commarea)
        assert ws.input_ok is True
        assert commarea.acct_id == "12345678901"
        assert commarea.next_prog == LIT_THISPGM
    
    def test_blank_input(self):
        """Test processing blank input."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        commarea.acctsidi = ""
        
        process_inputs(ws, commarea)
        assert ws.input_error is True
        assert ws.return_msg == "No input received"


# ============================================================================
# Test Read Account Flow
# ============================================================================

class TestReadAccount:
    """Test read_account function."""
    
    def test_read_account_success(self):
        """Test successful read of account."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        commarea.acct_id = "12345678901"
        
        # Create temp files
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_xref:
            xref_line = "1234567890123456" + "123456789" + "12345678901" + " " * 20
            f_xref.write(xref_line + "\n")
            xref_file = f_xref.name
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_acct:
            acct_line = format_account_record(
                AccountRecord(acct_id="12345678901", active_status="Y")
            )
            f_acct.write(acct_line + "\n")
            acct_file = f_acct.name
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_cust:
            cust_line = format_customer_record(
                CustomerRecord(cust_id="123456789", first_name="John")
            )
            f_cust.write(cust_line + "\n")
            cust_file = f_cust.name
        
        try:
            ws.xref_handler = XrefFileHandler(xref_file)
            ws.acct_handler = AcctFileHandler(acct_file)
            ws.cust_handler = CustFileHandler(cust_file)
            
            ws.xref_handler.open_file()
            ws.acct_handler.open_file()
            ws.cust_handler.open_file()
            
            read_account(ws, commarea)
            
            assert ws.found_acct_in_master is True
            assert ws.found_cust_in_master is True
            assert commarea.cust_id == "123456789"
            assert ws.info_msg == "Displaying details of given Account"
        finally:
            os.unlink(xref_file)
            os.unlink(acct_file)
            os.unlink(cust_file)
    
    def test_read_account_xref_not_found(self):
        """Test read account when xref not found."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        commarea.acct_id = "99999999999"
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            f.write("1234567890123456" + "123456789" + "12345678901" + " " * 20 + "\n")
            temp_file = f.name
        
        try:
            ws.xref_handler = XrefFileHandler(temp_file)
            ws.xref_handler.open_file()
            ws.acct_handler = AcctFileHandler("nonexistent.dat")
            ws.cust_handler = CustFileHandler("nonexistent.dat")
            
            read_account(ws, commarea)
            
            assert ws.input_error is True
            assert ws.found_acct_in_master is False
        finally:
            os.unlink(temp_file)


# ============================================================================
# Test Screen Setup
# ============================================================================

class TestSetupScreenVars:
    """Test setup_screen_vars function."""
    
    def test_setup_screen_vars_with_account(self):
        """Test setting up screen variables with account data."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        
        ws.acct_record = AccountRecord(
            acct_id="12345678901",
            active_status="Y",
            curr_bal=Decimal('1000.50'),
            credit_limit=Decimal('5000.00'),
            open_date="2020-01-15"
        )
        ws.found_acct_in_master = True
        
        ws.cust_record = CustomerRecord(
            cust_id="123456789",
            first_name="John",
            last_name="Doe",
            ssn="123456789",
            fico_credit_score=750
        )
        ws.found_cust_in_master = True
        
        commarea.acct_id = "12345678901"
        
        setup_screen_vars(ws, commarea)
        
        assert commarea.acstsido == "Y"
        assert "1000.50" in commarea.acurbalo
        assert commarea.acsfnamo == "John"
        assert commarea.acslnamo == "Doe"
        assert "123-45-6789" in commarea.acstssno
    
    def test_setup_screen_vars_blank(self):
        """Test setting up screen variables with blank account."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        ws.acctfilter_blank = True
        
        setup_screen_vars(ws, commarea)
        
        assert commarea.acctsido == ""
        assert ws.info_msg == "Enter or update id of account to display"


class TestSetupScreenAttrs:
    """Test setup_screen_attrs function."""
    
    def test_setup_screen_attrs_normal(self):
        """Test setting up screen attributes normally."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        
        setup_screen_attrs(ws, commarea)
        
        assert commarea.acctsida == "DFHBMFSE"
        assert commarea.acctsidc == "DFHDFCOL"
    
    def test_setup_screen_attrs_error(self):
        """Test setting up screen attributes with error."""
        ws = WorkingStorage()
        commarea = CardDemoCommarea()
        ws.acctfilter_not_ok = True
        
        setup_screen_attrs(ws, commarea)
        
        assert commarea.acctsidc == "DFHRED"


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_pfk03_exit(self):
        """Test main function with PF3 (exit)."""
        commarea = CardDemoCommarea()
        commarea.aid_pfk03 = True
        commarea.from_program = ""
        
        result = main(commarea=commarea, eibcalen=0)
        
        assert result.to_program == LIT_MENUPGM
        assert result.to_tranid == "CM00"
    
    def test_main_pgm_enter(self):
        """Test main function with PG-ENTER (first entry)."""
        commarea = CardDemoCommarea()
        commarea.pgm_enter = True
        commarea.aid_enter = True
        
        result = main(commarea=commarea, eibcalen=0)
        
        assert result.pgm_reenter is True
        assert result.next_mapset == "COACTVW "
    
    def test_main_pgm_reenter_success(self):
        """Test main function with PG-REENTER and valid input."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        commarea.aid_enter = True
        commarea.acctsidi = "12345678901"
        
        # Create temp files
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_xref:
            xref_line = "1234567890123456" + "123456789" + "12345678901" + " " * 20
            f_xref.write(xref_line + "\n")
            xref_file = f_xref.name
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_acct:
            acct_line = format_account_record(
                AccountRecord(acct_id="12345678901", active_status="Y")
            )
            f_acct.write(acct_line + "\n")
            acct_file = f_acct.name
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_cust:
            cust_line = format_customer_record(
                CustomerRecord(cust_id="123456789", first_name="John")
            )
            f_cust.write(cust_line + "\n")
            cust_file = f_cust.name
        
        try:
            result = main(
                xref_file=xref_file,
                acct_file=acct_file,
                cust_file=cust_file,
                commarea=commarea,
                eibcalen=100
            )
            
            assert result.acct_id == "12345678901"
            assert result.pgm_reenter is True
        finally:
            os.unlink(xref_file)
            os.unlink(acct_file)
            os.unlink(cust_file)
    
    def test_main_pgm_reenter_invalid_input(self):
        """Test main function with invalid input."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        commarea.aid_enter = True
        commarea.acctsidi = "INVALID"
        
        # Create empty temp files
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f:
            temp_file = f.name
        
        try:
            result = main(
                xref_file=temp_file,
                acct_file=temp_file,
                cust_file=temp_file,
                commarea=commarea,
                eibcalen=100
            )
            
            assert result.error_msg != ""
        finally:
            os.unlink(temp_file)
    
    def test_main_first_call(self):
        """Test main function on first call (eibcalen=0)."""
        result = main(eibcalen=0)
        
        assert result.pgm_reenter is True
        assert result.next_mapset == "COACTVW "
    
    @patch('COACTVWC.abend_program')
    def test_main_unexpected_scenario(self, mock_abend):
        """Test main function with unexpected scenario."""
        commarea = CardDemoCommarea()
        commarea.pgm_enter = False
        commarea.pgm_reenter = False
        commarea.aid_pfk03 = False
        commarea.aid_enter = False
        
        mock_abend.side_effect = SystemExit(1)
        
        with pytest.raises(SystemExit):
            main(commarea=commarea, eibcalen=100)


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_success(self):
        """Test complete flow from input to display."""
        # Create test files
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_xref:
            xref_line = "1234567890123456" + "123456789" + "12345678901" + " " * 20
            f_xref.write(xref_line + "\n")
            xref_file = f_xref.name
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_acct:
            acct_line = format_account_record(
                AccountRecord(
                    acct_id="12345678901",
                    active_status="Y",
                    curr_bal=Decimal('1500.75'),
                    credit_limit=Decimal('5000.00'),
                    cash_credit_limit=Decimal('1000.00'),
                    open_date="2020-01-15",
                    expiration_date="2025-01-15",
                    group_id="GROUP001"
                )
            )
            f_acct.write(acct_line + "\n")
            acct_file = f_acct.name
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.dat') as f_cust:
            cust_line = format_customer_record(
                CustomerRecord(
                    cust_id="123456789",
                    first_name="John",
                    middle_name="M",
                    last_name="Doe",
                    addr_line_1="123 Main St",
                    addr_state_cd="NY",
                    addr_zip="10001",
                    ssn="123456789",
                    fico_credit_score=750,
                    dob_yyyy_mm_dd="1990-01-15"
                )
            )
            f_cust.write(cust_line + "\n")
            cust_file = f_cust.name
        
        try:
            # First entry - show screen
            commarea = CardDemoCommarea()
            commarea.pgm_enter = True
            commarea.aid_enter = True
            
            result1 = main(
                xref_file=xref_file,
                acct_file=acct_file,
                cust_file=cust_file,
                commarea=commarea,
                eibcalen=0
            )
            assert result1.pgm_reenter is True
            
            # Reenter with valid account ID
            commarea2 = CardDemoCommarea()
            commarea2.pgm_reenter = True
            commarea2.aid_enter = True
            commarea2.acctsidi = "12345678901"
            
            result2 = main(
                xref_file=xref_file,
                acct_file=acct_file,
                cust_file=cust_file,
                commarea=commarea2,
                eibcalen=100
            )
            
            assert result2.acct_id == "12345678901"
            assert result2.acstsido == "Y"
            assert "1500.75" in result2.acurbalo
            assert result2.acsfnamo == "John"
            assert result2.acslnamo == "Doe"
            assert "123-45-6789" in result2.acstssno
        finally:
            os.unlink(xref_file)
            os.unlink(acct_file)
            os.unlink(cust_file)

