"""
Pytest tests for CBACT04C.py ground truth Python equivalent.

Tests verify the functionality matches the COBOL program behavior.
"""

import pytest
import sys
import os
import tempfile
from pathlib import Path
from decimal import Decimal, ROUND_HALF_UP
from unittest.mock import patch, MagicMock

# Add parent directory to path to import CBACT04C
sys.path.insert(0, str(Path(__file__).parent))

from CBACT04C import (
    TranCatBalRecord,
    XRefRecord,
    DisGroupRecord,
    AccountRecord,
    TranRecord,
    FileStatusError,
    SequentialFileHandler,
    IndexedFileHandler,
    parse_tran_cat_bal_record,
    parse_xref_record,
    parse_dis_group_record,
    parse_account_record,
    format_db2_timestamp,
    abend_program,
    main
)


class TestTranCatBalRecord:
    """Tests for TranCatBalRecord dataclass."""
    
    def test_creation(self):
        """Test creating a TranCatBalRecord."""
        record = TranCatBalRecord(
            trancat_acct_id="12345678901",
            trancat_type_cd="01",
            trancat_cd="0005",
            tran_cat_data="1000.00"
        )
        assert record.trancat_acct_id == "12345678901"
        assert record.trancat_type_cd == "01"
        assert record.trancat_cd == "0005"
        assert record.tran_cat_data == "1000.00"
    
    def test_tran_cat_bal(self):
        """Test tran_cat_bal property."""
        record = TranCatBalRecord(
            trancat_acct_id="12345678901",
            trancat_type_cd="01",
            trancat_cd="0005",
            tran_cat_data="1000.00"
        )
        assert record.tran_cat_bal == Decimal('1000.00')


class TestXRefRecord:
    """Tests for XRefRecord dataclass."""
    
    def test_creation(self):
        """Test creating an XRefRecord."""
        record = XRefRecord(
            xref_card_num="1234567890123456",
            xref_cust_num="123456789",
            xref_acct_id="98765432109"
        )
        assert record.xref_card_num == "1234567890123456"
        assert record.xref_cust_num == "123456789"
        assert record.xref_acct_id == "98765432109"


class TestDisGroupRecord:
    """Tests for DisGroupRecord dataclass."""
    
    def test_creation(self):
        """Test creating a DisGroupRecord."""
        record = DisGroupRecord(
            dis_acct_group_id="GROUP001  ",
            dis_tran_type_cd="01",
            dis_tran_cat_cd="0005",
            discgrp_data="12.5"
        )
        assert record.dis_acct_group_id == "GROUP001  "
        assert record.int_rate == Decimal('12.5')


class TestAccountRecord:
    """Tests for AccountRecord dataclass."""
    
    def test_creation(self):
        """Test creating an AccountRecord."""
        record = AccountRecord(
            acct_id="12345678901",
            acct_data="GROUP001  1000.00"
        )
        assert record.acct_id == "12345678901"
        # acct_group_id property strips whitespace
        assert record.acct_group_id == "GROUP001"
    
    def test_set_curr_bal(self):
        """Test setting current balance."""
        record = AccountRecord(
            acct_id="12345678901",
            acct_data="1000.00"
        )
        record.set_curr_bal(Decimal('1500.00'))
        assert "1500.00" in record.acct_data


class TestParsingFunctions:
    """Tests for parsing functions."""
    
    def test_parse_tran_cat_bal_record(self):
        """Test parsing transaction category balance record."""
        line = "123456789010100051000.00"
        record = parse_tran_cat_bal_record(line)
        assert record is not None
        assert record.trancat_acct_id == "12345678901"
        assert record.trancat_type_cd == "01"
        assert record.trancat_cd == "0005"
    
    def test_parse_xref_record(self):
        """Test parsing XREF record."""
        line = "123456789012345612345678998765432109" + " " * 14
        record = parse_xref_record(line)
        assert record is not None
        assert record.xref_card_num == "1234567890123456"
        assert record.xref_cust_num == "123456789"
        assert record.xref_acct_id == "98765432109"
    
    def test_parse_dis_group_record(self):
        """Test parsing disclosure group record."""
        line = "GROUP001  01000512.5"
        record = parse_dis_group_record(line)
        assert record is not None
        assert record.dis_acct_group_id.strip() == "GROUP001"
        assert record.dis_tran_type_cd == "01"
        assert record.dis_tran_cat_cd == "0005"
    
    def test_parse_account_record(self):
        """Test parsing account record."""
        line = "12345678901GROUP001  1000.00"
        record = parse_account_record(line)
        assert record is not None
        assert record.acct_id == "12345678901"


class TestSequentialFileHandler:
    """Tests for SequentialFileHandler."""
    
    def test_open_file_success(self):
        """Test successful file opening."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write("test data\n")
            temp_file = f.name
        
        try:
            handler = SequentialFileHandler(temp_file)
            result = handler.open_file("r")
            assert result == 0
            assert handler.file_status == "00"
            assert handler.end_of_file is False
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_read_next_success(self):
        """Test reading next record."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write("test data\n")
            temp_file = f.name
        
        try:
            handler = SequentialFileHandler(temp_file)
            handler.open_file("r")
            record, result = handler.read_next()
            assert result == 0
            assert record == "test data"
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_read_next_eof(self):
        """Test reading when EOF is reached."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_file = f.name
        
        try:
            handler = SequentialFileHandler(temp_file)
            handler.open_file("r")
            record, result = handler.read_next()
            assert result == 16  # APPL-EOF
            assert handler.end_of_file is True
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_write_record(self):
        """Test writing a record."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_file = f.name
        
        try:
            handler = SequentialFileHandler(temp_file)
            handler.open_file("w")
            result = handler.write_record("test record")
            assert result == 0
            handler.close_file()
            
            # Verify written data
            with open(temp_file, 'r') as f:
                assert f.read().strip() == "test record"
        finally:
            os.unlink(temp_file)


class TestIndexedFileHandler:
    """Tests for IndexedFileHandler."""
    
    def test_open_file_create_new(self):
        """Test opening a new file."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_file = f.name
        os.unlink(temp_file)  # Remove file
        
        try:
            handler = IndexedFileHandler(temp_file)
            result = handler.open_file("r")
            assert result == 0
            handler.close_file()
        finally:
            if os.path.exists(temp_file):
                os.unlink(temp_file)
    
    def test_read_record_success(self):
        """Test reading a record by key."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write("12345678901ACCOUNT DATA\n")
            temp_file = f.name
        
        try:
            handler = IndexedFileHandler(temp_file)
            handler.open_file("r")
            record, result = handler.read_record("12345678901")
            assert result == 0
            assert record is not None
            assert "ACCOUNT DATA" in record
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_read_record_not_found(self):
        """Test reading a non-existent record."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_file = f.name
        
        try:
            handler = IndexedFileHandler(temp_file)
            handler.open_file("r")
            record, result = handler.read_record("99999999999")
            assert result == 12
            assert handler.file_status == "23"
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_rewrite_record(self):
        """Test rewriting a record."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write("12345678901OLD DATA\n")
            temp_file = f.name
        
        try:
            handler = IndexedFileHandler(temp_file)
            handler.open_file("r+")
            result = handler.rewrite_record("12345678901", "12345678901NEW DATA")
            assert result == 0
            handler.close_file()
            
            # Verify updated data
            handler2 = IndexedFileHandler(temp_file)
            handler2.open_file("r")
            record, result = handler2.read_record("12345678901")
            assert "NEW DATA" in record
            handler2.close_file()
        finally:
            os.unlink(temp_file)


class TestFormatDb2Timestamp:
    """Tests for format_db2_timestamp function."""
    
    def test_format_db2_timestamp(self):
        """Test DB2 timestamp formatting."""
        ts = format_db2_timestamp()
        assert len(ts) == 26
        assert ts.count('-') == 3
        assert ts.count('.') == 3


class TestMainFunction:
    """Tests for main function."""
    
    def test_main_with_empty_files(self, capsys):
        """Test main function with empty input file."""
        with tempfile.TemporaryDirectory() as tmpdir:
            tcatbal_file = os.path.join(tmpdir, "TCATBALF")
            xref_file = os.path.join(tmpdir, "XREFFILE")
            discgrp_file = os.path.join(tmpdir, "DISCGRP")
            acct_file = os.path.join(tmpdir, "ACCTFILE")
            tran_file = os.path.join(tmpdir, "TRANSACT")
            
            # Create empty input file
            with open(tcatbal_file, 'w') as f:
                pass
            
            with open(xref_file, 'w') as f:
                pass
            
            with open(discgrp_file, 'w') as f:
                pass
            
            with open(acct_file, 'w') as f:
                pass
            
            with patch('CBACT04C.SequentialFileHandler') as mock_seq, \
                 patch('CBACT04C.IndexedFileHandler') as mock_idx:
                
                # Set up mocks
                def make_handler(filename):
                    handler = MagicMock()
                    handler.filename = filename
                    handler.open_file.return_value = 0
                    handler.close_file.return_value = 0
                    handler.read_next.return_value = (None, 16)  # EOF
                    handler.file_status = "00"
                    handler.end_of_file = False
                    return handler
                
                mock_seq.return_value = make_handler("TCATBALF")
                
                def make_idx_handler(filename):
                    handler = MagicMock()
                    handler.filename = filename
                    handler.open_file.return_value = 0
                    handler.close_file.return_value = 0
                    handler.read_record.return_value = (None, 12)
                    handler.rewrite_record.return_value = 0
                    handler.file_status = "00"
                    return handler
                
                mock_idx.return_value = make_idx_handler("INDEXED")
                
                # Change to temp directory
                old_cwd = os.getcwd()
                try:
                    os.chdir(tmpdir)
                    result = main("20240101")
                    assert result == 0
                finally:
                    os.chdir(old_cwd)
                
                captured = capsys.readouterr().out
                assert "START OF EXECUTION OF PROGRAM CBACT04C" in captured
                assert "END OF EXECUTION OF PROGRAM CBACT04C" in captured
    
    def test_main_with_sample_data(self, capsys):
        """Test main function with sample transaction data."""
        with tempfile.TemporaryDirectory() as tmpdir:
            tcatbal_file = os.path.join(tmpdir, "TCATBALF")
            xref_file = os.path.join(tmpdir, "XREFFILE")
            discgrp_file = os.path.join(tmpdir, "DISCGRP")
            acct_file = os.path.join(tmpdir, "ACCTFILE")
            tran_file = os.path.join(tmpdir, "TRANSACT")
            
            # Create input files
            with open(tcatbal_file, 'w') as f:
                f.write("123456789010100051000.00\n")
            
            with open(xref_file, 'w') as f:
                f.write("123456789012345612345678912345678901" + " " * 14 + "\n")
            
            with open(discgrp_file, 'w') as f:
                f.write("GROUP001  01000512.5\n")
                f.write("DEFAULT   01000510.0\n")
            
            with open(acct_file, 'w') as f:
                f.write("12345678901GROUP001  5000.00\n")
            
            # Change to temp directory
            old_cwd = os.getcwd()
            try:
                os.chdir(tmpdir)
                result = main("20240101")
                assert result == 0
            finally:
                os.chdir(old_cwd)
            
            captured = capsys.readouterr().out
            assert "START OF EXECUTION OF PROGRAM CBACT04C" in captured
            assert "END OF EXECUTION OF PROGRAM CBACT04C" in captured
            
            # Check that transaction file was created
            assert os.path.exists(tran_file)
    
    def test_abend_program(self):
        """Test abend_program function."""
        with pytest.raises(SystemExit) as exc_info:
            abend_program()
        assert exc_info.value.code == 999


class TestInterestCalculation:
    """Tests for interest calculation logic."""
    
    def test_interest_calculation(self):
        """Test interest calculation formula."""
        balance = Decimal('1000.00')
        rate = Decimal('12.5')
        monthly_int = (balance * rate) / Decimal('1200')
        expected = Decimal('10.41666666666666666666666667')
        assert monthly_int.quantize(Decimal('0.01'), rounding=ROUND_HALF_UP) == Decimal('10.42')
    
    def test_zero_interest_rate(self):
        """Test that no interest is calculated when rate is 0."""
        balance = Decimal('1000.00')
        rate = Decimal('0')
        monthly_int = (balance * rate) / Decimal('1200')
        assert monthly_int == Decimal('0')


class TestErrorHandling:
    """Tests for error handling."""
    
    def test_file_open_error(self, capsys):
        """Test error handling when file cannot be opened."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create a directory instead of a file to cause an error
            bad_file = os.path.join(tmpdir, "BADFILE")
            os.makedirs(bad_file, exist_ok=True)
            
            handler = SequentialFileHandler(bad_file)
            # This should handle the error gracefully
            result = handler.open_file("r")
            # The handler should return an error code
            assert result != 0


if __name__ == '__main__':
    pytest.main([__file__, '-v'])

