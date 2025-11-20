"""
Pytest tests for CBTRN01C.py ground truth Python equivalent.

Tests verify the functionality matches the COBOL program behavior.
"""

import pytest
import sys
import os
import tempfile
from pathlib import Path
from unittest.mock import patch, MagicMock

# Add parent directory to path to import CBTRN01C
sys.path.insert(0, str(Path(__file__).parent))

from CBTRN01C import (
    DailyTranRecord,
    CardXrefRecord,
    AccountRecord,
    SequentialFileHandler,
    XrefFileHandler,
    CustFileHandler,
    CardFileHandler,
    AcctFileHandler,
    TranFileHandler,
    parse_daily_tran_record,
    parse_xref_record,
    parse_account_record,
    abend_program,
    main
)


class TestDailyTranRecord:
    """Tests for DailyTranRecord dataclass."""
    
    def test_creation(self):
        """Test creating a DailyTranRecord."""
        record = DailyTranRecord(
            tran_id="TRAN000000000001",
            tran_data="1234567890123456TRANSACTION DATA"
        )
        assert record.tran_id == "TRAN000000000001"
        assert record.dalytran_id == "TRAN000000000001"
        assert "1234567890123456" in record.dalytran_card_num


class TestCardXrefRecord:
    """Tests for CardXrefRecord dataclass."""
    
    def test_creation(self):
        """Test creating a CardXrefRecord."""
        record = CardXrefRecord(
            card_num="1234567890123456",
            cust_num="123456789",
            acct_id="98765432109"
        )
        assert record.xref_card_num == "1234567890123456"
        assert record.xref_cust_id == "123456789"
        assert record.xref_acct_id == "98765432109"


class TestAccountRecord:
    """Tests for AccountRecord dataclass."""
    
    def test_creation(self):
        """Test creating an AccountRecord."""
        record = AccountRecord(
            acct_id="12345678901",
            acct_data="ACCOUNT DATA"
        )
        assert record.acct_id == "12345678901"
        assert record.acct_id_prop == "12345678901"


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
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_read_next_success(self):
        """Test reading next record."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write("TRAN000000000001DATA\n")
            temp_file = f.name
        
        try:
            handler = SequentialFileHandler(temp_file)
            handler.open_file("r")
            record, result = handler.read_next()
            assert result == 0
            assert record == "TRAN000000000001DATA"
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


class TestIndexedFileHandlers:
    """Tests for indexed file handlers."""
    
    def test_xref_file_handler_read(self):
        """Test XrefFileHandler reading by key."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write("123456789012345612345678998765432109\n")
            temp_file = f.name
        
        try:
            handler = XrefFileHandler(temp_file)
            handler.open_file("r")
            record, result = handler.read_record("1234567890123456")
            assert result == 0
            assert record is not None
            assert "1234567890123456" in record
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_cust_file_handler_read(self):
        """Test CustFileHandler reading by key."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write("123456789CUSTOMER DATA\n")
            temp_file = f.name
        
        try:
            handler = CustFileHandler(temp_file)
            handler.open_file("r")
            record, result = handler.read_record("123456789")
            assert result == 0
            assert record is not None
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_acct_file_handler_read(self):
        """Test AcctFileHandler reading by key."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write("12345678901ACCOUNT DATA\n")
            temp_file = f.name
        
        try:
            handler = AcctFileHandler(temp_file)
            handler.open_file("r")
            record, result = handler.read_record("12345678901")
            assert result == 0
            assert record is not None
            handler.close_file()
        finally:
            os.unlink(temp_file)


class TestParsingFunctions:
    """Tests for parsing functions."""
    
    def test_parse_daily_tran_record(self):
        """Test parsing daily transaction record."""
        line = "TRAN0000000000011234567890123456TRANSACTION DATA"
        record = parse_daily_tran_record(line)
        assert record.tran_id == "TRAN000000000001"
        assert record.dalytran_card_num == "1234567890123456"
    
    def test_parse_xref_record(self):
        """Test parsing XREF record."""
        line = "123456789012345612345678998765432109" + " " * 14
        record = parse_xref_record(line)
        assert record.card_num == "1234567890123456"
        assert record.cust_num == "123456789"
        assert record.acct_id == "98765432109"
    
    def test_parse_account_record(self):
        """Test parsing account record."""
        line = "12345678901ACCOUNT DATA"
        record = parse_account_record(line)
        assert record.acct_id == "12345678901"


class TestAbendProgram:
    """Tests for abend_program function."""
    
    def test_abend_program(self):
        """Test abend_program function."""
        import io
        from contextlib import redirect_stdout
        
        f = io.StringIO()
        with redirect_stdout(f):
            with pytest.raises(SystemExit) as exc_info:
                abend_program()
            
            assert exc_info.value.code == 999
        
        output = f.getvalue()
        assert "ABENDING PROGRAM" in output


class TestMainFunction:
    """Tests for main function."""
    
    def test_main_with_sample_data(self, capsys):
        """Test main function with sample transaction data."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create test files
            dalytran_file = os.path.join(tmpdir, "DALYTRAN")
            xref_file = os.path.join(tmpdir, "XREFFILE")
            acct_file = os.path.join(tmpdir, "ACCTFILE")
            cust_file = os.path.join(tmpdir, "CUSTFILE")
            card_file = os.path.join(tmpdir, "CARDFILE")
            tran_file = os.path.join(tmpdir, "TRANFILE")
            
            # Create test data
            with open(dalytran_file, 'w') as f:
                f.write("TRAN0000000000011234567890123456TRANSACTION DATA\n")
            
            with open(xref_file, 'w') as f:
                f.write("123456789012345612345678998765432109" + " " * 14 + "\n")
            
            with open(acct_file, 'w') as f:
                f.write("98765432109ACCOUNT DATA\n")
            
            with open(cust_file, 'w') as f:
                f.write("123456789CUSTOMER DATA\n")
            
            with open(card_file, 'w') as f:
                f.write("1234567890123456CARD DATA\n")
            
            with open(tran_file, 'w') as f:
                f.write("TRAN000000000001TRANSACTION DATA\n")
            
            # Change to temp directory
            old_cwd = os.getcwd()
            try:
                os.chdir(tmpdir)
                result = main()
                assert result == 0
            finally:
                os.chdir(old_cwd)
            
            captured = capsys.readouterr().out
            assert "START OF EXECUTION OF PROGRAM CBTRN01C" in captured
            assert "SUCCESSFUL READ OF XREF" in captured
            assert "SUCCESSFUL READ OF ACCOUNT FILE" in captured
            assert "END OF EXECUTION OF PROGRAM CBTRN01C" in captured
    
    def test_main_with_invalid_card(self, capsys):
        """Test main function when card number is not found."""
        with tempfile.TemporaryDirectory() as tmpdir:
            dalytran_file = os.path.join(tmpdir, "DALYTRAN")
            xref_file = os.path.join(tmpdir, "XREFFILE")
            acct_file = os.path.join(tmpdir, "ACCTFILE")
            cust_file = os.path.join(tmpdir, "CUSTFILE")
            card_file = os.path.join(tmpdir, "CARDFILE")
            tran_file = os.path.join(tmpdir, "TRANFILE")
            
            # Create test data with invalid card number
            with open(dalytran_file, 'w') as f:
                f.write("TRAN0000000000019999999999999999TRANSACTION DATA\n")
            
            with open(xref_file, 'w') as f:
                f.write("123456789012345612345678998765432109" + " " * 14 + "\n")
            
            with open(acct_file, 'w') as f:
                f.write("98765432109ACCOUNT DATA\n")
            
            with open(cust_file, 'w') as f:
                f.write("123456789CUSTOMER DATA\n")
            
            with open(card_file, 'w') as f:
                f.write("1234567890123456CARD DATA\n")
            
            with open(tran_file, 'w') as f:
                f.write("TRAN000000000001TRANSACTION DATA\n")
            
            old_cwd = os.getcwd()
            try:
                os.chdir(tmpdir)
                result = main()
                assert result == 0
            finally:
                os.chdir(old_cwd)
            
            captured = capsys.readouterr().out
            assert "INVALID CARD NUMBER FOR XREF" in captured
            assert "COULD NOT BE VERIFIED" in captured
    
    def test_main_file_not_found(self, capsys):
        """Test main function when input file is not found."""
        with tempfile.TemporaryDirectory() as tmpdir:
            old_cwd = os.getcwd()
            try:
                os.chdir(tmpdir)
                # Don't create DALYTRAN file
                with pytest.raises(SystemExit):
                    main()
            finally:
                os.chdir(old_cwd)
            
            captured = capsys.readouterr().out
            assert "ERROR OPENING DAILY TRANSACTION FILE" in captured


class TestIntegration:
    """Integration tests."""
    
    def test_full_workflow(self):
        """Test complete workflow with valid data."""
        with tempfile.TemporaryDirectory() as tmpdir:
            dalytran_file = os.path.join(tmpdir, "DALYTRAN")
            xref_file = os.path.join(tmpdir, "XREFFILE")
            acct_file = os.path.join(tmpdir, "ACCTFILE")
            cust_file = os.path.join(tmpdir, "CUSTFILE")
            card_file = os.path.join(tmpdir, "CARDFILE")
            tran_file = os.path.join(tmpdir, "TRANFILE")
            
            # Create test data
            with open(dalytran_file, 'w') as f:
                f.write("TRAN0000000000011234567890123456TRANSACTION DATA LINE 1\n")
                f.write("TRAN0000000000021234567890123456TRANSACTION DATA LINE 2\n")
            
            with open(xref_file, 'w') as f:
                f.write("123456789012345612345678998765432109" + " " * 14 + "\n")
            
            with open(acct_file, 'w') as f:
                f.write("98765432109ACCOUNT DATA\n")
            
            with open(cust_file, 'w') as f:
                f.write("123456789CUSTOMER DATA\n")
            
            with open(card_file, 'w') as f:
                f.write("1234567890123456CARD DATA\n")
            
            with open(tran_file, 'w') as f:
                f.write("TRAN000000000001TRANSACTION DATA\n")
            
            old_cwd = os.getcwd()
            try:
                os.chdir(tmpdir)
                result = main()
                assert result == 0
            finally:
                os.chdir(old_cwd)


if __name__ == '__main__':
    pytest.main([__file__, '-v'])

