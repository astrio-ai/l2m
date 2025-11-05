"""
Pytest tests for CBACT01C.py ground truth Python equivalent.

Tests verify the functionality matches the COBOL program behavior.
"""

import pytest
import sys
import tempfile
import os
from pathlib import Path
from unittest.mock import patch, mock_open, MagicMock

# Add parent directory to path to import CBACT01C
sys.path.insert(0, str(Path(__file__).parent))

from CBACT01C import (
    AccountRecord,
    AcctFileHandler,
    display_account_record,
    abend_program,
    main
)


class TestAccountRecord:
    """Tests for AccountRecord dataclass."""
    
    def test_account_record_creation(self):
        """Test creating an AccountRecord."""
        record = AccountRecord(
            acct_id="12345678901",
            acct_data="ACTIVE 1000.00 5000.00 2000.00 20240101 20251231 20240101 100.00 50.00 GROUP001"
        )
        assert record.acct_id == "12345678901"
        assert record.acct_data == "ACTIVE 1000.00 5000.00 2000.00 20240101 20251231 20240101 100.00 50.00 GROUP001"
    
    def test_account_record_default_values(self):
        """Test AccountRecord with default values."""
        record = AccountRecord(acct_id="123", acct_data="")
        assert record.acct_id == "123"
        assert record.acct_active_status == ""
        assert record.acct_curr_bal == ""
        assert record.acct_group_id == ""


class TestAcctFileHandler:
    """Tests for AcctFileHandler class."""
    
    def test_init(self):
        """Test AcctFileHandler initialization."""
        handler = AcctFileHandler("testfile.txt")
        assert handler.filename == "testfile.txt"
        assert handler.file_handle is None
        assert handler.file_status == "00"
        assert handler.end_of_file is False
    
    def test_open_file_success(self):
        """Test successful file opening."""
        handler = AcctFileHandler("testfile.txt")
        
        # Create a temporary file
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write("test data")
            temp_file = f.name
        
        try:
            handler.filename = temp_file
            result = handler.open_file()
            assert result == 0  # APPL-AOK
            assert handler.file_status == "00"
            assert handler.file_handle is not None
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_open_file_not_found(self):
        """Test file opening when file doesn't exist."""
        handler = AcctFileHandler("nonexistent_file.txt")
        result = handler.open_file()
        assert result == 12  # Error
        assert handler.file_status == "23"  # File not found
    
    def test_read_next_success(self):
        """Test reading next record successfully."""
        handler = AcctFileHandler()
        
        # Create test data: 11 chars for ID + data
        test_data = "12345678901ACTIVE 1000.00 5000.00 2000.00 20240101 20251231 20240101 100.00 50.00 GROUP001\n"
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write(test_data)
            temp_file = f.name
        
        try:
            handler.filename = temp_file
            handler.open_file()
            
            record, result = handler.read_next()
            
            assert result == 0  # APPL-AOK
            assert record is not None
            assert record.acct_id == "12345678901"
            assert handler.file_status == "00"
            assert handler.end_of_file is False
            
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_read_next_eof(self):
        """Test reading when end of file is reached."""
        handler = AcctFileHandler()
        
        # Create empty file
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_file = f.name
        
        try:
            handler.filename = temp_file
            handler.open_file()
            
            record, result = handler.read_next()
            
            assert result == 16  # APPL-EOF
            assert record is None
            assert handler.end_of_file is True
            assert handler.file_status == "10"  # EOF
            
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_read_next_multiple_records(self):
        """Test reading multiple records."""
        handler = AcctFileHandler()
        
        test_data = (
            "11111111111RECORD1 DATA\n"
            "22222222222RECORD2 DATA\n"
            "33333333333RECORD3 DATA\n"
        )
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write(test_data)
            temp_file = f.name
        
        try:
            handler.filename = temp_file
            handler.open_file()
            
            # Read first record
            record1, result1 = handler.read_next()
            assert result1 == 0
            assert record1.acct_id == "11111111111"
            
            # Read second record
            record2, result2 = handler.read_next()
            assert result2 == 0
            assert record2.acct_id == "22222222222"
            
            # Read third record
            record3, result3 = handler.read_next()
            assert result3 == 0
            assert record3.acct_id == "33333333333"
            
            # Read EOF
            record4, result4 = handler.read_next()
            assert result4 == 16  # APPL-EOF
            assert record4 is None
            
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_read_next_short_record(self):
        """Test reading a record that's too short."""
        handler = AcctFileHandler()
        
        # Record shorter than 11 characters (minimum for ID)
        test_data = "12345\n"
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write(test_data)
            temp_file = f.name
        
        try:
            handler.filename = temp_file
            handler.open_file()
            
            record, result = handler.read_next()
            
            assert result == 12  # Error
            assert record is None
            assert handler.file_status == "04"  # Invalid record
            
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_parse_account_data(self):
        """Test parsing account data fields."""
        handler = AcctFileHandler()
        record = AccountRecord(acct_id="12345678901", acct_data="")
        
        # Test with data
        data = "ACTIVE 1000.00 5000.00 2000.00 20240101 20251231 20240101 100.00 50.00 GROUP001"
        handler._parse_account_data(record, data)
        
        assert record.acct_active_status == "ACTIVE"
        assert record.acct_curr_bal == "1000.00"
        assert record.acct_credit_limit == "5000.00"
        assert record.acct_cash_credit_limit == "2000.00"
        assert record.acct_open_date == "20240101"
        assert record.acct_expiration_date == "20251231"
        assert record.acct_reissue_date == "20240101"
        assert record.acct_curr_cyc_credit == "100.00"
        assert record.acct_curr_cyc_debit == "50.00"
        assert record.acct_group_id == "GROUP001"
    
    def test_parse_account_data_empty(self):
        """Test parsing empty account data."""
        handler = AcctFileHandler()
        record = AccountRecord(acct_id="123", acct_data="")
        
        handler._parse_account_data(record, "")
        
        # All fields should remain empty
        assert record.acct_active_status == ""
        assert record.acct_curr_bal == ""
    
    def test_close_file_success(self):
        """Test successful file closing."""
        handler = AcctFileHandler()
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_file = f.name
        
        try:
            handler.filename = temp_file
            handler.open_file()
            
            result = handler.close_file()
            assert result == 0  # APPL-AOK
            assert handler.file_status == "00"
        finally:
            os.unlink(temp_file)
    
    def test_close_file_not_open(self):
        """Test closing file that was never opened."""
        handler = AcctFileHandler()
        result = handler.close_file()
        assert result == 0  # Should still return success
    
    def test_display_io_status_normal(self):
        """Test displaying normal IO status."""
        handler = AcctFileHandler()
        handler.file_status = "00"
        
        # Capture stdout
        import io
        from contextlib import redirect_stdout
        
        f = io.StringIO()
        with redirect_stdout(f):
            handler.display_io_status()
        
        output = f.getvalue()
        assert "FILE STATUS IS: NNNN" in output
        assert "0000" in output
    
    def test_display_io_status_error(self):
        """Test displaying error IO status."""
        handler = AcctFileHandler()
        handler.file_status = "23"  # File not found
        
        import io
        from contextlib import redirect_stdout
        
        f = io.StringIO()
        with redirect_stdout(f):
            handler.display_io_status()
        
        output = f.getvalue()
        assert "FILE STATUS IS: NNNN" in output
        assert "0023" in output
    
    def test_display_io_status_non_numeric(self):
        """Test displaying non-numeric IO status."""
        handler = AcctFileHandler()
        handler.file_status = "9A"  # Non-numeric
        
        import io
        from contextlib import redirect_stdout
        
        f = io.StringIO()
        with redirect_stdout(f):
            handler.display_io_status()
        
        output = f.getvalue()
        assert "FILE STATUS IS: NNNN" in output


class TestDisplayFunctions:
    """Tests for display functions."""
    
    def test_display_account_record(self):
        """Test displaying account record."""
        record = AccountRecord(
            acct_id="12345678901",
            acct_data="",
            acct_active_status="ACTIVE",
            acct_curr_bal="1000.00",
            acct_credit_limit="5000.00",
            acct_cash_credit_limit="2000.00",
            acct_open_date="20240101",
            acct_expiration_date="20251231",
            acct_reissue_date="20240101",
            acct_curr_cyc_credit="100.00",
            acct_curr_cyc_debit="50.00",
            acct_group_id="GROUP001"
        )
        
        import io
        from contextlib import redirect_stdout
        
        f = io.StringIO()
        with redirect_stdout(f):
            display_account_record(record)
        
        output = f.getvalue()
        
        # Check all fields are displayed
        assert "ACCT-ID" in output
        assert "12345678901" in output
        assert "ACCT-ACTIVE-STATUS" in output
        assert "ACTIVE" in output
        assert "ACCT-CURR-BAL" in output
        assert "1000.00" in output
        assert "ACCT-CREDIT-LIMIT" in output
        assert "5000.00" in output
        assert "ACCT-GROUP-ID" in output
        assert "GROUP001" in output
        assert "-------------------------------------------------" in output


class TestMainFunction:
    """Tests for main function."""
    
    def test_main_success(self):
        """Test main function with successful execution."""
        test_data = (
            "11111111111ACTIVE 1000.00 5000.00 2000.00 20240101 20251231 20240101 100.00 50.00 GROUP001\n"
            "22222222222INACTIVE 2000.00 6000.00 3000.00 20240201 20261231 20240201 200.00 100.00 GROUP002\n"
        )
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write(test_data)
            temp_file = f.name
        
        try:
            # Mock the file handler to use our test file
            import io
            from contextlib import redirect_stdout
            
            f = io.StringIO()
            with redirect_stdout(f), patch('CBACT01C.AcctFileHandler') as mock_handler:
                # Create a real handler instance
                real_handler = AcctFileHandler()
                real_handler.filename = temp_file
                real_handler.open_file()
                real_handler.end_of_file = False
                
                # Mock the handler class
                mock_handler.return_value = real_handler
                
                # Run main
                result = main()
                
                output = f.getvalue()
                
                # Check output
                assert "START OF EXECUTION OF PROGRAM CBACT01C" in output
                assert "ACCT-ID" in output
                assert "END OF EXECUTION OF PROGRAM CBACT01C" in output
                
                real_handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_main_file_not_found(self):
        """Test main function when file is not found."""
        with patch('CBACT01C.AcctFileHandler') as mock_handler:
            handler_instance = MagicMock()
            handler_instance.open_file.return_value = 12  # Error
            handler_instance.file_status = "23"
            handler_instance.display_io_status = MagicMock()
            mock_handler.return_value = handler_instance
            
            with patch('CBACT01C.abend_program') as mock_abend:
                main()
                # Should call abend_program when file open fails
                mock_abend.assert_called_once()
    
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


class TestIntegration:
    """Integration tests for full workflow."""
    
    def test_full_workflow_single_record(self):
        """Test complete workflow with single record."""
        handler = AcctFileHandler()
        
        test_data = "12345678901ACTIVE 1000.00 5000.00 2000.00 20240101 20251231 20240101 100.00 50.00 GROUP001\n"
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write(test_data)
            temp_file = f.name
        
        try:
            handler.filename = temp_file
            
            # Open
            result = handler.open_file()
            assert result == 0
            
            # Read
            record, result = handler.read_next()
            assert result == 0
            assert record.acct_id == "12345678901"
            
            # Display
            import io
            from contextlib import redirect_stdout
            f = io.StringIO()
            with redirect_stdout(f):
                display_account_record(record)
            output = f.getvalue()
            assert "12345678901" in output
            
            # EOF
            record2, result2 = handler.read_next()
            assert result2 == 16  # APPL-EOF
            
            # Close
            result = handler.close_file()
            assert result == 0
            
        finally:
            os.unlink(temp_file)
    
    def test_full_workflow_multiple_records(self):
        """Test complete workflow with multiple records."""
        handler = AcctFileHandler()
        
        test_data = (
            "11111111111RECORD1 DATA FIELD1 FIELD2\n"
            "22222222222RECORD2 DATA FIELD3 FIELD4\n"
            "33333333333RECORD3 DATA FIELD5 FIELD6\n"
        )
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write(test_data)
            temp_file = f.name
        
        try:
            handler.filename = temp_file
            handler.open_file()
            
            records = []
            while not handler.end_of_file:
                record, result = handler.read_next()
                if result == 0:
                    records.append(record)
                elif result == 16:
                    break
            
            assert len(records) == 3
            assert records[0].acct_id == "11111111111"
            assert records[1].acct_id == "22222222222"
            assert records[2].acct_id == "33333333333"
            
            handler.close_file()
        finally:
            os.unlink(temp_file)

