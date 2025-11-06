"""
Pytest tests for CBCUS01C.py ground truth Python equivalent.

Tests verify the functionality matches the COBOL program behavior.
"""

import pytest
import sys
import os
import tempfile
from pathlib import Path
from unittest.mock import patch, MagicMock

# Add parent directory to path to import CBCUS01C
sys.path.insert(0, str(Path(__file__).parent))

from CBCUS01C import (
    CustomerRecord,
    CustomerFileHandler,
    abend_program,
    main
)


class TestCustomerRecord:
    """Tests for CustomerRecord dataclass."""
    
    def test_customer_record_creation(self):
        """Test creating a CustomerRecord."""
        record = CustomerRecord(
            cust_id="123456789",
            cust_data="CUSTOMER DATA CONTENT HERE"
        )
        assert record.cust_id == "123456789"
        assert record.cust_data == "CUSTOMER DATA CONTENT HERE"
    
    def test_customer_record_string_representation(self):
        """Test CustomerRecord string representation."""
        record = CustomerRecord(
            cust_id="123456789",
            cust_data="CUSTOMER DATA"
        )
        str_repr = str(record)
        assert "123456789" in str_repr
        assert "CUSTOMER DATA" in str_repr


class TestCustomerFileHandler:
    """Tests for CustomerFileHandler class."""
    
    def test_init(self):
        """Test CustomerFileHandler initialization."""
        handler = CustomerFileHandler("testfile.txt")
        assert handler.filename == "testfile.txt"
        assert handler.file_handle is None
        assert handler.file_status == "00"
        assert handler.end_of_file is False
    
    def test_open_file_success(self):
        """Test successful file opening."""
        handler = CustomerFileHandler("testfile.txt")
        
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
        handler = CustomerFileHandler("nonexistent_file.txt")
        result = handler.open_file()
        assert result == 12  # Error
        assert handler.file_status == "23"  # File not found
    
    def test_read_next_success(self):
        """Test reading next record successfully."""
        handler = CustomerFileHandler()
        
        # Create test data: 9 chars for cust_id + cust_data
        test_data = "123456789CUSTOMER DATA CONTENT HERE\n"
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write(test_data)
            temp_file = f.name
        
        try:
            handler.filename = temp_file
            handler.open_file()
            
            record, result = handler.read_next()
            
            assert result == 0  # APPL-AOK
            assert record is not None
            assert record.cust_id == "123456789"
            assert handler.file_status == "00"
            assert handler.end_of_file is False
            
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_read_next_eof(self):
        """Test reading when end of file is reached."""
        handler = CustomerFileHandler()
        
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
        handler = CustomerFileHandler()
        
        test_data = (
            "111111111CUSTOMER RECORD 1 DATA\n"
            "222222222CUSTOMER RECORD 2 DATA\n"
            "333333333CUSTOMER RECORD 3 DATA\n"
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
            assert record1.cust_id == "111111111"
            
            # Read second record
            record2, result2 = handler.read_next()
            assert result2 == 0
            assert record2.cust_id == "222222222"
            
            # Read third record
            record3, result3 = handler.read_next()
            assert result3 == 0
            assert record3.cust_id == "333333333"
            
            # Read EOF
            record4, result4 = handler.read_next()
            assert result4 == 16  # APPL-EOF
            assert record4 is None
            
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_read_next_short_record(self):
        """Test reading a record that's too short."""
        handler = CustomerFileHandler()
        
        # Record shorter than 9 characters (minimum for cust_id)
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
    
    def test_close_file_success(self):
        """Test successful file closing."""
        handler = CustomerFileHandler()
        
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
        handler = CustomerFileHandler()
        result = handler.close_file()
        assert result == 0  # Should still return success
    
    def test_display_io_status_normal(self):
        """Test displaying normal IO status."""
        handler = CustomerFileHandler()
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
        handler = CustomerFileHandler()
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
        handler = CustomerFileHandler()
        handler.file_status = "9A"  # Non-numeric
        
        import io
        from contextlib import redirect_stdout
        
        f = io.StringIO()
        with redirect_stdout(f):
            handler.display_io_status()
        
        output = f.getvalue()
        assert "FILE STATUS IS: NNNN" in output


class TestMainFunction:
    """Tests for main function."""
    
    def test_main_success(self):
        """Test main function with successful execution."""
        test_data = (
            "111111111CUSTOMER RECORD 1 DATA\n"
            "222222222CUSTOMER RECORD 2 DATA\n"
        )
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write(test_data)
            temp_file = f.name
        
        try:
            # Mock the file handler to use our test file
            import io
            from contextlib import redirect_stdout
            
            f = io.StringIO()
            with redirect_stdout(f), patch('CBCUS01C.CustomerFileHandler') as mock_handler:
                # Create a real handler instance
                real_handler = CustomerFileHandler()
                real_handler.filename = temp_file
                real_handler.open_file()
                
                # Mock the handler class to return our real handler
                mock_handler.return_value = real_handler
                
                # Run main
                result = main()
                
                output = f.getvalue()
                
                # Check output
                assert "START OF EXECUTION OF PROGRAM CBCUS01C" in output
                assert "111111111" in output  # Customer record should be displayed
                assert "END OF EXECUTION OF PROGRAM CBCUS01C" in output
                
                real_handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_main_file_not_found(self):
        """Test main function when file is not found."""
        with patch('CBCUS01C.CustomerFileHandler') as mock_handler:
            handler_instance = MagicMock()
            handler_instance.open_file.return_value = 12  # Error
            handler_instance.file_status = "23"
            handler_instance.display_io_status = MagicMock()
            handler_instance.end_of_file = False
            handler_instance.read_next = MagicMock(return_value=(None, 16))  # Mock read_next
            handler_instance.close_file = MagicMock(return_value=0)  # Mock close_file
            mock_handler.return_value = handler_instance
            
            with patch('CBCUS01C.abend_program', side_effect=SystemExit(999)) as mock_abend:
                with pytest.raises(SystemExit) as exc_info:
                    main()
                
                assert exc_info.value.code == 999
                
                # Should call abend_program when file open fails
                assert mock_abend.call_count >= 1
    
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
        handler = CustomerFileHandler()
        
        test_data = "123456789CUSTOMER DATA CONTENT\n"
        
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
            assert record.cust_id == "123456789"
            
            # Display
            import io
            from contextlib import redirect_stdout
            f = io.StringIO()
            with redirect_stdout(f):
                print(record)
            output = f.getvalue()
            assert "123456789" in output
            
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
        handler = CustomerFileHandler()
        
        test_data = (
            "111111111RECORD1 DATA\n"
            "222222222RECORD2 DATA\n"
            "333333333RECORD3 DATA\n"
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
            assert records[0].cust_id == "111111111"
            assert records[1].cust_id == "222222222"
            assert records[2].cust_id == "333333333"
            
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_full_workflow_with_491_char_data(self):
        """Test reading record with full 491 character data field."""
        handler = CustomerFileHandler()
        
        # Create a record with exactly 491 chars of data
        cust_id = "123456789"
        cust_data = "X" * 491
        test_data = cust_id + cust_data + "\n"
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write(test_data)
            temp_file = f.name
        
        try:
            handler.filename = temp_file
            handler.open_file()
            
            record, result = handler.read_next()
            assert result == 0
            assert record.cust_id == "123456789"
            assert len(record.cust_data) == 491
            
            handler.close_file()
        finally:
            os.unlink(temp_file)


if __name__ == '__main__':
    pytest.main([__file__, '-v'])

