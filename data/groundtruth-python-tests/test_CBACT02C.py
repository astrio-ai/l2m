"""
Pytest tests for CBACT02C.py ground truth Python equivalent.

Tests verify the functionality matches the COBOL program behavior.
"""

import pytest
import sys
import os
import tempfile
from pathlib import Path
from unittest.mock import patch, MagicMock

# Add parent directory to path to import CBACT02C
sys.path.insert(0, str(Path(__file__).parent))

from CBACT02C import (
    CardRecord,
    CardFileHandler,
    abend_program,
    main
)


class TestCardRecord:
    """Tests for CardRecord dataclass."""
    
    def test_card_record_creation(self):
        """Test creating a CardRecord."""
        record = CardRecord(
            card_num="1234567890123456",
            card_data="CARD DATA CONTENT HERE"
        )
        assert record.card_num == "1234567890123456"
        assert record.card_data == "CARD DATA CONTENT HERE"
    
    def test_card_record_string_representation(self):
        """Test CardRecord string representation."""
        record = CardRecord(
            card_num="1234567890123456",
            card_data="CARD DATA"
        )
        str_repr = str(record)
        assert "1234567890123456" in str_repr
        assert "CARD DATA" in str_repr


class TestCardFileHandler:
    """Tests for CardFileHandler class."""
    
    def test_init(self):
        """Test CardFileHandler initialization."""
        handler = CardFileHandler("testfile.txt")
        assert handler.filename == "testfile.txt"
        assert handler.file_handle is None
        assert handler.file_status == "00"
        assert handler.end_of_file is False
    
    def test_open_file_success(self):
        """Test successful file opening."""
        handler = CardFileHandler("testfile.txt")
        
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
        handler = CardFileHandler("nonexistent_file.txt")
        result = handler.open_file()
        assert result == 12  # Error
        assert handler.file_status == "23"  # File not found
    
    def test_read_next_success(self):
        """Test reading next record successfully."""
        handler = CardFileHandler()
        
        # Create test data: 16 chars for card_num + card_data
        test_data = "1234567890123456CARD DATA CONTENT HERE\n"
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write(test_data)
            temp_file = f.name
        
        try:
            handler.filename = temp_file
            handler.open_file()
            
            record, result = handler.read_next()
            
            assert result == 0  # APPL-AOK
            assert record is not None
            assert record.card_num == "1234567890123456"
            assert handler.file_status == "00"
            assert handler.end_of_file is False
            
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_read_next_eof(self):
        """Test reading when end of file is reached."""
        handler = CardFileHandler()
        
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
        handler = CardFileHandler()
        
        test_data = (
            "1111111111111111CARD RECORD 1 DATA\n"
            "2222222222222222CARD RECORD 2 DATA\n"
            "3333333333333333CARD RECORD 3 DATA\n"
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
            assert record1.card_num == "1111111111111111"
            
            # Read second record
            record2, result2 = handler.read_next()
            assert result2 == 0
            assert record2.card_num == "2222222222222222"
            
            # Read third record
            record3, result3 = handler.read_next()
            assert result3 == 0
            assert record3.card_num == "3333333333333333"
            
            # Read EOF
            record4, result4 = handler.read_next()
            assert result4 == 16  # APPL-EOF
            assert record4 is None
            
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_read_next_short_record(self):
        """Test reading a record that's too short."""
        handler = CardFileHandler()
        
        # Record shorter than 16 characters (minimum for card_num)
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
        handler = CardFileHandler()
        
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
        handler = CardFileHandler()
        result = handler.close_file()
        assert result == 0  # Should still return success
    
    def test_display_io_status_normal(self):
        """Test displaying normal IO status."""
        handler = CardFileHandler()
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
        handler = CardFileHandler()
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
        handler = CardFileHandler()
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
            "1111111111111111CARD RECORD 1 DATA\n"
            "2222222222222222CARD RECORD 2 DATA\n"
        )
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write(test_data)
            temp_file = f.name
        
        try:
            # Mock the file handler to use our test file
            import io
            from contextlib import redirect_stdout
            
            f = io.StringIO()
            with redirect_stdout(f), patch('CBACT02C.CardFileHandler') as mock_handler:
                # Create a real handler instance
                real_handler = CardFileHandler()
                real_handler.filename = temp_file
                real_handler.open_file()
                
                # Mock the handler class to return our real handler
                mock_handler.return_value = real_handler
                
                # Run main
                result = main()
                
                output = f.getvalue()
                
                # Check output
                assert "START OF EXECUTION OF PROGRAM CBACT02C" in output
                assert "1111111111111111" in output  # Card record should be displayed
                assert "END OF EXECUTION OF PROGRAM CBACT02C" in output
                
                real_handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_main_file_not_found(self):
        """Test main function when file is not found."""
        with patch('CBACT02C.CardFileHandler') as mock_handler:
            handler_instance = MagicMock()
            handler_instance.open_file.return_value = 12  # Error
            handler_instance.file_status = "23"
            handler_instance.display_io_status = MagicMock()
            handler_instance.end_of_file = False
            handler_instance.read_next = MagicMock(return_value=(None, 16))  # Mock read_next
            handler_instance.close_file = MagicMock(return_value=0)  # Mock close_file
            mock_handler.return_value = handler_instance
            
            with patch('CBACT02C.abend_program', side_effect=SystemExit(999)) as mock_abend:
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
        handler = CardFileHandler()
        
        test_data = "1234567890123456CARD DATA CONTENT\n"
        
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
            assert record.card_num == "1234567890123456"
            
            # Display
            import io
            from contextlib import redirect_stdout
            f = io.StringIO()
            with redirect_stdout(f):
                print(record)
            output = f.getvalue()
            assert "1234567890123456" in output
            
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
        handler = CardFileHandler()
        
        test_data = (
            "1111111111111111RECORD1 DATA\n"
            "2222222222222222RECORD2 DATA\n"
            "3333333333333333RECORD3 DATA\n"
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
            assert records[0].card_num == "1111111111111111"
            assert records[1].card_num == "2222222222222222"
            assert records[2].card_num == "3333333333333333"
            
            handler.close_file()
        finally:
            os.unlink(temp_file)

