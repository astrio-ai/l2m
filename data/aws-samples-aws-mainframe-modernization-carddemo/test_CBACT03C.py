"""
Pytest tests for CBACT03C.py ground truth Python equivalent.

Tests verify the functionality matches the COBOL program behavior.
"""

import io
import pytest
from CBACT03C import (
    CardXrefRecord,
    XRefReader,
    process_xref_file,
    format_io_status,   
    FileStatusError,
)


def make_fixed_record(card_num: str, data: str) -> str:
    """
    Build a fixed-width 50-character record from inputs, padding or truncating as necessary.
    """
    c = card_num[:16].ljust(16)
    d = data[:34].ljust(34)
    return c + d


def test_format_io_status_numeric():
    assert format_io_status("00") == "0000"
    assert format_io_status("12") == "0012"
    assert format_io_status("99") == "0099"


def test_format_io_status_non_numeric():
    """Test format_io_status with non-numeric characters."""
    assert format_io_status("9A") == "900A"
    assert format_io_status("A0") == "A000"
    assert format_io_status("AB") == "A00B"


def test_format_io_status_edge_cases():
    """Test format_io_status with edge cases."""
    # Single character
    assert format_io_status("0") == "0000"
    assert format_io_status("A") == "A000"
    # Empty string
    assert format_io_status("") == "0000"
    # Long string (should truncate to 2 chars)
    assert format_io_status("12345") == "0012"


def test_process_reads_all_records(capsys):
    # build two records and wrap in StringIO
    r1 = make_fixed_record("CARD000000000001", "Alice account metadata")
    r2 = make_fixed_record("CARD000000000002", "Bob account metadata")
    s = io.StringIO(r1 + r2)

    process_xref_file(s)

    captured = capsys.readouterr().out.splitlines()
    # expect start, two records, end
    assert "START OF EXECUTION OF PROGRAM CBACT03C" in captured[0]
    # record lines should equal the raw fixed-width strings
    # find the two record lines (they may be at positions 1 and 2)
    assert any("CARD000000000001" in line for line in captured)
    assert any("CARD000000000002" in line for line in captured)
    assert "END OF EXECUTION OF PROGRAM CBACT03C" in captured[-1]


def test_process_empty_file(capsys):
    # empty StringIO should produce start and end with no record prints
    s = io.StringIO("")
    process_xref_file(s)
    captured = capsys.readouterr().out.splitlines()
    assert captured[0].startswith("START OF EXECUTION")
    assert captured[-1].startswith("END OF EXECUTION")


def test_open_nonexistent_path(tmp_path):
    # ensure that trying to open a non-existent file path raises FileStatusError
    bad_path = str(tmp_path / "does_not_exist.xyz")
    with pytest.raises(FileStatusError):
        process_xref_file(bad_path)


def test_partial_record_raises_error(capsys):
    # partial record (less than 50 chars) should cause a FileStatusError during read
    partial = "SHORT_RECORD"  # less than 50 chars
    s = io.StringIO(partial)
    with pytest.raises(FileStatusError):
        process_xref_file(s)


def test_card_xref_record():
    """Test CardXrefRecord dataclass."""
    record = CardXrefRecord(card_num="1234567890123456", data="Test data here")
    assert record.card_num == "1234567890123456"
    assert record.data == "Test data here"
    # Test __str__ method
    assert str(record) == "1234567890123456Test data here"


def test_file_status_error():
    """Test FileStatusError exception."""
    error = FileStatusError("12", "Test error")
    assert error.status == "12"
    assert str(error) == "Test error"
    
    # Test default message
    error2 = FileStatusError("99")
    assert error2.status == "99"
    assert "File status: 99" in str(error2)


def test_xref_reader_direct_usage():
    """Test XRefReader class directly."""
    test_data = make_fixed_record("CARD000000000001", "Test record data")
    s = io.StringIO(test_data)
    
    reader = XRefReader(s)
    record = reader.read_next()
    
    assert isinstance(record, CardXrefRecord)
    assert record.card_num == "CARD000000000001"
    assert "Test record data" in record.data
    
    # Should raise EOFError when no more data
    with pytest.raises(EOFError):
        reader.read_next()
    
    reader.close()


def test_xref_reader_read_after_close():
    """Test that reading from a closed file raises FileStatusError."""
    test_data = make_fixed_record("CARD000000000001", "Test data")
    s = io.StringIO(test_data)
    
    reader = XRefReader(s)
    reader.close()
    
    # Attempting to read after close should raise FileStatusError
    with pytest.raises(FileStatusError) as exc_info:
        reader.read_next()
    
    assert exc_info.value.status == "12"
    assert "closed" in str(exc_info.value).lower()


def test_xref_reader_open_from_path(tmp_path):
    """Test XRefReader.open_from_path with a valid file."""
    test_file = tmp_path / "test_file.txt"
    test_data = make_fixed_record("CARD000000000001", "Test data")
    test_file.write_text(test_data)
    
    reader = XRefReader.open_from_path(str(test_file))
    record = reader.read_next()
    
    assert record.card_num == "CARD000000000001"
    reader.close()


def test_xref_reader_open_from_path_nonexistent(tmp_path):
    """Test XRefReader.open_from_path with non-existent file."""
    bad_path = str(tmp_path / "does_not_exist.txt")
    
    with pytest.raises(FileStatusError) as exc_info:
        XRefReader.open_from_path(bad_path)
    
    assert exc_info.value.status == "12"
    assert "Error opening file" in str(exc_info.value)


def test_process_xref_file_invalid_source(capsys):
    """Test process_xref_file with invalid source type."""
    with pytest.raises(FileStatusError) as exc_info:
        process_xref_file(123)  # Invalid type
    
    assert exc_info.value.status == "12"
    assert "Invalid source" in str(exc_info.value)
    
    captured = capsys.readouterr().out
    assert "ERROR OPENING XREFFILE" in captured


def test_process_xref_file_error_messages(capsys):
    """Test that error messages are properly displayed."""
    # Test opening error
    bad_path = "/nonexistent/path/file.txt"
    with pytest.raises(FileStatusError):
        process_xref_file(bad_path)
    
    captured = capsys.readouterr().out
    assert "START OF EXECUTION OF PROGRAM CBACT03C" in captured
    assert "ERROR OPENING XREFFILE" in captured
    assert "FILE STATUS IS:" in captured
    
    # Test reading error
    partial = "SHORT"
    s = io.StringIO(partial)
    with pytest.raises(FileStatusError):
        process_xref_file(s)
    
    captured = capsys.readouterr().out
    assert "START OF EXECUTION OF PROGRAM CBACT03C" in captured
    assert "ERROR READING XREFFILE" in captured
    assert "FILE STATUS IS:" in captured


def test_process_xref_file_single_record(capsys):
    """Test processing a single record."""
    r1 = make_fixed_record("CARD000000000001", "Single record test")
    s = io.StringIO(r1)
    
    process_xref_file(s)
    
    captured = capsys.readouterr().out.splitlines()
    assert "START OF EXECUTION OF PROGRAM CBACT03C" in captured[0]
    assert "CARD000000000001" in captured[1]
    assert "Single record test" in captured[1]
    assert "END OF EXECUTION OF PROGRAM CBACT03C" in captured[-1]
    assert len(captured) == 3  # Start, record, end
