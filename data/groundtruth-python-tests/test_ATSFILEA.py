"""
Test suite for ATSFILEA.py

Tests for WOLA VSAM file service with REST operations.
"""

import pytest
import tempfile
import os
from ATSFILEA import (
    FileARecord,
    RequestData,
    ResponseData,
    FileAVSAMHandler,
    WOLARegistration,
    WOLAService,
    WOLAResponse,
    clear_fields,
    process_request,
    main,
    bboa1reg,
    bboa1srv,
    bboa1srp,
    bboa1cnr,
    bboa1urg
)


# ============================================================================
# Test FileA Record
# ============================================================================

class TestFileARecord:
    """Test FileARecord structure."""
    
    def test_record_creation(self):
        """Test creating a FileA record."""
        record = FileARecord(
            stat="A",
            numb="000001",
            name="John Doe",
            addrx="123 Main St",
            phone="5551234",
            datex="20250101",
            amount="100.00",
            comment="Test"
        )
        
        assert record.stat == "A"
        assert record.numb == "000001"
        assert record.name == "John Doe"
    
    def test_record_to_string(self):
        """Test converting record to string."""
        record = FileARecord(
            numb="000001",
            name="John Doe"
        )
        record_str = record.to_string()
        
        assert len(record_str) == 80
        assert record.numb in record_str
    
    def test_record_from_string(self):
        """Test creating record from string."""
        # Format: stat(1) + numb(6) + name(20) + addrx(20) + phone(8) + datex(8) + amount(8) + comment(9) = 80
        # Create a properly formatted 80-character string
        record = FileARecord(
            stat="A",
            numb="000001",
            name="John Doe",
            addrx="123 Main St",
            phone="5551234",
            datex="20250101",
            amount="100.00",
            comment="Test"
        )
        record_str = record.to_string()
        
        # Parse it back
        parsed_record = FileARecord.from_string(record_str)
        
        assert parsed_record.stat == "A"
        assert parsed_record.numb == "000001"
        assert parsed_record.name.strip() == "John Doe"
        assert parsed_record.addrx.strip() == "123 Main St"
        assert parsed_record.phone.strip() == "5551234"
    
    def test_record_to_dict(self):
        """Test converting record to dictionary."""
        record = FileARecord(
            numb="000001",
            name="John Doe"
        )
        data = record.to_dict()
        
        assert data['numb'] == "000001"
        assert data['name'] == "John Doe"
    
    def test_record_from_dict(self):
        """Test creating record from dictionary."""
        data = {
            'numb': '000001',
            'name': 'John Doe',
            'stat': 'A'
        }
        record = FileARecord.from_dict(data)
        
        assert record.numb == "000001"
        assert record.name == "John Doe"


# ============================================================================
# Test Request/Response Data
# ============================================================================

class TestRequestResponseData:
    """Test RequestData and ResponseData structures."""
    
    def test_request_data_to_filea_record(self):
        """Test converting RequestData to FileARecord."""
        request = RequestData(
            request_type="P",
            numb="000001",
            name="John Doe"
        )
        record = request.to_filea_record()
        
        assert record.numb == "000001"
        assert record.name == "John Doe"
    
    def test_response_data_from_filea_record(self):
        """Test creating ResponseData from FileARecord."""
        record = FileARecord(
            numb="000001",
            name="John Doe"
        )
        response = ResponseData.from_filea_record(
            record,
            "00",
            "GET successful"
        )
        
        assert response.numb == "000001"
        assert response.results_message == "GET successful"
        assert response.vsam_status_code == "00"


# ============================================================================
# Test VSAM File Handler
# ============================================================================

class TestFileAVSAMHandler:
    """Test FileAVSAMHandler operations."""
    
    def test_open_file_new(self):
        """Test opening a new file."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            result = handler.open_file()
            
            assert result is True
            assert handler.is_normal()
            assert handler._file_open is True
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_write_record(self):
        """Test writing a record."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            record = FileARecord(numb="000001", name="John Doe")
            result = handler.write_record(record)
            
            assert result is True
            assert handler.is_normal()
            assert "000001" in handler._data
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_write_duplicate_record(self):
        """Test writing a duplicate record."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            record = FileARecord(numb="000001", name="John Doe")
            handler.write_record(record)
            result = handler.write_record(record)  # Duplicate
            
            assert result is False
            assert handler.is_duplicate()
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_read_record_found(self):
        """Test reading an existing record."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            record = FileARecord(numb="000001", name="John Doe")
            handler.write_record(record)
            
            read_record = handler.read_record("000001")
            
            assert read_record is not None
            assert read_record.numb == "000001"
            assert read_record.name == "John Doe"
            assert handler.is_normal()
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_read_record_not_found(self):
        """Test reading a non-existent record."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            read_record = handler.read_record("000999")
            
            assert read_record is None
            assert handler.is_notfound()
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_rewrite_record(self):
        """Test rewriting an existing record."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            record1 = FileARecord(numb="000001", name="John Doe")
            handler.write_record(record1)
            
            record2 = FileARecord(numb="000001", name="Jane Doe")
            result = handler.rewrite_record(record2)
            
            assert result is True
            assert handler.is_normal()
            read_record = handler.read_record("000001")
            assert read_record.name == "Jane Doe"
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_rewrite_record_not_found(self):
        """Test rewriting a non-existent record."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            record = FileARecord(numb="000999", name="John Doe")
            result = handler.rewrite_record(record)
            
            assert result is False
            assert handler.is_notfound()
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_delete_record(self):
        """Test deleting a record."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            record = FileARecord(numb="000001", name="John Doe")
            handler.write_record(record)
            
            result = handler.delete_record("000001")
            
            assert result is True
            assert handler.is_normal()
            assert "000001" not in handler._data
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_delete_record_not_found(self):
        """Test deleting a non-existent record."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            result = handler.delete_record("000999")
            
            assert result is False
            assert handler.is_notfound()
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_close_file_persists_data(self):
        """Test that closing file persists data."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            # Write data
            handler1 = FileAVSAMHandler(temp_filename)
            handler1.open_file()
            record = FileARecord(numb="000001", name="John Doe")
            handler1.write_record(record)
            handler1.close_file()
            
            # Read data back
            handler2 = FileAVSAMHandler(temp_filename)
            handler2.open_file()
            read_record = handler2.read_record("000001")
            
            assert read_record is not None
            assert read_record.name == "John Doe"
            handler2.close_file()
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)


# ============================================================================
# Test Process Request
# ============================================================================

class TestProcessRequest:
    """Test process_request function."""
    
    def test_get_success(self):
        """Test GET request (success)."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            # Write a record first
            record = FileARecord(numb="000001", name="John Doe")
            handler.write_record(record)
            
            # GET request
            request = RequestData(request_type="G", numb="000001")
            result_record, status_code, message = process_request("G", request, handler)
            
            assert status_code == "00"
            assert "GET successful" in message
            assert result_record.numb == "000001"
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_get_not_found(self):
        """Test GET request (not found)."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            request = RequestData(request_type="G", numb="000999")
            result_record, status_code, message = process_request("G", request, handler)
            
            assert status_code == "23"
            assert "No record found" in message
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_post_success(self):
        """Test POST request (success)."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            request = RequestData(request_type="P", numb="000001", name="John Doe")
            result_record, status_code, message = process_request("P", request, handler)
            
            assert status_code == "00"
            assert "POST successful" in message
            
            # Verify record was written
            read_record = handler.read_record("000001")
            assert read_record is not None
            assert read_record.name == "John Doe"
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_post_duplicate(self):
        """Test POST request (duplicate)."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            # Write first record
            record = FileARecord(numb="000001", name="John Doe")
            handler.write_record(record)
            
            # Try to POST duplicate
            request = RequestData(request_type="P", numb="000001", name="Jane Doe")
            result_record, status_code, message = process_request("P", request, handler)
            
            assert status_code == "22"
            assert "Duplicate record" in message
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_put_success(self):
        """Test PUT request (success)."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            # Write initial record
            record = FileARecord(numb="000001", name="John Doe")
            handler.write_record(record)
            
            # PUT (update) request
            request = RequestData(request_type="U", numb="000001", name="Jane Doe")
            result_record, status_code, message = process_request("U", request, handler)
            
            assert status_code == "00"
            assert "PUT successful" in message
            
            # Verify update
            read_record = handler.read_record("000001")
            assert read_record.name == "Jane Doe"
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_put_not_found(self):
        """Test PUT request (not found)."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            request = RequestData(request_type="U", numb="000999", name="John Doe")
            result_record, status_code, message = process_request("U", request, handler)
            
            assert status_code == "23"
            assert "PUT unsuccessful" in message
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_delete_success(self):
        """Test DELETE request (success)."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            # Write record first
            record = FileARecord(numb="000001", name="John Doe")
            handler.write_record(record)
            
            # DELETE request
            request = RequestData(request_type="D", numb="000001")
            result_record, status_code, message = process_request("D", request, handler)
            
            assert status_code == "00"
            assert "DELETE successful" in message
            
            # Verify deletion
            read_record = handler.read_record("000001")
            assert read_record is None
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_delete_not_found(self):
        """Test DELETE request (not found)."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            request = RequestData(request_type="D", numb="000999")
            result_record, status_code, message = process_request("D", request, handler)
            
            assert status_code == "23"
            assert "No record found" in message
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_unknown_verb(self):
        """Test unknown HTTP verb."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            
            request = RequestData(request_type="X", numb="000001")
            result_record, status_code, message = process_request("X", request, handler)
            
            assert status_code == "99"
            assert "Unknown action" in message
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_with_get_request(self):
        """Test main with GET request."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            # Setup: write a record first
            handler_setup = FileAVSAMHandler(temp_filename)
            handler_setup.open_file()
            record = FileARecord(numb="000001", name="John Doe")
            handler_setup.write_record(record)
            handler_setup.close_file()
            
            # Mock request callback
            request_count = [0]
            def mock_request(call_count):
                if call_count == 0:
                    return RequestData(request_type="G", numb="000001")
                return None  # Stop after first request
            
            result = main(
                filea_filename=temp_filename,
                stop_flag=False,
                mock_request_callback=mock_request
            )
            
            assert result == 0
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_main_with_post_request(self):
        """Test main with POST request."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            def mock_request(call_count):
                if call_count == 0:
                    return RequestData(request_type="P", numb="000001", name="John Doe")
                return None
            
            result = main(
                filea_filename=temp_filename,
                stop_flag=False,
                mock_request_callback=mock_request
            )
            
            assert result == 0
            
            # Verify record was written
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            record = handler.read_record("000001")
            assert record is not None
            assert record.name == "John Doe"
            handler.close_file()
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_main_with_put_request(self):
        """Test main with PUT request."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            # Setup: write initial record
            handler_setup = FileAVSAMHandler(temp_filename)
            handler_setup.open_file()
            record = FileARecord(numb="000001", name="John Doe")
            handler_setup.write_record(record)
            handler_setup.close_file()
            
            def mock_request(call_count):
                if call_count == 0:
                    return RequestData(request_type="U", numb="000001", name="Jane Doe")
                return None
            
            result = main(
                filea_filename=temp_filename,
                stop_flag=False,
                mock_request_callback=mock_request
            )
            
            assert result == 0
            
            # Verify update
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            record = handler.read_record("000001")
            assert record.name == "Jane Doe"
            handler.close_file()
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_main_with_delete_request(self):
        """Test main with DELETE request."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            # Setup: write record first
            handler_setup = FileAVSAMHandler(temp_filename)
            handler_setup.open_file()
            record = FileARecord(numb="000001", name="John Doe")
            handler_setup.write_record(record)
            handler_setup.close_file()
            
            def mock_request(call_count):
                if call_count == 0:
                    return RequestData(request_type="D", numb="000001")
                return None
            
            result = main(
                filea_filename=temp_filename,
                stop_flag=False,
                mock_request_callback=mock_request
            )
            
            assert result == 0
            
            # Verify deletion
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            record = handler.read_record("000001")
            assert record is None
            handler.close_file()
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_main_with_unknown_verb(self):
        """Test main with unknown HTTP verb (stops loop)."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            def mock_request(call_count):
                if call_count == 0:
                    return RequestData(request_type="X", numb="000001")
                return None
            
            result = main(
                filea_filename=temp_filename,
                stop_flag=False,
                mock_request_callback=mock_request
            )
            
            assert result == 0
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_main_multiple_requests(self):
        """Test main with multiple requests."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            request_count = [0]
            def mock_request(call_count):
                if call_count == 0:
                    # POST
                    return RequestData(request_type="P", numb="000001", name="John Doe")
                elif call_count == 1:
                    # GET
                    return RequestData(request_type="G", numb="000001")
                elif call_count == 2:
                    # PUT
                    return RequestData(request_type="U", numb="000001", name="Jane Doe")
                elif call_count == 3:
                    # DELETE
                    return RequestData(request_type="D", numb="000001")
                return None
            
            result = main(
                filea_filename=temp_filename,
                stop_flag=False,
                mock_request_callback=mock_request
            )
            
            assert result == 0
            
            # Verify final state (record deleted)
            handler = FileAVSAMHandler(temp_filename)
            handler.open_file()
            record = handler.read_record("000001")
            assert record is None
            handler.close_file()
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)


# ============================================================================
# Test Clear Fields
# ============================================================================

class TestClearFields:
    """Test clear_fields function."""
    
    def test_clear_fields(self):
        """Test clearing request and response fields."""
        request = RequestData(
            request_type="G",
            numb="000001",
            name="John Doe"
        )
        response = ResponseData(
            vsam_status_code="00",
            results_message="GET successful",
            numb="000001"
        )
        
        clear_fields(request, response)
        
        assert request.request_type == ""
        assert request.numb == ""
        assert request.name == ""
        assert response.vsam_status_code == "00"  # Status code not cleared
        assert response.results_message == ""
        assert response.numb == ""

