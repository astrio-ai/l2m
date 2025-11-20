"""
Test suite for CSCVINC.py

Tests for CICS FILEA operations via channels and containers.
"""

import pytest
from CSCVINC import (
    FileAArea,
    RequestContainer,
    ResponseContainer,
    CICSChannel,
    CICSFileHandler,
    CSMTQueueHandler,
    process_filea_operation,
    get_and_format_current_time,
    write_to_csmt_queue,
    serialize_request_container,
    deserialize_request_container,
    serialize_response_container,
    deserialize_response_container,
    main,
    DFHRESP_NORMAL,
    DFHRESP_NOTFND,
    DFHRESP_DUPKEY,
    DFHRESP_CONTAINERERR
)


# ============================================================================
# Test FileA Area
# ============================================================================

class TestFileAArea:
    """Test FileAArea structure."""
    
    def test_filea_area_creation(self):
        """Test creating FileA area."""
        filea = FileAArea(
            stat="A",
            numb="000001",
            name="John Doe"
        )
        
        assert filea.stat == "A"
        assert filea.numb == "000001"
        assert filea.name == "John Doe"
    
    def test_filea_area_to_string(self):
        """Test converting FileA area to string."""
        filea = FileAArea(
            numb="000001",
            name="John Doe"
        )
        filea_str = filea.to_string()
        
        assert len(filea_str) == 80
        assert "000001" in filea_str
    
    def test_filea_area_from_string(self):
        """Test creating FileA area from string."""
        # Create properly formatted string
        filea = FileAArea(
            stat="A",
            numb="000001",
            name="John Doe",
            addrx="123 Main St",
            phone="5551234",
            datex="20250101",
            amount="100.00",
            comment="Test"
        )
        filea_str = filea.to_string()
        
        # Parse it back
        parsed_filea = FileAArea.from_string(filea_str)
        
        assert parsed_filea.stat == "A"
        assert parsed_filea.numb == "000001"
        assert parsed_filea.name.strip() == "John Doe"
        assert parsed_filea.addrx.strip() == "123 Main St"


# ============================================================================
# Test Request/Response Containers
# ============================================================================

class TestContainers:
    """Test request/response container structures."""
    
    def test_request_container_creation(self):
        """Test creating request container."""
        request = RequestContainer(
            action="R",
            numb="000001",
            filea_area=FileAArea(numb="000001", name="John Doe")
        )
        
        assert request.action == "R"
        assert request.numb == "000001"
        assert request.filea_area.numb == "000001"
    
    def test_response_container_creation(self):
        """Test creating response container."""
        response = ResponseContainer(
            userid="TESTUSER",
            action="R",
            numb="000001"
        )
        
        assert response.userid == "TESTUSER"
        assert response.action == "R"
        assert response.numb == "000001"


# ============================================================================
# Test Serialization
# ============================================================================

class TestSerialization:
    """Test container serialization/deserialization."""
    
    def test_serialize_request_container(self):
        """Test serializing request container."""
        request = RequestContainer(
            action="R",
            numb="000001",
            filea_area=FileAArea(numb="000001", name="John Doe")
        )
        data = serialize_request_container(request)
        
        assert len(data) >= 87
        assert b"R" in data
        assert b"000001" in data
    
    def test_deserialize_request_container(self):
        """Test deserializing request container."""
        request = RequestContainer(
            action="R",
            numb="000001",
            filea_area=FileAArea(numb="000001", name="John Doe")
        )
        data = serialize_request_container(request)
        deserialized = deserialize_request_container(data)
        
        assert deserialized.action == "R"
        assert deserialized.numb == "000001"
        assert deserialized.filea_area.numb == "000001"
    
    def test_serialize_response_container(self):
        """Test serializing response container."""
        response = ResponseContainer(
            userid="TESTUSER",
            action="R",
            numb="000001",
            filea_area=FileAArea(numb="000001", name="John Doe")
        )
        data = serialize_response_container(response)
        
        assert len(data) >= 95
        assert b"TESTUSER" in data
        assert b"R" in data
    
    def test_deserialize_response_container(self):
        """Test deserializing response container."""
        response = ResponseContainer(
            userid="TESTUSER",
            action="R",
            numb="000001",
            filea_area=FileAArea(numb="000001", name="John Doe")
        )
        data = serialize_response_container(response)
        deserialized = deserialize_response_container(data)
        
        assert deserialized.userid == "TESTUSER"
        assert deserialized.action == "R"
        assert deserialized.numb == "000001"


# ============================================================================
# Test CICS Channel
# ============================================================================

class TestCICSChannel:
    """Test CICS channel operations."""
    
    def test_startbrowse_container(self):
        """Test starting container browse."""
        channel = CICSChannel()
        channel.put_container("TESTCHAN", "CONTAINER1", b"data")
        
        token, resp, resp2 = channel.startbrowse_container("TESTCHAN")
        
        assert resp == DFHRESP_NORMAL
        assert token > 0
    
    def test_getnext_container(self):
        """Test getting next container."""
        channel = CICSChannel()
        channel.put_container("TESTCHAN", "CONTAINER1", b"data1")
        channel.put_container("TESTCHAN", "CONTAINER2", b"data2")
        
        token, resp, resp2 = channel.startbrowse_container("TESTCHAN")
        container_name, resp, resp2 = channel.getnext_container(token)
        
        assert resp == DFHRESP_NORMAL
        assert container_name in ["CONTAINER1", "CONTAINER2"]
    
    def test_get_container_flength(self):
        """Test getting container length."""
        channel = CICSChannel()
        channel.put_container("TESTCHAN", "CONTAINER1", b"test data")
        
        length, resp, resp2 = channel.get_container_flength("TESTCHAN", "CONTAINER1")
        
        assert resp == DFHRESP_NORMAL
        assert length == 9
    
    def test_get_container(self):
        """Test getting container data."""
        channel = CICSChannel()
        channel.put_container("TESTCHAN", "CONTAINER1", b"test data")
        
        data, resp, resp2 = channel.get_container("TESTCHAN", "CONTAINER1")
        
        assert resp == DFHRESP_NORMAL
        assert data == b"test data"
    
    def test_get_container_not_found(self):
        """Test getting non-existent container."""
        channel = CICSChannel()
        
        data, resp, resp2 = channel.get_container("TESTCHAN", "MISSING")
        
        assert resp == DFHRESP_CONTAINERERR


# ============================================================================
# Test CICS File Handler
# ============================================================================

class TestCICSFileHandler:
    """Test CICS file handler operations."""
    
    def test_read_file(self):
        """Test reading file record."""
        handler = CICSFileHandler()
        record = FileAArea(numb="000001", name="John Doe")
        handler.add_record(record)
        
        read_record, resp, resp2 = handler.read_file("000001")
        
        assert resp == DFHRESP_NORMAL
        assert read_record is not None
        assert read_record.numb == "000001"
    
    def test_read_file_not_found(self):
        """Test reading non-existent record."""
        handler = CICSFileHandler()
        
        read_record, resp, resp2 = handler.read_file("000999")
        
        assert resp == DFHRESP_NOTFND
        assert read_record is None
    
    def test_write_file(self):
        """Test writing file record."""
        handler = CICSFileHandler()
        record = FileAArea(numb="000001", name="John Doe")
        
        resp, resp2 = handler.write_file(record)
        
        assert resp == DFHRESP_NORMAL
        read_record, resp2, resp3 = handler.read_file("000001")
        assert read_record is not None
    
    def test_write_file_duplicate(self):
        """Test writing duplicate record."""
        handler = CICSFileHandler()
        record = FileAArea(numb="000001", name="John Doe")
        handler.write_file(record)
        
        resp, resp2 = handler.write_file(record)  # Duplicate
        
        assert resp == DFHRESP_DUPKEY
    
    def test_rewrite_file(self):
        """Test rewriting file record."""
        handler = CICSFileHandler()
        record1 = FileAArea(numb="000001", name="John Doe")
        handler.write_file(record1)
        handler.read_file("000001", update=True)  # Lock for update
        
        record2 = FileAArea(numb="000001", name="Jane Doe")
        resp, resp2 = handler.rewrite_file(record2)
        
        assert resp == DFHRESP_NORMAL
        read_record, resp2, resp3 = handler.read_file("000001")
        assert read_record.name == "Jane Doe"
    
    def test_delete_file(self):
        """Test deleting file record."""
        handler = CICSFileHandler()
        record = FileAArea(numb="000001", name="John Doe")
        handler.write_file(record)
        
        resp, resp2 = handler.delete_file("000001")
        
        assert resp == DFHRESP_NORMAL
        read_record, resp2, resp3 = handler.read_file("000001")
        assert read_record is None


# ============================================================================
# Test Process FileA Operation
# ============================================================================

class TestProcessFileAOperation:
    """Test process_filea_operation function."""
    
    def test_delete_operation(self):
        """Test DELETE operation."""
        handler = CICSFileHandler()
        record = FileAArea(numb="000001", name="John Doe")
        handler.add_record(record)
        request = RequestContainer(action="D", numb="000001", filea_area=record)
        csmt_handler = CSMTQueueHandler()
        
        filea_area, resp, resp2, error_msg = process_filea_operation(
            "D",
            request,
            handler
        )
        
        assert resp == DFHRESP_NORMAL
        assert error_msg == ""
        # Verify record was deleted
        read_record, resp2, resp3 = handler.read_file("000001")
        assert read_record is None
    
    def test_insert_operation(self):
        """Test INSERT operation."""
        handler = CICSFileHandler()
        record = FileAArea(numb="000001", name="John Doe")
        request = RequestContainer(action="I", numb="000001", filea_area=record)
        csmt_handler = CSMTQueueHandler()
        
        filea_area, resp, resp2, error_msg = process_filea_operation(
            "I",
            request,
            handler
        )
        
        assert resp == DFHRESP_NORMAL
        assert error_msg == ""
        # Verify record was written
        read_record, resp2, resp3 = handler.read_file("000001")
        assert read_record is not None
        assert read_record.name == "John Doe"
    
    def test_update_operation(self):
        """Test UPDATE operation."""
        handler = CICSFileHandler()
        record1 = FileAArea(numb="000001", name="John Doe")
        handler.add_record(record1)
        
        record2 = FileAArea(numb="000001", name="Jane Doe")
        request = RequestContainer(action="U", numb="000001", filea_area=record2)
        csmt_handler = CSMTQueueHandler()
        
        filea_area, resp, resp2, error_msg = process_filea_operation(
            "U",
            request,
            handler
        )
        
        assert resp == DFHRESP_NORMAL
        assert error_msg == ""
        # Verify record was updated
        read_record, resp2, resp3 = handler.read_file("000001")
        assert read_record.name == "Jane Doe"
    
    def test_read_operation(self):
        """Test READ operation."""
        handler = CICSFileHandler()
        record = FileAArea(numb="000001", name="John Doe")
        handler.add_record(record)
        request = RequestContainer(action="R", numb="000001")
        csmt_handler = CSMTQueueHandler()
        
        filea_area, resp, resp2, error_msg = process_filea_operation(
            "R",
            request,
            handler
        )
        
        assert resp == DFHRESP_NORMAL
        assert error_msg == ""
        assert filea_area.name == "John Doe"
    
    def test_read_operation_not_found(self):
        """Test READ operation with non-existent record."""
        handler = CICSFileHandler()
        request = RequestContainer(action="R", numb="000999")
        csmt_handler = CSMTQueueHandler()
        
        filea_area, resp, resp2, error_msg = process_filea_operation(
            "R",
            request,
            handler
        )
        
        assert resp == DFHRESP_NOTFND
        assert "Error reading record" in error_msg


# ============================================================================
# Test CSMT Queue
# ============================================================================

class TestCSMTQueue:
    """Test CSMT queue operations."""
    
    def test_write_to_csmt_queue(self):
        """Test writing to CSMT queue."""
        handler = CSMTQueueHandler()
        
        resp = write_to_csmt_queue("Test message", handler)
        
        assert resp == 0
        messages = handler.get_messages()
        assert len(messages) == 1
        assert "CSCVINC" in messages[0]
        assert "Test message" in messages[0]
    
    def test_get_and_format_current_time(self):
        """Test getting and formatting current time."""
        current_date, current_time = get_and_format_current_time()
        
        assert "/" in current_date
        assert ":" in current_time
        assert len(current_date) == 10  # YYYY/MM/DD
        assert len(current_time) == 8  # HH:MM:SS


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_success_read(self):
        """Test main with successful READ operation."""
        channel = CICSChannel()
        file_handler = CICSFileHandler()
        csmt_handler = CSMTQueueHandler()
        
        # Add record to file
        record = FileAArea(numb="000001", name="John Doe", addrx="123 Main St")
        file_handler.add_record(record)
        
        # Create request container
        request = RequestContainer(action="R", numb="000001", filea_area=record)
        request_data = serialize_request_container(request)
        
        # Put request in channel
        channel.put_container("TESTCHAN", "REQUEST", request_data)
        
        result_code, error_msg = main(
            channel_name="TESTCHAN",
            cics_channel=channel,
            file_handler=file_handler,
            csmt_handler=csmt_handler,
            userid="TESTUSER"
        )
        
        assert result_code == 0
        assert error_msg == ""
        
        # Verify response was written to channel
        response_data, resp, resp2 = channel.get_container("TESTCHAN", "REQUEST")
        assert response_data is not None
        response = deserialize_response_container(response_data)
        assert response.userid == "TESTUSER"
        assert response.numb == "000001"
        assert response.filea_area.name == "John Doe"
    
    def test_main_success_insert(self):
        """Test main with successful INSERT operation."""
        channel = CICSChannel()
        file_handler = CICSFileHandler()
        csmt_handler = CSMTQueueHandler()
        
        # Create request container
        record = FileAArea(numb="000001", name="John Doe")
        request = RequestContainer(action="I", numb="000001", filea_area=record)
        request_data = serialize_request_container(request)
        
        # Put request in channel
        channel.put_container("TESTCHAN", "REQUEST", request_data)
        
        result_code, error_msg = main(
            channel_name="TESTCHAN",
            cics_channel=channel,
            file_handler=file_handler,
            csmt_handler=csmt_handler
        )
        
        assert result_code == 0
        
        # Verify record was written
        read_record, resp, resp2 = file_handler.read_file("000001")
        assert read_record is not None
        assert read_record.name == "John Doe"
    
    def test_main_success_update(self):
        """Test main with successful UPDATE operation."""
        channel = CICSChannel()
        file_handler = CICSFileHandler()
        csmt_handler = CSMTQueueHandler()
        
        # Add initial record
        record1 = FileAArea(numb="000001", name="John Doe")
        file_handler.add_record(record1)
        
        # Create update request
        record2 = FileAArea(numb="000001", name="Jane Doe")
        request = RequestContainer(action="U", numb="000001", filea_area=record2)
        request_data = serialize_request_container(request)
        
        # Put request in channel
        channel.put_container("TESTCHAN", "REQUEST", request_data)
        
        result_code, error_msg = main(
            channel_name="TESTCHAN",
            cics_channel=channel,
            file_handler=file_handler,
            csmt_handler=csmt_handler
        )
        
        assert result_code == 0
        
        # Verify record was updated
        read_record, resp, resp2 = file_handler.read_file("000001")
        assert read_record.name == "Jane Doe"
    
    def test_main_success_delete(self):
        """Test main with successful DELETE operation."""
        channel = CICSChannel()
        file_handler = CICSFileHandler()
        csmt_handler = CSMTQueueHandler()
        
        # Add record
        record = FileAArea(numb="000001", name="John Doe")
        file_handler.add_record(record)
        
        # Create delete request
        request = RequestContainer(action="D", numb="000001", filea_area=record)
        request_data = serialize_request_container(request)
        
        # Put request in channel
        channel.put_container("TESTCHAN", "REQUEST", request_data)
        
        result_code, error_msg = main(
            channel_name="TESTCHAN",
            cics_channel=channel,
            file_handler=file_handler,
            csmt_handler=csmt_handler
        )
        
        assert result_code == 0
        
        # Verify record was deleted
        read_record, resp, resp2 = file_handler.read_file("000001")
        assert read_record is None
    
    def test_main_no_channel(self):
        """Test main with no channel."""
        channel = CICSChannel()
        file_handler = CICSFileHandler()
        csmt_handler = CSMTQueueHandler()
        
        result_code, error_msg = main(
            channel_name="",
            cics_channel=channel,
            file_handler=file_handler,
            csmt_handler=csmt_handler
        )
        
        assert result_code == 1
        assert "No channel provided" in error_msg
    
    def test_main_read_not_found(self):
        """Test main with READ operation - record not found."""
        channel = CICSChannel()
        file_handler = CICSFileHandler()
        csmt_handler = CSMTQueueHandler()
        
        # Create request for non-existent record
        record = FileAArea(numb="000999")
        request = RequestContainer(action="R", numb="000999", filea_area=record)
        request_data = serialize_request_container(request)
        
        # Put request in channel
        channel.put_container("TESTCHAN", "REQUEST", request_data)
        
        result_code, error_msg = main(
            channel_name="TESTCHAN",
            cics_channel=channel,
            file_handler=file_handler,
            csmt_handler=csmt_handler
        )
        
        assert result_code == 0  # Main completes but error is logged
        # Check that error message was written to CSMT queue
        messages = csmt_handler.get_messages()
        assert len(messages) > 0
        assert "Error reading record" in messages[0]
    
    def test_main_container_error(self):
        """Test main with container error."""
        channel = CICSChannel()
        file_handler = CICSFileHandler()
        csmt_handler = CSMTQueueHandler()
        
        # Start browse on non-existent channel
        result_code, error_msg = main(
            channel_name="MISSING",
            cics_channel=channel,
            file_handler=file_handler,
            csmt_handler=csmt_handler
        )
        
        assert result_code == 1
        assert "STARTBROWSE" in error_msg or "GETNEXT" in error_msg


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_insert_read_update_delete(self):
        """Test full flow: INSERT -> READ -> UPDATE -> DELETE."""
        channel = CICSChannel()
        file_handler = CICSFileHandler()
        csmt_handler = CSMTQueueHandler()
        
        # 1. INSERT
        record1 = FileAArea(numb="000001", name="John Doe", addrx="123 Main St")
        request1 = RequestContainer(action="I", numb="000001", filea_area=record1)
        request_data1 = serialize_request_container(request1)
        channel.put_container("TESTCHAN", "REQUEST", request_data1)
        
        result_code, error_msg = main(
            channel_name="TESTCHAN",
            cics_channel=channel,
            file_handler=file_handler,
            csmt_handler=csmt_handler
        )
        assert result_code == 0
        
        # 2. READ
        channel2 = CICSChannel()
        request2 = RequestContainer(action="R", numb="000001")
        request_data2 = serialize_request_container(request2)
        channel2.put_container("TESTCHAN2", "REQUEST", request_data2)
        
        result_code, error_msg = main(
            channel_name="TESTCHAN2",
            cics_channel=channel2,
            file_handler=file_handler,
            csmt_handler=csmt_handler
        )
        assert result_code == 0
        response_data, resp, resp2 = channel2.get_container("TESTCHAN2", "REQUEST")
        response = deserialize_response_container(response_data)
        assert response.filea_area.name == "John Doe"
        
        # 3. UPDATE
        channel3 = CICSChannel()
        record3 = FileAArea(numb="000001", name="Jane Doe", addrx="456 Oak Ave")
        request3 = RequestContainer(action="U", numb="000001", filea_area=record3)
        request_data3 = serialize_request_container(request3)
        channel3.put_container("TESTCHAN3", "REQUEST", request_data3)
        
        result_code, error_msg = main(
            channel_name="TESTCHAN3",
            cics_channel=channel3,
            file_handler=file_handler,
            csmt_handler=csmt_handler
        )
        assert result_code == 0
        
        # 4. DELETE
        channel4 = CICSChannel()
        record4 = FileAArea(numb="000001")
        request4 = RequestContainer(action="D", numb="000001", filea_area=record4)
        request_data4 = serialize_request_container(request4)
        channel4.put_container("TESTCHAN4", "REQUEST", request_data4)
        
        result_code, error_msg = main(
            channel_name="TESTCHAN4",
            cics_channel=channel4,
            file_handler=file_handler,
            csmt_handler=csmt_handler
        )
        assert result_code == 0
        
        # Verify record was deleted
        read_record, resp, resp2 = file_handler.read_file("000001")
        assert read_record is None

