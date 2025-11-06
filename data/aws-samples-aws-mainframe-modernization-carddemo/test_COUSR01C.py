"""
Test suite for COUSR01C.py

Tests for Add User transaction program.
"""

import pytest
from unittest.mock import patch

from COUSR01C import (
    UserAddMap,
    UserSecFileHandler,
    WorkingStorage,
    CardDemoCommarea,
    UserSecurityRecord,
    populate_header_info,
    send_usradd_screen,
    receive_usradd_screen,
    initialize_all_fields,
    write_user_sec_file,
    process_enter_key,
    clear_current_screen,
    return_to_prev_screen,
    main,
    WS_PGMNAME,
    WS_TRANID,
    DFHENTER,
    DFHPF3,
    DFHPF4,
    COSGN00C,
    COADM01C,
    DFHRESP_NORMAL,
    DFHRESP_DUPKEY
)


# ============================================================================
# Test UserSecFileHandler Write
# ============================================================================

class TestUserSecFileHandlerWrite:
    """Test UserSecFileHandler write operations."""
    
    def test_write_record_success(self):
        """Test writing a new user record."""
        import tempfile
        import os
        
        # Use unique temp file
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            handler = UserSecFileHandler(temp_filename)
            handler.open_file()  # Will create empty file
            
            record = UserSecurityRecord(
                user_id="USER001",
                first_name="John",
                last_name="Doe",
                password="PASSWORD",
                user_type="A"
            )
            
            resp_cd = handler.write_record(record)
            assert resp_cd == DFHRESP_NORMAL
            assert "USER001" in handler._data
        finally:
            # Clean up temp file
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_write_record_duplicate(self):
        """Test writing a duplicate user ID."""
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.open_file()
        
        record1 = UserSecurityRecord(user_id="USER001", first_name="John")
        handler.write_record(record1)
        
        record2 = UserSecurityRecord(user_id="USER001", first_name="Jane")
        resp_cd = handler.write_record(record2)
        assert resp_cd == DFHRESP_DUPKEY


# ============================================================================
# Test Map Functions
# ============================================================================

class TestMapFunctions:
    """Test map functions."""
    
    def test_populate_header_info(self):
        """Test populating header info."""
        ws = WorkingStorage()
        map_out = UserAddMap()
        
        populate_header_info(ws, map_out)
        
        assert map_out.title01o != ""
        assert map_out.trnnameo == WS_TRANID
        assert map_out.pgmnameo == WS_PGMNAME
    
    def test_initialize_all_fields(self):
        """Test initializing all fields."""
        ws = WorkingStorage()
        map_in = UserAddMap()
        map_in.useridi = "USER001"
        map_in.fnamei = "John"
        ws.message = "Test message"
        
        initialize_all_fields(ws, map_in)
        
        assert map_in.useridi == ""
        assert map_in.fnamei == ""
        assert ws.message == ""
        assert map_in.fnamel == -1


# ============================================================================
# Test File Operations
# ============================================================================

class TestFileOperations:
    """Test file operations."""
    
    def test_write_user_sec_file_success(self):
        """Test successful write of user."""
        import tempfile
        import os
        
        # Use unique temp file
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            ws = WorkingStorage()
            handler = UserSecFileHandler(temp_filename)
            handler.open_file()  # Will create empty file
            ws.usrsec_handler = handler
            
            record = UserSecurityRecord(
                user_id="USER001",
                first_name="John",
                last_name="Doe",
                password="PASSWORD",
                user_type="A"
            )
            
            result = write_user_sec_file(ws, record)
            
            assert result is True
            assert ws.err_flg is False
            assert "USER001" in handler._data
        finally:
            # Clean up temp file
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_write_user_sec_file_duplicate(self):
        """Test write of duplicate user ID."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(user_id="USER001", first_name="John"))
        handler.open_file()
        ws.usrsec_handler = handler
        
        record = UserSecurityRecord(user_id="USER001", first_name="Jane")
        
        result = write_user_sec_file(ws, record)
        
        assert result is False
        assert ws.err_flg is True
        assert "already exist" in ws.message


# ============================================================================
# Test Process Enter Key
# ============================================================================

class TestProcessEnterKey:
    """Test process_enter_key function."""
    
    def test_missing_first_name(self):
        """Test processing with missing first name."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.open_file()
        ws.usrsec_handler = handler
        map_in = UserAddMap()
        map_in.fnamei = ""  # Missing
        map_in.lnamei = "Doe"
        map_in.useridi = "USER001"
        map_in.passwdi = "PASSWORD"
        map_in.usrtypei = "A"
        map_out = UserAddMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "First Name" in ws.message
    
    def test_missing_last_name(self):
        """Test processing with missing last name."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.open_file()
        ws.usrsec_handler = handler
        map_in = UserAddMap()
        map_in.fnamei = "John"
        map_in.lnamei = ""  # Missing
        map_in.useridi = "USER001"
        map_in.passwdi = "PASSWORD"
        map_in.usrtypei = "A"
        map_out = UserAddMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "Last Name" in ws.message
    
    def test_missing_user_id(self):
        """Test processing with missing user ID."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.open_file()
        ws.usrsec_handler = handler
        map_in = UserAddMap()
        map_in.fnamei = "John"
        map_in.lnamei = "Doe"
        map_in.useridi = ""  # Missing
        map_in.passwdi = "PASSWORD"
        map_in.usrtypei = "A"
        map_out = UserAddMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "User ID" in ws.message
    
    def test_missing_password(self):
        """Test processing with missing password."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.open_file()
        ws.usrsec_handler = handler
        map_in = UserAddMap()
        map_in.fnamei = "John"
        map_in.lnamei = "Doe"
        map_in.useridi = "USER001"
        map_in.passwdi = ""  # Missing
        map_in.usrtypei = "A"
        map_out = UserAddMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "Password" in ws.message
    
    def test_missing_user_type(self):
        """Test processing with missing user type."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.open_file()
        ws.usrsec_handler = handler
        map_in = UserAddMap()
        map_in.fnamei = "John"
        map_in.lnamei = "Doe"
        map_in.useridi = "USER001"
        map_in.passwdi = "PASSWORD"
        map_in.usrtypei = ""  # Missing
        map_out = UserAddMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "User Type" in ws.message
    
    def test_valid_user(self):
        """Test processing with valid user data."""
        import tempfile
        import os
        
        # Use unique temp file
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            ws = WorkingStorage()
            handler = UserSecFileHandler(temp_filename)
            handler.open_file()  # Will create empty file
            ws.usrsec_handler = handler
            map_in = UserAddMap()
            map_in.fnamei = "John"
            map_in.lnamei = "Doe"
            map_in.useridi = "USER001"
            map_in.passwdi = "PASSWORD"
            map_in.usrtypei = "A"
            map_out = UserAddMap()
            
            result = process_enter_key(ws, map_in, map_out)
            
            assert result is True
            assert ws.err_flg is False
            assert "has been added" in ws.message
            assert "USER001" in ws.message
            assert handler._data.get("USER001") is not None
        finally:
            # Clean up temp file
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_duplicate_user_id(self):
        """Test processing with duplicate user ID."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(
            user_id="USER001",
            first_name="Existing",
            last_name="User",
            password="PASSWORD",
            user_type="A"
        ))
        handler.open_file()
        ws.usrsec_handler = handler
        map_in = UserAddMap()
        map_in.fnamei = "John"
        map_in.lnamei = "Doe"
        map_in.useridi = "USER001"  # Duplicate
        map_in.passwdi = "PASSWORD"
        map_in.usrtypei = "A"
        map_out = UserAddMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "already exist" in ws.message


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_first_entry(self):
        """Test main on first entry."""
        commarea = CardDemoCommarea()
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=0
        )
        
        assert xctl_prog == COSGN00C
    
    def test_main_first_screen_display(self):
        """Test main showing screen for first time."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100
        )
        
        assert result_commarea.pgm_reenter is True
        assert result_map.title01o != ""
        assert result_map.fnamel == -1
    
    def test_main_pf3_exit(self):
        """Test main with PF3."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF3
        )
        
        assert xctl_prog == COADM01C
    
    def test_main_pf4_clear_screen(self):
        """Test main with PF4."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = UserAddMap()
        map_in.useridi = "USER001"
        map_in.fnamei = "John"
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF4,
            map_input=map_in
        )
        
        assert xctl_prog is None
        assert result_map.useridi == ""
        assert result_map.fnamei == ""
    
    def test_main_enter_valid_user(self):
        """Test main with enter key and valid user."""
        import tempfile
        import os
        
        # Use unique temp file
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            commarea = CardDemoCommarea()
            commarea.pgm_reenter = True
            handler = UserSecFileHandler(temp_filename)
            handler.open_file()  # Will create empty file
            map_in = UserAddMap()
            map_in.fnamei = "John"
            map_in.lnamei = "Doe"
            map_in.useridi = "USER001"
            map_in.passwdi = "PASSWORD"
            map_in.usrtypei = "A"
            
            result_commarea, result_map, xctl_prog = main(
                commarea=commarea,
                eibcalen=100,
                eibaid=DFHENTER,
                map_input=map_in,
                usrsec_handler=handler
            )
            
            assert xctl_prog is None
            assert "has been added" in result_map.errmsgo
            assert "USER001" in result_map.errmsgo
            assert handler._data.get("USER001") is not None
        finally:
            # Clean up temp file
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_main_enter_missing_fields(self):
        """Test main with enter key and missing fields."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.open_file()
        map_in = UserAddMap()
        map_in.fnamei = ""  # Missing
        map_in.lnamei = "Doe"
        map_in.useridi = "USER001"
        map_in.passwdi = "PASSWORD"
        map_in.usrtypei = "A"
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            usrsec_handler=handler
        )
        
        assert xctl_prog is None
        assert "First Name" in result_map.errmsgo


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_add_user(self):
        """Test full flow: add user."""
        # First entry - show screen
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.clear_records()  # Ensure clean state
        handler.open_file()
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            usrsec_handler=handler
        )
        
        assert result_map.title01o != ""
        assert xctl_prog is None
        
        # Enter user data - reuse the same handler
        commarea2 = CardDemoCommarea()
        commarea2.pgm_reenter = True
        map_in = UserAddMap()
        map_in.fnamei = "John"
        map_in.lnamei = "Doe"
        map_in.useridi = "USER001"
        map_in.passwdi = "PASSWORD"
        map_in.usrtypei = "A"
        
        # Ensure handler is still clean (no data from file reload)
        handler._data = {}
        handler._sorted_keys = []
        
        result_commarea2, result_map2, xctl_prog2 = main(
            commarea=commarea2,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            usrsec_handler=handler
        )
        
        assert xctl_prog2 is None
        assert "has been added" in result_map2.errmsgo
        assert handler._data.get("USER001") is not None
        
        # Verify user was saved
        saved_record = handler.read_record("USER001")
        assert saved_record is not None
        assert saved_record.first_name == "John"
        assert saved_record.last_name == "Doe"
        assert saved_record.user_type == "A"
    
    def test_full_flow_duplicate_user(self):
        """Test full flow: try to add duplicate user."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(
            user_id="USER001",
            first_name="Existing",
            last_name="User",
            password="PASSWORD",
            user_type="A"
        ))
        handler.open_file()
        map_in = UserAddMap()
        map_in.fnamei = "John"
        map_in.lnamei = "Doe"
        map_in.useridi = "USER001"  # Duplicate
        map_in.passwdi = "PASSWORD"
        map_in.usrtypei = "A"
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            usrsec_handler=handler
        )
        
        assert xctl_prog is None
        assert "already exist" in result_map.errmsgo
    
    def test_full_flow_pf4_clear(self):
        """Test full flow: add user and clear screen."""
        import tempfile
        import os
        
        # Use unique temp file
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            commarea = CardDemoCommarea()
            commarea.pgm_reenter = True
            handler = UserSecFileHandler(temp_filename)
            handler.open_file()  # Will create empty file
            map_in = UserAddMap()
            map_in.fnamei = "John"
            map_in.lnamei = "Doe"
            map_in.useridi = "USER001"
            map_in.passwdi = "PASSWORD"
            map_in.usrtypei = "A"
            
            # First, add user
            result_commarea, result_map, xctl_prog = main(
                commarea=commarea,
                eibcalen=100,
                eibaid=DFHENTER,
                map_input=map_in,
                usrsec_handler=handler
            )
            
            assert "has been added" in result_map.errmsgo
            
            # Then, clear screen - reuse the same handler
            commarea2 = CardDemoCommarea()
            commarea2.pgm_reenter = True
            map_in2 = UserAddMap()
            map_in2.useridi = "TEST"
            map_in2.fnamei = "Test"
            
            result_commarea2, result_map2, xctl_prog2 = main(
                commarea=commarea2,
                eibcalen=100,
                eibaid=DFHPF4,
                map_input=map_in2,
                usrsec_handler=handler
            )
            
            assert xctl_prog2 is None
            assert result_map2.useridi == ""
            assert result_map2.fnamei == ""
        finally:
            # Clean up temp file
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)

