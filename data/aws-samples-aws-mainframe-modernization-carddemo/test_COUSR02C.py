"""
Test suite for COUSR02C.py

Tests for Update User transaction program.
"""

import pytest
from unittest.mock import patch
import tempfile
import os

from COUSR02C import (
    UserUpdateMap,
    ThisProgCommarea,
    UserSecFileHandler,
    WorkingStorage,
    CardDemoCommarea,
    UserSecurityRecord,
    populate_header_info,
    send_usrupd_screen,
    receive_usrupd_screen,
    initialize_all_fields,
    read_user_sec_file,
    update_user_sec_file,
    process_enter_key,
    update_user_info,
    clear_current_screen,
    return_to_prev_screen,
    main,
    WS_PGMNAME,
    WS_TRANID,
    DFHENTER,
    DFHPF3,
    DFHPF4,
    DFHPF5,
    DFHPF12,
    COSGN00C,
    COADM01C,
    DFHRESP_NORMAL,
    DFHRESP_NOTFND
)


# ============================================================================
# Test ThisProgCommarea
# ============================================================================

class TestThisProgCommarea:
    """Test ThisProgCommarea dataclass."""
    
    def test_commarea_creation(self):
        """Test creating program-specific commarea."""
        commarea = ThisProgCommarea()
        assert commarea.usr_selected == ""
        assert commarea.page_num == 0


# ============================================================================
# Test UserSecFileHandler Update
# ============================================================================

class TestUserSecFileHandlerUpdate:
    """Test UserSecFileHandler update operations."""
    
    def test_read_for_update_success(self):
        """Test reading a record for update."""
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(
            user_id="USER001",
            first_name="John",
            last_name="Doe",
            password="PASSWORD",
            user_type="A"
        ))
        handler.open_file()
        
        record = handler.read_for_update("USER001")
        
        assert record is not None
        assert record.user_id == "USER001"
        assert handler.resp_cd == DFHRESP_NORMAL
        assert "USER001" in handler._locked_records
    
    def test_read_for_update_not_found(self):
        """Test reading a non-existent record for update."""
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.open_file()
        
        record = handler.read_for_update("USER999")
        
        assert record is None
        assert handler.resp_cd == DFHRESP_NOTFND
    
    def test_rewrite_record_success(self):
        """Test rewriting a locked record."""
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(
            user_id="USER001",
            first_name="John",
            last_name="Doe",
            password="PASSWORD",
            user_type="A"
        ))
        handler.open_file()
        
        # Lock the record
        handler.read_for_update("USER001")
        
        # Update the record
        updated_record = UserSecurityRecord(
            user_id="USER001",
            first_name="Jane",
            last_name="Smith",
            password="NEWPASS",
            user_type="U"
        )
        
        resp_cd = handler.rewrite_record(updated_record)
        
        assert resp_cd == DFHRESP_NORMAL
        assert handler._data["USER001"].first_name == "Jane"
        assert "USER001" not in handler._locked_records
    
    def test_rewrite_record_not_locked(self):
        """Test rewriting a record that wasn't locked."""
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(
            user_id="USER001",
            first_name="John"
        ))
        handler.open_file()
        
        # Try to rewrite without locking
        updated_record = UserSecurityRecord(
            user_id="USER001",
            first_name="Jane"
        )
        
        resp_cd = handler.rewrite_record(updated_record)
        
        assert resp_cd == DFHRESP_NOTFND


# ============================================================================
# Test Map Functions
# ============================================================================

class TestMapFunctions:
    """Test map functions."""
    
    def test_populate_header_info(self):
        """Test populating header info."""
        ws = WorkingStorage()
        map_out = UserUpdateMap()
        
        populate_header_info(ws, map_out)
        
        assert map_out.title01o != ""
        assert map_out.trnnameo == WS_TRANID
        assert map_out.pgmnameo == WS_PGMNAME
    
    def test_initialize_all_fields(self):
        """Test initializing all fields."""
        ws = WorkingStorage()
        map_in = UserUpdateMap()
        map_in.useridi = "USER001"
        map_in.fnamei = "John"
        ws.message = "Test message"
        
        initialize_all_fields(ws, map_in)
        
        assert map_in.useridi == ""
        assert map_in.fnamei == ""
        assert ws.message == ""
        assert map_in.useridl == -1


# ============================================================================
# Test File Operations
# ============================================================================

class TestFileOperations:
    """Test file operations."""
    
    def test_read_user_sec_file_success(self):
        """Test successful read of user."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(
            user_id="USER001",
            first_name="John",
            last_name="Doe",
            password="PASSWORD",
            user_type="A"
        ))
        handler.open_file()
        ws.usrsec_handler = handler
        map_out = UserUpdateMap()
        
        result = read_user_sec_file(ws, "USER001", map_out)
        
        assert result is True
        assert ws.err_flg is False
        assert ws.sec_user_record is not None
        assert "Press PF5" in ws.message
        assert map_out.errmsgc == "NEUTR"
    
    def test_read_user_sec_file_not_found(self):
        """Test read of non-existent user."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.open_file()
        ws.usrsec_handler = handler
        map_out = UserUpdateMap()
        
        result = read_user_sec_file(ws, "USER999", map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "NOT found" in ws.message
    
    def test_update_user_sec_file_success(self):
        """Test successful update of user."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(
            user_id="USER001",
            first_name="John",
            last_name="Doe",
            password="PASSWORD",
            user_type="A"
        ))
        handler.open_file()
        handler.read_for_update("USER001")  # Lock the record
        ws.usrsec_handler = handler
        
        updated_record = UserSecurityRecord(
            user_id="USER001",
            first_name="Jane",
            last_name="Smith",
            password="NEWPASS",
            user_type="U"
        )
        
        result = update_user_sec_file(ws, updated_record)
        
        assert result is True
        assert ws.err_flg is False
        assert handler._data["USER001"].first_name == "Jane"


# ============================================================================
# Test Process Enter Key
# ============================================================================

class TestProcessEnterKey:
    """Test process_enter_key function."""
    
    def test_missing_user_id(self):
        """Test processing with missing user ID."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.open_file()
        ws.usrsec_handler = handler
        map_in = UserUpdateMap()
        map_in.useridi = ""  # Missing
        map_out = UserUpdateMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "User ID" in ws.message
    
    def test_valid_user_id(self):
        """Test processing with valid user ID."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(
            user_id="USER001",
            first_name="John",
            last_name="Doe",
            password="PASSWORD",
            user_type="A"
        ))
        handler.open_file()
        ws.usrsec_handler = handler
        map_in = UserUpdateMap()
        map_in.useridi = "USER001"
        map_out = UserUpdateMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is True
        assert ws.err_flg is False
        assert map_out.fnamei == "John"
        assert map_out.lnamei == "Doe"
        assert map_out.passwdi == "PASSWORD"
        assert map_out.usrtypei == "A"


# ============================================================================
# Test Update User Info
# ============================================================================

class TestUpdateUserInfo:
    """Test update_user_info function."""
    
    def test_missing_fields(self):
        """Test update with missing fields."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(
            user_id="USER001",
            first_name="John"
        ))
        handler.open_file()
        handler.read_for_update("USER001")
        ws.usrsec_handler = handler
        map_in = UserUpdateMap()
        map_in.useridi = "USER001"
        map_in.fnamei = ""  # Missing
        map_in.lnamei = "Doe"
        map_in.passwdi = "PASSWORD"
        map_in.usrtypei = "A"
        map_out = UserUpdateMap()
        
        result = update_user_info(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "First Name" in ws.message
    
    def test_no_changes(self):
        """Test update with no changes."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        original_record = UserSecurityRecord(
            user_id="USER001",
            first_name="John",
            last_name="Doe",
            password="PASSWORD",
            user_type="A"
        )
        handler.add_record(original_record)
        handler.open_file()
        handler.read_for_update("USER001")
        ws.usrsec_handler = handler
        map_in = UserUpdateMap()
        map_in.useridi = "USER001"
        map_in.fnamei = "John"  # Same
        map_in.lnamei = "Doe"  # Same
        map_in.passwdi = "PASSWORD"  # Same
        map_in.usrtypei = "A"  # Same
        map_out = UserUpdateMap()
        
        result = update_user_info(ws, map_in, map_out)
        
        assert result is False
        assert ws.usr_modified is False
        assert "modify to update" in ws.message.lower()
    
    def test_with_changes(self):
        """Test update with changes."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(
            user_id="USER001",
            first_name="John",
            last_name="Doe",
            password="PASSWORD",
            user_type="A"
        ))
        handler.open_file()
        handler.read_for_update("USER001")
        ws.usrsec_handler = handler
        map_in = UserUpdateMap()
        map_in.useridi = "USER001"
        map_in.fnamei = "Jane"  # Changed
        map_in.lnamei = "Smith"  # Changed
        map_in.passwdi = "NEWPASS"  # Changed
        map_in.usrtypei = "U"  # Changed
        map_out = UserUpdateMap()
        
        result = update_user_info(ws, map_in, map_out)
        
        assert result is True
        assert ws.usr_modified is True
        assert "has been updated" in ws.message
        assert handler._data["USER001"].first_name == "Jane"
        assert handler._data["USER001"].last_name == "Smith"


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_first_entry(self):
        """Test main on first entry."""
        commarea = CardDemoCommarea()
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=0
        )
        
        assert xctl_prog == COSGN00C
    
    def test_main_first_screen_display(self):
        """Test main showing screen for first time."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100
        )
        
        assert result_commarea.pgm_reenter is True
        assert result_map.title01o != ""
        assert result_map.useridl == -1
    
    def test_main_with_selected_user(self):
        """Test main with user selected from list."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(
            user_id="USER001",
            first_name="John",
            last_name="Doe",
            password="PASSWORD",
            user_type="A"
        ))
        handler.open_file()
        prog_commarea = ThisProgCommarea()
        prog_commarea.usr_selected = "USER001"
        
        result_commarea, result_map, xctl_prog, prog_commarea2 = main(
            commarea=commarea,
            eibcalen=100,
            prog_commarea=prog_commarea,
            usrsec_handler=handler
        )
        
        assert xctl_prog is None
        assert result_map.fnamei == "John"
        assert result_map.lnamei == "Doe"
    
    def test_main_pf3_exit(self):
        """Test main with PF3."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(
            user_id="USER001",
            first_name="John",
            last_name="Doe",
            password="PASSWORD",
            user_type="A"
        ))
        handler.open_file()
        handler.read_for_update("USER001")
        map_in = UserUpdateMap()
        map_in.useridi = "USER001"
        map_in.fnamei = "Jane"
        map_in.lnamei = "Smith"
        map_in.passwdi = "NEWPASS"
        map_in.usrtypei = "U"
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF3,
            map_input=map_in,
            usrsec_handler=handler
        )
        
        assert xctl_prog == COADM01C
    
    def test_main_pf4_clear_screen(self):
        """Test main with PF4."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = UserUpdateMap()
        map_in.useridi = "USER001"
        map_in.fnamei = "John"
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF4,
            map_input=map_in
        )
        
        assert xctl_prog is None
        assert result_map.useridi == ""
        assert result_map.fnamei == ""
    
    def test_main_pf5_update(self):
        """Test main with PF5."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(
            user_id="USER001",
            first_name="John",
            last_name="Doe",
            password="PASSWORD",
            user_type="A"
        ))
        handler.open_file()
        handler.read_for_update("USER001")
        map_in = UserUpdateMap()
        map_in.useridi = "USER001"
        map_in.fnamei = "Jane"
        map_in.lnamei = "Smith"
        map_in.passwdi = "NEWPASS"
        map_in.usrtypei = "U"
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF5,
            map_input=map_in,
            usrsec_handler=handler
        )
        
        assert xctl_prog is None
        assert "has been updated" in result_map.errmsgo
        assert handler._data["USER001"].first_name == "Jane"
    
    def test_main_pf12_exit(self):
        """Test main with PF12."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF12
        )
        
        assert xctl_prog == COADM01C


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_update_user(self):
        """Test full flow: update user."""
        # Create temp file
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            # First entry - show screen
            commarea = CardDemoCommarea()
            commarea.pgm_reenter = False
            handler = UserSecFileHandler(temp_filename)
            handler.add_record(UserSecurityRecord(
                user_id="USER001",
                first_name="John",
                last_name="Doe",
                password="PASSWORD",
                user_type="A"
            ))
            handler.open_file()
            
            result_commarea, result_map, xctl_prog, prog_commarea = main(
                commarea=commarea,
                eibcalen=100,
                usrsec_handler=handler
            )
            
            assert result_map.title01o != ""
            assert xctl_prog is None
            
            # Enter user ID
            commarea2 = CardDemoCommarea()
            commarea2.pgm_reenter = True
            map_in = UserUpdateMap()
            map_in.useridi = "USER001"
            
            result_commarea2, result_map2, xctl_prog2, prog_commarea2 = main(
                commarea=commarea2,
                eibcalen=100,
                eibaid=DFHENTER,
                map_input=map_in,
                usrsec_handler=handler
            )
            
            assert xctl_prog2 is None
            assert result_map2.fnamei == "John"
            
            # Update user
            commarea3 = CardDemoCommarea()
            commarea3.pgm_reenter = True
            map_in2 = UserUpdateMap()
            map_in2.useridi = "USER001"
            map_in2.fnamei = "Jane"
            map_in2.lnamei = "Smith"
            map_in2.passwdi = "NEWPASS"
            map_in2.usrtypei = "U"
            
            # Need to lock the record first
            handler.read_for_update("USER001")
            
            result_commarea3, result_map3, xctl_prog3, prog_commarea3 = main(
                commarea=commarea3,
                eibcalen=100,
                eibaid=DFHPF5,
                map_input=map_in2,
                usrsec_handler=handler
            )
            
            assert xctl_prog3 is None
            assert "has been updated" in result_map3.errmsgo
            assert handler._data["USER001"].first_name == "Jane"
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_full_flow_from_list(self):
        """Test full flow: update user from list selection."""
        # Create temp file
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            commarea = CardDemoCommarea()
            commarea.pgm_reenter = False
            handler = UserSecFileHandler(temp_filename)
            handler.add_record(UserSecurityRecord(
                user_id="USER001",
                first_name="John",
                last_name="Doe",
                password="PASSWORD",
                user_type="A"
            ))
            handler.open_file()
            prog_commarea = ThisProgCommarea()
            prog_commarea.usr_selected = "USER001"
            
            result_commarea, result_map, xctl_prog, prog_commarea2 = main(
                commarea=commarea,
                eibcalen=100,
                prog_commarea=prog_commarea,
                usrsec_handler=handler
            )
            
            assert xctl_prog is None
            assert result_map.fnamei == "John"
            assert result_map.lnamei == "Doe"
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)

