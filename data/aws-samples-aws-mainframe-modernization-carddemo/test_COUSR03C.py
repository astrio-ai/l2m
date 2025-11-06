"""
Test suite for COUSR03C.py

Tests for Delete User transaction program.
"""

import pytest
from unittest.mock import patch
import tempfile
import os

from COUSR03C import (
    UserDeleteMap,
    ThisProgCommarea,
    UserSecFileHandler,
    WorkingStorage,
    CardDemoCommarea,
    UserSecurityRecord,
    populate_header_info,
    send_usrdel_screen,
    receive_usrdel_screen,
    initialize_all_fields,
    read_user_sec_file,
    delete_user_sec_file,
    process_enter_key,
    delete_user_info,
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
# Test UserSecFileHandler Delete
# ============================================================================

class TestUserSecFileHandlerDelete:
    """Test UserSecFileHandler delete operations."""
    
    def test_delete_record_success(self):
        """Test deleting a locked record."""
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
        
        # Delete the record
        resp_cd = handler.delete_record("USER001")
        
        assert resp_cd == DFHRESP_NORMAL
        assert "USER001" not in handler._data
        assert "USER001" not in handler._locked_records
    
    def test_delete_record_not_locked(self):
        """Test deleting a record that wasn't locked."""
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(
            user_id="USER001",
            first_name="John"
        ))
        handler.open_file()
        
        # Try to delete without locking
        resp_cd = handler.delete_record("USER001")
        
        assert resp_cd == DFHRESP_NOTFND
        assert "USER001" in handler._data  # Still exists
    
    def test_delete_record_not_found(self):
        """Test deleting a non-existent record."""
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.open_file()
        
        # Lock a non-existent record
        handler.read_for_update("USER999")
        
        # Try to delete
        resp_cd = handler.delete_record("USER999")
        
        assert resp_cd == DFHRESP_NOTFND


# ============================================================================
# Test Map Functions
# ============================================================================

class TestMapFunctions:
    """Test map functions."""
    
    def test_populate_header_info(self):
        """Test populating header info."""
        ws = WorkingStorage()
        map_out = UserDeleteMap()
        
        populate_header_info(ws, map_out)
        
        assert map_out.title01o != ""
        assert map_out.trnnameo == WS_TRANID
        assert map_out.pgmnameo == WS_PGMNAME
    
    def test_initialize_all_fields(self):
        """Test initializing all fields."""
        ws = WorkingStorage()
        map_in = UserDeleteMap()
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
        map_out = UserDeleteMap()
        
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
        map_out = UserDeleteMap()
        
        result = read_user_sec_file(ws, "USER999", map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "NOT found" in ws.message
    
    def test_delete_user_sec_file_success(self):
        """Test successful delete of user."""
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
        
        result = delete_user_sec_file(ws, "USER001")
        
        assert result is True
        assert ws.err_flg is False
        assert "USER001" not in handler._data


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
        map_in = UserDeleteMap()
        map_in.useridi = ""  # Missing
        map_out = UserDeleteMap()
        
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
        map_in = UserDeleteMap()
        map_in.useridi = "USER001"
        map_out = UserDeleteMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is True
        assert ws.err_flg is False
        assert map_out.fnamei == "John"
        assert map_out.lnamei == "Doe"
        assert map_out.usrtypei == "A"
        # Password field should not exist in delete map
        assert not hasattr(map_out, 'passwdi')


# ============================================================================
# Test Delete User Info
# ============================================================================

class TestDeleteUserInfo:
    """Test delete_user_info function."""
    
    def test_missing_user_id(self):
        """Test delete with missing user ID."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.open_file()
        ws.usrsec_handler = handler
        map_in = UserDeleteMap()
        map_in.useridi = ""  # Missing
        map_out = UserDeleteMap()
        
        result = delete_user_info(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "User ID" in ws.message
    
    def test_delete_success(self):
        """Test successful delete."""
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
        map_in = UserDeleteMap()
        map_in.useridi = "USER001"
        map_out = UserDeleteMap()
        
        result = delete_user_info(ws, map_in, map_out)
        
        assert result is True
        assert ws.err_flg is False
        assert "has been deleted" in ws.message
        assert "USER001" not in handler._data
        # Fields should be cleared
        assert map_out.useridi == ""
    
    def test_delete_not_found(self):
        """Test delete of non-existent user."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.open_file()
        ws.usrsec_handler = handler
        map_in = UserDeleteMap()
        map_in.useridi = "USER999"
        map_out = UserDeleteMap()
        
        result = delete_user_info(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "NOT found" in ws.message


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
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF3
        )
        
        assert xctl_prog == COADM01C
    
    def test_main_pf4_clear_screen(self):
        """Test main with PF4."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = UserDeleteMap()
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
    
    def test_main_pf5_delete(self):
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
        map_in = UserDeleteMap()
        map_in.useridi = "USER001"
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF5,
            map_input=map_in,
            usrsec_handler=handler
        )
        
        assert xctl_prog is None
        assert "has been deleted" in result_map.errmsgo
        assert "USER001" not in handler._data
    
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
    
    def test_full_flow_delete_user(self):
        """Test full flow: delete user."""
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
            map_in = UserDeleteMap()
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
            
            # Delete user
            commarea3 = CardDemoCommarea()
            commarea3.pgm_reenter = True
            map_in2 = UserDeleteMap()
            map_in2.useridi = "USER001"
            
            result_commarea3, result_map3, xctl_prog3, prog_commarea3 = main(
                commarea=commarea3,
                eibcalen=100,
                eibaid=DFHPF5,
                map_input=map_in2,
                usrsec_handler=handler
            )
            
            assert xctl_prog3 is None
            assert "has been deleted" in result_map3.errmsgo
            assert "USER001" not in handler._data
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)
    
    def test_full_flow_from_list(self):
        """Test full flow: delete user from list selection."""
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
    
    def test_full_flow_delete_and_verify(self):
        """Test full flow: delete user and verify deletion."""
        # Create temp file
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            temp_filename = f.name
        
        try:
            commarea = CardDemoCommarea()
            commarea.pgm_reenter = True
            handler = UserSecFileHandler(temp_filename)
            handler.add_record(UserSecurityRecord(
                user_id="USER001",
                first_name="John",
                last_name="Doe",
                password="PASSWORD",
                user_type="A"
            ))
            handler.add_record(UserSecurityRecord(
                user_id="USER002",
                first_name="Jane",
                last_name="Smith",
                password="PASSWORD",
                user_type="U"
            ))
            handler.open_file()
            map_in = UserDeleteMap()
            map_in.useridi = "USER001"
            
            # Delete user
            result_commarea, result_map, xctl_prog, prog_commarea = main(
                commarea=commarea,
                eibcalen=100,
                eibaid=DFHPF5,
                map_input=map_in,
                usrsec_handler=handler
            )
            
            assert "has been deleted" in result_map.errmsgo
            assert "USER001" not in handler._data
            # Other user should still exist
            assert "USER002" in handler._data
        finally:
            if os.path.exists(temp_filename):
                os.unlink(temp_filename)

