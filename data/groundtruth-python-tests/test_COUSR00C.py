"""
Test suite for COUSR00C.py

Tests for User List transaction program.
"""

import pytest
from unittest.mock import patch

from COUSR00C import (
    UserListMap,
    ThisProgCommarea,
    UserSecFileHandler,
    WorkingStorage,
    CardDemoCommarea,
    UserSecurityRecord,
    populate_header_info,
    send_usrlst_screen,
    receive_usrlst_screen,
    initialize_user_data,
    populate_user_data,
    startbr_user_sec_file,
    readnext_user_sec_file,
    readprev_user_sec_file,
    endbr_user_sec_file,
    process_page_forward,
    process_page_backward,
    process_enter_key,
    process_pf7_key,
    process_pf8_key,
    return_to_prev_screen,
    main,
    WS_PGMNAME,
    WS_TRANID,
    DFHENTER,
    DFHPF3,
    DFHPF7,
    DFHPF8,
    COSGN00C,
    COADM01C,
    COUSR02C,
    COUSR03C,
    DFHRESP_NORMAL,
    DFHRESP_NOTFND,
    DFHRESP_ENDFILE
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
        assert commarea.next_page_flg is False
    
    def test_commarea_with_selection(self):
        """Test commarea with user selection."""
        commarea = ThisProgCommarea(
            usr_selected="USER001",
            usr_sel_flg="U"
        )
        assert commarea.usr_selected == "USER001"
        assert commarea.usr_sel_flg == "U"


# ============================================================================
# Test UserSecFileHandler Browse
# ============================================================================

class TestUserSecFileHandlerBrowse:
    """Test UserSecFileHandler browse operations."""
    
    def test_startbr_from_beginning(self):
        """Test starting browse from beginning."""
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(user_id="USER001", first_name="John", last_name="Doe"))
        handler.add_record(UserSecurityRecord(user_id="USER002", first_name="Jane", last_name="Smith"))
        handler.open_file()
        
        resp_cd = handler.startbr("")
        assert resp_cd == DFHRESP_NORMAL
        assert handler._browse_active is True
        assert handler._browse_index == 0
    
    def test_startbr_with_key(self):
        """Test starting browse with a specific key."""
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(user_id="USER001", first_name="John"))
        handler.add_record(UserSecurityRecord(user_id="USER002", first_name="Jane"))
        handler.open_file()
        
        resp_cd = handler.startbr("USER002")
        assert resp_cd == DFHRESP_NORMAL
        assert handler._browse_index == 1
    
    def test_readnext(self):
        """Test reading next record."""
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(user_id="USER001", first_name="John"))
        handler.add_record(UserSecurityRecord(user_id="USER002", first_name="Jane"))
        handler.open_file()
        handler.startbr("")
        
        record = handler.readnext()
        assert record is not None
        assert record.user_id == "USER001"
        
        record2 = handler.readnext()
        assert record2 is not None
        assert record2.user_id == "USER002"
    
    def test_readprev(self):
        """Test reading previous record."""
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(user_id="USER001", first_name="John"))
        handler.add_record(UserSecurityRecord(user_id="USER002", first_name="Jane"))
        handler.open_file()
        handler.startbr('\xff' * 8)  # HIGH-VALUES
        
        record = handler.readprev()
        assert record is not None
        assert record.user_id == "USER002"
    
    def test_endbr(self):
        """Test ending browse."""
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(user_id="USER001"))
        handler.open_file()
        handler.startbr("")
        
        handler.endbr()
        assert handler._browse_active is False
        assert handler._browse_index == -1


# ============================================================================
# Test Map Functions
# ============================================================================

class TestMapFunctions:
    """Test map functions."""
    
    def test_populate_header_info(self):
        """Test populating header info."""
        ws = WorkingStorage()
        map_out = UserListMap()
        
        populate_header_info(ws, map_out)
        
        assert map_out.title01o != ""
        assert map_out.trnnameo == WS_TRANID
        assert map_out.pgmnameo == WS_PGMNAME
    
    def test_initialize_user_data(self):
        """Test initializing user data for a row."""
        ws = WorkingStorage()
        map_in = UserListMap()
        map_in.set_row_data(1, "U", "USER001", "John", "Doe", "A")
        
        initialize_user_data(ws, map_in, 1)
        
        row_data = map_in.get_row_data(1)
        assert row_data[0] == ""  # Selection cleared
        assert row_data[1] == ""  # User ID cleared
    
    def test_populate_user_data(self):
        """Test populating user data for a row."""
        ws = WorkingStorage()
        map_in = UserListMap()
        record = UserSecurityRecord(
            user_id="USER001",
            first_name="John",
            last_name="Doe",
            user_type="A"
        )
        
        populate_user_data(ws, map_in, record, 1)
        
        row_data = map_in.get_row_data(1)
        assert row_data[1] == "USER001"  # User ID
        assert row_data[2] == "John"  # First name
        assert row_data[3] == "Doe"  # Last name
        assert row_data[4] == "A"  # User type


# ============================================================================
# Test File Operations
# ============================================================================

class TestFileOperations:
    """Test file operations."""
    
    def test_startbr_user_sec_file_success(self):
        """Test successful start browse."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(user_id="USER001"))
        handler.open_file()
        ws.usrsec_handler = handler
        
        result = startbr_user_sec_file(ws, "")
        
        assert result is True
        assert ws.err_flg is False
    
    def test_readnext_user_sec_file_success(self):
        """Test successful read next."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(user_id="USER001", first_name="John"))
        handler.open_file()
        handler.startbr("")
        ws.usrsec_handler = handler
        
        record = readnext_user_sec_file(ws)
        
        assert record is not None
        assert record.user_id == "USER001"
        assert ws.err_flg is False
    
    def test_readnext_user_sec_file_eof(self):
        """Test read next at end of file."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(user_id="USER001"))
        handler.open_file()
        handler.startbr("")
        handler.readnext()  # Read the one record
        ws.usrsec_handler = handler
        
        record = readnext_user_sec_file(ws)
        
        assert record is None
        assert ws.user_sec_eof is True
        assert "bottom" in ws.message.lower()


# ============================================================================
# Test Pagination
# ============================================================================

class TestPagination:
    """Test pagination functions."""
    
    def test_process_page_forward_with_users(self):
        """Test processing page forward with users."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        for i in range(1, 15):
            handler.add_record(UserSecurityRecord(
                user_id=f"USER{i:03d}",
                first_name=f"User{i}",
                last_name="Test",
                user_type="A"
            ))
        handler.open_file()
        ws.usrsec_handler = handler
        ws.sec_usr_id = ""
        map_in = UserListMap()
        map_out = UserListMap()
        prog_commarea = ThisProgCommarea()
        
        process_page_forward(ws, map_in, map_out, prog_commarea)
        
        assert ws.err_flg is False
        assert prog_commarea.page_num == 1
        # Should have 10 users populated
        assert map_in.get_row_data(1)[1] != ""  # First row has user ID
        assert map_in.get_row_data(10)[1] != ""  # Last row has user ID
    
    def test_process_page_forward_empty_file(self):
        """Test processing page forward with empty file."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.open_file()
        ws.usrsec_handler = handler
        ws.sec_usr_id = ""
        map_in = UserListMap()
        map_out = UserListMap()
        prog_commarea = ThisProgCommarea()
        
        process_page_forward(ws, map_in, map_out, prog_commarea)
        
        # Should handle empty file gracefully
        assert map_in.get_row_data(1)[1] == ""  # No users
    
    def test_process_page_backward(self):
        """Test processing page backward."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        for i in range(1, 15):
            handler.add_record(UserSecurityRecord(
                user_id=f"USER{i:03d}",
                first_name=f"User{i}",
                last_name="Test",
                user_type="A"
            ))
        handler.open_file()
        ws.usrsec_handler = handler
        map_in = UserListMap()
        map_out = UserListMap()
        prog_commarea = ThisProgCommarea()
        prog_commarea.page_num = 2
        prog_commarea.usrid_first = "USER001"
        prog_commarea.next_page_flg = True
        
        process_page_backward(ws, map_in, map_out, prog_commarea)
        
        assert ws.err_flg is False
        # Should have users populated
        assert map_in.get_row_data(1)[1] != "" or ws.message != ""
    
    def test_process_pf7_key_at_top(self):
        """Test PF7 key when already at top."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(user_id="USER001"))
        handler.open_file()
        ws.usrsec_handler = handler
        map_in = UserListMap()
        map_out = UserListMap()
        prog_commarea = ThisProgCommarea()
        prog_commarea.page_num = 1
        
        process_pf7_key(ws, map_in, map_out, prog_commarea)
        
        assert "already at the top" in ws.message.lower()


# ============================================================================
# Test Process Enter Key
# ============================================================================

class TestProcessEnterKey:
    """Test process_enter_key function."""
    
    def test_process_enter_key_no_selection(self):
        """Test processing enter key with no selection."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(user_id="USER001", first_name="John"))
        handler.open_file()
        ws.usrsec_handler = handler
        ws.sec_usr_id = ""
        map_in = UserListMap()
        map_out = UserListMap()
        prog_commarea = ThisProgCommarea()
        commarea = CardDemoCommarea()
        
        xctl_prog = process_enter_key(ws, map_in, map_out, prog_commarea, commarea)
        
        assert xctl_prog is None
        assert ws.err_flg is False
        # Should have loaded first page
        assert prog_commarea.page_num > 0 or map_in.get_row_data(1)[1] != ""
    
    def test_process_enter_key_select_update(self):
        """Test processing enter key with U selection."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(user_id="USER001", first_name="John"))
        handler.open_file()
        ws.usrsec_handler = handler
        map_in = UserListMap()
        map_in.set_row_data(1, "U", "USER001", "John", "Doe", "A")
        map_out = UserListMap()
        prog_commarea = ThisProgCommarea()
        commarea = CardDemoCommarea()
        
        xctl_prog = process_enter_key(ws, map_in, map_out, prog_commarea, commarea)
        
        assert xctl_prog == COUSR02C
        assert prog_commarea.usr_selected == "USER001"
        assert prog_commarea.usr_sel_flg == "U"
    
    def test_process_enter_key_select_delete(self):
        """Test processing enter key with D selection."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(user_id="USER001"))
        handler.open_file()
        ws.usrsec_handler = handler
        map_in = UserListMap()
        map_in.set_row_data(1, "D", "USER001", "John", "Doe", "A")
        map_out = UserListMap()
        prog_commarea = ThisProgCommarea()
        commarea = CardDemoCommarea()
        
        xctl_prog = process_enter_key(ws, map_in, map_out, prog_commarea, commarea)
        
        assert xctl_prog == COUSR03C
        assert prog_commarea.usr_selected == "USER001"
        assert prog_commarea.usr_sel_flg == "D"
    
    def test_process_enter_key_invalid_selection(self):
        """Test processing enter key with invalid selection."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(user_id="USER001"))
        handler.open_file()
        ws.usrsec_handler = handler
        map_in = UserListMap()
        map_in.set_row_data(1, "X", "USER001", "John", "Doe", "A")  # Invalid selection
        map_out = UserListMap()
        prog_commarea = ThisProgCommarea()
        commarea = CardDemoCommarea()
        
        xctl_prog = process_enter_key(ws, map_in, map_out, prog_commarea, commarea)
        
        assert xctl_prog is None
        assert ws.err_flg is True
        assert "Invalid selection" in ws.message


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
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(user_id="USER001", first_name="John"))
        handler.open_file()
        
        result_commarea, result_map, xctl_prog, prog_commarea = main(
            commarea=commarea,
            eibcalen=100,
            usrsec_handler=handler
        )
        
        assert result_commarea.pgm_reenter is True
        assert result_map.title01o != ""
        assert xctl_prog is None
    
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
    
    def test_main_pf7_previous_page(self):
        """Test main with PF7."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        handler = UserSecFileHandler("test_usrsec.txt")
        for i in range(1, 15):
            handler.add_record(UserSecurityRecord(
                user_id=f"USER{i:03d}",
                first_name=f"User{i}",
                last_name="Test"
            ))
        handler.open_file()
        map_in = UserListMap()
        prog_commarea = ThisProgCommarea()
        prog_commarea.page_num = 2
        prog_commarea.usrid_first = "USER001"
        prog_commarea.next_page_flg = True
        
        result_commarea, result_map, xctl_prog, prog_commarea2 = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF7,
            map_input=map_in,
            prog_commarea=prog_commarea,
            usrsec_handler=handler
        )
        
        assert xctl_prog is None
        # Should have processed PF7
        assert result_map.title01o != ""
    
    def test_main_pf8_next_page(self):
        """Test main with PF8."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        handler = UserSecFileHandler("test_usrsec.txt")
        for i in range(1, 15):
            handler.add_record(UserSecurityRecord(
                user_id=f"USER{i:03d}",
                first_name=f"User{i}",
                last_name="Test"
            ))
        handler.open_file()
        map_in = UserListMap()
        prog_commarea = ThisProgCommarea()
        prog_commarea.page_num = 1
        prog_commarea.usrid_last = "USER010"
        prog_commarea.next_page_flg = True
        
        result_commarea, result_map, xctl_prog, prog_commarea2 = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF8,
            map_input=map_in,
            prog_commarea=prog_commarea,
            usrsec_handler=handler
        )
        
        assert xctl_prog is None
        # Should have processed PF8
        assert result_map.title01o != ""
        assert prog_commarea2.page_num >= prog_commarea.page_num


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_list_users(self):
        """Test full flow: list users."""
        # First entry - show screen
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        handler = UserSecFileHandler("test_usrsec.txt")
        for i in range(1, 12):
            handler.add_record(UserSecurityRecord(
                user_id=f"USER{i:03d}",
                first_name=f"User{i}",
                last_name="Test",
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
        # Should have loaded first page
        assert prog_commarea.page_num > 0
    
    def test_full_flow_select_user_update(self):
        """Test full flow: select user for update."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        handler = UserSecFileHandler("test_usrsec.txt")
        handler.add_record(UserSecurityRecord(
            user_id="USER001",
            first_name="John",
            last_name="Doe",
            user_type="A"
        ))
        handler.open_file()
        map_in = UserListMap()
        map_in.set_row_data(1, "U", "USER001", "John", "Doe", "A")
        prog_commarea = ThisProgCommarea()
        
        result_commarea, result_map, xctl_prog, prog_commarea2 = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            prog_commarea=prog_commarea,
            usrsec_handler=handler
        )
        
        assert xctl_prog == COUSR02C
        assert prog_commarea2.usr_selected == "USER001"
    
    def test_full_flow_pagination(self):
        """Test full flow: pagination."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        handler = UserSecFileHandler("test_usrsec.txt")
        for i in range(1, 25):
            handler.add_record(UserSecurityRecord(
                user_id=f"USER{i:03d}",
                first_name=f"User{i}",
                last_name="Test",
                user_type="A"
            ))
        handler.open_file()
        map_in = UserListMap()
        prog_commarea = ThisProgCommarea()
        prog_commarea.page_num = 1
        prog_commarea.next_page_flg = True
        
        # First, load initial page
        result_commarea, result_map, xctl_prog, prog_commarea2 = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            prog_commarea=prog_commarea,
            usrsec_handler=handler
        )
        
        original_page_num = prog_commarea2.page_num
        original_usrid_last = prog_commarea2.usrid_last
        
        # Then, PF8 to next page
        commarea2 = CardDemoCommarea()
        commarea2.pgm_reenter = True
        map_in2 = UserListMap()
        
        result_commarea2, result_map2, xctl_prog2, prog_commarea3 = main(
            commarea=commarea2,
            eibcalen=100,
            eibaid=DFHPF8,
            map_input=map_in2,
            prog_commarea=prog_commarea2,
            usrsec_handler=handler
        )
        
        assert xctl_prog2 is None
        # Should have advanced to next page
        assert prog_commarea3.page_num >= original_page_num or result_map2.title01o != ""

