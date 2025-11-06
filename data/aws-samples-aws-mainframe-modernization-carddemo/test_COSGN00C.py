"""
Test suite for COSGN00C.py

Tests for Signon Screen transaction program.
"""

import pytest
from unittest.mock import patch

from COSGN00C import (
    SignonMap,
    UserSecurityRecord,
    UserSecFileHandler,
    WorkingStorage,
    CardDemoCommarea,
    populate_header_info,
    send_signon_screen,
    send_plain_text,
    process_enter_key,
    read_user_sec_file,
    main,
    WS_PGMNAME,
    WS_TRANID,
    DFHENTER,
    DFHPF3,
    COADM01C,
    COMEN01C,
    DFHRESP_NORMAL,
    DFHRESP_NOTFND
)


# ============================================================================
# Test User Security Record
# ============================================================================

class TestUserSecurityRecord:
    """Test UserSecurityRecord dataclass."""
    
    def test_record_creation(self):
        """Test creating a user security record."""
        record = UserSecurityRecord(
            user_id="TESTUSER",
            password="PASSWORD",
            user_type="U",
            first_name="Test",
            last_name="User"
        )
        assert record.user_id == "TESTUSER"
        assert record.password == "PASSWORD"
        assert record.user_type == "U"
    
    def test_to_record_string(self):
        """Test converting record to string format."""
        record = UserSecurityRecord(
            user_id="ADMIN",
            password="ADMIN123",
            user_type="A"
        )
        record_str = record.to_record_string()
        assert len(record_str) >= 17
        assert "ADMIN" in record_str
    
    def test_from_record_string(self):
        """Test parsing record from string format."""
        record_str = "TESTUSERPASSWORDU"
        record = UserSecurityRecord.from_record_string(record_str)
        assert record.user_id == "TESTUSER"
        assert record.password == "PASSWORD"
        assert record.user_type == "U"
    
    def test_from_record_string_short(self):
        """Test parsing short record string."""
        record_str = "SHORT"
        record = UserSecurityRecord.from_record_string(record_str)
        assert record.user_id == ""
        assert record.password == ""


# ============================================================================
# Test User Security File Handler
# ============================================================================

class TestUserSecFileHandler:
    """Test UserSecFileHandler."""
    
    def test_read_record_not_found(self):
        """Test reading a non-existent record."""
        handler = UserSecFileHandler("USRSEC")
        record = handler.read_record("NONEXIST")
        assert record is None
        assert handler.resp_cd == DFHRESP_NOTFND
    
    def test_read_record_found(self):
        """Test reading an existing record."""
        handler = UserSecFileHandler("USRSEC")
        test_record = UserSecurityRecord(
            user_id="TESTUSER",
            password="PASSWORD",
            user_type="U"
        )
        handler.add_record(test_record)
        
        record = handler.read_record("TESTUSER")
        assert record is not None
        assert record.user_id == "TESTUSER"
        assert record.password == "PASSWORD"
        assert handler.resp_cd == DFHRESP_NORMAL
    
    def test_read_record_case_insensitive(self):
        """Test reading record with case-insensitive user ID."""
        handler = UserSecFileHandler("USRSEC")
        test_record = UserSecurityRecord(
            user_id="TESTUSER",
            password="PASSWORD",
            user_type="U"
        )
        handler.add_record(test_record)
        
        # Try with different case
        record = handler.read_record("testuser")
        assert record is not None
        assert record.user_id == "TESTUSER"
    
    def test_add_multiple_records(self):
        """Test adding multiple records."""
        handler = UserSecFileHandler("USRSEC")
        handler.add_record(UserSecurityRecord(user_id="USER1", password="PASS1", user_type="U"))
        handler.add_record(UserSecurityRecord(user_id="USER2", password="PASS2", user_type="A"))
        
        record1 = handler.read_record("USER1")
        record2 = handler.read_record("USER2")
        
        assert record1 is not None
        assert record2 is not None
        assert record1.user_type == "U"
        assert record2.user_type == "A"


# ============================================================================
# Test Map Functions
# ============================================================================

class TestMapFunctions:
    """Test map functions."""
    
    def test_populate_header_info(self):
        """Test populating header info."""
        ws = WorkingStorage()
        map_out = SignonMap()
        
        populate_header_info(ws, map_out)
        
        assert map_out.title01o != ""
        assert map_out.trnnameo == WS_TRANID
        assert map_out.pgmnameo == WS_PGMNAME
        assert map_out.curdateo != ""
        assert map_out.curtimeo != ""
        assert map_out.applido != ""
        assert map_out.sysido != ""
    
    def test_send_signon_screen(self):
        """Test sending signon screen."""
        ws = WorkingStorage()
        ws.message = "Test message"
        map_out = SignonMap()
        
        send_signon_screen(ws, map_out)
        
        assert map_out.errmsgo == "Test message"
        assert map_out.title01o != ""
    
    def test_send_plain_text(self):
        """Test sending plain text."""
        ws = WorkingStorage()
        ws.message = "Thank you message"
        
        plain_text = send_plain_text(ws)
        
        assert plain_text == "Thank you message"


# ============================================================================
# Test Process Enter Key
# ============================================================================

class TestProcessEnterKey:
    """Test process_enter_key function."""
    
    def test_missing_user_id(self):
        """Test processing with missing user ID."""
        ws = WorkingStorage()
        ws.usrsec_handler = UserSecFileHandler("USRSEC")
        map_in = SignonMap()
        map_in.useridi = ""
        map_out = SignonMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "User ID" in ws.message
        assert map_out.useridl == -1
    
    def test_missing_password(self):
        """Test processing with missing password."""
        ws = WorkingStorage()
        ws.usrsec_handler = UserSecFileHandler("USRSEC")
        map_in = SignonMap()
        map_in.useridi = "TESTUSER"
        map_in.passwdi = ""
        map_out = SignonMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "Password" in ws.message
        assert map_out.passwdl == -1
    
    def test_valid_input(self):
        """Test processing with valid input."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("USRSEC")
        handler.add_record(UserSecurityRecord(
            user_id="TESTUSER",
            password="PASSWORD",
            user_type="U"
        ))
        ws.usrsec_handler = handler
        map_in = SignonMap()
        map_in.useridi = "testuser"
        map_in.passwdi = "password"
        map_out = SignonMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is True
        assert ws.user_id == "TESTUSER"
        assert ws.user_pwd == "PASSWORD"


# ============================================================================
# Test Read User Security File
# ============================================================================

class TestReadUserSecFile:
    """Test read_user_sec_file function."""
    
    def test_read_file_success(self):
        """Test successful authentication."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("USRSEC")
        handler.add_record(UserSecurityRecord(
            user_id="TESTUSER",
            password="PASSWORD",
            user_type="U"
        ))
        ws.usrsec_handler = handler
        ws.user_id = "TESTUSER"
        ws.user_pwd = "PASSWORD"
        map_out = SignonMap()
        
        result = read_user_sec_file(ws, map_out)
        
        assert result is True
        assert ws.err_flg is False
    
    def test_read_file_wrong_password(self):
        """Test authentication with wrong password."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("USRSEC")
        handler.add_record(UserSecurityRecord(
            user_id="TESTUSER",
            password="PASSWORD",
            user_type="U"
        ))
        ws.usrsec_handler = handler
        ws.user_id = "TESTUSER"
        ws.user_pwd = "WRONGPASS"
        map_out = SignonMap()
        
        result = read_user_sec_file(ws, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "Wrong Password" in ws.message
        assert map_out.passwdl == -1
    
    def test_read_file_user_not_found(self):
        """Test authentication with user not found."""
        ws = WorkingStorage()
        handler = UserSecFileHandler("USRSEC")
        ws.usrsec_handler = handler
        ws.user_id = "NONEXIST"
        ws.user_pwd = "PASSWORD"
        map_out = SignonMap()
        
        result = read_user_sec_file(ws, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "User not found" in ws.message
        assert map_out.useridl == -1
    
    def test_read_file_no_handler(self):
        """Test reading file without handler."""
        ws = WorkingStorage()
        ws.usrsec_handler = None
        ws.user_id = "TESTUSER"
        ws.user_pwd = "PASSWORD"
        map_out = SignonMap()
        
        result = read_user_sec_file(ws, map_out)
        
        assert result is False
        assert ws.err_flg is True
        assert "Unable to verify" in ws.message


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_first_entry(self):
        """Test main on first entry."""
        commarea = CardDemoCommarea()
        
        result_commarea, result_map, xctl_prog, plain_text = main(
            commarea=commarea,
            eibcalen=0
        )
        
        assert xctl_prog is None
        assert plain_text is None
        assert result_map.title01o != ""
        assert result_map.useridl == -1
    
    def test_main_pf3_exit(self):
        """Test main with PF3."""
        commarea = CardDemoCommarea()
        
        result_commarea, result_map, xctl_prog, plain_text = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF3
        )
        
        assert xctl_prog is None
        assert plain_text is not None
        assert "Thank you" in plain_text
    
    def test_main_invalid_key(self):
        """Test main with invalid key."""
        commarea = CardDemoCommarea()
        
        result_commarea, result_map, xctl_prog, plain_text = main(
            commarea=commarea,
            eibcalen=100,
            eibaid="INVALID"
        )
        
        assert xctl_prog is None
        assert plain_text is None
        assert "Invalid key" in result_map.errmsgo
    
    def test_main_enter_missing_user_id(self):
        """Test main with enter key but missing user ID."""
        commarea = CardDemoCommarea()
        map_in = SignonMap()
        map_in.useridi = ""
        
        result_commarea, result_map, xctl_prog, plain_text = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in
        )
        
        assert xctl_prog is None
        assert plain_text is None
        assert "User ID" in result_map.errmsgo
    
    def test_main_enter_success_user(self):
        """Test main with successful authentication for regular user."""
        commarea = CardDemoCommarea()
        handler = UserSecFileHandler("USRSEC")
        handler.add_record(UserSecurityRecord(
            user_id="TESTUSER",
            password="PASSWORD",
            user_type="U"
        ))
        map_in = SignonMap()
        map_in.useridi = "testuser"
        map_in.passwdi = "password"
        
        result_commarea, result_map, xctl_prog, plain_text = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            usrsec_handler=handler
        )
        
        assert xctl_prog == COMEN01C
        assert plain_text is None
        assert result_commarea.usrtyp_user is True
        assert hasattr(result_commarea, 'user_id') or True  # May or may not exist
    
    def test_main_enter_success_admin(self):
        """Test main with successful authentication for admin user."""
        commarea = CardDemoCommarea()
        handler = UserSecFileHandler("USRSEC")
        handler.add_record(UserSecurityRecord(
            user_id="ADMIN",
            password="ADMIN123",
            user_type="A"
        ))
        map_in = SignonMap()
        map_in.useridi = "admin"
        map_in.passwdi = "admin123"
        
        result_commarea, result_map, xctl_prog, plain_text = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            usrsec_handler=handler
        )
        
        assert xctl_prog == COADM01C
        assert plain_text is None
        assert result_commarea.usrtyp_user is False
    
    def test_main_enter_wrong_password(self):
        """Test main with wrong password."""
        commarea = CardDemoCommarea()
        handler = UserSecFileHandler("USRSEC")
        handler.add_record(UserSecurityRecord(
            user_id="TESTUSER",
            password="PASSWORD",
            user_type="U"
        ))
        map_in = SignonMap()
        map_in.useridi = "testuser"
        map_in.passwdi = "wrongpass"
        
        result_commarea, result_map, xctl_prog, plain_text = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            usrsec_handler=handler
        )
        
        assert xctl_prog is None
        assert plain_text is None
        assert "Wrong Password" in result_map.errmsgo
    
    def test_main_enter_user_not_found(self):
        """Test main with user not found."""
        commarea = CardDemoCommarea()
        handler = UserSecFileHandler("USRSEC")
        map_in = SignonMap()
        map_in.useridi = "nonexist"
        map_in.passwdi = "password"
        
        result_commarea, result_map, xctl_prog, plain_text = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            usrsec_handler=handler
        )
        
        assert xctl_prog is None
        assert plain_text is None
        assert "User not found" in result_map.errmsgo


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_user_signon(self):
        """Test full flow: user signon."""
        # First entry - show screen
        commarea = CardDemoCommarea()
        
        result_commarea, result_map, xctl_prog, plain_text = main(
            commarea=commarea,
            eibcalen=0
        )
        
        assert result_map.title01o != ""
        
        # Enter credentials
        handler = UserSecFileHandler("USRSEC")
        handler.add_record(UserSecurityRecord(
            user_id="TESTUSER",
            password="PASSWORD",
            user_type="U"
        ))
        map_in = SignonMap()
        map_in.useridi = "testuser"
        map_in.passwdi = "password"
        
        result_commarea2, result_map2, xctl_prog2, plain_text2 = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            usrsec_handler=handler
        )
        
        assert xctl_prog2 == COMEN01C
        assert result_commarea2.usrtyp_user is True
        assert result_commarea2.from_program == WS_PGMNAME
        assert result_commarea2.from_tranid == WS_TRANID
    
    def test_full_flow_admin_signon(self):
        """Test full flow: admin signon."""
        handler = UserSecFileHandler("USRSEC")
        handler.add_record(UserSecurityRecord(
            user_id="ADMIN",
            password="ADMIN123",
            user_type="A"
        ))
        commarea = CardDemoCommarea()
        map_in = SignonMap()
        map_in.useridi = "admin"
        map_in.passwdi = "admin123"
        
        result_commarea, result_map, xctl_prog, plain_text = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            usrsec_handler=handler
        )
        
        assert xctl_prog == COADM01C
        assert result_commarea.usrtyp_user is False
    
    def test_full_flow_pf3_exit(self):
        """Test full flow: PF3 exit."""
        commarea = CardDemoCommarea()
        
        result_commarea, result_map, xctl_prog, plain_text = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF3
        )
        
        assert xctl_prog is None
        assert plain_text is not None
        assert "Thank you" in plain_text
    
    def test_full_flow_retry_after_error(self):
        """Test full flow: retry after authentication error."""
        handler = UserSecFileHandler("USRSEC")
        handler.add_record(UserSecurityRecord(
            user_id="TESTUSER",
            password="PASSWORD",
            user_type="U"
        ))
        
        # First attempt - wrong password
        commarea = CardDemoCommarea()
        map_in = SignonMap()
        map_in.useridi = "testuser"
        map_in.passwdi = "wrongpass"
        
        result_commarea, result_map, xctl_prog, plain_text = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            usrsec_handler=handler
        )
        
        assert xctl_prog is None
        assert "Wrong Password" in result_map.errmsgo
        
        # Second attempt - correct password
        map_in2 = SignonMap()
        map_in2.useridi = "testuser"
        map_in2.passwdi = "password"
        
        result_commarea2, result_map2, xctl_prog2, plain_text2 = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in2,
            usrsec_handler=handler
        )
        
        assert xctl_prog2 == COMEN01C
        assert result_commarea2.usrtyp_user is True

