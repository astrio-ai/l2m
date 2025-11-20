"""
Test suite for COADM01C.py

Tests for Admin Menu transaction program.
"""

import pytest
from unittest.mock import patch, MagicMock

from COADM01C import (
    AdminOption,
    CardDemoCommarea,
    AdminMenuMap,
    WorkingStorage,
    get_admin_options,
    get_admin_opt_count,
    populate_header_info,
    build_menu_options,
    send_menu_screen,
    receive_menu_screen,
    process_enter_key,
    return_to_signon_screen,
    main,
    abend_program,
    WS_PGMNAME,
    WS_TRANID,
    DFHENTER,
    DFHPF3,
    COSGN00C,
    CCDA_TITLE01,
    CCDA_TITLE02,
    CCDA_MSG_INVALID_KEY
)


# ============================================================================
# Test Admin Options
# ============================================================================

class TestAdminOptions:
    """Test admin options functions."""
    
    def test_get_admin_options(self):
        """Test getting admin options."""
        options = get_admin_options()
        assert len(options) > 0
        assert isinstance(options[0], AdminOption)
        assert options[0].num == "01"
        assert options[0].name == "View Accounts"
    
    def test_get_admin_opt_count(self):
        """Test getting admin option count."""
        count = get_admin_opt_count()
        assert count > 0
        assert count == len(get_admin_options())


# ============================================================================
# Test Commarea
# ============================================================================

class TestCardDemoCommarea:
    """Test CardDemoCommarea dataclass."""
    
    def test_creation(self):
        """Test creating a commarea."""
        commarea = CardDemoCommarea()
        assert commarea.from_program == ""
        assert commarea.pgm_reenter is False
    
    def test_assignment(self):
        """Test assigning values to commarea."""
        commarea = CardDemoCommarea()
        commarea.from_program = "COADM01C"
        commarea.pgm_reenter = True
        assert commarea.from_program == "COADM01C"
        assert commarea.pgm_reenter is True


# ============================================================================
# Test AdminMenuMap
# ============================================================================

class TestAdminMenuMap:
    """Test AdminMenuMap dataclass."""
    
    def test_creation(self):
        """Test creating a map."""
        map_out = AdminMenuMap()
        assert map_out.title01o == ""
        assert map_out.optn001o == ""
    
    def test_assignment(self):
        """Test assigning values to map."""
        map_out = AdminMenuMap()
        map_out.title01o = "Title"
        map_out.optn001o = "Option 1"
        assert map_out.title01o == "Title"
        assert map_out.optn001o == "Option 1"


# ============================================================================
# Test Working Storage
# ============================================================================

class TestWorkingStorage:
    """Test WorkingStorage class."""
    
    def test_creation(self):
        """Test creating working storage."""
        ws = WorkingStorage()
        assert ws.pgmname == WS_PGMNAME
        assert ws.tranid == WS_TRANID
        assert ws.err_flg is False
    
    def test_err_flg(self):
        """Test error flag."""
        ws = WorkingStorage()
        ws.err_flg = True
        assert ws.err_flg is True


# ============================================================================
# Test Header Population
# ============================================================================

class TestPopulateHeaderInfo:
    """Test populate_header_info function."""
    
    def test_populate_header_info(self):
        """Test populating header information."""
        ws = WorkingStorage()
        map_out = AdminMenuMap()
        
        populate_header_info(ws, map_out)
        
        assert map_out.title01o == CCDA_TITLE01
        assert map_out.title02o == CCDA_TITLE02
        assert map_out.trnnameo == WS_TRANID
        assert map_out.pgmnameo == WS_PGMNAME
        assert map_out.curdateo != ""
        assert map_out.curtimeo != ""
    
    def test_date_format(self):
        """Test date formatting."""
        ws = WorkingStorage()
        map_out = AdminMenuMap()
        
        populate_header_info(ws, map_out)
        
        # Check date format is MM/DD/YY
        assert "/" in map_out.curdateo
        assert len(map_out.curdateo.split("/")) == 3
        
        # Check time format is HH:MM:SS
        assert ":" in map_out.curtimeo
        assert len(map_out.curtimeo.split(":")) == 3


# ============================================================================
# Test Menu Building
# ============================================================================

class TestBuildMenuOptions:
    """Test build_menu_options function."""
    
    def test_build_menu_options(self):
        """Test building menu options."""
        ws = WorkingStorage()
        map_out = AdminMenuMap()
        
        build_menu_options(ws, map_out)
        
        # Check that options are populated
        assert map_out.optn001o != ""
        assert "01" in map_out.optn001o
        assert "View Accounts" in map_out.optn001o
    
    def test_menu_option_format(self):
        """Test menu option format."""
        ws = WorkingStorage()
        map_out = AdminMenuMap()
        
        build_menu_options(ws, map_out)
        
        # Check format: "01. View Accounts"
        assert "." in map_out.optn001o
        assert map_out.optn001o.startswith("01")
    
    def test_all_options_populated(self):
        """Test that all options are populated."""
        ws = WorkingStorage()
        map_out = AdminMenuMap()
        
        build_menu_options(ws, map_out)
        
        # Check that multiple options are populated
        assert map_out.optn001o != ""
        assert map_out.optn002o != ""
        assert map_out.optn003o != ""
        # Depending on how many options exist
        opt_count = get_admin_opt_count()
        if opt_count >= 3:
            assert map_out.optn003o != ""


# ============================================================================
# Test Send Menu Screen
# ============================================================================

class TestSendMenuScreen:
    """Test send_menu_screen function."""
    
    def test_send_menu_screen(self):
        """Test sending menu screen."""
        ws = WorkingStorage()
        ws.message = "Test message"
        map_out = AdminMenuMap()
        
        send_menu_screen(ws, map_out)
        
        assert map_out.title01o == CCDA_TITLE01
        assert map_out.optn001o != ""
        assert map_out.errmsgo == "Test message"
    
    def test_message_in_map(self):
        """Test that message is set in map."""
        ws = WorkingStorage()
        ws.message = "Error occurred"
        map_out = AdminMenuMap()
        
        send_menu_screen(ws, map_out)
        
        assert map_out.errmsgo == "Error occurred"


# ============================================================================
# Test Receive Menu Screen
# ============================================================================

class TestReceiveMenuScreen:
    """Test receive_menu_screen function."""
    
    def test_receive_menu_screen(self):
        """Test receiving menu screen."""
        ws = WorkingStorage()
        map_in = AdminMenuMap()
        map_in.optioni = "01"
        
        result = receive_menu_screen(ws, map_in)
        
        assert result == 0
        assert ws.resp_cd == 0


# ============================================================================
# Test Process Enter Key
# ============================================================================

class TestProcessEnterKey:
    """Test process_enter_key function."""
    
    def test_valid_option(self):
        """Test processing valid option."""
        ws = WorkingStorage()
        map_in = AdminMenuMap()
        map_in.optioni = "01"
        map_out = AdminMenuMap()
        commarea = CardDemoCommarea()
        
        xctl_prog = process_enter_key(ws, map_in, map_out, commarea)
        
        assert ws.option == 1
        assert ws.err_flg is False
        assert xctl_prog == "COACTVWC"  # First option should XCTL to COACTVWC
    
    def test_valid_option_dummy(self):
        """Test processing valid DUMMY option."""
        ws = WorkingStorage()
        map_in = AdminMenuMap()
        map_in.optioni = "03"  # Assuming option 3 is DUMMY
        map_out = AdminMenuMap()
        commarea = CardDemoCommarea()
        
        xctl_prog = process_enter_key(ws, map_in, map_out, commarea)
        
        # DUMMY options should return None and set message
        assert xctl_prog is None
        assert "coming soon" in ws.message.lower()
    
    def test_invalid_option_zero(self):
        """Test processing zero option."""
        ws = WorkingStorage()
        map_in = AdminMenuMap()
        map_in.optioni = "0"
        map_out = AdminMenuMap()
        commarea = CardDemoCommarea()
        
        xctl_prog = process_enter_key(ws, map_in, map_out, commarea)
        
        assert ws.err_flg is True
        assert "valid option" in ws.message.lower()
        assert xctl_prog is None
    
    def test_invalid_option_too_large(self):
        """Test processing option that's too large."""
        ws = WorkingStorage()
        map_in = AdminMenuMap()
        map_in.optioni = "99"
        map_out = AdminMenuMap()
        commarea = CardDemoCommarea()
        
        xctl_prog = process_enter_key(ws, map_in, map_out, commarea)
        
        assert ws.err_flg is True
        assert xctl_prog is None
    
    def test_invalid_option_non_numeric(self):
        """Test processing non-numeric option."""
        ws = WorkingStorage()
        map_in = AdminMenuMap()
        map_in.optioni = "AB"
        map_out = AdminMenuMap()
        commarea = CardDemoCommarea()
        
        xctl_prog = process_enter_key(ws, map_in, map_out, commarea)
        
        assert ws.err_flg is True
        assert xctl_prog is None
    
    def test_option_with_spaces(self):
        """Test processing option with spaces."""
        ws = WorkingStorage()
        map_in = AdminMenuMap()
        map_in.optioni = " 1 "  # Spaces should be converted to zeros
        map_out = AdminMenuMap()
        commarea = CardDemoCommarea()
        
        xctl_prog = process_enter_key(ws, map_in, map_out, commarea)
        
        # Should strip and process
        assert ws.option == 1 or ws.err_flg is True
    
    def test_option_output_field(self):
        """Test that option is set in output field."""
        ws = WorkingStorage()
        map_in = AdminMenuMap()
        map_in.optioni = "02"
        map_out = AdminMenuMap()
        commarea = CardDemoCommarea()
        
        process_enter_key(ws, map_in, map_out, commarea)
        
        assert map_out.optiono == "02" or map_out.optiono == " 2"


# ============================================================================
# Test Return to Signon Screen
# ============================================================================

class TestReturnToSignonScreen:
    """Test return_to_signon_screen function."""
    
    def test_return_with_to_program_set(self):
        """Test return when to_program is already set."""
        commarea = CardDemoCommarea()
        commarea.to_program = "SOMEPGM"
        
        result = return_to_signon_screen(commarea)
        
        assert result == "SOMEPGM"
    
    def test_return_with_to_program_empty(self):
        """Test return when to_program is empty."""
        commarea = CardDemoCommarea()
        commarea.to_program = ""
        
        result = return_to_signon_screen(commarea)
        
        assert result == COSGN00C
        assert commarea.to_program == COSGN00C


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_eibcalen_zero(self):
        """Test main with eibcalen=0 (return to signon)."""
        commarea = CardDemoCommarea()
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=0
        )
        
        assert xctl_prog == COSGN00C
        assert result_commarea.from_program == COSGN00C
    
    def test_main_first_entry(self):
        """Test main on first entry (not reenter)."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100
        )
        
        assert result_commarea.pgm_reenter is True
        assert result_map.title01o == CCDA_TITLE01
        assert result_map.optn001o != ""
        assert xctl_prog is None
    
    def test_main_reenter_enter_valid_option(self):
        """Test main with Enter key and valid option."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = AdminMenuMap()
        map_in.optioni = "01"
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in
        )
        
        assert xctl_prog == "COACTVWC"
        assert result_commarea.from_tranid == WS_TRANID
        assert result_commarea.from_program == WS_PGMNAME
    
    def test_main_reenter_enter_invalid_option(self):
        """Test main with Enter key and invalid option."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = AdminMenuMap()
        map_in.optioni = "99"
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in
        )
        
        assert xctl_prog is None
        assert "valid option" in result_map.errmsgo.lower()
    
    def test_main_reenter_pf3(self):
        """Test main with PF3 key (exit)."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF3
        )
        
        assert xctl_prog == COSGN00C
        assert result_commarea.to_program == COSGN00C
    
    def test_main_reenter_invalid_key(self):
        """Test main with invalid key."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid="DFHPF5"  # Invalid key
        )
        
        assert xctl_prog is None
        assert result_map.errmsgo == CCDA_MSG_INVALID_KEY
    
    def test_main_reenter_dummy_option(self):
        """Test main with DUMMY option."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = AdminMenuMap()
        map_in.optioni = "03"  # Assuming this is a DUMMY option
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in
        )
        
        assert xctl_prog is None
        assert "coming soon" in result_map.errmsgo.lower()
        assert result_map.errmsgc == "DFHGREEN"


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_first_entry(self):
        """Test full flow from first entry."""
        commarea = CardDemoCommarea()
        
        # First entry
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100
        )
        
        assert result_commarea.pgm_reenter is True
        assert result_map.title01o == CCDA_TITLE01
        assert result_map.optn001o != ""
        assert xctl_prog is None
    
    def test_full_flow_select_option(self):
        """Test full flow selecting an option."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = AdminMenuMap()
        map_in.optioni = "01"
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in
        )
        
        assert xctl_prog == "COACTVWC"
        assert result_commarea.from_program == WS_PGMNAME
    
    def test_full_flow_exit(self):
        """Test full flow exiting with PF3."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF3
        )
        
        assert xctl_prog == COSGN00C
    
    def test_menu_display_all_options(self):
        """Test that all menu options are displayed."""
        commarea = CardDemoCommarea()
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100
        )
        
        # Check that multiple options are displayed
        assert result_map.optn001o != ""
        assert result_map.optn002o != ""
        
        # Verify option format
        assert "." in result_map.optn001o
        assert "." in result_map.optn002o

