"""
Test suite for COMEN01C.py

Tests for Main Menu transaction program.
"""

import pytest
from unittest.mock import patch

from COMEN01C import (
    MenuOption,
    MenuMap,
    WorkingStorage,
    CardDemoCommarea,
    get_menu_options,
    get_menu_opt_count,
    populate_header_info,
    build_menu_options,
    send_menu_screen,
    receive_menu_screen,
    process_enter_key,
    return_to_signon_screen,
    main,
    WS_PGMNAME,
    WS_TRANID,
    DFHENTER,
    DFHPF3,
    COSGN00C,
    CCDA_TITLE01,
    CCDA_TITLE02
)


# ============================================================================
# Test MenuOption
# ============================================================================

class TestMenuOption:
    """Test MenuOption dataclass."""
    
    def test_creation(self):
        """Test creating a menu option."""
        opt = MenuOption(num="01", name="Test Option", pgmname="TESTPGM", usrtype="U")
        assert opt.num == "01"
        assert opt.name == "Test Option"
        assert opt.pgmname == "TESTPGM"
        assert opt.usrtype == "U"


# ============================================================================
# Test Menu Options Configuration
# ============================================================================

class TestMenuOptionsConfig:
    """Test menu options configuration."""
    
    def test_get_menu_options(self):
        """Test getting menu options."""
        options = get_menu_options()
        assert len(options) > 0
        assert all(isinstance(opt, MenuOption) for opt in options)
    
    def test_get_menu_opt_count(self):
        """Test getting menu option count."""
        count = get_menu_opt_count()
        assert count > 0
        assert count == len(get_menu_options())


# ============================================================================
# Test Map Functions
# ============================================================================

class TestMapFunctions:
    """Test map functions."""
    
    def test_populate_header_info(self):
        """Test populating header info."""
        ws = WorkingStorage()
        map_out = MenuMap()
        
        populate_header_info(ws, map_out)
        
        assert map_out.title01o == CCDA_TITLE01
        assert map_out.title02o == CCDA_TITLE02
        assert map_out.trnnameo == WS_TRANID
        assert map_out.pgmnameo == WS_PGMNAME
        assert map_out.curdateo != ""
        assert map_out.curtimeo != ""
    
    def test_build_menu_options(self):
        """Test building menu options."""
        ws = WorkingStorage()
        map_out = MenuMap()
        
        build_menu_options(ws, map_out)
        
        assert map_out.optn001o != ""
        assert "01." in map_out.optn001o
        assert map_out.optn002o != ""
        assert "02." in map_out.optn002o
    
    def test_send_menu_screen(self):
        """Test sending menu screen."""
        ws = WorkingStorage()
        map_out = MenuMap()
        ws.message = "Test message"
        
        send_menu_screen(ws, map_out)
        
        assert map_out.title01o != ""
        assert map_out.optn001o != ""
        assert map_out.errmsgo == "Test message"
    
    def test_receive_menu_screen(self):
        """Test receiving menu screen."""
        ws = WorkingStorage()
        map_in = MenuMap()
        map_in.optioni = "01"
        
        receive_menu_screen(ws, map_in)
        
        assert ws.optioni == "01"


# ============================================================================
# Test Process Enter Key
# ============================================================================

class TestProcessEnterKey:
    """Test process_enter_key function."""
    
    def test_valid_option(self):
        """Test processing valid option."""
        ws = WorkingStorage()
        map_in = MenuMap()
        map_in.optioni = "01"
        map_out = MenuMap()
        commarea = CardDemoCommarea()
        commarea.usrtyp_user = True
        
        xctl_prog = process_enter_key(ws, map_in, map_out, commarea)
        
        assert xctl_prog is not None
        assert ws.err_flg is False
        assert map_out.optiono == "01"
    
    def test_invalid_option_non_numeric(self):
        """Test processing invalid non-numeric option."""
        ws = WorkingStorage()
        map_in = MenuMap()
        map_in.optioni = "XX"
        map_out = MenuMap()
        commarea = CardDemoCommarea()
        
        xctl_prog = process_enter_key(ws, map_in, map_out, commarea)
        
        assert xctl_prog is None
        assert ws.err_flg is True
        assert "valid option" in ws.message.lower()
    
    def test_invalid_option_too_high(self):
        """Test processing option number too high."""
        ws = WorkingStorage()
        map_in = MenuMap()
        map_in.optioni = "99"
        map_out = MenuMap()
        commarea = CardDemoCommarea()
        
        xctl_prog = process_enter_key(ws, map_in, map_out, commarea)
        
        assert xctl_prog is None
        assert ws.err_flg is True
    
    def test_invalid_option_zero(self):
        """Test processing option zero."""
        ws = WorkingStorage()
        map_in = MenuMap()
        map_in.optioni = "00"
        map_out = MenuMap()
        commarea = CardDemoCommarea()
        
        xctl_prog = process_enter_key(ws, map_in, map_out, commarea)
        
        assert xctl_prog is None
        assert ws.err_flg is True
    
    def test_admin_only_option_by_regular_user(self):
        """Test regular user trying to access admin-only option."""
        ws = WorkingStorage()
        map_in = MenuMap()
        map_in.optioni = "07"  # Admin Menu option
        map_out = MenuMap()
        commarea = CardDemoCommarea()
        commarea.usrtyp_user = True  # Regular user
        
        xctl_prog = process_enter_key(ws, map_in, map_out, commarea)
        
        assert xctl_prog is None
        assert ws.err_flg is True
        assert "Admin Only" in ws.message
    
    def test_dummy_option(self):
        """Test processing dummy option."""
        ws = WorkingStorage()
        map_in = MenuMap()
        map_in.optioni = "08"  # Dummy option
        map_out = MenuMap()
        commarea = CardDemoCommarea()
        commarea.usrtyp_user = True
        
        xctl_prog = process_enter_key(ws, map_in, map_out, commarea)
        
        assert xctl_prog is None
        assert "coming soon" in ws.message.lower()
        assert map_out.errmsgc == "DFHGREEN"
    
    def test_option_with_spaces(self):
        """Test processing option with spaces."""
        ws = WorkingStorage()
        map_in = MenuMap()
        map_in.optioni = " 1 "  # Option with spaces
        map_out = MenuMap()
        commarea = CardDemoCommarea()
        commarea.usrtyp_user = True
        
        xctl_prog = process_enter_key(ws, map_in, map_out, commarea)
        
        assert xctl_prog is not None
        assert ws.err_flg is False


# ============================================================================
# Test Return to Signon Screen
# ============================================================================

class TestReturnToSignonScreen:
    """Test return_to_signon_screen function."""
    
    def test_return_to_signon_default(self):
        """Test returning to signon with default."""
        commarea = CardDemoCommarea()
        commarea.to_program = ""
        
        xctl_prog = return_to_signon_screen(commarea)
        
        assert xctl_prog == COSGN00C
    
    def test_return_to_signon_custom(self):
        """Test returning to signon with custom program."""
        commarea = CardDemoCommarea()
        commarea.to_program = "CUSTOM"
        
        xctl_prog = return_to_signon_screen(commarea)
        
        assert xctl_prog == "CUSTOM"


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
        assert result_commarea.from_program == COSGN00C
    
    def test_main_first_menu_display(self):
        """Test main showing menu for first time."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100
        )
        
        assert result_commarea.pgm_reenter is True
        assert result_map.title01o != ""
        assert result_map.optn001o != ""
        assert xctl_prog is None
    
    def test_main_pf3_exit(self):
        """Test main with PF3."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF3
        )
        
        assert xctl_prog == COSGN00C
        assert result_commarea.to_program == COSGN00C
    
    def test_main_invalid_key(self):
        """Test main with invalid key."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid="DFHPF5"  # Invalid key
        )
        
        assert xctl_prog is None
        assert "Invalid key" in result_map.errmsgo
    
    def test_main_valid_option_selection(self):
        """Test main with valid option selection."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        commarea.usrtyp_user = True
        map_in = MenuMap()
        map_in.optioni = "01"
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in
        )
        
        assert xctl_prog is not None
        assert result_commarea.from_program == WS_PGMNAME
        assert result_commarea.from_tranid == WS_TRANID
    
    def test_main_admin_only_option(self):
        """Test main with admin-only option selected by regular user."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        commarea.usrtyp_user = True
        map_in = MenuMap()
        map_in.optioni = "07"  # Admin Menu
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in
        )
        
        assert xctl_prog is None
        assert "Admin Only" in result_map.errmsgo
    
    def test_main_dummy_option(self):
        """Test main with dummy option."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        commarea.usrtyp_user = True
        map_in = MenuMap()
        map_in.optioni = "08"  # Dummy option
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in
        )
        
        assert xctl_prog is None
        assert "coming soon" in result_map.errmsgo.lower()


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_menu_display_and_selection(self):
        """Test full flow: menu display and option selection."""
        # First entry - show menu
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100
        )
        
        assert result_commarea.pgm_reenter is True
        assert result_map.optn001o != ""
        
        # Select option
        commarea2 = CardDemoCommarea()
        commarea2.pgm_reenter = True
        commarea2.usrtyp_user = True
        map_in = MenuMap()
        map_in.optioni = "01"
        
        result_commarea2, result_map2, xctl_prog2 = main(
            commarea=commarea2,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in
        )
        
        assert xctl_prog2 is not None
        assert result_commarea2.from_program == WS_PGMNAME
    
    def test_full_flow_pf3_exit(self):
        """Test full flow: menu display and PF3 exit."""
        # First entry - show menu
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100
        )
        
        assert xctl_prog is None
        
        # PF3 to exit
        commarea2 = CardDemoCommarea()
        commarea2.pgm_reenter = True
        
        result_commarea2, result_map2, xctl_prog2 = main(
            commarea=commarea2,
            eibcalen=100,
            eibaid=DFHPF3
        )
        
        assert xctl_prog2 == COSGN00C
    
    def test_full_flow_invalid_option(self):
        """Test full flow: menu display and invalid option."""
        # First entry - show menu
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100
        )
        
        # Enter invalid option
        commarea2 = CardDemoCommarea()
        commarea2.pgm_reenter = True
        map_in = MenuMap()
        map_in.optioni = "99"
        
        result_commarea2, result_map2, xctl_prog2 = main(
            commarea=commarea2,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in
        )
        
        assert xctl_prog2 is None
        assert "valid option" in result_map2.errmsgo.lower()

