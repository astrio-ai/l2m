"""
Python Translation of COMEN01C.cbl

Program: COMEN01C
Application: CardDemo
Type: CICS COBOL Program
Function: Main Menu for the Regular users

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, List, Tuple
from datetime import datetime

# Import shared structures
from COACTVWC import CardDemoCommarea


# ============================================================================
# Constants and Literals
# ============================================================================

WS_PGMNAME = "COMEN01C"
WS_TRANID = "CM00"
WS_USRSEC_FILE = "USRSEC  "
COSGN00C = "COSGN00C"
DFHENTER = "DFHENTER"
DFHPF3 = "DFHPF3"
DFHGREEN = "DFHGREEN"

# Default titles (from COTTL01Y)
CCDA_TITLE01 = "CardDemo Application"
CCDA_TITLE02 = "Main Menu"
CCDA_MSG_INVALID_KEY = "Invalid key pressed. Please use Enter or PF3."


# ============================================================================
# Menu Option Structure
# ============================================================================

@dataclass
class MenuOption:
    """Menu option structure (from COMEN02Y)."""
    num: str = ""  # Option number (e.g., "01")
    name: str = ""  # Option name
    pgmname: str = ""  # Program name to XCTL to, or "DUMMY"
    usrtype: str = ""  # User type: 'U' for user, 'A' for admin only


# ============================================================================
# Map Structure (COMEN1AO/COMEN1AI)
# ============================================================================

@dataclass
class MenuMap:
    """Menu map structure (COMEN1A)."""
    # Header fields
    title01o: str = ""
    title02o: str = ""
    trnnameo: str = ""
    pgmnameo: str = ""
    curdateo: str = ""
    curtimeo: str = ""
    
    # Option fields (up to 12 options)
    optn001o: str = ""
    optn002o: str = ""
    optn003o: str = ""
    optn004o: str = ""
    optn005o: str = ""
    optn006o: str = ""
    optn007o: str = ""
    optn008o: str = ""
    optn009o: str = ""
    optn010o: str = ""
    optn011o: str = ""
    optn012o: str = ""
    
    # Input/Output fields
    optioni: str = ""  # Input field
    optiono: str = ""  # Output field
    
    # Message fields
    errmsgo: str = ""
    errmsgc: str = ""  # Color attribute


# ============================================================================
# Working Storage
# ============================================================================

class WorkingStorage:
    """Working storage for program state."""
    
    def __init__(self):
        self.pgmname: str = WS_PGMNAME
        self.tranid: str = WS_TRANID
        self.message: str = ""
        self.usrsec_file: str = WS_USRSEC_FILE
        self.err_flg: bool = False  # False = 'N', True = 'Y'
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self.option_x: str = ""
        self.option: int = 0
        self.idx: int = 0
        self.menu_opt_txt: str = ""
        
        # Current date/time
        self.curdate_data: str = ""
        self.curdate_month: str = ""
        self.curdate_day: str = ""
        self.curdate_year: str = ""
        self.curdate_yy: str = ""
        self.curdate_mm_dd_yy: str = ""
        self.curtime_hours: str = ""
        self.curtime_minute: str = ""
        self.curtime_second: str = ""
        self.curtime_hh_mm_ss: str = ""


# ============================================================================
# Menu Options Configuration
# ============================================================================

def get_menu_options() -> List[MenuOption]:
    """Get menu options (from COMEN02Y).
    
    This simulates the menu options array. In a real system,
    these would come from a copybook or configuration.
    
    Returns:
        List of MenuOption objects
    """
    options = [
        MenuOption(num="01", name="View Account", pgmname="COACTVWC", usrtype="U"),
        MenuOption(num="02", name="Update Account", pgmname="COACTUPC", usrtype="U"),
        MenuOption(num="03", name="Credit Card List", pgmname="COCRDLIC", usrtype="U"),
        MenuOption(num="04", name="Credit Card Detail", pgmname="COCRDSLC", usrtype="U"),
        MenuOption(num="05", name="Credit Card Update", pgmname="COCRDUPC", usrtype="U"),
        MenuOption(num="06", name="Bill Payment", pgmname="COBIL00C", usrtype="U"),
        MenuOption(num="07", name="Admin Menu", pgmname="COADM01C", usrtype="A"),  # Admin only
        MenuOption(num="08", name="Coming Soon 1", pgmname="DUMMY01", usrtype="U"),
        MenuOption(num="09", name="Coming Soon 2", pgmname="DUMMY02", usrtype="U"),
        MenuOption(num="10", name="Coming Soon 3", pgmname="DUMMY03", usrtype="U"),
    ]
    return options


def get_menu_opt_count() -> int:
    """Get menu option count."""
    return len(get_menu_options())


# ============================================================================
# Map Functions
# ============================================================================

def populate_header_info(ws: WorkingStorage, map_out: MenuMap):
    """Populate header information (POPULATE-HEADER-INFO)."""
    now = datetime.now()
    ws.curdate_data = now.strftime("%Y%m%d%H%M%S")
    
    ws.curdate_month = f"{now.month:02d}"
    ws.curdate_day = f"{now.day:02d}"
    ws.curdate_year = now.strftime("%Y")
    ws.curdate_yy = now.strftime("%y")
    ws.curdate_mm_dd_yy = f"{ws.curdate_month}/{ws.curdate_day}/{ws.curdate_yy}"
    
    ws.curtime_hours = f"{now.hour:02d}"
    ws.curtime_minute = f"{now.minute:02d}"
    ws.curtime_second = f"{now.second:02d}"
    ws.curtime_hh_mm_ss = f"{ws.curtime_hours}:{ws.curtime_minute}:{ws.curtime_second}"
    
    map_out.title01o = CCDA_TITLE01
    map_out.title02o = CCDA_TITLE02
    map_out.trnnameo = ws.tranid
    map_out.pgmnameo = ws.pgmname
    map_out.curdateo = ws.curdate_mm_dd_yy
    map_out.curtimeo = ws.curtime_hh_mm_ss


def build_menu_options(ws: WorkingStorage, map_out: MenuMap):
    """Build menu options (BUILD-MENU-OPTIONS)."""
    options = get_menu_options()
    opt_count = len(options)
    
    for idx in range(1, opt_count + 1):
        if idx <= len(options):
            opt = options[idx - 1]
            ws.menu_opt_txt = f"{opt.num}. {opt.name}"
            
            # Map to output fields
            if idx == 1:
                map_out.optn001o = ws.menu_opt_txt
            elif idx == 2:
                map_out.optn002o = ws.menu_opt_txt
            elif idx == 3:
                map_out.optn003o = ws.menu_opt_txt
            elif idx == 4:
                map_out.optn004o = ws.menu_opt_txt
            elif idx == 5:
                map_out.optn005o = ws.menu_opt_txt
            elif idx == 6:
                map_out.optn006o = ws.menu_opt_txt
            elif idx == 7:
                map_out.optn007o = ws.menu_opt_txt
            elif idx == 8:
                map_out.optn008o = ws.menu_opt_txt
            elif idx == 9:
                map_out.optn009o = ws.menu_opt_txt
            elif idx == 10:
                map_out.optn010o = ws.menu_opt_txt
            elif idx == 11:
                map_out.optn011o = ws.menu_opt_txt
            elif idx == 12:
                map_out.optn012o = ws.menu_opt_txt


def send_menu_screen(ws: WorkingStorage, map_out: MenuMap):
    """Send menu screen (SEND-MENU-SCREEN)."""
    populate_header_info(ws, map_out)
    build_menu_options(ws, map_out)
    
    map_out.errmsgo = ws.message
    # Reset color to default (will be set to DFHGREEN if needed)


def receive_menu_screen(ws: WorkingStorage, map_in: MenuMap):
    """Receive menu screen (RECEIVE-MENU-SCREEN)."""
    ws.resp_cd = 0
    ws.reas_cd = 0
    # Extract option from map input
    ws.optioni = map_in.optioni


# ============================================================================
# Process Enter Key
# ============================================================================

def process_enter_key(ws: WorkingStorage, map_in: MenuMap, map_out: MenuMap, commarea: CardDemoCommarea) -> Optional[str]:
    """Process enter key (PROCESS-ENTER-KEY).
    
    Returns:
        Program name to XCTL to, or None if error or dummy option
    """
    # Extract option from input
    option_str = map_in.optioni.strip()
    
    # Find last non-space character (COBOL performs from end)
    ws.idx = len(option_str)
    while ws.idx > 1 and option_str[ws.idx - 1] == ' ':
        ws.idx -= 1
    
    # Extract option number
    if ws.idx > 0:
        ws.option_x = option_str[:ws.idx]
    else:
        ws.option_x = ""
    
    # Replace spaces with zeros (COBOL INSPECT)
    ws.option_x = ws.option_x.replace(' ', '0')
    
    # Convert to integer
    try:
        ws.option = int(ws.option_x) if ws.option_x.isdigit() else 0
    except ValueError:
        ws.option = 0
    
    map_out.optiono = str(ws.option).zfill(2)
    
    # Validate option
    opt_count = get_menu_opt_count()
    if not ws.option_x.isdigit() or ws.option > opt_count or ws.option == 0:
        ws.err_flg = True
        ws.message = "Please enter a valid option number..."
        return None
    
    # Get menu options
    options = get_menu_options()
    if ws.option > len(options):
        ws.err_flg = True
        ws.message = "Please enter a valid option number..."
        return None
    
    selected_opt = options[ws.option - 1]
    
    # Check if user is regular user and option is admin-only
    if commarea.usrtyp_user and selected_opt.usrtype == "A":
        ws.err_flg = True
        ws.message = "No access - Admin Only option... "
        return None
    
    # If not error, process the option
    if not ws.err_flg:
        # Check if it's a dummy option
        if selected_opt.pgmname[:5] != "DUMMY":
            # Set commarea for XCTL
            commarea.from_tranid = ws.tranid
            commarea.from_program = ws.pgmname
            commarea.pgm_context = 0
            return selected_opt.pgmname
        else:
            # Dummy option - show "coming soon" message
            ws.message = f"This option {selected_opt.name} is coming soon ..."
            map_out.errmsgc = DFHGREEN
            return None
    
    return None


# ============================================================================
# Return to Signon Screen
# ============================================================================

def return_to_signon_screen(commarea: CardDemoCommarea) -> str:
    """Return to signon screen (RETURN-TO-SIGNON-SCREEN).
    
    Returns:
        Program name to XCTL to
    """
    if not commarea.to_program or commarea.to_program.strip() == "":
        return COSGN00C
    return commarea.to_program


# ============================================================================
# Main Program
# ============================================================================

def main(
    commarea: Optional[CardDemoCommarea] = None,
    eibcalen: int = 0,
    eibaid: str = DFHENTER,
    map_input: Optional[MenuMap] = None
) -> Tuple[CardDemoCommarea, MenuMap, Optional[str]]:
    """Main program entry point (MAIN-PARA).
    
    Returns:
        Tuple of (commarea, map_output, xctl_program)
    """
    # Initialize working storage
    ws = WorkingStorage()
    ws.err_flg = False
    ws.message = ""
    
    # Initialize commarea if not provided
    if commarea is None:
        commarea = CardDemoCommarea()
    
    # Initialize maps
    map_out = MenuMap()
    if map_input is None:
        map_input = MenuMap()
    
    # Main logic
    if eibcalen == 0:
        # First call - return to signon
        commarea.from_program = COSGN00C
        xctl_prog = return_to_signon_screen(commarea)
        return commarea, map_out, xctl_prog
    else:
        # Process commarea
        if not commarea.pgm_reenter:
            # First entry - show menu
            commarea.pgm_reenter = True
            send_menu_screen(ws, map_out)
        else:
            # Reenter - process input
            receive_menu_screen(ws, map_input)
            
            if eibaid == DFHENTER:
                xctl_prog = process_enter_key(ws, map_input, map_out, commarea)
                if xctl_prog:
                    # XCTL to program
                    return commarea, map_out, xctl_prog
                else:
                    # Error or dummy option - show menu again
                    send_menu_screen(ws, map_out)
                    xctl_prog = None
            elif eibaid == DFHPF3:
                # PF3 - return to signon
                commarea.to_program = COSGN00C
                xctl_prog = return_to_signon_screen(commarea)
                return commarea, map_out, xctl_prog
            else:
                # Invalid key
                ws.err_flg = True
                ws.message = CCDA_MSG_INVALID_KEY
                send_menu_screen(ws, map_out)
                xctl_prog = None
    
    # Set message in map
    map_out.errmsgo = ws.message
    
    return commarea, map_out, None

