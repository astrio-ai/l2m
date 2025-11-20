"""
Python Translation of COADM01C.cbl

Program: COADM01C
Application: CardDemo
Type: CICS COBOL Program
Function: Admin Menu for Admin users

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, List, Tuple
from datetime import datetime


# ============================================================================
# Constants and Literals
# ============================================================================

WS_PGMNAME = "COADM01C"
WS_TRANID = "CA00"
WS_USRSEC_FILE = "USRSEC  "
COSGN00C = "COSGN00C"
DFHENTER = "DFHENTER"
DFHPF3 = "DFHPF3"
DFHGREEN = "DFHGREEN"

# Default titles (from COTTL01Y)
CCDA_TITLE01 = "CardDemo Application"
CCDA_TITLE02 = "Admin Menu"
CCDA_MSG_INVALID_KEY = "Invalid key pressed. Please use Enter or PF3."


# ============================================================================
# Admin Options Structure
# ============================================================================

@dataclass
class AdminOption:
    """Admin menu option."""
    num: str = ""  # Option number (e.g., "01")
    name: str = ""  # Option name
    pgmname: str = ""  # Program name to XCTL to, or "DUMMY"


# ============================================================================
# Commarea Structure
# ============================================================================

@dataclass
class CardDemoCommarea:
    """CardDemo commarea structure."""
    from_program: str = ""
    from_tranid: str = ""
    to_program: str = ""
    to_tranid: str = ""
    pgm_enter: bool = False
    pgm_reenter: bool = False
    pgm_context: int = 0
    usrtyp_user: bool = True
    last_mapset: str = ""
    last_map: str = ""
    
    # Other fields that may be in commarea
    acct_id: str = ""
    cust_id: str = ""
    card_num: str = ""


# ============================================================================
# Map Structure (COADM1AO/COADM1AI)
# ============================================================================

@dataclass
class AdminMenuMap:
    """Admin menu map structure."""
    # Header fields
    title01o: str = ""
    title02o: str = ""
    trnnameo: str = ""
    pgmnameo: str = ""
    curdateo: str = ""
    curtimeo: str = ""
    
    # Option fields (up to 10 options)
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
        self.admin_opt_txt: str = ""
        
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
# Admin Options Configuration
# ============================================================================

def get_admin_options() -> List[AdminOption]:
    """Get admin menu options (from COADM02Y).
    
    This simulates the admin options array. In a real system,
    this would come from a copybook or configuration file.
    """
    return [
        AdminOption(num="01", name="View Accounts", pgmname="COACTVWC"),
        AdminOption(num="02", name="Update Accounts", pgmname="COACTUPC"),
        AdminOption(num="03", name="View Customers", pgmname="DUMMY"),
        AdminOption(num="04", name="Update Customers", pgmname="DUMMY"),
        AdminOption(num="05", name="Transaction Reports", pgmname="DUMMY"),
        AdminOption(num="06", name="System Settings", pgmname="DUMMY"),
        AdminOption(num="07", name="User Management", pgmname="DUMMY"),
        AdminOption(num="08", name="Audit Logs", pgmname="DUMMY"),
        AdminOption(num="09", name="Backup/Restore", pgmname="DUMMY"),
        AdminOption(num="10", name="Exit", pgmname="COSGN00C"),
    ]


def get_admin_opt_count() -> int:
    """Get count of admin options."""
    return len(get_admin_options())


# ============================================================================
# Map Functions
# ============================================================================

def populate_header_info(ws: WorkingStorage, map_out: AdminMenuMap):
    """Populate header information (POPULATE-HEADER-INFO)."""
    # Get current date/time
    now = datetime.now()
    ws.curdate_data = now.strftime("%Y%m%d%H%M%S")
    
    # Format date
    ws.curdate_month = f"{now.month:02d}"
    ws.curdate_day = f"{now.day:02d}"
    ws.curdate_year = now.strftime("%Y")
    ws.curdate_yy = now.strftime("%y")
    ws.curdate_mm_dd_yy = f"{ws.curdate_month}/{ws.curdate_day}/{ws.curdate_yy}"
    
    # Format time
    ws.curtime_hours = f"{now.hour:02d}"
    ws.curtime_minute = f"{now.minute:02d}"
    ws.curtime_second = f"{now.second:02d}"
    ws.curtime_hh_mm_ss = f"{ws.curtime_hours}:{ws.curtime_minute}:{ws.curtime_second}"
    
    # Populate map fields
    map_out.title01o = CCDA_TITLE01
    map_out.title02o = CCDA_TITLE02
    map_out.trnnameo = ws.tranid
    map_out.pgmnameo = ws.pgmname
    map_out.curdateo = ws.curdate_mm_dd_yy
    map_out.curtimeo = ws.curtime_hh_mm_ss


def build_menu_options(ws: WorkingStorage, map_out: AdminMenuMap):
    """Build menu options (BUILD-MENU-OPTIONS)."""
    options = get_admin_options()
    opt_count = get_admin_opt_count()
    
    # Map option output fields
    opt_fields = [
        map_out.optn001o, map_out.optn002o, map_out.optn003o,
        map_out.optn004o, map_out.optn005o, map_out.optn006o,
        map_out.optn007o, map_out.optn008o, map_out.optn009o,
        map_out.optn010o
    ]
    
    for idx in range(1, opt_count + 1):
        if idx <= len(options):
            opt = options[idx - 1]
            # Build option text: "01. View Accounts"
            opt_txt = f"{opt.num}. {opt.name}"
            
            # Assign to appropriate output field
            if idx <= 10:
                # Use list indexing to update the field
                if idx == 1:
                    map_out.optn001o = opt_txt
                elif idx == 2:
                    map_out.optn002o = opt_txt
                elif idx == 3:
                    map_out.optn003o = opt_txt
                elif idx == 4:
                    map_out.optn004o = opt_txt
                elif idx == 5:
                    map_out.optn005o = opt_txt
                elif idx == 6:
                    map_out.optn006o = opt_txt
                elif idx == 7:
                    map_out.optn007o = opt_txt
                elif idx == 8:
                    map_out.optn008o = opt_txt
                elif idx == 9:
                    map_out.optn009o = opt_txt
                elif idx == 10:
                    map_out.optn010o = opt_txt


def send_menu_screen(ws: WorkingStorage, map_out: AdminMenuMap):
    """Send menu screen (SEND-MENU-SCREEN)."""
    populate_header_info(ws, map_out)
    build_menu_options(ws, map_out)
    map_out.errmsgo = ws.message


def receive_menu_screen(ws: WorkingStorage, map_in: AdminMenuMap) -> int:
    """Receive menu screen (RECEIVE-MENU-SCREEN).
    
    Returns response code (0 = normal).
    """
    # In real CICS, this would receive from the map
    # For simulation, we just return success
    ws.resp_cd = 0
    ws.reas_cd = 0
    return 0


# ============================================================================
# Option Processing
# ============================================================================

def process_enter_key(ws: WorkingStorage, map_in: AdminMenuMap, map_out: AdminMenuMap, 
                     commarea: CardDemoCommarea) -> Optional[str]:
    """Process Enter key (PROCESS-ENTER-KEY).
    
    Returns target program name if XCTL should occur, None otherwise.
    """
    # Find last non-space character in option input
    option_input = map_in.optioni.strip()
    
    # Replace spaces with zeros
    option_x = option_input.replace(' ', '0')
    
    # Convert to integer
    try:
        ws.option = int(option_x) if option_x else 0
    except ValueError:
        ws.option = 0
    
    # Store in output field
    map_out.optiono = f"{ws.option:02d}"
    
    # Validate option
    opt_count = get_admin_opt_count()
    if not option_x.isdigit() or ws.option > opt_count or ws.option == 0:
        ws.err_flg = True
        ws.message = "Please enter a valid option number..."
        send_menu_screen(ws, map_out)
        return None
    
    # Process valid option
    if not ws.err_flg:
        options = get_admin_options()
        if ws.option > 0 and ws.option <= len(options):
            selected_opt = options[ws.option - 1]
            
            # Check if not DUMMY
            if selected_opt.pgmname and selected_opt.pgmname[:5] != "DUMMY":
                # Set commarea fields for XCTL
                commarea.from_tranid = ws.tranid
                commarea.from_program = ws.pgmname
                commarea.pgm_context = 0
                # Return target program name for XCTL
                return selected_opt.pgmname
            else:
                # DUMMY option - show "coming soon" message
                ws.message = "This option is coming soon ..."
                map_out.errmsgc = DFHGREEN
                send_menu_screen(ws, map_out)
                return None
    
    return None


def return_to_signon_screen(commarea: CardDemoCommarea) -> str:
    """Return to signon screen (RETURN-TO-SIGNON-SCREEN).
    
    Returns target program name.
    """
    if not commarea.to_program or commarea.to_program.strip() == "":
        commarea.to_program = COSGN00C
    return commarea.to_program


# ============================================================================
# Main Program
# ============================================================================

def abend_program(abend_code: str = "9999", reason: str = ""):
    """Abend program."""
    print(f"ABEND: {abend_code} - {reason}")
    sys.exit(1)


def main(
    commarea: Optional[CardDemoCommarea] = None,
    eibcalen: int = 0,
    eibaid: str = DFHENTER,
    map_input: Optional[AdminMenuMap] = None
) -> Tuple[CardDemoCommarea, AdminMenuMap, Optional[str]]:
    """Main program entry point (MAIN-PARA).
    
    Returns:
        Tuple of (commarea, map_output, xctl_program)
        - commarea: Updated commarea
        - map_output: Output map
        - xctl_program: Program name to XCTL to, or None
    """
    # Initialize working storage
    ws = WorkingStorage()
    ws.err_flg = False
    ws.message = ""
    
    # Initialize commarea if not provided
    if commarea is None:
        commarea = CardDemoCommarea()
    
    # Initialize map output
    map_out = AdminMenuMap()
    
    # Initialize map input if not provided
    if map_input is None:
        map_input = AdminMenuMap()
    
    # Main logic
    if eibcalen == 0:
        # Return to signon screen
        commarea.from_program = COSGN00C
        xctl_prog = return_to_signon_screen(commarea)
        return commarea, map_out, xctl_prog
    else:
        # Process commarea
        if not commarea.pgm_reenter:
            # First entry - send menu screen
            commarea.pgm_reenter = True
            # Clear map (LOW-VALUES)
            map_out = AdminMenuMap()
            send_menu_screen(ws, map_out)
            return commarea, map_out, None
        else:
            # Reenter - receive and process input
            receive_menu_screen(ws, map_input)
            
            xctl_prog = None
            
            if eibaid == DFHENTER:
                # Process Enter key
                xctl_prog = process_enter_key(ws, map_input, map_out, commarea)
                # If no XCTL, send menu screen with message
                if xctl_prog is None:
                    send_menu_screen(ws, map_out)
            elif eibaid == DFHPF3:
                # Exit - return to signon
                commarea.to_program = COSGN00C
                xctl_prog = return_to_signon_screen(commarea)
            else:
                # Invalid key
                ws.err_flg = True
                ws.message = CCDA_MSG_INVALID_KEY
                send_menu_screen(ws, map_out)
            
            return commarea, map_out, xctl_prog
    
    return commarea, map_out, None

