"""
Python Translation of COUSR01C.cbl

Program: COUSR01C
Application: CardDemo
Type: CICS COBOL Program
Function: Add a new Regular/Admin user to USRSEC file

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, Tuple
from datetime import datetime

# Import shared structures
from COACTVWC import CardDemoCommarea
from COSGN00C import UserSecurityRecord


# ============================================================================
# Constants and Literals
# ============================================================================

WS_PGMNAME = "COUSR01C"
WS_TRANID = "CU01"
WS_USRSEC_FILE = "USRSEC  "
COSGN00C = "COSGN00C"
COADM01C = "COADM01C"
DFHENTER = "DFHENTER"
DFHPF3 = "DFHPF3"
DFHPF4 = "DFHPF4"

# Default titles (from COTTL01Y)
CCDA_TITLE01 = "CardDemo Application"
CCDA_TITLE02 = "Add User"
CCDA_MSG_INVALID_KEY = "Invalid key pressed. Please use Enter, PF3, or PF4."

# CICS Response Codes
DFHRESP_NORMAL = 0
DFHRESP_NOTFND = 12
DFHRESP_DUPKEY = 22
DFHRESP_DUPREC = 22


# ============================================================================
# Map Structure (COUSR1AO/COUSR1AI)
# ============================================================================

@dataclass
class UserAddMap:
    """User add map structure (COUSR1A)."""
    # Header fields
    title01o: str = ""
    title02o: str = ""
    trnnameo: str = ""
    pgmnameo: str = ""
    curdateo: str = ""
    curtimeo: str = ""
    
    # User fields
    useridi: str = ""  # User ID input
    userido: str = ""  # User ID output
    useridl: int = 0  # User ID length (cursor)
    
    fnamei: str = ""  # First name input
    fnameo: str = ""  # First name output
    fnamel: int = 0  # First name length (cursor)
    
    lnamei: str = ""  # Last name input
    lnameo: str = ""  # Last name output
    lnamel: int = 0  # Last name length (cursor)
    
    passwdi: str = ""  # Password input
    passwdo: str = ""  # Password output
    passwdl: int = 0  # Password length (cursor)
    
    usrtypei: str = ""  # User type input (A/U)
    usrtypeo: str = ""  # User type output
    usrtypel: int = 0  # User type length (cursor)
    
    # Message fields
    errmsgo: str = ""
    errmsgc: str = ""  # Error message color


# ============================================================================
# User Security File Handler with Write Support
# ============================================================================

class UserSecFileHandler:
    """Handler for USRSEC file with write operations."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self._data: dict = {}  # Key: user_id (uppercase)
        self._sorted_keys: list = []
    
    def open_file(self) -> int:
        """Open file and load data."""
        try:
            existing_data = len(self._data) > 0
            
            if not existing_data:
                try:
                    with open(self.filename, "r", encoding='utf-8') as f:
                        for line in f:
                            line = line.rstrip('\n\r')
                            if line:
                                record = UserSecurityRecord.from_record_string(line)
                                if record and record.user_id:
                                    self._data[record.user_id.upper()] = record
                except FileNotFoundError:
                    pass
            
            self._sorted_keys = sorted(self._data.keys())
            self.resp_cd = DFHRESP_NORMAL
            return 0
        except Exception as e:
            self.resp_cd = 13
            return 13
    
    def close_file(self):
        """Close file and save data."""
        try:
            with open(self.filename, "w", encoding='utf-8') as f:
                for key in self._sorted_keys:
                    record = self._data[key]
                    line = record.to_record_string()
                    f.write(line + '\n')
        except Exception:
            pass
    
    def read_record(self, user_id: str) -> Optional[UserSecurityRecord]:
        """Read user security record by user ID (CICS READ)."""
        user_id = user_id.upper().strip()
        
        if user_id in self._data:
            self.resp_cd = DFHRESP_NORMAL
            return self._data[user_id]
        else:
            self.resp_cd = DFHRESP_NOTFND
            return None
    
    def write_record(self, record: UserSecurityRecord) -> int:
        """Write user security record (CICS WRITE).
        
        Args:
            record: UserSecurityRecord to write
        
        Returns:
            Response code
        """
        if not record.user_id:
            self.resp_cd = 13
            return 13
        
        user_id = record.user_id.upper().strip()
        
        # Check for duplicate key
        if user_id in self._data:
            self.resp_cd = DFHRESP_DUPKEY
            return DFHRESP_DUPKEY
        
        # Add record
        self._data[user_id] = record
        self._sorted_keys = sorted(self._data.keys())
        
        self.resp_cd = DFHRESP_NORMAL
        return DFHRESP_NORMAL
    
    def add_record(self, record: UserSecurityRecord):
        """Add a record (for testing)."""
        if record.user_id:
            self._data[record.user_id.upper()] = record
            self._sorted_keys = sorted(self._data.keys())
    
    def clear_records(self):
        """Clear all records (for testing)."""
        self._data = {}
        self._sorted_keys = []


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
        self.err_flg: bool = False
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        
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
        
        # File handler
        self.usrsec_handler: Optional[UserSecFileHandler] = None


# ============================================================================
# Map Functions
# ============================================================================

def populate_header_info(ws: WorkingStorage, map_out: UserAddMap):
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


def send_usradd_screen(ws: WorkingStorage, map_out: UserAddMap):
    """Send user add screen (SEND-USRADD-SCREEN)."""
    populate_header_info(ws, map_out)
    map_out.errmsgo = ws.message


def receive_usradd_screen(ws: WorkingStorage, map_in: UserAddMap):
    """Receive user add screen (RECEIVE-USRADD-SCREEN)."""
    ws.resp_cd = 0
    ws.reas_cd = 0
    # Extract input fields from map
    # (In real CICS, this would be done automatically by RECEIVE MAP)


def initialize_all_fields(ws: WorkingStorage, map_in: UserAddMap):
    """Initialize all fields (INITIALIZE-ALL-FIELDS)."""
    map_in.fnamel = -1
    map_in.useridi = ""
    map_in.fnamei = ""
    map_in.lnamei = ""
    map_in.passwdi = ""
    map_in.usrtypei = ""
    ws.message = ""


# ============================================================================
# File Operations
# ============================================================================

def write_user_sec_file(ws: WorkingStorage, record: UserSecurityRecord) -> bool:
    """Write user security file (WRITE-USER-SEC-FILE).
    
    Returns:
        True if successful, False otherwise
    """
    if not ws.usrsec_handler:
        ws.err_flg = True
        ws.message = "Unable to Add User..."
        return False
    
    resp_cd = ws.usrsec_handler.write_record(record)
    ws.resp_cd = resp_cd
    
    if resp_cd == DFHRESP_NORMAL:
        # Save to file
        ws.usrsec_handler.close_file()
        return True
    elif resp_cd == DFHRESP_DUPKEY:
        ws.err_flg = True
        ws.message = "User ID already exist..."
        return False
    else:
        ws.err_flg = True
        ws.message = "Unable to Add User..."
        return False


# ============================================================================
# Process Enter Key
# ============================================================================

def process_enter_key(ws: WorkingStorage, map_in: UserAddMap, map_out: UserAddMap) -> bool:
    """Process enter key (PROCESS-ENTER-KEY).
    
    Returns:
        True if user added, False otherwise
    """
    # Validate required fields
    required_fields = [
        ("fnamei", "fnamel", "First Name can NOT be empty..."),
        ("lnamei", "lnamel", "Last Name can NOT be empty..."),
        ("useridi", "useridl", "User ID can NOT be empty..."),
        ("passwdi", "passwdl", "Password can NOT be empty..."),
        ("usrtypei", "usrtypel", "User Type can NOT be empty..."),
    ]
    
    for field_name, length_field, error_msg in required_fields:
        field_value = getattr(map_in, field_name, "").strip()
        if not field_value:
            ws.err_flg = True
            ws.message = error_msg
            setattr(map_out, length_field, -1)
            send_usradd_screen(ws, map_out)
            return False
    
    # All fields validated - create user record
    new_record = UserSecurityRecord()
    new_record.user_id = map_in.useridi.strip()
    new_record.first_name = map_in.fnamei.strip()
    new_record.last_name = map_in.lnamei.strip()
    new_record.password = map_in.passwdi.strip()
    new_record.user_type = map_in.usrtypei.strip()
    
    # Write user record
    if write_user_sec_file(ws, new_record):
        # Success - initialize fields and show success message
        initialize_all_fields(ws, map_in)
        map_out.useridi = ""
        map_out.fnamei = ""
        map_out.lnamei = ""
        map_out.passwdi = ""
        map_out.usrtypei = ""
        
        ws.message = f"User {new_record.user_id} has been added ..."
        map_out.errmsgc = "GREEN"  # DFHGREEN equivalent
        send_usradd_screen(ws, map_out)
        return True
    else:
        # Error already set in write_user_sec_file
        send_usradd_screen(ws, map_out)
        return False


# ============================================================================
# Clear Current Screen
# ============================================================================

def clear_current_screen(ws: WorkingStorage, map_in: UserAddMap, map_out: UserAddMap):
    """Clear current screen (CLEAR-CURRENT-SCREEN)."""
    initialize_all_fields(ws, map_in)
    map_out.useridi = ""
    map_out.fnamei = ""
    map_out.lnamei = ""
    map_out.passwdi = ""
    map_out.usrtypei = ""
    send_usradd_screen(ws, map_out)


# ============================================================================
# Return to Previous Screen
# ============================================================================

def return_to_prev_screen(commarea: CardDemoCommarea) -> str:
    """Return to previous screen (RETURN-TO-PREV-SCREEN).
    
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
    map_input: Optional[UserAddMap] = None,
    usrsec_handler: Optional[UserSecFileHandler] = None
) -> Tuple[CardDemoCommarea, UserAddMap, Optional[str]]:
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
    map_out = UserAddMap()
    if map_input is None:
        map_input = UserAddMap()
    
    # Initialize file handler
    if usrsec_handler is None:
        ws.usrsec_handler = UserSecFileHandler(WS_USRSEC_FILE)
        ws.usrsec_handler.open_file()
    else:
        ws.usrsec_handler = usrsec_handler
    
    map_out.fnamel = -1
    
    # Main logic
    if eibcalen == 0:
        # First call - return to signon
        commarea.to_program = COSGN00C
        xctl_prog = return_to_prev_screen(commarea)
        return commarea, map_out, xctl_prog
    else:
        # Process commarea
        if not commarea.pgm_reenter:
            # First entry - show screen
            commarea.pgm_reenter = True
            
            # Copy map_input to map_out for processing
            map_out = map_input
            map_out.fnamel = -1
            
            send_usradd_screen(ws, map_out)
            xctl_prog = None
        else:
            # Reenter - process input
            receive_usradd_screen(ws, map_input)
            
            # Copy map_input to map_out for processing
            map_out = map_input
            
            if eibaid == DFHENTER:
                # Process enter key
                process_enter_key(ws, map_input, map_out)
                xctl_prog = None
            elif eibaid == DFHPF3:
                # PF3 - return to admin menu
                commarea.to_program = COADM01C
                xctl_prog = return_to_prev_screen(commarea)
                return commarea, map_out, xctl_prog
            elif eibaid == DFHPF4:
                # PF4 - clear screen
                clear_current_screen(ws, map_input, map_out)
                xctl_prog = None
            else:
                # Invalid key
                ws.err_flg = True
                ws.message = CCDA_MSG_INVALID_KEY
                map_out.fnamel = -1
                send_usradd_screen(ws, map_out)
                xctl_prog = None
    
    # Set message in map
    map_out.errmsgo = ws.message
    
    return commarea, map_out, None

