"""
Python Translation of COUSR03C.cbl

Program: COUSR03C
Application: CardDemo
Type: CICS COBOL Program
Function: Delete a user from USRSEC file

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

WS_PGMNAME = "COUSR03C"
WS_TRANID = "CU03"
WS_USRSEC_FILE = "USRSEC  "
COSGN00C = "COSGN00C"
COADM01C = "COADM01C"
DFHENTER = "DFHENTER"
DFHPF3 = "DFHPF3"
DFHPF4 = "DFHPF4"
DFHPF5 = "DFHPF5"
DFHPF12 = "DFHPF12"

# Default titles (from COTTL01Y)
CCDA_TITLE01 = "CardDemo Application"
CCDA_TITLE02 = "Delete User"
CCDA_MSG_INVALID_KEY = "Invalid key pressed. Please use Enter, PF3, PF4, PF5, or PF12."

# CICS Response Codes
DFHRESP_NORMAL = 0
DFHRESP_NOTFND = 12


# ============================================================================
# Program-Specific Commarea (CDEMO-CU03-INFO)
# ============================================================================

@dataclass
class ThisProgCommarea:
    """Program-specific commarea (CDEMO-CU03-INFO)."""
    usrid_first: str = ""  # CDEMO-CU03-USRID-FIRST (8 chars)
    usrid_last: str = ""  # CDEMO-CU03-USRID-LAST (8 chars)
    page_num: int = 0  # CDEMO-CU03-PAGE-NUM
    next_page_flg: bool = False  # CDEMO-CU03-NEXT-PAGE-FLG
    usr_sel_flg: str = ""  # CDEMO-CU03-USR-SEL-FLG
    usr_selected: str = ""  # CDEMO-CU03-USR-SELECTED (8 chars)


# ============================================================================
# Map Structure (COUSR3AO/COUSR3AI)
# ============================================================================

@dataclass
class UserDeleteMap:
    """User delete map structure (COUSR3A)."""
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
    
    usrtypei: str = ""  # User type input (A/U)
    usrtypeo: str = ""  # User type output
    usrtypel: int = 0  # User type length (cursor)
    
    # Message fields
    errmsgo: str = ""
    errmsgc: str = ""  # Error message color


# ============================================================================
# User Security File Handler with Delete Support
# ============================================================================

class UserSecFileHandler:
    """Handler for USRSEC file with delete operations."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self._data: dict = {}  # Key: user_id (uppercase)
        self._sorted_keys: list = []
        self._locked_records: dict = {}  # Track locked records for READ UPDATE
    
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
    
    def read_for_update(self, user_id: str) -> Optional[UserSecurityRecord]:
        """Read user security record for update (CICS READ UPDATE).
        
        This locks the record for update/delete.
        
        Args:
            user_id: User ID (8 chars)
        
        Returns:
            UserSecurityRecord if found, None otherwise
        """
        user_id = user_id.upper().strip()
        
        if user_id in self._data:
            # Lock the record
            self._locked_records[user_id] = self._data[user_id]
            self.resp_cd = DFHRESP_NORMAL
            return self._data[user_id]
        else:
            self.resp_cd = DFHRESP_NOTFND
            return None
    
    def delete_record(self, user_id: str) -> int:
        """Delete user security record (CICS DELETE).
        
        This deletes a record that was previously read for update.
        
        Args:
            user_id: User ID to delete
        
        Returns:
            Response code
        """
        user_id = user_id.upper().strip()
        
        # Check if record was locked
        if user_id not in self._locked_records:
            self.resp_cd = DFHRESP_NOTFND
            return DFHRESP_NOTFND
        
        # Check if record still exists
        if user_id not in self._data:
            self.resp_cd = DFHRESP_NOTFND
            return DFHRESP_NOTFND
        
        # Delete the record
        del self._data[user_id]
        self._sorted_keys = sorted(self._data.keys())
        # Unlock the record
        if user_id in self._locked_records:
            del self._locked_records[user_id]
        
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
        self._locked_records = {}


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
        self.usr_modified: bool = False  # Not really used for delete, but kept for compatibility
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        
        # Current user record
        self.sec_user_record: Optional[UserSecurityRecord] = None
        
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

def populate_header_info(ws: WorkingStorage, map_out: UserDeleteMap):
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


def send_usrdel_screen(ws: WorkingStorage, map_out: UserDeleteMap):
    """Send user delete screen (SEND-USRDEL-SCREEN)."""
    populate_header_info(ws, map_out)
    map_out.errmsgo = ws.message


def receive_usrdel_screen(ws: WorkingStorage, map_in: UserDeleteMap):
    """Receive user delete screen (RECEIVE-USRDEL-SCREEN)."""
    ws.resp_cd = 0
    ws.reas_cd = 0
    # Extract input fields from map
    # (In real CICS, this would be done automatically by RECEIVE MAP)


def initialize_all_fields(ws: WorkingStorage, map_in: UserDeleteMap):
    """Initialize all fields (INITIALIZE-ALL-FIELDS)."""
    map_in.useridl = -1
    map_in.useridi = ""
    map_in.fnamei = ""
    map_in.lnamei = ""
    map_in.usrtypei = ""
    ws.message = ""


# ============================================================================
# File Operations
# ============================================================================

def read_user_sec_file(ws: WorkingStorage, user_id: str, map_out: Optional[UserDeleteMap] = None) -> bool:
    """Read user security file for update (READ-USER-SEC-FILE).
    
    Args:
        ws: Working storage
        user_id: User ID to read
        map_out: Optional map to set message color
    
    Returns:
        True if successful, False otherwise
    """
    if not ws.usrsec_handler:
        ws.err_flg = True
        ws.message = "Unable to lookup User..."
        return False
    
    record = ws.usrsec_handler.read_for_update(user_id)
    ws.resp_cd = ws.usrsec_handler.resp_cd
    
    if ws.resp_cd == DFHRESP_NORMAL:
        ws.sec_user_record = record
        ws.message = "Press PF5 key to delete this user ..."
        if map_out:
            map_out.errmsgc = "NEUTR"  # DFHNEUTR equivalent
        return True
    elif ws.resp_cd == DFHRESP_NOTFND:
        ws.err_flg = True
        ws.message = "User ID NOT found..."
        return False
    else:
        ws.err_flg = True
        ws.message = "Unable to lookup User..."
        return False


def delete_user_sec_file(ws: WorkingStorage, user_id: str) -> bool:
    """Delete user security file (DELETE-USER-SEC-FILE).
    
    Args:
        ws: Working storage
        user_id: User ID to delete
    
    Returns:
        True if successful, False otherwise
    """
    if not ws.usrsec_handler:
        ws.err_flg = True
        ws.message = "Unable to Update User..."
        return False
    
    resp_cd = ws.usrsec_handler.delete_record(user_id)
    ws.resp_cd = resp_cd
    
    if resp_cd == DFHRESP_NORMAL:
        # Save to file
        ws.usrsec_handler.close_file()
        return True
    elif resp_cd == DFHRESP_NOTFND:
        ws.err_flg = True
        ws.message = "User ID NOT found..."
        return False
    else:
        ws.err_flg = True
        ws.message = "Unable to Update User..."
        return False


# ============================================================================
# Process Enter Key
# ============================================================================

def process_enter_key(ws: WorkingStorage, map_in: UserDeleteMap, map_out: UserDeleteMap) -> bool:
    """Process enter key (PROCESS-ENTER-KEY).
    
    Returns:
        True if user read successfully, False otherwise
    """
    # Validate user ID
    if not map_in.useridi or not map_in.useridi.strip():
        ws.err_flg = True
        ws.message = "User ID can NOT be empty..."
        map_out.useridl = -1
        send_usrdel_screen(ws, map_out)
        return False
    
    # Clear other fields
    map_in.fnamei = ""
    map_in.lnamei = ""
    map_in.usrtypei = ""
    
    # Read user record
    user_id = map_in.useridi.strip()
    if read_user_sec_file(ws, user_id, map_out):
        # Populate fields from record (no password shown)
        if ws.sec_user_record:
            map_in.fnamei = ws.sec_user_record.first_name
            map_in.lnamei = ws.sec_user_record.last_name
            map_in.usrtypei = ws.sec_user_record.user_type
            map_out.fnamei = ws.sec_user_record.first_name
            map_out.lnamei = ws.sec_user_record.last_name
            map_out.usrtypei = ws.sec_user_record.user_type
        
        send_usrdel_screen(ws, map_out)
        return True
    else:
        send_usrdel_screen(ws, map_out)
        return False


# ============================================================================
# Delete User Info
# ============================================================================

def delete_user_info(ws: WorkingStorage, map_in: UserDeleteMap, map_out: UserDeleteMap) -> bool:
    """Delete user info (DELETE-USER-INFO).
    
    Returns:
        True if delete successful, False otherwise
    """
    # Validate user ID
    if not map_in.useridi or not map_in.useridi.strip():
        ws.err_flg = True
        ws.message = "User ID can NOT be empty..."
        map_out.useridl = -1
        send_usrdel_screen(ws, map_out)
        return False
    
    # Read user record for update (to lock it)
    user_id = map_in.useridi.strip()
    if not read_user_sec_file(ws, user_id, map_out):
        send_usrdel_screen(ws, map_out)
        return False
    
    # Delete the record
    if delete_user_sec_file(ws, user_id):
        # Success - initialize fields and show success message
        initialize_all_fields(ws, map_in)
        map_out.useridi = ""
        map_out.fnamei = ""
        map_out.lnamei = ""
        map_out.usrtypei = ""
        
        ws.message = f"User {user_id} has been deleted ..."
        map_out.errmsgc = "GREEN"  # DFHGREEN equivalent
        send_usrdel_screen(ws, map_out)
        return True
    else:
        # Error already set in delete_user_sec_file
        send_usrdel_screen(ws, map_out)
        return False


# ============================================================================
# Clear Current Screen
# ============================================================================

def clear_current_screen(ws: WorkingStorage, map_in: UserDeleteMap, map_out: UserDeleteMap):
    """Clear current screen (CLEAR-CURRENT-SCREEN)."""
    initialize_all_fields(ws, map_in)
    map_out.useridi = ""
    map_out.fnamei = ""
    map_out.lnamei = ""
    map_out.usrtypei = ""
    send_usrdel_screen(ws, map_out)


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
    map_input: Optional[UserDeleteMap] = None,
    prog_commarea: Optional[ThisProgCommarea] = None,
    usrsec_handler: Optional[UserSecFileHandler] = None
) -> Tuple[CardDemoCommarea, UserDeleteMap, Optional[str], ThisProgCommarea]:
    """Main program entry point (MAIN-PARA).
    
    Returns:
        Tuple of (commarea, map_output, xctl_program, prog_commarea)
    """
    # Initialize working storage
    ws = WorkingStorage()
    ws.err_flg = False
    ws.usr_modified = False
    ws.message = ""
    
    # Initialize commarea if not provided
    if commarea is None:
        commarea = CardDemoCommarea()
    
    # Initialize program-specific commarea
    if prog_commarea is None:
        prog_commarea = ThisProgCommarea()
    
    # Initialize maps
    map_out = UserDeleteMap()
    if map_input is None:
        map_input = UserDeleteMap()
    
    # Initialize file handler
    if usrsec_handler is None:
        ws.usrsec_handler = UserSecFileHandler(WS_USRSEC_FILE)
        ws.usrsec_handler.open_file()
    else:
        ws.usrsec_handler = usrsec_handler
    
    map_out.useridl = -1
    
    # Main logic
    if eibcalen == 0:
        # First call - return to signon
        commarea.to_program = COSGN00C
        xctl_prog = return_to_prev_screen(commarea)
        return commarea, map_out, xctl_prog, prog_commarea
    else:
        # Process commarea
        if not commarea.pgm_reenter:
            # First entry - show screen
            commarea.pgm_reenter = True
            
            # Copy map_input to map_out for processing
            map_out = map_input
            map_out.useridl = -1
            
            # If user was selected from list, process it
            if prog_commarea and prog_commarea.usr_selected and prog_commarea.usr_selected.strip():
                map_out.useridi = prog_commarea.usr_selected
                map_in = map_input
                map_in.useridi = prog_commarea.usr_selected
                process_enter_key(ws, map_in, map_out)
            
            send_usrdel_screen(ws, map_out)
            xctl_prog = None
        else:
            # Reenter - process input
            receive_usrdel_screen(ws, map_input)
            
            # Copy map_input to map_out for processing
            map_out = map_input
            
            if eibaid == DFHENTER:
                # Process enter key
                process_enter_key(ws, map_input, map_out)
                xctl_prog = None
            elif eibaid == DFHPF3:
                # PF3 - exit
                if not commarea.from_program or not commarea.from_program.strip():
                    commarea.to_program = COADM01C
                else:
                    commarea.to_program = commarea.from_program
                xctl_prog = return_to_prev_screen(commarea)
                return commarea, map_out, xctl_prog, prog_commarea
            elif eibaid == DFHPF4:
                # PF4 - clear screen
                clear_current_screen(ws, map_input, map_out)
                xctl_prog = None
            elif eibaid == DFHPF5:
                # PF5 - delete user
                delete_user_info(ws, map_input, map_out)
                xctl_prog = None
            elif eibaid == DFHPF12:
                # PF12 - exit
                commarea.to_program = COADM01C
                xctl_prog = return_to_prev_screen(commarea)
                return commarea, map_out, xctl_prog, prog_commarea
            else:
                # Invalid key
                ws.err_flg = True
                ws.message = CCDA_MSG_INVALID_KEY
                map_out.useridl = -1
                send_usrdel_screen(ws, map_out)
                xctl_prog = None
    
    # Set message in map
    map_out.errmsgo = ws.message
    
    return commarea, map_out, None, prog_commarea

