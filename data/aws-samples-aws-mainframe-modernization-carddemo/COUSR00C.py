"""
Python Translation of COUSR00C.cbl

Program: COUSR00C
Application: CardDemo
Type: CICS COBOL Program
Function: List all users from USRSEC file

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, Tuple, List, Dict
from datetime import datetime

# Import shared structures
from COACTVWC import CardDemoCommarea
from COSGN00C import UserSecurityRecord


# ============================================================================
# Constants and Literals
# ============================================================================

WS_PGMNAME = "COUSR00C"
WS_TRANID = "CU00"
WS_USRSEC_FILE = "USRSEC  "
COSGN00C = "COSGN00C"
COADM01C = "COADM01C"
COUSR02C = "COUSR02C"
COUSR03C = "COUSR03C"
DFHENTER = "DFHENTER"
DFHPF3 = "DFHPF3"
DFHPF7 = "DFHPF7"
DFHPF8 = "DFHPF8"

# Default titles (from COTTL01Y)
CCDA_TITLE01 = "CardDemo Application"
CCDA_TITLE02 = "User List"
CCDA_MSG_INVALID_KEY = "Invalid key pressed. Please use Enter, PF3, PF7, or PF8."

# CICS Response Codes
DFHRESP_NORMAL = 0
DFHRESP_NOTFND = 12
DFHRESP_ENDFILE = 10


# ============================================================================
# Program-Specific Commarea (CDEMO-CU00-INFO)
# ============================================================================

@dataclass
class ThisProgCommarea:
    """Program-specific commarea (CDEMO-CU00-INFO)."""
    usrid_first: str = ""  # CDEMO-CU00-USRID-FIRST (8 chars)
    usrid_last: str = ""  # CDEMO-CU00-USRID-LAST (8 chars)
    page_num: int = 0  # CDEMO-CU00-PAGE-NUM
    next_page_flg: bool = False  # CDEMO-CU00-NEXT-PAGE-FLG (False = 'N', True = 'Y')
    usr_sel_flg: str = ""  # CDEMO-CU00-USR-SEL-FLG
    usr_selected: str = ""  # CDEMO-CU00-USR-SELECTED (8 chars)


# ============================================================================
# Map Structure (COUSR0AO/COUSR0AI)
# ============================================================================

@dataclass
class UserListMap:
    """User list map structure (COUSR0A)."""
    # Header fields
    title01o: str = ""
    title02o: str = ""
    trnnameo: str = ""
    pgmnameo: str = ""
    curdateo: str = ""
    curtimeo: str = ""
    
    # User ID input field
    usridini: str = ""  # User ID input
    usridino: str = ""  # User ID output
    usridinl: int = 0  # User ID length (cursor)
    
    # Page number
    pagenumi: int = 0  # Page number input
    
    # Selection and user data fields (10 rows)
    # Row 1
    sel0001i: str = ""  # Selection input (U/D)
    usrid01i: str = ""  # User ID
    fname01i: str = ""  # First name
    lname01i: str = ""  # Last name
    utype01i: str = ""  # User type
    
    # Row 2
    sel0002i: str = ""
    usrid02i: str = ""
    fname02i: str = ""
    lname02i: str = ""
    utype02i: str = ""
    
    # Row 3
    sel0003i: str = ""
    usrid03i: str = ""
    fname03i: str = ""
    lname03i: str = ""
    utype03i: str = ""
    
    # Row 4
    sel0004i: str = ""
    usrid04i: str = ""
    fname04i: str = ""
    lname04i: str = ""
    utype04i: str = ""
    
    # Row 5
    sel0005i: str = ""
    usrid05i: str = ""
    fname05i: str = ""
    lname05i: str = ""
    utype05i: str = ""
    
    # Row 6
    sel0006i: str = ""
    usrid06i: str = ""
    fname06i: str = ""
    lname06i: str = ""
    utype06i: str = ""
    
    # Row 7
    sel0007i: str = ""
    usrid07i: str = ""
    fname07i: str = ""
    lname07i: str = ""
    utype07i: str = ""
    
    # Row 8
    sel0008i: str = ""
    usrid08i: str = ""
    fname08i: str = ""
    lname08i: str = ""
    utype08i: str = ""
    
    # Row 9
    sel0009i: str = ""
    usrid09i: str = ""
    fname09i: str = ""
    lname09i: str = ""
    utype09i: str = ""
    
    # Row 10
    sel0010i: str = ""
    usrid10i: str = ""
    fname10i: str = ""
    lname10i: str = ""
    utype10i: str = ""
    
    # Message fields
    errmsgo: str = ""
    
    def get_row_data(self, row_num: int) -> Tuple[str, str, str, str, str]:
        """Get row data (selection, user_id, first_name, last_name, user_type)."""
        if row_num == 1:
            return (self.sel0001i, self.usrid01i, self.fname01i, self.lname01i, self.utype01i)
        elif row_num == 2:
            return (self.sel0002i, self.usrid02i, self.fname02i, self.lname02i, self.utype02i)
        elif row_num == 3:
            return (self.sel0003i, self.usrid03i, self.fname03i, self.lname03i, self.utype03i)
        elif row_num == 4:
            return (self.sel0004i, self.usrid04i, self.fname04i, self.lname04i, self.utype04i)
        elif row_num == 5:
            return (self.sel0005i, self.usrid05i, self.fname05i, self.lname05i, self.utype05i)
        elif row_num == 6:
            return (self.sel0006i, self.usrid06i, self.fname06i, self.lname06i, self.utype06i)
        elif row_num == 7:
            return (self.sel0007i, self.usrid07i, self.fname07i, self.lname07i, self.utype07i)
        elif row_num == 8:
            return (self.sel0008i, self.usrid08i, self.fname08i, self.lname08i, self.utype08i)
        elif row_num == 9:
            return (self.sel0009i, self.usrid09i, self.fname09i, self.lname09i, self.utype09i)
        elif row_num == 10:
            return (self.sel0010i, self.usrid10i, self.fname10i, self.lname10i, self.utype10i)
        else:
            return ("", "", "", "", "")
    
    def set_row_data(self, row_num: int, sel: str, user_id: str, first_name: str, last_name: str, user_type: str):
        """Set row data."""
        if row_num == 1:
            self.sel0001i = sel
            self.usrid01i = user_id
            self.fname01i = first_name
            self.lname01i = last_name
            self.utype01i = user_type
        elif row_num == 2:
            self.sel0002i = sel
            self.usrid02i = user_id
            self.fname02i = first_name
            self.lname02i = last_name
            self.utype02i = user_type
        elif row_num == 3:
            self.sel0003i = sel
            self.usrid03i = user_id
            self.fname03i = first_name
            self.lname03i = last_name
            self.utype03i = user_type
        elif row_num == 4:
            self.sel0004i = sel
            self.usrid04i = user_id
            self.fname04i = first_name
            self.lname04i = last_name
            self.utype04i = user_type
        elif row_num == 5:
            self.sel0005i = sel
            self.usrid05i = user_id
            self.fname05i = first_name
            self.lname05i = last_name
            self.utype05i = user_type
        elif row_num == 6:
            self.sel0006i = sel
            self.usrid06i = user_id
            self.fname06i = first_name
            self.lname06i = last_name
            self.utype06i = user_type
        elif row_num == 7:
            self.sel0007i = sel
            self.usrid07i = user_id
            self.fname07i = first_name
            self.lname07i = last_name
            self.utype07i = user_type
        elif row_num == 8:
            self.sel0008i = sel
            self.usrid08i = user_id
            self.fname08i = first_name
            self.lname08i = last_name
            self.utype08i = user_type
        elif row_num == 9:
            self.sel0009i = sel
            self.usrid09i = user_id
            self.fname09i = first_name
            self.lname09i = last_name
            self.utype09i = user_type
        elif row_num == 10:
            self.sel0010i = sel
            self.usrid10i = user_id
            self.fname10i = first_name
            self.lname10i = last_name
            self.utype10i = user_type


# ============================================================================
# User Security File Handler with Browse Support
# ============================================================================

class UserSecFileHandler:
    """Handler for USRSEC file with browse operations."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self._data: Dict[str, UserSecurityRecord] = {}
        self._sorted_keys: List[str] = []
        self._browse_active: bool = False
        self._browse_key: Optional[str] = None
        self._browse_index: int = -1
        self._browse_direction: str = "FORWARD"  # FORWARD or BACKWARD
    
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
        pass  # Read-only for this program
    
    def startbr(self, user_id: str = "") -> int:
        """Start browse (CICS STARTBR).
        
        Args:
            user_id: Starting user ID (empty = start from beginning, LOW-VALUES = start from beginning, HIGH-VALUES = end)
        
        Returns:
            Response code
        """
        if not self._sorted_keys:
            # Empty file - allow browse but set position to end
            self._browse_active = True
            self._browse_index = len(self._sorted_keys)  # Index beyond end
            self._browse_key = None
            self._browse_direction = "FORWARD"
            self.resp_cd = DFHRESP_NORMAL
            return DFHRESP_NORMAL
        
        self._browse_active = True
        
        # Handle HIGH-VALUES (end of file) - check if all chars are high-value
        if user_id and len(user_id) == 8:
            # Check if all bytes are 0xFF (HIGH-VALUES)
            is_high_values = all(ord(c) == 255 for c in user_id)
            if is_high_values:
                # HIGH-VALUES - start from end
                self._browse_index = len(self._sorted_keys)
                self._browse_key = None
                self._browse_direction = "BACKWARD"
                self.resp_cd = DFHRESP_NORMAL
                return DFHRESP_NORMAL
        
        if not user_id or not user_id.strip() or (len(user_id) == 8 and user_id == '\x00' * 8):
            # LOW-VALUES or empty - start from beginning
            self._browse_index = 0
            if self._sorted_keys:
                self._browse_key = self._sorted_keys[0]
            else:
                self._browse_key = None
            self._browse_direction = "FORWARD"
        else:
            # Find starting position
            user_id = user_id.strip().upper()
            if user_id in self._sorted_keys:
                self._browse_index = self._sorted_keys.index(user_id)
                self._browse_key = user_id
            else:
                # Find next key >= user_id
                found = False
                for i, key in enumerate(self._sorted_keys):
                    if key >= user_id:
                        self._browse_index = i
                        self._browse_key = key
                        found = True
                        break
                if not found:
                    # Start from end
                    self._browse_index = len(self._sorted_keys)
                    self._browse_key = None
                    self.resp_cd = DFHRESP_NOTFND
                    return DFHRESP_NOTFND
        
        self._browse_direction = "FORWARD"
        self.resp_cd = DFHRESP_NORMAL
        return DFHRESP_NORMAL
    
    def readnext(self) -> Optional[UserSecurityRecord]:
        """Read next record (CICS READNEXT).
        
        Returns:
            UserSecurityRecord if found, None if end of file
        """
        if not self._browse_active:
            self.resp_cd = 13
            return None
        
        if self._browse_index >= len(self._sorted_keys):
            self.resp_cd = DFHRESP_ENDFILE
            return None
        
        key = self._sorted_keys[self._browse_index]
        record = self._data[key]
        self._browse_key = key
        self._browse_index += 1
        self.resp_cd = DFHRESP_NORMAL
        return record
    
    def readprev(self) -> Optional[UserSecurityRecord]:
        """Read previous record (CICS READPREV).
        
        Returns:
            UserSecurityRecord if found, None if end of file
        """
        if not self._browse_active:
            self.resp_cd = 13
            return None
        
        if self._browse_index <= 0:
            self.resp_cd = DFHRESP_ENDFILE
            return None
        
        self._browse_index -= 1
        key = self._sorted_keys[self._browse_index]
        record = self._data[key]
        self._browse_key = key
        self.resp_cd = DFHRESP_NORMAL
        return record
    
    def endbr(self):
        """End browse (CICS ENDBR)."""
        self._browse_active = False
        self._browse_key = None
        self._browse_index = -1
        self._browse_direction = "FORWARD"
    
    def add_record(self, record: UserSecurityRecord):
        """Add a record (for testing)."""
        if record.user_id:
            self._data[record.user_id.upper()] = record
            self._sorted_keys = sorted(self._data.keys())
    
    def clear_records(self):
        """Clear all records (for testing)."""
        self._data = {}
        self._sorted_keys = []
        self._browse_active = False
        self._browse_key = None
        self._browse_index = -1


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
        self.user_sec_eof: bool = False
        self.send_erase_flg: bool = True
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self.rec_count: int = 0
        self.idx: int = 0
        self.page_num: int = 0
        
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
        
        # Current user ID for browse
        self.sec_usr_id: str = ""


# ============================================================================
# Map Functions
# ============================================================================

def populate_header_info(ws: WorkingStorage, map_out: UserListMap):
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


def send_usrlst_screen(ws: WorkingStorage, map_out: UserListMap):
    """Send user list screen (SEND-USRLST-SCREEN)."""
    populate_header_info(ws, map_out)
    map_out.errmsgo = ws.message


def receive_usrlst_screen(ws: WorkingStorage, map_in: UserListMap):
    """Receive user list screen (RECEIVE-USRLST-SCREEN)."""
    ws.resp_cd = 0
    ws.reas_cd = 0
    # Extract input fields from map
    # (In real CICS, this would be done automatically by RECEIVE MAP)


def initialize_user_data(ws: WorkingStorage, map_in: UserListMap, idx: int):
    """Initialize user data for a row (INITIALIZE-USER-DATA)."""
    map_in.set_row_data(idx, "", "", "", "", "")


def populate_user_data(ws: WorkingStorage, map_in: UserListMap, record: UserSecurityRecord, idx: int):
    """Populate user data for a row (POPULATE-USER-DATA)."""
    map_in.set_row_data(
        idx,
        "",  # Selection field (empty initially)
        record.user_id,
        record.first_name,
        record.last_name,
        record.user_type
    )
    
    # Store first and last user IDs
    if idx == 1:
        ws.sec_usr_id = record.user_id
    elif idx == 10:
        pass  # Last user ID will be set after all rows are populated


# ============================================================================
# File Operations
# ============================================================================

def startbr_user_sec_file(ws: WorkingStorage, user_id: str = "") -> bool:
    """Start browse on user security file (STARTBR-USER-SEC-FILE).
    
    Returns:
        True if successful, False otherwise
    """
    if not ws.usrsec_handler:
        ws.err_flg = True
        ws.message = "Unable to lookup User..."
        return False
    
    resp_cd = ws.usrsec_handler.startbr(user_id)
    ws.resp_cd = resp_cd
    
    if resp_cd == DFHRESP_NORMAL:
        return True
    elif resp_cd == DFHRESP_NOTFND:
        ws.user_sec_eof = True
        ws.message = "You are at the top of the page..."
        return False
    else:
        ws.err_flg = True
        ws.message = "Unable to lookup User..."
        return False


def readnext_user_sec_file(ws: WorkingStorage) -> Optional[UserSecurityRecord]:
    """Read next user security record (READNEXT-USER-SEC-FILE).
    
    Returns:
        UserSecurityRecord if found, None if end of file
    """
    if not ws.usrsec_handler:
        ws.err_flg = True
        ws.message = "Unable to lookup User..."
        return None
    
    record = ws.usrsec_handler.readnext()
    ws.resp_cd = ws.usrsec_handler.resp_cd
    
    if ws.resp_cd == DFHRESP_NORMAL:
        return record
    elif ws.resp_cd == DFHRESP_ENDFILE:
        ws.user_sec_eof = True
        ws.message = "You have reached the bottom of the page..."
        return None
    else:
        ws.err_flg = True
        ws.message = "Unable to lookup User..."
        return None


def readprev_user_sec_file(ws: WorkingStorage) -> Optional[UserSecurityRecord]:
    """Read previous user security record (READPREV-USER-SEC-FILE).
    
    Returns:
        UserSecurityRecord if found, None if end of file
    """
    if not ws.usrsec_handler:
        ws.err_flg = True
        ws.message = "Unable to lookup User..."
        return None
    
    record = ws.usrsec_handler.readprev()
    ws.resp_cd = ws.usrsec_handler.resp_cd
    
    if ws.resp_cd == DFHRESP_NORMAL:
        return record
    elif ws.resp_cd == DFHRESP_ENDFILE:
        ws.user_sec_eof = True
        ws.message = "You have reached the top of the page..."
        return None
    else:
        ws.err_flg = True
        ws.message = "Unable to lookup User..."
        return None


def endbr_user_sec_file(ws: WorkingStorage):
    """End browse on user security file (ENDBR-USER-SEC-FILE)."""
    if ws.usrsec_handler:
        ws.usrsec_handler.endbr()


# ============================================================================
# Process Page Forward
# ============================================================================

def process_page_forward(ws: WorkingStorage, map_in: UserListMap, map_out: UserListMap, prog_commarea: ThisProgCommarea, eibaid: str = DFHENTER):
    """Process page forward (PROCESS-PAGE-FORWARD)."""
    # Start browse
    if not startbr_user_sec_file(ws, ws.sec_usr_id):
        if ws.user_sec_eof:
            send_usrlst_screen(ws, map_out)
        return
    
    # Skip first record if not Enter/PF7/PF3
    if eibaid not in (DFHENTER, DFHPF7, DFHPF3):
        readnext_user_sec_file(ws)
    
    # Initialize all rows
    if not ws.user_sec_eof and not ws.err_flg:
        for idx in range(1, 11):
            initialize_user_data(ws, map_in, idx)
    
    # Read up to 10 records
    ws.idx = 1
    
    while ws.idx < 11 and not ws.user_sec_eof and not ws.err_flg:
        record = readnext_user_sec_file(ws)
        if record and not ws.user_sec_eof and not ws.err_flg:
            populate_user_data(ws, map_in, record, ws.idx)
            ws.idx += 1
        else:
            break
    
    # Check if there's a next page
    if not ws.user_sec_eof and not ws.err_flg:
        # Increment page number
        prog_commarea.page_num += 1
        # Try to read one more record to check for next page
        next_record = readnext_user_sec_file(ws)
        if next_record and not ws.user_sec_eof and not ws.err_flg:
            prog_commarea.next_page_flg = True
        else:
            prog_commarea.next_page_flg = False
    else:
        # EOF reached
        prog_commarea.next_page_flg = False
        if ws.idx > 1:
            prog_commarea.page_num += 1
    
    # Store first and last user IDs
    if ws.idx > 1:
        first_record = map_in.get_row_data(1)
        if first_record[1]:  # user_id
            prog_commarea.usrid_first = first_record[1]
        
        # Find last populated row
        for idx in range(10, 0, -1):
            row_data = map_in.get_row_data(idx)
            if row_data[1]:  # user_id
                prog_commarea.usrid_last = row_data[1]
                break
    
    # End browse
    endbr_user_sec_file(ws)
    
    # Update page number in map
    map_in.pagenumi = prog_commarea.page_num
    map_out.pagenumi = prog_commarea.page_num
    
    # Send screen
    send_usrlst_screen(ws, map_out)


# ============================================================================
# Process Page Backward
# ============================================================================

def process_page_backward(ws: WorkingStorage, map_in: UserListMap, map_out: UserListMap, prog_commarea: ThisProgCommarea, eibaid: str = DFHPF7):
    """Process page backward (PROCESS-PAGE-BACKWARD)."""
    # Start browse from first user ID or beginning
    start_user_id = prog_commarea.usrid_first if prog_commarea.usrid_first else ""
    if not startbr_user_sec_file(ws, start_user_id):
        if ws.user_sec_eof:
            send_usrlst_screen(ws, map_out)
        return
    
    # Skip first record if not Enter/PF8
    if eibaid not in (DFHENTER, DFHPF8):
        readprev_user_sec_file(ws)
    
    # Initialize all rows
    if not ws.user_sec_eof and not ws.err_flg:
        for idx in range(1, 11):
            initialize_user_data(ws, map_in, idx)
    
    # Read up to 10 records backwards (starting from index 10, going down to 1)
    ws.idx = 10
    records = []
    
    while ws.idx > 0 and not ws.user_sec_eof and not ws.err_flg:
        record = readprev_user_sec_file(ws)
        if record and not ws.user_sec_eof and not ws.err_flg:
            records.insert(0, record)  # Insert at beginning to maintain forward order
            ws.idx -= 1
        else:
            break
    
    # Populate rows from records (in forward order)
    for i, record in enumerate(records):
        populate_user_data(ws, map_in, record, i + 1)
    
    # Check if there's a previous page
    if not ws.user_sec_eof and not ws.err_flg:
        # Read one more record to check
        prev_record = readprev_user_sec_file(ws)
        if prog_commarea.next_page_flg:
            if prev_record and not ws.user_sec_eof and not ws.err_flg and prog_commarea.page_num > 1:
                prog_commarea.page_num -= 1
            else:
                prog_commarea.page_num = 1
    
    # Store first and last user IDs
    if records:
        prog_commarea.usrid_first = records[0].user_id
        prog_commarea.usrid_last = records[-1].user_id
    
    # End browse
    endbr_user_sec_file(ws)
    
    # Update page number in map
    map_in.pagenumi = prog_commarea.page_num
    map_out.pagenumi = prog_commarea.page_num
    
    # Send screen
    send_usrlst_screen(ws, map_out)


# ============================================================================
# Process Enter Key
# ============================================================================

def process_enter_key(ws: WorkingStorage, map_in: UserListMap, map_out: UserListMap, prog_commarea: ThisProgCommarea, commarea: CardDemoCommarea, eibaid: str = DFHENTER) -> Optional[str]:
    """Process enter key (PROCESS-ENTER-KEY).
    
    Returns:
        Program name to XCTL to, or None
    """
    # Check for selection in any row
    selected_usr_id = ""
    selected_sel_flg = ""
    
    for idx in range(1, 11):
        row_data = map_in.get_row_data(idx)
        sel = row_data[0].strip()  # Selection field
        usr_id = row_data[1].strip()  # User ID
        
        if sel and usr_id:
            selected_sel_flg = sel
            selected_usr_id = usr_id
            break
    
    if selected_sel_flg and selected_usr_id:
        prog_commarea.usr_sel_flg = selected_sel_flg
        prog_commarea.usr_selected = selected_usr_id
        
        # Route based on selection
        if selected_sel_flg.upper() == 'U':
            # Update user
            commarea.to_program = COUSR02C
            commarea.from_tranid = ws.tranid
            commarea.from_program = ws.pgmname
            commarea.pgm_context = 0
            return COUSR02C
        elif selected_sel_flg.upper() == 'D':
            # Delete user
            commarea.to_program = COUSR03C
            commarea.from_tranid = ws.tranid
            commarea.from_program = ws.pgmname
            commarea.pgm_context = 0
            return COUSR03C
        else:
            # Invalid selection
            ws.err_flg = True
            ws.message = "Invalid selection. Valid values are U and D"
            map_out.usridinl = -1
            send_usrlst_screen(ws, map_out)
            return None
    else:
        # No selection - clear flags
        prog_commarea.usr_sel_flg = ""
        prog_commarea.usr_selected = ""
    
    # Set user ID for browse
    if not map_in.usridini or not map_in.usridini.strip():
        ws.sec_usr_id = ""  # LOW-VALUES equivalent
    else:
        ws.sec_usr_id = map_in.usridini.strip()
    
    map_out.usridinl = -1
    
    # Reset page number
    prog_commarea.page_num = 0
    
    # Process page forward
    process_page_forward(ws, map_in, map_out, prog_commarea, eibaid)
    
    if not ws.err_flg:
        map_out.usridino = ""
    
    return None


# ============================================================================
# Process PF7 Key (Previous Page)
# ============================================================================

def process_pf7_key(ws: WorkingStorage, map_in: UserListMap, map_out: UserListMap, prog_commarea: ThisProgCommarea):
    """Process PF7 key (PROCESS-PF7-KEY)."""
    # Set user ID for browse
    if not prog_commarea.usrid_first or not prog_commarea.usrid_first.strip():
        ws.sec_usr_id = ""  # LOW-VALUES equivalent
    else:
        ws.sec_usr_id = prog_commarea.usrid_first
    
    prog_commarea.next_page_flg = True
    map_out.usridinl = -1
    
    if prog_commarea.page_num > 1:
        process_page_backward(ws, map_in, map_out, prog_commarea, DFHPF7)
    else:
        ws.message = "You are already at the top of the page..."
        ws.send_erase_flg = False
        send_usrlst_screen(ws, map_out)


# ============================================================================
# Process PF8 Key (Next Page)
# ============================================================================

def process_pf8_key(ws: WorkingStorage, map_in: UserListMap, map_out: UserListMap, prog_commarea: ThisProgCommarea):
    """Process PF8 key (PROCESS-PF8-KEY)."""
    # Set user ID for browse
    if not prog_commarea.usrid_last or not prog_commarea.usrid_last.strip():
        ws.sec_usr_id = '\xff' * 8  # HIGH-VALUES equivalent (8 bytes of 0xFF)
    else:
        ws.sec_usr_id = prog_commarea.usrid_last
    
    map_out.usridinl = -1
    
    if prog_commarea.next_page_flg:
        process_page_forward(ws, map_in, map_out, prog_commarea, DFHPF8)
    else:
        ws.message = "You are already at the bottom of the page..."
        ws.send_erase_flg = False
        send_usrlst_screen(ws, map_out)


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
    map_input: Optional[UserListMap] = None,
    prog_commarea: Optional[ThisProgCommarea] = None,
    usrsec_handler: Optional[UserSecFileHandler] = None
) -> Tuple[CardDemoCommarea, UserListMap, Optional[str], ThisProgCommarea]:
    """Main program entry point (MAIN-PARA).
    
    Returns:
        Tuple of (commarea, map_output, xctl_program, prog_commarea)
    """
    # Initialize working storage
    ws = WorkingStorage()
    ws.err_flg = False
    ws.user_sec_eof = False
    ws.send_erase_flg = True
    ws.message = ""
    
    # Initialize commarea if not provided
    if commarea is None:
        commarea = CardDemoCommarea()
    
    # Initialize program-specific commarea
    if prog_commarea is None:
        prog_commarea = ThisProgCommarea()
    
    # Initialize maps
    map_out = UserListMap()
    if map_input is None:
        map_input = UserListMap()
    
    # Initialize file handler
    if usrsec_handler is None:
        ws.usrsec_handler = UserSecFileHandler(WS_USRSEC_FILE)
        ws.usrsec_handler.open_file()
    else:
        ws.usrsec_handler = usrsec_handler
    
    map_out.usridinl = -1
    
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
            
            # Process enter key to load first page
            xctl_prog = process_enter_key(ws, map_input, map_out, prog_commarea, commarea, eibaid)
            if xctl_prog:
                return commarea, map_out, xctl_prog, prog_commarea
            xctl_prog = None
        else:
            # Reenter - process input
            receive_usrlst_screen(ws, map_input)
            
            # Copy map_input to map_out for processing
            map_out = map_input
            
            if eibaid == DFHENTER:
                # Process enter key
                xctl_prog = process_enter_key(ws, map_input, map_out, prog_commarea, commarea, eibaid)
                if xctl_prog:
                    return commarea, map_out, xctl_prog, prog_commarea
            elif eibaid == DFHPF3:
                # PF3 - return to admin menu
                commarea.to_program = COADM01C
                xctl_prog = return_to_prev_screen(commarea)
                return commarea, map_out, xctl_prog, prog_commarea
            elif eibaid == DFHPF7:
                # PF7 - previous page
                process_pf7_key(ws, map_input, map_out, prog_commarea)
                xctl_prog = None
            elif eibaid == DFHPF8:
                # PF8 - next page
                process_pf8_key(ws, map_input, map_out, prog_commarea)
                xctl_prog = None
            else:
                # Invalid key
                ws.err_flg = True
                ws.message = CCDA_MSG_INVALID_KEY
                map_out.usridinl = -1
                send_usrlst_screen(ws, map_out)
                xctl_prog = None
    
    # Set message in map
    map_out.errmsgo = ws.message
    
    return commarea, map_out, None, prog_commarea

