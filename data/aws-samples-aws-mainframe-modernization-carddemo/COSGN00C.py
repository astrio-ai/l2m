"""
Python Translation of COSGN00C.cbl

Program: COSGN00C
Application: CardDemo
Type: CICS COBOL Program
Function: Signon Screen for the CardDemo Application

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, Dict, Tuple

# Import shared structures
from COACTVWC import CardDemoCommarea


# ============================================================================
# Constants and Literals
# ============================================================================

WS_PGMNAME = "COSGN00C"
WS_TRANID = "CC00"
WS_USRSEC_FILE = "USRSEC  "
COADM01C = "COADM01C"
COMEN01C = "COMEN01C"
DFHENTER = "DFHENTER"
DFHPF3 = "DFHPF3"

# Default titles (from COTTL01Y)
CCDA_TITLE01 = "CardDemo Application"
CCDA_TITLE02 = "Signon Screen"
CCDA_MSG_INVALID_KEY = "Invalid key pressed. Please use Enter or PF3."
CCDA_MSG_THANK_YOU = "Thank you for using CardDemo Application."

# CICS Response Codes
DFHRESP_NORMAL = 0
DFHRESP_NOTFND = 13  # File not found / user not found


# ============================================================================
# User Security Record Structure (SEC-USER-DATA)
# ============================================================================

@dataclass
class UserSecurityRecord:
    """User security record structure (SEC-USER-DATA)."""
    user_id: str = ""  # SEC-USR-ID (8 chars)
    password: str = ""  # SEC-USR-PWD (8 chars)
    user_type: str = ""  # SEC-USR-TYPE (1 char: 'A' for admin, 'U' for user)
    first_name: str = ""  # SEC-USR-FNAME
    last_name: str = ""  # SEC-USR-LNAME
    # Other fields as needed
    
    def to_record_string(self) -> str:
        """Convert record to fixed-width string format."""
        # Format: user_id (8) + password (8) + user_type (1) + first_name + last_name
        # This is a simplified format - adjust based on actual record structure
        return f"{self.user_id[:8]:<8}{self.password[:8]:<8}{self.user_type[:1]:<1}{self.first_name:<20}{self.last_name:<20}"
    
    @classmethod
    def from_record_string(cls, record: str) -> 'UserSecurityRecord':
        """Parse record from fixed-width string format."""
        if len(record) < 17:
            return cls()
        
        user_id = record[0:8].strip()
        password = record[8:16].strip()
        user_type = record[16:17].strip()
        first_name = record[17:37].strip() if len(record) > 37 else ""
        last_name = record[37:57].strip() if len(record) > 57 else ""
        
        return cls(
            user_id=user_id,
            password=password,
            user_type=user_type,
            first_name=first_name,
            last_name=last_name
        )


# ============================================================================
# Map Structure (COSGN0AO/COSGN0AI)
# ============================================================================

@dataclass
class SignonMap:
    """Signon map structure (COSGN0A)."""
    # Header fields
    title01o: str = ""
    title02o: str = ""
    trnnameo: str = ""
    pgmnameo: str = ""
    curdateo: str = ""
    curtimeo: str = ""
    applido: str = ""  # Application ID
    sysido: str = ""  # System ID
    
    # Input fields
    useridi: str = ""  # User ID input
    userido: str = ""  # User ID output
    useridl: int = 0  # User ID length (cursor)
    
    passwdi: str = ""  # Password input
    passwdo: str = ""  # Password output
    passwdl: int = 0  # Password length (cursor)
    
    # Message fields
    errmsgo: str = ""


# ============================================================================
# User Security File Handler
# ============================================================================

class UserSecFileHandler:
    """Handler for user security file (USRSEC) operations."""
    
    def __init__(self, file_name: str):
        self.file_name = file_name
        self._records: Dict[str, UserSecurityRecord] = {}
        self.resp_cd: int = 0
        self.reas_cd: int = 0
    
    def open_file(self):
        """Open file (simulated)."""
        pass
    
    def close_file(self):
        """Close file (simulated)."""
        pass
    
    def read_record(self, user_id: str) -> Optional[UserSecurityRecord]:
        """Read user security record by user ID (CICS READ).
        
        Args:
            user_id: User ID (8 chars)
        
        Returns:
            UserSecurityRecord if found, None otherwise
        """
        user_id = user_id.upper().strip()
        
        if user_id in self._records:
            self.resp_cd = DFHRESP_NORMAL
            return self._records[user_id]
        else:
            self.resp_cd = DFHRESP_NOTFND
            return None
    
    def add_record(self, record: UserSecurityRecord):
        """Add a user security record (for testing)."""
        self._records[record.user_id.upper()] = record
    
    def clear_records(self):
        """Clear all records (for testing)."""
        self._records = {}


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
        self.user_id: str = ""
        self.user_pwd: str = ""
        
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

def populate_header_info(ws: WorkingStorage, map_out: SignonMap):
    """Populate header information (POPULATE-HEADER-INFO)."""
    from datetime import datetime
    
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
    
    # Simulate CICS ASSIGN
    map_out.applido = "CICS"  # Simulated application ID
    map_out.sysido = "SYS1"  # Simulated system ID


def send_signon_screen(ws: WorkingStorage, map_out: SignonMap):
    """Send signon screen (SEND-SIGNON-SCREEN)."""
    populate_header_info(ws, map_out)
    map_out.errmsgo = ws.message


def send_plain_text(ws: WorkingStorage) -> str:
    """Send plain text (SEND-PLAIN-TEXT).
    
    Returns:
        Message to display
    """
    return ws.message


# ============================================================================
# Process Enter Key
# ============================================================================

def process_enter_key(ws: WorkingStorage, map_in: SignonMap, map_out: SignonMap) -> bool:
    """Process enter key (PROCESS-ENTER-KEY).
    
    Returns:
        True if authentication successful, False otherwise
    """
    # Validate user ID
    if not map_in.useridi or map_in.useridi.strip() == "":
        ws.err_flg = True
        ws.message = "Please enter User ID ..."
        map_out.useridl = -1
        send_signon_screen(ws, map_out)
        return False
    
    # Validate password
    if not map_in.passwdi or map_in.passwdi.strip() == "":
        ws.err_flg = True
        ws.message = "Please enter Password ..."
        map_out.passwdl = -1
        send_signon_screen(ws, map_out)
        return False
    
    # Convert to uppercase
    ws.user_id = map_in.useridi.strip().upper()
    ws.user_pwd = map_in.passwdi.strip().upper()
    
    # Read user security file
    if not ws.err_flg:
        return read_user_sec_file(ws, map_out)
    
    return False


# ============================================================================
# Read User Security File
# ============================================================================

def read_user_sec_file(ws: WorkingStorage, map_out: SignonMap) -> bool:
    """Read user security file (READ-USER-SEC-FILE).
    
    Returns:
        True if authentication successful, False otherwise
    """
    if not ws.usrsec_handler:
        ws.err_flg = True
        ws.message = "Unable to verify the User ..."
        map_out.useridl = -1
        send_signon_screen(ws, map_out)
        return False
    
    # Read user record
    user_record = ws.usrsec_handler.read_record(ws.user_id)
    resp_cd = ws.usrsec_handler.resp_cd
    
    if resp_cd == DFHRESP_NORMAL and user_record:
        # Check password
        if user_record.password == ws.user_pwd:
            # Authentication successful
            return True
        else:
            # Wrong password
            ws.err_flg = True
            ws.message = "Wrong Password. Try again ..."
            map_out.passwdl = -1
            send_signon_screen(ws, map_out)
            return False
    elif resp_cd == DFHRESP_NOTFND:
        # User not found
        ws.err_flg = True
        ws.message = "User not found. Try again ..."
        map_out.useridl = -1
        send_signon_screen(ws, map_out)
        return False
    else:
        # Other error
        ws.err_flg = True
        ws.message = "Unable to verify the User ..."
        map_out.useridl = -1
        send_signon_screen(ws, map_out)
        return False


# ============================================================================
# Main Program
# ============================================================================

def main(
    commarea: Optional[CardDemoCommarea] = None,
    eibcalen: int = 0,
    eibaid: str = DFHENTER,
    map_input: Optional[SignonMap] = None,
    usrsec_handler: Optional[UserSecFileHandler] = None
) -> Tuple[CardDemoCommarea, SignonMap, Optional[str], Optional[str]]:
    """Main program entry point (MAIN-PARA).
    
    Returns:
        Tuple of (commarea, map_output, xctl_program, plain_text_message)
        - xctl_program: Program name to XCTL to (COADM01C or COMEN01C)
        - plain_text_message: Message to display if PF3 pressed
    """
    # Initialize working storage
    ws = WorkingStorage()
    ws.err_flg = False
    ws.message = ""
    
    # Initialize commarea if not provided
    if commarea is None:
        commarea = CardDemoCommarea()
    
    # Initialize maps
    map_out = SignonMap()
    if map_input is None:
        map_input = SignonMap()
    
    # Initialize user security file handler
    if usrsec_handler is None:
        ws.usrsec_handler = UserSecFileHandler(WS_USRSEC_FILE)
    else:
        ws.usrsec_handler = usrsec_handler
    
    # Main logic
    if eibcalen == 0:
        # First call - show signon screen
        map_out.useridl = -1
        send_signon_screen(ws, map_out)
        return commarea, map_out, None, None
    else:
        # Process input
        if eibaid == DFHENTER:
            # Process enter key
            auth_success = process_enter_key(ws, map_input, map_out)
            
            if auth_success:
                # Authentication successful - set up commarea and route
                commarea.from_tranid = ws.tranid
                commarea.from_program = ws.pgmname
                # Store user_id (dataclass allows dynamic attribute assignment)
                commarea.user_id = ws.user_id  # type: ignore
                if hasattr(commarea, 'pgm_context'):
                    commarea.pgm_context = 0
                
                # Get user record to determine user type
                user_record = ws.usrsec_handler.read_record(ws.user_id)
                if user_record and user_record.user_type.upper() == "A":
                    # Admin user
                    commarea.usrtyp_user = False
                    xctl_prog = COADM01C
                else:
                    # Regular user
                    commarea.usrtyp_user = True
                    xctl_prog = COMEN01C
                
                return commarea, map_out, xctl_prog, None
            else:
                # Authentication failed - show error
                send_signon_screen(ws, map_out)
                return commarea, map_out, None, None
        elif eibaid == DFHPF3:
            # PF3 - exit with thank you message
            ws.message = CCDA_MSG_THANK_YOU
            plain_text = send_plain_text(ws)
            return commarea, map_out, None, plain_text
        else:
            # Invalid key
            ws.err_flg = True
            ws.message = CCDA_MSG_INVALID_KEY
            send_signon_screen(ws, map_out)
            return commarea, map_out, None, None
    
    # Set message in map
    map_out.errmsgo = ws.message
    
    return commarea, map_out, None, None

