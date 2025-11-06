"""
Python Translation of COTRN00C.cbl

Program: COTRN00C
Application: CardDemo
Type: CICS COBOL Program
Function: List Transactions from TRANSACT file

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, List, Tuple
from decimal import Decimal
from datetime import datetime

# Import shared structures
from COACTVWC import CardDemoCommarea
from COBIL00C import TranRecord, parse_tran_record, format_tran_record


# ============================================================================
# Constants and Literals
# ============================================================================

WS_PGMNAME = "COTRN00C"
WS_TRANID = "CT00"
WS_TRANSACT_FILE = "TRANSACT"
COSGN00C = "COSGN00C"
COMEN01C = "COMEN01C"
COTRN01C = "COTRN01C"
DFHENTER = "DFHENTER"
DFHPF3 = "DFHPF3"
DFHPF7 = "DFHPF7"
DFHPF8 = "DFHPF8"

# Default titles (from COTTL01Y)
CCDA_TITLE01 = "CardDemo Application"
CCDA_TITLE02 = "Transaction List"
CCDA_MSG_INVALID_KEY = "Invalid key pressed. Please use Enter, PF3, PF7, or PF8."

# CICS Response Codes
DFHRESP_NORMAL = 0
DFHRESP_NOTFND = 12
DFHRESP_ENDFILE = 10


# ============================================================================
# Program-Specific Commarea (CDEMO-CT00-INFO)
# ============================================================================

@dataclass
class ThisProgCommarea:
    """Program-specific commarea (CDEMO-CT00-INFO)."""
    trnid_first: str = ""  # CDEMO-CT00-TRNID-FIRST (16 chars)
    trnid_last: str = ""  # CDEMO-CT00-TRNID-LAST (16 chars)
    page_num: int = 0  # CDEMO-CT00-PAGE-NUM
    next_page_flg: bool = False  # CDEMO-CT00-NEXT-PAGE-FLG (False = 'N', True = 'Y')
    trn_sel_flg: str = ""  # CDEMO-CT00-TRN-SEL-FLG
    trn_selected: str = ""  # CDEMO-CT00-TRN-SELECTED (16 chars)


# ============================================================================
# Map Structure (COTRN0AO/COTRN0AI)
# ============================================================================

@dataclass
class TransactionListMap:
    """Transaction list map structure (COTRN0A)."""
    # Header fields
    title01o: str = ""
    title02o: str = ""
    trnnameo: str = ""
    pgmnameo: str = ""
    curdateo: str = ""
    curtimeo: str = ""
    
    # Transaction ID input
    trnidini: str = ""  # Transaction ID input
    trnidino: str = ""  # Transaction ID output
    trnidinl: int = 0  # Transaction ID length (cursor)
    
    # Page number
    pagenumi: str = ""  # Page number input (from commarea)
    pagenumo: str = ""  # Page number output
    
    # Transaction rows (up to 10)
    sel0001i: str = ""  # Selection input row 1
    trnid01i: str = ""  # Transaction ID row 1
    tdate01i: str = ""  # Transaction date row 1
    tdesc01i: str = ""  # Transaction description row 1
    tamt001i: str = ""  # Transaction amount row 1
    
    sel0002i: str = ""
    trnid02i: str = ""
    tdate02i: str = ""
    tdesc02i: str = ""
    tamt002i: str = ""
    
    sel0003i: str = ""
    trnid03i: str = ""
    tdate03i: str = ""
    tdesc03i: str = ""
    tamt003i: str = ""
    
    sel0004i: str = ""
    trnid04i: str = ""
    tdate04i: str = ""
    tdesc04i: str = ""
    tamt004i: str = ""
    
    sel0005i: str = ""
    trnid05i: str = ""
    tdate05i: str = ""
    tdesc05i: str = ""
    tamt005i: str = ""
    
    sel0006i: str = ""
    trnid06i: str = ""
    tdate06i: str = ""
    tdesc06i: str = ""
    tamt006i: str = ""
    
    sel0007i: str = ""
    trnid07i: str = ""
    tdate07i: str = ""
    tdesc07i: str = ""
    tamt007i: str = ""
    
    sel0008i: str = ""
    trnid08i: str = ""
    tdate08i: str = ""
    tdesc08i: str = ""
    tamt008i: str = ""
    
    sel0009i: str = ""
    trnid09i: str = ""
    tdate09i: str = ""
    tdesc09i: str = ""
    tamt009i: str = ""
    
    sel0010i: str = ""
    trnid10i: str = ""
    tdate10i: str = ""
    tdesc10i: str = ""
    tamt010i: str = ""
    
    # Message fields
    errmsgo: str = ""


# ============================================================================
# Transaction File Handler
# ============================================================================

class TranFileHandler:
    """Handler for TRANSACT file with browse support."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self._data: dict = {}
        self._sorted_keys: List[str] = []
        self._browse_active: bool = False
        self._browse_key: Optional[str] = None
        self._browse_index: int = -1
        self._browse_direction: str = "FORWARD"  # FORWARD or BACKWARD
    
    def open_file(self) -> int:
        """Open file and load data."""
        try:
            # If data already exists (from add_record), just update sorted keys
            existing_data = len(self._data) > 0
            
            if not existing_data:
                try:
                    with open(self.filename, "r", encoding='utf-8') as f:
                        for line in f:
                            line = line.rstrip('\n\r')
                            if line:
                                record = parse_tran_record(line)
                                if record.tran_id:
                                    self._data[record.tran_id] = record
                except FileNotFoundError:
                    # File doesn't exist yet - create empty
                    pass
            
            # Always update sorted keys
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
                    line = format_tran_record(record)
                    f.write(line + '\n')
        except Exception:
            pass
    
    def startbr(self, tran_id: str = "") -> int:
        """Start browse (CICS STARTBR).
        
        Args:
            tran_id: Starting transaction ID (empty = start from beginning)
        
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
        
        if not tran_id or tran_id.strip() == "":
            # Start from beginning
            self._browse_index = 0
            self._browse_key = self._sorted_keys[0]
            self._browse_direction = "FORWARD"
        else:
            # Find starting position
            tran_id = tran_id.strip()
            if tran_id in self._sorted_keys:
                self._browse_index = self._sorted_keys.index(tran_id)
                self._browse_key = tran_id
            else:
                # Find next key >= tran_id
                found = False
                for i, key in enumerate(self._sorted_keys):
                    if key >= tran_id:
                        self._browse_index = i
                        self._browse_key = key
                        found = True
                        break
                if not found:
                    self.resp_cd = DFHRESP_NOTFND
                    return DFHRESP_NOTFND
        
        self._browse_direction = "FORWARD"
        self.resp_cd = DFHRESP_NORMAL
        return DFHRESP_NORMAL
    
    def readnext(self) -> Optional[TranRecord]:
        """Read next record (CICS READNEXT).
        
        Returns:
            TranRecord if found, None if end of file
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
    
    def readprev(self) -> Optional[TranRecord]:
        """Read previous record (CICS READPREV).
        
        Returns:
            TranRecord if found, None if end of file
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
    
    def endbr(self) -> int:
        """End browse (CICS ENDBR).
        
        Returns:
            Response code
        """
        self._browse_active = False
        self._browse_key = None
        self._browse_index = -1
        self.resp_cd = DFHRESP_NORMAL
        return DFHRESP_NORMAL
    
    def add_record(self, record: TranRecord):
        """Add a record (for testing)."""
        if record.tran_id:
            self._data[record.tran_id] = record
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
        self.transact_file: str = WS_TRANSACT_FILE
        self.err_flg: bool = False
        self.transact_eof: bool = False
        self.send_erase_flg: bool = True
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self.rec_count: int = 0
        self.idx: int = 0
        self.page_num: int = 0
        
        # Transaction amount and date
        self.tran_amt: Decimal = Decimal('0')
        self.tran_date: str = ""
        
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
        
        # Timestamp parsing
        self.timestamp: str = ""
        self.timestamp_dt_yyyy: str = ""
        self.timestamp_dt_mm: str = ""
        self.timestamp_dt_dd: str = ""
        
        # File handler
        self.tran_handler: Optional[TranFileHandler] = None
        
        # Current transaction ID for browse
        self.tran_id: str = ""


# ============================================================================
# Map Functions
# ============================================================================

def populate_header_info(ws: WorkingStorage, map_out: TransactionListMap):
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


def send_trnlst_screen(ws: WorkingStorage, map_out: TransactionListMap):
    """Send transaction list screen (SEND-TRNLST-SCREEN)."""
    populate_header_info(ws, map_out)
    map_out.errmsgo = ws.message


def receive_trnlst_screen(ws: WorkingStorage, map_in: TransactionListMap):
    """Receive transaction list screen (RECEIVE-TRNLST-SCREEN)."""
    ws.resp_cd = 0
    ws.reas_cd = 0
    # Extract input fields from map
    # (In real CICS, this would be done automatically by RECEIVE MAP)


def initialize_tran_data(ws: WorkingStorage, map_in: TransactionListMap, idx: int):
    """Initialize transaction data for a row (INITIALIZE-TRAN-DATA)."""
    row_fields = [
        ("sel0001i", "trnid01i", "tdate01i", "tdesc01i", "tamt001i"),
        ("sel0002i", "trnid02i", "tdate02i", "tdesc02i", "tamt002i"),
        ("sel0003i", "trnid03i", "tdate03i", "tdesc03i", "tamt003i"),
        ("sel0004i", "trnid04i", "tdate04i", "tdesc04i", "tamt004i"),
        ("sel0005i", "trnid05i", "tdate05i", "tdesc05i", "tamt005i"),
        ("sel0006i", "trnid06i", "tdate06i", "tdesc06i", "tamt006i"),
        ("sel0007i", "trnid07i", "tdate07i", "tdesc07i", "tamt007i"),
        ("sel0008i", "trnid08i", "tdate08i", "tdesc08i", "tamt008i"),
        ("sel0009i", "trnid09i", "tdate09i", "tdesc09i", "tamt009i"),
        ("sel0010i", "trnid10i", "tdate10i", "tdesc10i", "tamt010i"),
    ]
    
    if 1 <= idx <= 10:
        sel_field, trnid_field, tdate_field, tdesc_field, tamt_field = row_fields[idx - 1]
        setattr(map_in, sel_field, "")
        setattr(map_in, trnid_field, "")
        setattr(map_in, tdate_field, "")
        setattr(map_in, tdesc_field, "")
        setattr(map_in, tamt_field, "")


def populate_tran_data(ws: WorkingStorage, map_in: TransactionListMap, record: TranRecord, idx: int, prog_commarea: ThisProgCommarea):
    """Populate transaction data for a row (POPULATE-TRAN-DATA)."""
    # Format transaction amount
    ws.tran_amt = record.tran_amt
    amt_str = f"{ws.tran_amt:>12.2f}"
    
    # Parse transaction timestamp for date
    ws.timestamp = record.tran_orig_ts
    if len(ws.timestamp) >= 10:
        # Format: YYYY-MM-DD-HH.MM.SS.000000
        parts = ws.timestamp.split('-')
        if len(parts) >= 3:
            ws.timestamp_dt_yyyy = parts[0]
            ws.timestamp_dt_mm = parts[1]
            ws.timestamp_dt_dd = parts[2]
            ws.curdate_yy = ws.timestamp_dt_yyyy[2:4] if len(ws.timestamp_dt_yyyy) >= 4 else ""
            ws.tran_date = f"{ws.timestamp_dt_mm}/{ws.timestamp_dt_dd}/{ws.curdate_yy}"
        else:
            ws.tran_date = ""
    else:
        ws.tran_date = ""
    
    row_fields = [
        ("sel0001i", "trnid01i", "tdate01i", "tdesc01i", "tamt001i"),
        ("sel0002i", "trnid02i", "tdate02i", "tdesc02i", "tamt002i"),
        ("sel0003i", "trnid03i", "tdate03i", "tdesc03i", "tamt003i"),
        ("sel0004i", "trnid04i", "tdate04i", "tdesc04i", "tamt004i"),
        ("sel0005i", "trnid05i", "tdate05i", "tdesc05i", "tamt005i"),
        ("sel0006i", "trnid06i", "tdate06i", "tdesc06i", "tamt006i"),
        ("sel0007i", "trnid07i", "tdate07i", "tdesc07i", "tamt007i"),
        ("sel0008i", "trnid08i", "tdate08i", "tdesc08i", "tamt008i"),
        ("sel0009i", "trnid09i", "tdate09i", "tdesc09i", "tamt009i"),
        ("sel0010i", "trnid10i", "tdate10i", "tdesc10i", "tamt010i"),
    ]
    
    if 1 <= idx <= 10:
        sel_field, trnid_field, tdate_field, tdesc_field, tamt_field = row_fields[idx - 1]
        setattr(map_in, trnid_field, record.tran_id)
        setattr(map_in, tdate_field, ws.tran_date)
        setattr(map_in, tdesc_field, record.tran_desc)
        setattr(map_in, tamt_field, amt_str)
        
        # Store first and last transaction IDs
        if idx == 1:
            prog_commarea.trnid_first = record.tran_id
        elif idx == 10:
            prog_commarea.trnid_last = record.tran_id


# ============================================================================
# File Operations
# ============================================================================

def startbr_transact_file(ws: WorkingStorage, prog_commarea: ThisProgCommarea) -> bool:
    """Start browse on transaction file (STARTBR-TRANSACT-FILE).
    
    Returns:
        True if successful, False otherwise
    """
    if not ws.tran_handler:
        ws.err_flg = True
        ws.message = "Unable to lookup transaction..."
        return False
    
    resp_cd = ws.tran_handler.startbr(ws.tran_id)
    ws.resp_cd = resp_cd
    
    if resp_cd == DFHRESP_NORMAL:
        return True
    elif resp_cd == DFHRESP_NOTFND:
        ws.transact_eof = True
        ws.message = "You are at the top of the page..."
        return False
    else:
        ws.err_flg = True
        ws.message = "Unable to lookup transaction..."
        return False


def readnext_transact_file(ws: WorkingStorage) -> Optional[TranRecord]:
    """Read next transaction (READNEXT-TRANSACT-FILE).
    
    Returns:
        TranRecord if found, None if end of file
    """
    if not ws.tran_handler:
        ws.err_flg = True
        ws.message = "Unable to lookup transaction..."
        return None
    
    record = ws.tran_handler.readnext()
    ws.resp_cd = ws.tran_handler.resp_cd
    
    if ws.resp_cd == DFHRESP_NORMAL:
        return record
    elif ws.resp_cd == DFHRESP_ENDFILE:
        ws.transact_eof = True
        ws.message = "You have reached the bottom of the page..."
        return None
    else:
        ws.err_flg = True
        ws.message = "Unable to lookup transaction..."
        return None


def readprev_transact_file(ws: WorkingStorage) -> Optional[TranRecord]:
    """Read previous transaction (READPREV-TRANSACT-FILE).
    
    Returns:
        TranRecord if found, None if end of file
    """
    if not ws.tran_handler:
        ws.err_flg = True
        ws.message = "Unable to lookup transaction..."
        return None
    
    record = ws.tran_handler.readprev()
    ws.resp_cd = ws.tran_handler.resp_cd
    
    if ws.resp_cd == DFHRESP_NORMAL:
        return record
    elif ws.resp_cd == DFHRESP_ENDFILE:
        ws.transact_eof = True
        ws.message = "You have reached the top of the page..."
        return None
    else:
        ws.err_flg = True
        ws.message = "Unable to lookup transaction..."
        return None


def endbr_transact_file(ws: WorkingStorage):
    """End browse on transaction file (ENDBR-TRANSACT-FILE)."""
    if ws.tran_handler:
        ws.tran_handler.endbr()


# ============================================================================
# Pagination Logic
# ============================================================================

def process_page_forward(ws: WorkingStorage, map_in: TransactionListMap, prog_commarea: ThisProgCommarea, eibaid: str):
    """Process page forward (PROCESS-PAGE-FORWARD)."""
    if not startbr_transact_file(ws, prog_commarea):
        return
    
    if not ws.err_flg:
        # Skip first record if not Enter/PF7/PF3
        if eibaid not in (DFHENTER, DFHPF7, DFHPF3):
            readnext_transact_file(ws)
        
        # Initialize all rows
        if not ws.transact_eof and not ws.err_flg:
            for idx in range(1, 11):
                initialize_tran_data(ws, map_in, idx)
        
        # Read up to 10 records
        ws.idx = 1
        while ws.idx < 11 and not ws.transact_eof and not ws.err_flg:
            record = readnext_transact_file(ws)
            if not ws.transact_eof and not ws.err_flg and record:
                populate_tran_data(ws, map_in, record, ws.idx, prog_commarea)
                ws.idx += 1
        
        # Check if there are more records
        if not ws.transact_eof and not ws.err_flg:
            prog_commarea.page_num += 1
            record = readnext_transact_file(ws)
            if not ws.transact_eof and not ws.err_flg and record:
                prog_commarea.next_page_flg = True
            else:
                prog_commarea.next_page_flg = False
        else:
            prog_commarea.next_page_flg = False
            if ws.idx > 1:
                prog_commarea.page_num += 1
        
        endbr_transact_file(ws)
        
        map_in.pagenumi = str(prog_commarea.page_num)
        map_in.trnidino = ""
        send_trnlst_screen(ws, map_in)


def process_page_backward(ws: WorkingStorage, map_in: TransactionListMap, prog_commarea: ThisProgCommarea, eibaid: str):
    """Process page backward (PROCESS-PAGE-BACKWARD)."""
    if not startbr_transact_file(ws, prog_commarea):
        # If startbr failed, send screen with error message
        send_trnlst_screen(ws, map_in)
        return
    
    if not ws.err_flg:
        # For backward pagination: start browse positions us AT the key
        # We need to read backward from there. If we're at the first record,
        # we need to position ourselves correctly.
        # The COBOL code reads one prev if eibaid is not Enter/PF8, which
        # positions us before the starting key.
        if eibaid not in (DFHENTER, DFHPF8):
            # Position: read prev to get before starting key
            readprev_transact_file(ws)
            # If we hit EOF, we're at the beginning - reset EOF flag and start from beginning
            if ws.transact_eof:
                ws.transact_eof = False
                # Restart browse from beginning
                endbr_transact_file(ws)
                ws.tran_id = ""
                startbr_transact_file(ws, prog_commarea)
        
        # Initialize all rows
        if not ws.transact_eof and not ws.err_flg:
            for idx in range(1, 11):
                initialize_tran_data(ws, map_in, idx)
        
        # Read up to 10 records (backward)
        ws.idx = 10
        while ws.idx > 0 and not ws.transact_eof and not ws.err_flg:
            record = readprev_transact_file(ws)
            if not ws.transact_eof and not ws.err_flg and record:
                populate_tran_data(ws, map_in, record, ws.idx, prog_commarea)
                ws.idx -= 1
        
        # Adjust page number
        if not ws.transact_eof and not ws.err_flg:
            record = readprev_transact_file(ws)
            if prog_commarea.next_page_flg:
                if not ws.transact_eof and not ws.err_flg and prog_commarea.page_num > 1:
                    prog_commarea.page_num -= 1
                else:
                    prog_commarea.page_num = 1
        
        endbr_transact_file(ws)
        
        map_in.pagenumi = str(prog_commarea.page_num)
        # Ensure message is set if EOF was reached
        if ws.transact_eof and not ws.message:
            ws.message = "You have reached the top of the page..."
        send_trnlst_screen(ws, map_in)


# ============================================================================
# Process Enter Key
# ============================================================================

def process_enter_key(ws: WorkingStorage, map_in: TransactionListMap, prog_commarea: ThisProgCommarea, eibaid: str) -> Optional[str]:
    """Process enter key (PROCESS-ENTER-KEY).
    
    Returns:
        XCTL program name if selection made, None otherwise
    """
    # Check for selection
    selection_fields = [
        ("sel0001i", "trnid01i"),
        ("sel0002i", "trnid02i"),
        ("sel0003i", "trnid03i"),
        ("sel0004i", "trnid04i"),
        ("sel0005i", "trnid05i"),
        ("sel0006i", "trnid06i"),
        ("sel0007i", "trnid07i"),
        ("sel0008i", "trnid08i"),
        ("sel0009i", "trnid09i"),
        ("sel0010i", "trnid10i"),
    ]
    
    for sel_field, trnid_field in selection_fields:
        sel_value = getattr(map_in, sel_field, "").strip()
        if sel_value:
            trnid_value = getattr(map_in, trnid_field, "").strip()
            if trnid_value:
                prog_commarea.trn_sel_flg = sel_value
                prog_commarea.trn_selected = trnid_value
                break
    
    # Check if selection is valid
    if prog_commarea.trn_sel_flg and prog_commarea.trn_selected:
        if prog_commarea.trn_sel_flg.upper() == "S":
            # Valid selection - route to COTRN01C
            return COTRN01C
        else:
            # Invalid selection
            ws.err_flg = True
            ws.message = "Invalid selection. Valid value is S"
            return None
    
    # Validate transaction ID input
    if map_in.trnidini and map_in.trnidini.strip():
        if not map_in.trnidini.strip().isdigit():
            ws.err_flg = True
            ws.message = "Tran ID must be Numeric ..."
            map_in.trnidinl = -1
            send_trnlst_screen(ws, map_in)
            return None
        else:
            ws.tran_id = map_in.trnidini.strip()
    else:
        ws.tran_id = ""  # Start from beginning
    
    map_in.trnidinl = -1
    
    # Process page forward
    prog_commarea.page_num = 0
    process_page_forward(ws, map_in, prog_commarea, eibaid)
    
    if not ws.err_flg:
        map_in.trnidino = ""
    
    return None


# ============================================================================
# Process PF Keys
# ============================================================================

def process_pf7_key(ws: WorkingStorage, map_in: TransactionListMap, prog_commarea: ThisProgCommarea):
    """Process PF7 key (previous page) (PROCESS-PF7-KEY)."""
    if not prog_commarea.trnid_first or prog_commarea.trnid_first.strip() == "":
        ws.tran_id = ""
    else:
        ws.tran_id = prog_commarea.trnid_first
    
    prog_commarea.next_page_flg = True
    map_in.trnidinl = -1
    
    if prog_commarea.page_num > 1:
        process_page_backward(ws, map_in, prog_commarea, DFHPF7)
    else:
        ws.message = "You are already at the top of the page..."
        ws.send_erase_flg = False
        send_trnlst_screen(ws, map_in)


def process_pf8_key(ws: WorkingStorage, map_in: TransactionListMap, prog_commarea: ThisProgCommarea):
    """Process PF8 key (next page) (PROCESS-PF8-KEY)."""
    if not prog_commarea.trnid_last or prog_commarea.trnid_last.strip() == "":
        ws.tran_id = "ZZZZZZZZZZZZZZZZ"  # HIGH-VALUES equivalent
    else:
        ws.tran_id = prog_commarea.trnid_last
    
    map_in.trnidinl = -1
    
    if prog_commarea.next_page_flg:
        process_page_forward(ws, map_in, prog_commarea, DFHPF8)
    else:
        ws.message = "You are already at the bottom of the page..."
        ws.send_erase_flg = False
        send_trnlst_screen(ws, map_in)


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
    map_input: Optional[TransactionListMap] = None,
    prog_commarea: Optional[ThisProgCommarea] = None,
    tran_handler: Optional[TranFileHandler] = None
) -> Tuple[CardDemoCommarea, TransactionListMap, Optional[str], ThisProgCommarea]:
    """Main program entry point (MAIN-PARA).
    
    Returns:
        Tuple of (commarea, map_output, xctl_program, prog_commarea)
    """
    # Initialize working storage
    ws = WorkingStorage()
    ws.err_flg = False
    ws.transact_eof = False
    ws.send_erase_flg = True
    ws.message = ""
    
    # Initialize commarea if not provided
    if commarea is None:
        commarea = CardDemoCommarea()
    
    # Initialize program-specific commarea
    if prog_commarea is None:
        prog_commarea = ThisProgCommarea()
    
    # Initialize maps
    map_out = TransactionListMap()
    if map_input is None:
        map_input = TransactionListMap()
    
    # Initialize transaction file handler
    if tran_handler is None:
        ws.tran_handler = TranFileHandler(WS_TRANSACT_FILE)
        ws.tran_handler.open_file()
    else:
        ws.tran_handler = tran_handler
    
    map_out.trnidinl = -1
    
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
            process_enter_key(ws, map_input, prog_commarea, eibaid)
            send_trnlst_screen(ws, map_out)
            xctl_prog = None
        else:
            # Reenter - process input
            receive_trnlst_screen(ws, map_input)
            
            # Copy map_input to map_out for processing
            map_out = map_input
            
            if eibaid == DFHENTER:
                # Process enter key
                xctl_prog = process_enter_key(ws, map_input, prog_commarea, eibaid)
                if xctl_prog:
                    # Selection made - route to detail program
                    commarea.to_program = xctl_prog
                    commarea.from_tranid = ws.tranid
                    commarea.from_program = ws.pgmname
                    commarea.pgm_context = 0
                    return commarea, map_out, xctl_prog, prog_commarea
            elif eibaid == DFHPF3:
                # PF3 - return to menu
                commarea.to_program = COMEN01C
                xctl_prog = return_to_prev_screen(commarea)
                return commarea, map_out, xctl_prog, prog_commarea
            elif eibaid == DFHPF7:
                # PF7 - previous page
                process_pf7_key(ws, map_input, prog_commarea)
                xctl_prog = None
            elif eibaid == DFHPF8:
                # PF8 - next page
                process_pf8_key(ws, map_input, prog_commarea)
                xctl_prog = None
            else:
                # Invalid key
                ws.err_flg = True
                ws.message = CCDA_MSG_INVALID_KEY
                map_out.trnidinl = -1
                send_trnlst_screen(ws, map_out)
                xctl_prog = None
    
    # Set message in map
    map_out.errmsgo = ws.message
    
    return commarea, map_out, None, prog_commarea

