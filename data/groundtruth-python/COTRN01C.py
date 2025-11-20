"""
Python Translation of COTRN01C.cbl

Program: COTRN01C
Application: CardDemo
Type: CICS COBOL Program
Function: View a Transaction from TRANSACT file

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, Tuple, List
from decimal import Decimal
from datetime import datetime

# Import shared structures
from COACTVWC import CardDemoCommarea
from COBIL00C import TranRecord, parse_tran_record, format_tran_record


# ============================================================================
# Constants and Literals
# ============================================================================

WS_PGMNAME = "COTRN01C"
WS_TRANID = "CT01"
WS_TRANSACT_FILE = "TRANSACT"
COSGN00C = "COSGN00C"
COMEN01C = "COMEN01C"
COTRN00C = "COTRN00C"
DFHENTER = "DFHENTER"
DFHPF3 = "DFHPF3"
DFHPF4 = "DFHPF4"
DFHPF5 = "DFHPF5"

# Default titles (from COTTL01Y)
CCDA_TITLE01 = "CardDemo Application"
CCDA_TITLE02 = "Transaction View"
CCDA_MSG_INVALID_KEY = "Invalid key pressed. Please use Enter, PF3, PF4, or PF5."

# CICS Response Codes
DFHRESP_NORMAL = 0
DFHRESP_NOTFND = 12


# ============================================================================
# Program-Specific Commarea (CDEMO-CT01-INFO)
# ============================================================================

@dataclass
class ThisProgCommarea:
    """Program-specific commarea (CDEMO-CT01-INFO)."""
    trnid_first: str = ""  # CDEMO-CT01-TRNID-FIRST (16 chars)
    trnid_last: str = ""  # CDEMO-CT01-TRNID-LAST (16 chars)
    page_num: int = 0  # CDEMO-CT01-PAGE-NUM
    next_page_flg: bool = False  # CDEMO-CT01-NEXT-PAGE-FLG (False = 'N', True = 'Y')
    trn_sel_flg: str = ""  # CDEMO-CT01-TRN-SEL-FLG
    trn_selected: str = ""  # CDEMO-CT01-TRN-SELECTED (16 chars)


# ============================================================================
# Map Structure (COTRN1AO/COTRN1AI)
# ============================================================================

@dataclass
class TransactionViewMap:
    """Transaction view map structure (COTRN1A)."""
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
    
    # Transaction detail fields
    trnidi: str = ""  # Transaction ID
    trnido: str = ""  # Transaction ID output
    
    cardnumi: str = ""  # Card number input
    cardnumo: str = ""  # Card number output
    
    ttypcdi: str = ""  # Transaction type code input
    ttypcdo: str = ""  # Transaction type code output
    
    tcatcdi: str = ""  # Transaction category code input
    tcatcdo: str = ""  # Transaction category code output
    
    trnsrci: str = ""  # Transaction source input
    trnsrco: str = ""  # Transaction source output
    
    trnamti: str = ""  # Transaction amount input
    trnamto: str = ""  # Transaction amount output
    
    tdesci: str = ""  # Transaction description input
    tdesco: str = ""  # Transaction description output
    
    torigdti: str = ""  # Transaction original date/time input
    torigdto: str = ""  # Transaction original date/time output
    
    tprocdti: str = ""  # Transaction processed date/time input
    tprocdto: str = ""  # Transaction processed date/time output
    
    midi: str = ""  # Merchant ID input
    mido: str = ""  # Merchant ID output
    
    mnamei: str = ""  # Merchant name input
    mnameo: str = ""  # Merchant name output
    
    mcityi: str = ""  # Merchant city input
    mcityo: str = ""  # Merchant city output
    
    mzipi: str = ""  # Merchant zip input
    mzipo: str = ""  # Merchant zip output
    
    # Message fields
    errmsgo: str = ""


# ============================================================================
# Transaction File Handler
# ============================================================================

class TranFileHandler:
    """Handler for TRANSACT file with read operations."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self._data: dict = {}
        self._sorted_keys: List[str] = []
    
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
    
    def read_record(self, tran_id: str) -> Optional[TranRecord]:
        """Read transaction record by ID (CICS READ UPDATE).
        
        Args:
            tran_id: Transaction ID (16 chars)
        
        Returns:
            TranRecord if found, None otherwise
        """
        tran_id = tran_id.strip()
        
        if tran_id in self._data:
            self.resp_cd = DFHRESP_NORMAL
            return self._data[tran_id]
        else:
            self.resp_cd = DFHRESP_NOTFND
            return None
    
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
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self.usr_modified: bool = False
        
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
        
        # File handler
        self.tran_handler: Optional[TranFileHandler] = None
        
        # Current transaction ID
        self.tran_id: str = ""


# ============================================================================
# Map Functions
# ============================================================================

def populate_header_info(ws: WorkingStorage, map_out: TransactionViewMap):
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


def send_trnview_screen(ws: WorkingStorage, map_out: TransactionViewMap):
    """Send transaction view screen (SEND-TRNVIEW-SCREEN)."""
    populate_header_info(ws, map_out)
    map_out.errmsgo = ws.message


def receive_trnview_screen(ws: WorkingStorage, map_in: TransactionViewMap):
    """Receive transaction view screen (RECEIVE-TRNVIEW-SCREEN)."""
    ws.resp_cd = 0
    ws.reas_cd = 0
    # Extract input fields from map
    # (In real CICS, this would be done automatically by RECEIVE MAP)


def initialize_all_fields(ws: WorkingStorage, map_in: TransactionViewMap):
    """Initialize all fields (INITIALIZE-ALL-FIELDS)."""
    map_in.trnidinl = -1
    map_in.trnidini = ""
    map_in.trnidi = ""
    map_in.cardnumi = ""
    map_in.ttypcdi = ""
    map_in.tcatcdi = ""
    map_in.trnsrci = ""
    map_in.trnamti = ""
    map_in.tdesci = ""
    map_in.torigdti = ""
    map_in.tprocdti = ""
    map_in.midi = ""
    map_in.mnamei = ""
    map_in.mcityi = ""
    map_in.mzipi = ""
    ws.message = ""


# ============================================================================
# File Operations
# ============================================================================

def read_transact_file(ws: WorkingStorage, tran_id: str) -> Optional[TranRecord]:
    """Read transaction file (READ-TRANSACT-FILE).
    
    Returns:
        TranRecord if found, None otherwise
    """
    if not ws.tran_handler:
        ws.err_flg = True
        ws.message = "Unable to lookup Transaction..."
        return None
    
    record = ws.tran_handler.read_record(tran_id)
    ws.resp_cd = ws.tran_handler.resp_cd
    
    if ws.resp_cd == DFHRESP_NORMAL:
        return record
    elif ws.resp_cd == DFHRESP_NOTFND:
        ws.err_flg = True
        ws.message = "Transaction ID NOT found..."
        return None
    else:
        ws.err_flg = True
        ws.message = "Unable to lookup Transaction..."
        return None


# ============================================================================
# Process Enter Key
# ============================================================================

def process_enter_key(ws: WorkingStorage, map_in: TransactionViewMap, map_out: TransactionViewMap) -> bool:
    """Process enter key (PROCESS-ENTER-KEY).
    
    Returns:
        True if successful, False otherwise
    """
    # Validate transaction ID
    if not map_in.trnidini or map_in.trnidini.strip() == "":
        ws.err_flg = True
        ws.message = "Tran ID can NOT be empty..."
        map_out.trnidinl = -1
        send_trnview_screen(ws, map_out)
        return False
    
    map_out.trnidinl = -1
    
    # Clear all fields in both maps
    map_in.trnidi = ""
    map_in.cardnumi = ""
    map_in.ttypcdi = ""
    map_in.tcatcdi = ""
    map_in.trnsrci = ""
    map_in.trnamti = ""
    map_in.tdesci = ""
    map_in.torigdti = ""
    map_in.tprocdti = ""
    map_in.midi = ""
    map_in.mnamei = ""
    map_in.mcityi = ""
    map_in.mzipi = ""
    
    map_out.trnidi = ""
    map_out.cardnumi = ""
    map_out.ttypcdi = ""
    map_out.tcatcdi = ""
    map_out.trnsrci = ""
    map_out.trnamti = ""
    map_out.tdesci = ""
    map_out.torigdti = ""
    map_out.tprocdti = ""
    map_out.midi = ""
    map_out.mnamei = ""
    map_out.mcityi = ""
    map_out.mzipi = ""
    
    # Read transaction
    tran_id = map_in.trnidini.strip()
    record = read_transact_file(ws, tran_id)
    
    if record and not ws.err_flg:
        # Populate fields (input fields serve as both input and output)
        ws.tran_amt = record.tran_amt
        amt_str = f"{ws.tran_amt:>12.2f}"
        
        # Populate both map_in and map_out
        map_in.trnidi = record.tran_id
        map_in.cardnumi = record.tran_card_num
        map_in.ttypcdi = record.tran_type_cd
        map_in.tcatcdi = record.tran_cat_cd
        map_in.trnsrci = record.tran_source
        map_in.trnamti = amt_str
        map_in.tdesci = record.tran_desc
        map_in.torigdti = record.tran_orig_ts
        map_in.tprocdti = record.tran_proc_ts
        map_in.midi = record.tran_merchant_id
        map_in.mnamei = record.tran_merchant_name
        map_in.mcityi = record.tran_merchant_city
        map_in.mzipi = record.tran_merchant_zip
        
        # Also populate map_out
        map_out.trnidi = record.tran_id
        map_out.cardnumi = record.tran_card_num
        map_out.ttypcdi = record.tran_type_cd
        map_out.tcatcdi = record.tran_cat_cd
        map_out.trnsrci = record.tran_source
        map_out.trnamti = amt_str
        map_out.tdesci = record.tran_desc
        map_out.torigdti = record.tran_orig_ts
        map_out.tprocdti = record.tran_proc_ts
        map_out.midi = record.tran_merchant_id
        map_out.mnamei = record.tran_merchant_name
        map_out.mcityi = record.tran_merchant_city
        map_out.mzipi = record.tran_merchant_zip
        
        # Also populate output fields
        map_out.trnido = record.tran_id
        map_out.cardnumo = record.tran_card_num
        map_out.ttypcdo = record.tran_type_cd
        map_out.tcatcdo = record.tran_cat_cd
        map_out.trnsrco = record.tran_source
        map_out.trnamto = amt_str
        map_out.tdesco = record.tran_desc
        map_out.torigdto = record.tran_orig_ts
        map_out.tprocdto = record.tran_proc_ts
        map_out.mido = record.tran_merchant_id
        map_out.mnameo = record.tran_merchant_name
        map_out.mcityo = record.tran_merchant_city
        map_out.mzipo = record.tran_merchant_zip
        
        send_trnview_screen(ws, map_out)
        return True
    else:
        # Error already set in read_transact_file
        send_trnview_screen(ws, map_out)
        return False


# ============================================================================
# Clear Current Screen
# ============================================================================

def clear_current_screen(ws: WorkingStorage, map_in: TransactionViewMap, map_out: TransactionViewMap):
    """Clear current screen (CLEAR-CURRENT-SCREEN)."""
    initialize_all_fields(ws, map_in)
    send_trnview_screen(ws, map_out)


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
    map_input: Optional[TransactionViewMap] = None,
    prog_commarea: Optional[ThisProgCommarea] = None,
    tran_handler: Optional[TranFileHandler] = None
) -> Tuple[CardDemoCommarea, TransactionViewMap, Optional[str], ThisProgCommarea]:
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
    map_out = TransactionViewMap()
    if map_input is None:
        map_input = TransactionViewMap()
    
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
            map_out.trnidinl = -1
            
            # Copy map_input to map_out for processing
            map_out = map_input
            
            # If transaction was selected from list, populate it
            if prog_commarea.trn_selected and prog_commarea.trn_selected.strip():
                map_out.trnidini = prog_commarea.trn_selected
                # Use map_out as both input and output
                process_enter_key(ws, map_out, map_out)
            else:
                send_trnview_screen(ws, map_out)
            xctl_prog = None
        else:
            # Reenter - process input
            receive_trnview_screen(ws, map_input)
            
            # Copy map_input to map_out for processing
            map_out = map_input
            
            if eibaid == DFHENTER:
                # Process enter key - use map_out as both input and output
                # First copy trnidini from map_input if needed
                if map_input.trnidini:
                    map_out.trnidini = map_input.trnidini
                process_enter_key(ws, map_out, map_out)
                xctl_prog = None
            elif eibaid == DFHPF3:
                # PF3 - return to previous program or menu
                if not commarea.from_program or commarea.from_program.strip() == "":
                    commarea.to_program = COMEN01C
                else:
                    commarea.to_program = commarea.from_program
                xctl_prog = return_to_prev_screen(commarea)
                return commarea, map_out, xctl_prog, prog_commarea
            elif eibaid == DFHPF4:
                # PF4 - clear screen
                clear_current_screen(ws, map_input, map_out)
                xctl_prog = None
            elif eibaid == DFHPF5:
                # PF5 - return to transaction list
                commarea.to_program = COTRN00C
                xctl_prog = return_to_prev_screen(commarea)
                return commarea, map_out, xctl_prog, prog_commarea
            else:
                # Invalid key
                ws.err_flg = True
                ws.message = CCDA_MSG_INVALID_KEY
                send_trnview_screen(ws, map_out)
                xctl_prog = None
    
    # Set message in map
    map_out.errmsgo = ws.message
    
    return commarea, map_out, None, prog_commarea

