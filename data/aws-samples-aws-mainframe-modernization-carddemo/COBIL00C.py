"""
Python Translation of COBIL00C.cbl

Program: COBIL00C
Application: CardDemo
Type: CICS COBOL Program
Function: Bill Payment - Pay account balance in full and create transaction

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, Tuple
from decimal import Decimal
from datetime import datetime

# Import shared record structures and functions from COACTUPC
from COACTUPC import (
    AccountRecord,
    CardXrefRecord,
    parse_account_record,
    parse_xref_record,
    format_account_record,
    IndexedFileHandler,
    AcctFileHandler,
    XrefFileHandler
)


# ============================================================================
# Constants and Literals
# ============================================================================

WS_PGMNAME = "COBIL00C"
WS_TRANID = "CB00"
WS_TRANSACT_FILE = "TRANSACT"
WS_ACCTDAT_FILE = "ACCTDAT "
WS_CXACAIX_FILE = "CXACAIX "
COSGN00C = "COSGN00C"
COMEN01C = "COMEN01C"
DFHENTER = "DFHENTER"
DFHPF3 = "DFHPF3"
DFHPF4 = "DFHPF4"
DFHGREEN = "DFHGREEN"

# Default titles
CCDA_TITLE01 = "CardDemo Application"
CCDA_TITLE02 = "Bill Payment"
CCDA_MSG_INVALID_KEY = "Invalid key pressed. Please use Enter, PF3, or PF4."

# CICS Response Codes
DFHRESP_NORMAL = 0
DFHRESP_NOTFND = 12
DFHRESP_ENDFILE = 16
DFHRESP_DUPKEY = 15
DFHRESP_DUPREC = 15


# ============================================================================
# Transaction Record Structure
# ============================================================================

@dataclass
class TranRecord:
    """Transaction record (TRAN-RECORD from COPY CVTRA05Y)."""
    tran_id: str = ""  # PIC X(16)
    tran_type_cd: str = ""  # 2 chars
    tran_cat_cd: str = ""  # 4 chars (numeric)
    tran_source: str = ""
    tran_desc: str = ""
    tran_amt: Decimal = Decimal('0')
    tran_card_num: str = ""  # 16 chars
    tran_merchant_id: str = ""  # 9 chars
    tran_merchant_name: str = ""
    tran_merchant_city: str = ""
    tran_merchant_zip: str = ""
    tran_orig_ts: str = ""  # 26 chars (timestamp)
    tran_proc_ts: str = ""  # 26 chars (timestamp)


def format_tran_record(record: TranRecord) -> str:
    """Format transaction record for writing."""
    # Based on CVTRA05Y structure - approximate format
    # TRAN-ID(16) + TRAN-TYPE-CD(2) + TRAN-CAT-CD(4) + TRAN-SOURCE + TRAN-DESC + 
    # TRAN-AMT + TRAN-CARD-NUM(16) + TRAN-MERCHANT-ID(9) + TRAN-MERCHANT-NAME + 
    # TRAN-MERCHANT-CITY + TRAN-MERCHANT-ZIP + TRAN-ORIG-TS(26) + TRAN-PROC-TS(26)
    line = (
        f"{record.tran_id:<16}"
        f"{record.tran_type_cd:<2}"
        f"{record.tran_cat_cd:<4}"
        f"{record.tran_source:<20}"
        f"{record.tran_desc:<30}"
        f"{record.tran_amt:>12.2f}"
        f"{record.tran_card_num:<16}"
        f"{record.tran_merchant_id:<9}"
        f"{record.tran_merchant_name:<30}"
        f"{record.tran_merchant_city:<30}"
        f"{record.tran_merchant_zip:<10}"
        f"{record.tran_orig_ts:<26}"
        f"{record.tran_proc_ts:<26}"
    )
    # Pad to appropriate length (approximately 300 chars)
    return line[:300].ljust(300)


def parse_tran_record(line: str) -> TranRecord:
    """Parse transaction record."""
    if len(line) < 16:
        return TranRecord()
    
    # Extract fields based on approximate positions
    tran_id = line[0:16].strip()
    tran_type_cd = line[16:18].strip() if len(line) >= 18 else ""
    tran_cat_cd = line[18:22].strip() if len(line) >= 22 else ""
    tran_source = line[22:42].strip() if len(line) >= 42 else ""
    tran_desc = line[42:72].strip() if len(line) >= 72 else ""
    
    # Parse amount
    try:
        tran_amt_str = line[72:84].strip() if len(line) >= 84 else "0"
        tran_amt = Decimal(tran_amt_str) if tran_amt_str and tran_amt_str.replace('.', '').replace('-', '').strip().isdigit() else Decimal('0')
    except:
        tran_amt = Decimal('0')
    
    tran_card_num = line[84:100].strip() if len(line) >= 100 else ""
    tran_merchant_id = line[100:109].strip() if len(line) >= 109 else ""
    tran_merchant_name = line[109:139].strip() if len(line) >= 139 else ""
    tran_merchant_city = line[139:169].strip() if len(line) >= 169 else ""
    tran_merchant_zip = line[169:179].strip() if len(line) >= 179 else ""
    tran_orig_ts = line[179:205].strip() if len(line) >= 205 else ""
    tran_proc_ts = line[205:231].strip() if len(line) >= 231 else ""
    
    return TranRecord(
        tran_id=tran_id,
        tran_type_cd=tran_type_cd,
        tran_cat_cd=tran_cat_cd,
        tran_source=tran_source,
        tran_desc=tran_desc,
        tran_amt=tran_amt,
        tran_card_num=tran_card_num,
        tran_merchant_id=tran_merchant_id,
        tran_merchant_name=tran_merchant_name,
        tran_merchant_city=tran_merchant_city,
        tran_merchant_zip=tran_merchant_zip,
        tran_orig_ts=tran_orig_ts,
        tran_proc_ts=tran_proc_ts
    )


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
    
    # CB00 specific fields
    cb00_trnid_first: str = ""
    cb00_trnid_last: str = ""
    cb00_page_num: int = 0
    cb00_next_page_flg: bool = False
    cb00_trn_sel_flg: str = ""
    cb00_trn_selected: str = ""


# ============================================================================
# Map Structure (COBIL0AO/COBIL0AI)
# ============================================================================

@dataclass
class BillPayMap:
    """Bill payment map structure."""
    # Header fields
    title01o: str = ""
    title02o: str = ""
    trnnameo: str = ""
    pgmnameo: str = ""
    curdateo: str = ""
    curtimeo: str = ""
    
    # Input fields
    actidini: str = ""  # Account ID input
    actidinl: int = 0  # Account ID length (cursor position)
    curbali: str = ""  # Current balance input
    confirml: int = 0  # Confirm length (cursor position)
    confirmi: str = ""  # Confirm input (Y/N)
    
    # Output fields
    errmsgo: str = ""  # Error message output
    errmsgc: str = ""  # Error message color


# ============================================================================
# Working Storage
# ============================================================================

class WorkingStorage:
    """Working storage for program state."""
    
    def __init__(self):
        self.pgmname: str = WS_PGMNAME
        self.tranid: str = WS_TRANID
        self.message: str = ""
        self.err_flg: bool = False  # False = 'N', True = 'Y'
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self.usr_modified: bool = False
        self.conf_pay_flg: bool = False  # False = 'N', True = 'Y'
        
        # Transaction fields
        self.tran_amt: Decimal = Decimal('0')
        self.curr_bal: Decimal = Decimal('0')
        self.tran_id_num: int = 0
        
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
        self.timestamp: str = ""
        
        # File handlers
        self.acct_handler: Optional[AcctFileHandler] = None
        self.xref_handler: Optional[XrefFileHandler] = None
        self.tran_handler: Optional['TranFileHandler'] = None
        
        # Records
        self.acct_record: Optional[AccountRecord] = None
        self.xref_record: Optional[CardXrefRecord] = None
        self.tran_record: Optional[TranRecord] = None
        
        # Browse state
        self.browse_active: bool = False


# ============================================================================
# Transaction File Handler
# ============================================================================

class TranFileHandler(IndexedFileHandler):
    """Handler for TRANSACT file with browse support."""
    
    def __init__(self, filename: str):
        super().__init__(filename)
        self._browse_active: bool = False
        self._browse_key: Optional[str] = None
    
    def _extract_key(self, line: str) -> str:
        """Extract transaction ID (primary key)."""
        if len(line) >= 16:
            return line[:16]
        return ""
    
    def _extract_alt_key(self, line: str) -> str:
        """No alternate index for TRANSACT."""
        return ""
    
    def startbr(self, key: str) -> Tuple[int, int]:
        """Start browse (STARTBR)."""
        try:
            # Sort keys to enable backward browsing
            sorted_keys = sorted(self._data.keys(), reverse=True)
            self._browse_active = True
            self._browse_key = key
            self.file_status = "00"
            return 0, 0
        except Exception as e:
            self.file_status = "99"
            return 12, 0
    
    def readprev(self) -> Tuple[Optional[str], int]:
        """Read previous record (READPREV)."""
        if not self._browse_active:
            self.file_status = "99"
            return None, 12
        
        try:
            # Find the highest key less than or equal to browse key
            sorted_keys = sorted([k for k in self._data.keys() if k <= self._browse_key], reverse=True)
            if sorted_keys:
                record = self._data[sorted_keys[0]]
                self._browse_key = sorted_keys[0]  # Update for next readprev
                self.file_status = "00"
                return record, 0
            else:
                self.file_status = "10"  # ENDFILE
                return None, 16
        except Exception as e:
            self.file_status = "99"
            return None, 12
    
    def endbr(self) -> int:
        """End browse (ENDBR)."""
        self._browse_active = False
        self._browse_key = None
        self.file_status = "00"
        return 0
    
    def write_record(self, key: str, record: str) -> Tuple[int, int]:
        """Write record (WRITE)."""
        try:
            if key in self._data:
                # Duplicate key
                self.file_status = "15"  # DUPKEY
                return 15, 0
            
            self._data[key] = record
            self.file_status = "00"
            return 0, 0
        except Exception as e:
            self.file_status = "99"
            return 12, 0


# ============================================================================
# Map Functions
# ============================================================================

def populate_header_info(ws: WorkingStorage, map_out: BillPayMap):
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


def get_current_timestamp(ws: WorkingStorage) -> str:
    """Get current timestamp (GET-CURRENT-TIMESTAMP)."""
    now = datetime.now()
    date_str = now.strftime("%Y-%m-%d")
    time_str = now.strftime("%H:%M:%S")
    timestamp = f"{date_str}-{time_str}.000000"
    ws.timestamp = timestamp
    return timestamp


def initialize_all_fields(ws: WorkingStorage, map_in: BillPayMap, map_out: BillPayMap):
    """Initialize all fields (INITIALIZE-ALL-FIELDS)."""
    map_in.actidinl = -1
    map_in.actidini = ""
    map_in.curbali = ""
    map_in.confirmi = ""
    ws.message = ""


def send_billpay_screen(ws: WorkingStorage, map_out: BillPayMap):
    """Send bill payment screen (SEND-BILLPAY-SCREEN)."""
    populate_header_info(ws, map_out)
    map_out.errmsgo = ws.message


def receive_billpay_screen(ws: WorkingStorage, map_in: BillPayMap) -> int:
    """Receive bill payment screen (RECEIVE-BILLPAY-SCREEN)."""
    ws.resp_cd = 0
    ws.reas_cd = 0
    return 0


# ============================================================================
# File Operations
# ============================================================================

def read_acctdat_file(ws: WorkingStorage, acct_id: str) -> bool:
    """Read account file (READ-ACCTDAT-FILE)."""
    try:
        if not ws.acct_handler:
            return False
        
        record_line, resp_cd = ws.acct_handler.read_for_update(acct_id)
        ws.resp_cd = resp_cd
        ws.reas_cd = 0
        
        if resp_cd == DFHRESP_NORMAL:
            ws.acct_record = parse_account_record(record_line)
            return True
        elif resp_cd == DFHRESP_NOTFND:
            ws.err_flg = True
            ws.message = "Account ID NOT found..."
            return False
        else:
            ws.err_flg = True
            ws.message = "Unable to lookup Account..."
            return False
    except Exception as e:
        ws.err_flg = True
        ws.message = f"Unable to lookup Account: {str(e)}"
        return False


def update_acctdat_file(ws: WorkingStorage, acct_id: str) -> bool:
    """Update account file (UPDATE-ACCTDAT-FILE)."""
    try:
        if not ws.acct_handler or not ws.acct_record:
            return False
        
        record_line = format_account_record(ws.acct_record)
        resp_cd = ws.acct_handler.rewrite_record(acct_id, record_line)
        ws.resp_cd = resp_cd
        ws.reas_cd = 0
        
        if resp_cd == 0:
            return True
        else:
            ws.err_flg = True
            ws.message = "Unable to Update Account..."
            return False
    except Exception as e:
        ws.err_flg = True
        ws.message = f"Unable to Update Account: {str(e)}"
        return False


def read_cxacaix_file(ws: WorkingStorage, acct_id: str) -> bool:
    """Read CXACAIX file (READ-CXACAIX-FILE)."""
    try:
        if not ws.xref_handler:
            return False
        
        record_line, resp_cd = ws.xref_handler.read_record(acct_id, use_alt_index=True)
        ws.resp_cd = resp_cd
        ws.reas_cd = 0
        
        if resp_cd == DFHRESP_NORMAL:
            ws.xref_record = parse_xref_record(record_line)
            return True
        elif resp_cd == DFHRESP_NOTFND:
            ws.err_flg = True
            ws.message = "Account ID NOT found..."
            return False
        else:
            ws.err_flg = True
            ws.message = "Unable to lookup XREF AIX file..."
            return False
    except Exception as e:
        ws.err_flg = True
        ws.message = f"Unable to lookup XREF AIX file: {str(e)}"
        return False


def startbr_transact_file(ws: WorkingStorage, tran_id: str) -> bool:
    """Start browse transaction file (STARTBR-TRANSACT-FILE)."""
    try:
        if not ws.tran_handler:
            return False
        
        resp_cd, reas_cd = ws.tran_handler.startbr(tran_id)
        ws.resp_cd = resp_cd
        ws.reas_cd = reas_cd
        
        if resp_cd == 0:
            ws.browse_active = True
            return True
        else:
            ws.err_flg = True
            ws.message = "Unable to lookup Transaction..."
            return False
    except Exception as e:
        ws.err_flg = True
        ws.message = f"Unable to lookup Transaction: {str(e)}"
        return False


def readprev_transact_file(ws: WorkingStorage) -> Optional[str]:
    """Read previous transaction (READPREV-TRANSACT-FILE)."""
    try:
        if not ws.tran_handler:
            return None
        
        record_line, resp_cd = ws.tran_handler.readprev()
        ws.resp_cd = resp_cd
        
        if resp_cd == DFHRESP_NORMAL:
            return record_line
        elif resp_cd == DFHRESP_ENDFILE:
            return None  # Return empty/zero tran_id equivalent
        else:
            ws.err_flg = True
            ws.message = "Unable to lookup Transaction..."
            return None
    except Exception as e:
        ws.err_flg = True
        ws.message = f"Unable to lookup Transaction: {str(e)}"
        return None


def endbr_transact_file(ws: WorkingStorage):
    """End browse transaction file (ENDBR-TRANSACT-FILE)."""
    if ws.tran_handler:
        ws.tran_handler.endbr()
        ws.browse_active = False


def write_transact_file(ws: WorkingStorage, tran_record: TranRecord) -> bool:
    """Write transaction file (WRITE-TRANSACT-FILE)."""
    try:
        if not ws.tran_handler:
            return False
        
        record_line = format_tran_record(tran_record)
        resp_cd, reas_cd = ws.tran_handler.write_record(tran_record.tran_id, record_line)
        ws.resp_cd = resp_cd
        ws.reas_cd = reas_cd
        
        if resp_cd == 0:
            return True
        elif resp_cd == DFHRESP_DUPKEY or resp_cd == DFHRESP_DUPREC:
            ws.err_flg = True
            ws.message = "Tran ID already exist..."
            return False
        else:
            ws.err_flg = True
            ws.message = "Unable to Add Bill pay Transaction..."
            return False
    except Exception as e:
        ws.err_flg = True
        ws.message = f"Unable to Add Bill pay Transaction: {str(e)}"
        return False


# ============================================================================
# Process Enter Key
# ============================================================================

def process_enter_key(
    ws: WorkingStorage,
    map_in: BillPayMap,
    map_out: BillPayMap,
    commarea: CardDemoCommarea,
    acct_file: str,
    xref_file: str,
    tran_file: str
) -> bool:
    """Process Enter key (PROCESS-ENTER-KEY).
    
    Returns True if payment was successful, False otherwise.
    """
    ws.conf_pay_flg = False
    
    # Validate account ID
    acct_id = map_in.actidini.strip()
    if not acct_id or acct_id == "":
        ws.err_flg = True
        ws.message = "Acct ID can NOT be empty..."
        map_in.actidinl = -1
        send_billpay_screen(ws, map_out)
        return False
    
    # Process confirmation
    confirm = map_in.confirmi.strip().upper()
    
    if confirm == 'Y':
        ws.conf_pay_flg = True
        if not read_acctdat_file(ws, acct_id):
            map_in.actidinl = -1
            send_billpay_screen(ws, map_out)
            return False
    elif confirm == 'N':
        # Clear screen
        initialize_all_fields(ws, map_in, map_out)
        ws.err_flg = True
        send_billpay_screen(ws, map_out)
        return False
    elif not confirm or confirm == "":
        # No confirmation - just read account
        if not read_acctdat_file(ws, acct_id):
            map_in.actidinl = -1
            send_billpay_screen(ws, map_out)
            return False
    else:
        ws.err_flg = True
        ws.message = "Invalid value. Valid values are (Y/N)..."
        map_in.confirml = -1
        send_billpay_screen(ws, map_out)
        return False
    
    # Update current balance in map
    if ws.acct_record:
        ws.curr_bal = ws.acct_record.curr_bal
        map_in.curbali = f"{ws.curr_bal:>12.2f}"
    
    # Check if balance is zero or negative
    if ws.acct_record and ws.acct_record.curr_bal <= Decimal('0') and acct_id:
        ws.err_flg = True
        ws.message = "You have nothing to pay..."
        map_in.actidinl = -1
        send_billpay_screen(ws, map_out)
        return False
    
    # Process payment if confirmed
    if not ws.err_flg and ws.conf_pay_flg:
        # Read xref to get card number
        if not read_cxacaix_file(ws, acct_id):
            map_in.actidinl = -1
            send_billpay_screen(ws, map_out)
            return False
        
        # Find highest transaction ID
        high_value = "9999999999999999"  # HIGH-VALUES equivalent
        if not startbr_transact_file(ws, high_value):
            map_in.actidinl = -1
            send_billpay_screen(ws, map_out)
            return False
        
        # Read previous (highest) transaction
        prev_record_line = readprev_transact_file(ws)
        endbr_transact_file(ws)
        
        # Get next transaction ID
        if prev_record_line:
            prev_record = parse_tran_record(prev_record_line)
            try:
                ws.tran_id_num = int(prev_record.tran_id) + 1
            except:
                ws.tran_id_num = 1
        else:
            ws.tran_id_num = 1
        
        # Create transaction record
        tran_record = TranRecord()
        tran_record.tran_id = f"{ws.tran_id_num:016d}"
        tran_record.tran_type_cd = "02"
        tran_record.tran_cat_cd = "0002"
        tran_record.tran_source = "POS TERM"
        tran_record.tran_desc = "BILL PAYMENT - ONLINE"
        tran_record.tran_amt = ws.acct_record.curr_bal
        tran_record.tran_card_num = ws.xref_record.card_num if ws.xref_record else ""
        tran_record.tran_merchant_id = "999999999"
        tran_record.tran_merchant_name = "BILL PAYMENT"
        tran_record.tran_merchant_city = "N/A"
        tran_record.tran_merchant_zip = "N/A"
        
        timestamp = get_current_timestamp(ws)
        tran_record.tran_orig_ts = timestamp
        tran_record.tran_proc_ts = timestamp
        
        # Write transaction
        if not write_transact_file(ws, tran_record):
            map_in.actidinl = -1
            send_billpay_screen(ws, map_out)
            return False
        
        # Update account balance
        ws.acct_record.curr_bal = ws.acct_record.curr_bal - tran_record.tran_amt
        
        # Update account file
        if not update_acctdat_file(ws, acct_id):
            map_in.actidinl = -1
            send_billpay_screen(ws, map_out)
            return False
        
        # Success - initialize fields and show success message
        initialize_all_fields(ws, map_in, map_out)
        ws.message = f"Payment successful.  Your Transaction ID is {tran_record.tran_id}."
        map_out.errmsgc = DFHGREEN
        send_billpay_screen(ws, map_out)
        return True
    else:
        # Not confirmed - show confirmation message
        ws.message = "Confirm to make a bill payment..."
        map_in.confirml = -1
        send_billpay_screen(ws, map_out)
        return False
    
    send_billpay_screen(ws, map_out)
    return False


# ============================================================================
# Clear and Return Functions
# ============================================================================

def clear_current_screen(ws: WorkingStorage, map_in: BillPayMap, map_out: BillPayMap):
    """Clear current screen (CLEAR-CURRENT-SCREEN)."""
    initialize_all_fields(ws, map_in, map_out)
    send_billpay_screen(ws, map_out)


def return_to_prev_screen(commarea: CardDemoCommarea) -> str:
    """Return to previous screen (RETURN-TO-PREV-SCREEN)."""
    if not commarea.to_program or commarea.to_program.strip() == "":
        commarea.to_program = COSGN00C
    commarea.from_tranid = WS_TRANID
    commarea.from_program = WS_PGMNAME
    commarea.pgm_context = 0
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
    map_input: Optional[BillPayMap] = None,
    acct_file: str = "data/ACCTDAT.dat",
    xref_file: str = "data/CARDXREF.dat",
    tran_file: str = "data/TRANSACT.dat"
) -> Tuple[CardDemoCommarea, BillPayMap, Optional[str]]:
    """Main program entry point (MAIN-PARA).
    
    Returns:
        Tuple of (commarea, map_output, xctl_program)
    """
    # Initialize working storage
    ws = WorkingStorage()
    ws.err_flg = False
    ws.usr_modified = False
    ws.message = ""
    
    # Initialize commarea if not provided
    if commarea is None:
        commarea = CardDemoCommarea()
    
    # Initialize maps
    map_out = BillPayMap()
    if map_input is None:
        map_input = BillPayMap()
    
    # Open files
    ws.acct_handler = AcctFileHandler(acct_file)
    ws.xref_handler = XrefFileHandler(xref_file)
    ws.tran_handler = TranFileHandler(tran_file)
    
    ws.acct_handler.open_file()
    ws.xref_handler.open_file()
    ws.tran_handler.open_file()
    
    # Main logic
    if eibcalen == 0:
        commarea.to_program = COSGN00C
        xctl_prog = return_to_prev_screen(commarea)
        return commarea, map_out, xctl_prog
    else:
        if not commarea.pgm_reenter:
            # First entry
            commarea.pgm_reenter = True
            map_out = BillPayMap()  # Clear map
            map_input.actidinl = -1
            
            # If transaction selected, process it
            if commarea.cb00_trn_selected and commarea.cb00_trn_selected.strip():
                map_input.actidini = commarea.cb00_trn_selected
                process_enter_key(ws, map_input, map_out, commarea, acct_file, xref_file, tran_file)
            
            send_billpay_screen(ws, map_out)
            return commarea, map_out, None
        else:
            # Reenter
            receive_billpay_screen(ws, map_input)
            
            xctl_prog = None
            
            if eibaid == DFHENTER:
                process_enter_key(ws, map_input, map_out, commarea, acct_file, xref_file, tran_file)
            elif eibaid == DFHPF3:
                if not commarea.from_program or commarea.from_program.strip() == "":
                    commarea.to_program = COMEN01C
                else:
                    commarea.to_program = commarea.from_program
                xctl_prog = return_to_prev_screen(commarea)
            elif eibaid == DFHPF4:
                clear_current_screen(ws, map_input, map_out)
            else:
                ws.err_flg = True
                ws.message = CCDA_MSG_INVALID_KEY
                send_billpay_screen(ws, map_out)
            
            return commarea, map_out, xctl_prog
    
    return commarea, map_out, None

