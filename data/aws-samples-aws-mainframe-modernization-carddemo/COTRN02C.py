"""
Python Translation of COTRN02C.cbl

Program: COTRN02C
Application: CardDemo
Type: CICS COBOL Program
Function: Add a new Transaction to TRANSACT file

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
from COACTUPC import CardXrefRecord, parse_xref_record
from COBIL00C import TranRecord, parse_tran_record, format_tran_record
from CORPT00C import DateValidationResult, validate_date
from COTRN00C import TranFileHandler as TranFileHandlerBase


# ============================================================================
# Constants and Literals
# ============================================================================

WS_PGMNAME = "COTRN02C"
WS_TRANID = "CT02"
WS_TRANSACT_FILE = "TRANSACT"
WS_ACCTDAT_FILE = "ACCTDAT "
WS_CCXREF_FILE = "CCXREF  "
WS_CXACAIX_FILE = "CXACAIX "
COSGN00C = "COSGN00C"
COMEN01C = "COMEN01C"
DFHENTER = "DFHENTER"
DFHPF3 = "DFHPF3"
DFHPF4 = "DFHPF4"
DFHPF5 = "DFHPF5"

# Default titles (from COTTL01Y)
CCDA_TITLE01 = "CardDemo Application"
CCDA_TITLE02 = "Add Transaction"
CCDA_MSG_INVALID_KEY = "Invalid key pressed. Please use Enter, PF3, PF4, or PF5."

# CICS Response Codes
DFHRESP_NORMAL = 0
DFHRESP_NOTFND = 12
DFHRESP_ENDFILE = 10
DFHRESP_DUPKEY = 22
DFHRESP_DUPREC = 22

# Date format
WS_DATE_FORMAT = "YYYY-MM-DD"


# ============================================================================
# Program-Specific Commarea (CDEMO-CT02-INFO)
# ============================================================================

@dataclass
class ThisProgCommarea:
    """Program-specific commarea (CDEMO-CT02-INFO)."""
    trnid_first: str = ""  # CDEMO-CT02-TRNID-FIRST (16 chars)
    trnid_last: str = ""  # CDEMO-CT02-TRNID-LAST (16 chars)
    page_num: int = 0  # CDEMO-CT02-PAGE-NUM
    next_page_flg: bool = False  # CDEMO-CT02-NEXT-PAGE-FLG (False = 'N', True = 'Y')
    trn_sel_flg: str = ""  # CDEMO-CT02-TRN-SEL-FLG
    trn_selected: str = ""  # CDEMO-CT02-TRN-SELECTED (16 chars)


# ============================================================================
# Map Structure (COTRN2AO/COTRN2AI)
# ============================================================================

@dataclass
class TransactionAddMap:
    """Transaction add map structure (COTRN2A)."""
    # Header fields
    title01o: str = ""
    title02o: str = ""
    trnnameo: str = ""
    pgmnameo: str = ""
    curdateo: str = ""
    curtimeo: str = ""
    
    # Key fields (Account ID or Card Number)
    actidini: str = ""  # Account ID input
    actidino: str = ""  # Account ID output
    actidinl: int = 0  # Account ID length (cursor)
    
    cardnini: str = ""  # Card number input
    cardnino: str = ""  # Card number output
    cardninl: int = 0  # Card number length (cursor)
    
    # Transaction detail fields
    ttypcdi: str = ""  # Transaction type code input
    ttypcdo: str = ""  # Transaction type code output
    ttypcdl: int = 0  # Transaction type code length (cursor)
    
    tcatcdi: str = ""  # Transaction category code input
    tcatcdo: str = ""  # Transaction category code output
    tcatcdl: int = 0  # Transaction category code length (cursor)
    
    trnsrci: str = ""  # Transaction source input
    trnsrco: str = ""  # Transaction source output
    trnsrcl: int = 0  # Transaction source length (cursor)
    
    trnamti: str = ""  # Transaction amount input
    trnamto: str = ""  # Transaction amount output
    trnamtl: int = 0  # Transaction amount length (cursor)
    
    tdesci: str = ""  # Transaction description input
    tdesco: str = ""  # Transaction description output
    tdescl: int = 0  # Transaction description length (cursor)
    
    torigdti: str = ""  # Transaction original date input
    torigdto: str = ""  # Transaction original date output
    torigdtl: int = 0  # Transaction original date length (cursor)
    
    tprocdti: str = ""  # Transaction processed date input
    tprocdto: str = ""  # Transaction processed date output
    tprocdtl: int = 0  # Transaction processed date length (cursor)
    
    midi: str = ""  # Merchant ID input
    mido: str = ""  # Merchant ID output
    midl: int = 0  # Merchant ID length (cursor)
    
    mnamei: str = ""  # Merchant name input
    mnameo: str = ""  # Merchant name output
    mnamel: int = 0  # Merchant name length (cursor)
    
    mcityi: str = ""  # Merchant city input
    mcityo: str = ""  # Merchant city output
    mcityl: int = 0  # Merchant city length (cursor)
    
    mzipi: str = ""  # Merchant zip input
    mzipo: str = ""  # Merchant zip output
    mzipl: int = 0  # Merchant zip length (cursor)
    
    # Confirmation field
    confirmi: str = ""  # Confirmation input (Y/N)
    confirmo: str = ""  # Confirmation output
    confirml: int = 0  # Confirmation length (cursor)
    
    # Message fields
    errmsgo: str = ""
    errmsgc: str = ""  # Error message color


# ============================================================================
# XREF File Handlers
# ============================================================================

class XrefFileHandler:
    """Handler for CCXREF file (primary key by card number)."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self._data: dict = {}  # Key: card_num
    
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
                                record = parse_xref_record(line)
                                if record.card_num:
                                    self._data[record.card_num] = record
                except FileNotFoundError:
                    pass
            
            self.resp_cd = DFHRESP_NORMAL
            return 0
        except Exception as e:
            self.resp_cd = 13
            return 13
    
    def close_file(self):
        """Close file and save data."""
        pass  # Read-only for this program
    
    def read_record(self, card_num: str) -> Optional[CardXrefRecord]:
        """Read record by card number."""
        card_num = card_num.strip()
        
        if card_num in self._data:
            self.resp_cd = DFHRESP_NORMAL
            return self._data[card_num]
        else:
            self.resp_cd = DFHRESP_NOTFND
            return None
    
    def add_record(self, record: CardXrefRecord):
        """Add a record (for testing)."""
        if record.card_num:
            self._data[record.card_num] = record


class XrefAixFileHandler:
    """Handler for CXACAIX file (alternate index by account ID)."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self._data: dict = {}  # Key: acct_id
        self._card_num_to_record: dict = {}  # Reverse lookup
    
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
                                record = parse_xref_record(line)
                                if record.card_num and record.acct_id:
                                    # Store by account ID (alternate index)
                                    self._data[record.acct_id] = record
                                    self._card_num_to_record[record.card_num] = record
                except FileNotFoundError:
                    pass
            
            self.resp_cd = DFHRESP_NORMAL
            return 0
        except Exception as e:
            self.resp_cd = 13
            return 13
    
    def close_file(self):
        """Close file and save data."""
        pass  # Read-only for this program
    
    def read_record(self, acct_id: str) -> Optional[CardXrefRecord]:
        """Read record by account ID (alternate index)."""
        acct_id = acct_id.strip()
        
        if acct_id in self._data:
            self.resp_cd = DFHRESP_NORMAL
            return self._data[acct_id]
        else:
            self.resp_cd = DFHRESP_NOTFND
            return None
    
    def add_record(self, record: CardXrefRecord):
        """Add a record (for testing)."""
        if record.card_num and record.acct_id:
            self._data[record.acct_id] = record
            self._card_num_to_record[record.card_num] = record


# ============================================================================
# Transaction File Handler (with WRITE support)
# ============================================================================

class TranFileHandler(TranFileHandlerBase):
    """Extended transaction file handler with WRITE support."""
    
    def write_record(self, record: TranRecord) -> int:
        """Write transaction record (CICS WRITE).
        
        Args:
            record: TranRecord to write
        
        Returns:
            Response code
        """
        if not record.tran_id:
            self.resp_cd = 13
            return 13
        
        tran_id = record.tran_id.strip()
        
        # Ensure sorted_keys is updated (in case data was added before open_file)
        if not self._sorted_keys:
            self._sorted_keys = sorted(self._data.keys())
        
        # Check for duplicate key
        if tran_id in self._data:
            self.resp_cd = DFHRESP_DUPKEY
            return DFHRESP_DUPKEY
        
        # Add record
        self._data[tran_id] = record
        self._sorted_keys = sorted(self._data.keys())
        
        self.resp_cd = DFHRESP_NORMAL
        return DFHRESP_NORMAL
    
    def startbr_high_values(self) -> int:
        """Start browse from HIGH-VALUES (end of file)."""
        if not self._sorted_keys:
            self._browse_active = True
            self._browse_index = 0
            self._browse_key = None
            self.resp_cd = DFHRESP_NORMAL
            return DFHRESP_NORMAL
        
        # Start from end (HIGH-VALUES)
        self._browse_active = True
        self._browse_index = len(self._sorted_keys)  # Position beyond end
        self._browse_key = None
        self._browse_direction = "BACKWARD"
        self.resp_cd = DFHRESP_NORMAL
        return DFHRESP_NORMAL
    
    def readprev_high_values(self) -> Optional[TranRecord]:
        """Read previous record when starting from HIGH-VALUES."""
        if not self._browse_active:
            self.resp_cd = 13
            return None
        
        if self._browse_index <= 0:
            self.resp_cd = DFHRESP_ENDFILE
            return None
        
        # Read from end backwards
        self._browse_index -= 1
        if self._browse_index < len(self._sorted_keys):
            key = self._sorted_keys[self._browse_index]
            record = self._data[key]
            self._browse_key = key
            self.resp_cd = DFHRESP_NORMAL
            return record
        else:
            self.resp_cd = DFHRESP_ENDFILE
            return None


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
        self.acctdat_file: str = WS_ACCTDAT_FILE
        self.ccxref_file: str = WS_CCXREF_FILE
        self.cxacaix_file: str = WS_CXACAIX_FILE
        self.err_flg: bool = False
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self.usr_modified: bool = False
        
        # Transaction amount and date
        self.tran_amt: Decimal = Decimal('0')
        self.tran_amt_n: Decimal = Decimal('0')
        self.tran_amt_e: str = ""
        self.tran_date: str = ""
        self.date_format: str = WS_DATE_FORMAT
        
        # Account and card number (numeric)
        self.acct_id_n: int = 0
        self.card_num_n: int = 0
        self.tran_id_n: int = 0
        
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
        
        # File handlers
        self.tran_handler: Optional[TranFileHandler] = None
        self.xref_handler: Optional[XrefFileHandler] = None
        self.xref_aix_handler: Optional[XrefAixFileHandler] = None
        
        # Current transaction ID for browse
        self.tran_id: str = ""
        
        # Current XREF record
        self.xref_record: Optional[CardXrefRecord] = None
        self.xref_acct_id: str = ""
        self.xref_card_num: str = ""


# ============================================================================
# Map Functions
# ============================================================================

def populate_header_info(ws: WorkingStorage, map_out: TransactionAddMap):
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


def send_trnadd_screen(ws: WorkingStorage, map_out: TransactionAddMap):
    """Send transaction add screen (SEND-TRNADD-SCREEN)."""
    populate_header_info(ws, map_out)
    map_out.errmsgo = ws.message


def receive_trnadd_screen(ws: WorkingStorage, map_in: TransactionAddMap):
    """Receive transaction add screen (RECEIVE-TRNADD-SCREEN)."""
    ws.resp_cd = 0
    ws.reas_cd = 0
    # Extract input fields from map
    # (In real CICS, this would be done automatically by RECEIVE MAP)


def initialize_all_fields(ws: WorkingStorage, map_in: TransactionAddMap):
    """Initialize all fields (INITIALIZE-ALL-FIELDS)."""
    map_in.actidinl = -1
    map_in.actidini = ""
    map_in.cardnini = ""
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
    map_in.confirmi = ""
    ws.message = ""


# ============================================================================
# File Operations
# ============================================================================

def read_cxacaix_file(ws: WorkingStorage, acct_id: str) -> Optional[CardXrefRecord]:
    """Read CXACAIX file (alternate index by account ID) (READ-CXACAIX-FILE).
    
    Returns:
        CardXrefRecord if found, None otherwise
    """
    if not ws.xref_aix_handler:
        ws.err_flg = True
        ws.message = "Unable to lookup Acct in XREF AIX file..."
        return None
    
    record = ws.xref_aix_handler.read_record(acct_id)
    ws.resp_cd = ws.xref_aix_handler.resp_cd
    
    if ws.resp_cd == DFHRESP_NORMAL:
        ws.xref_record = record
        ws.xref_acct_id = record.acct_id if record else ""
        ws.xref_card_num = record.card_num if record else ""
        return record
    elif ws.resp_cd == DFHRESP_NOTFND:
        ws.err_flg = True
        ws.message = "Account ID NOT found..."
        return None
    else:
        ws.err_flg = True
        ws.message = "Unable to lookup Acct in XREF AIX file..."
        return None


def read_ccxref_file(ws: WorkingStorage, card_num: str) -> Optional[CardXrefRecord]:
    """Read CCXREF file (primary key by card number) (READ-CCXREF-FILE).
    
    Returns:
        CardXrefRecord if found, None otherwise
    """
    if not ws.xref_handler:
        ws.err_flg = True
        ws.message = "Unable to lookup Card # in XREF file..."
        return None
    
    record = ws.xref_handler.read_record(card_num)
    ws.resp_cd = ws.xref_handler.resp_cd
    
    if ws.resp_cd == DFHRESP_NORMAL:
        ws.xref_record = record
        ws.xref_acct_id = record.acct_id if record else ""
        ws.xref_card_num = record.card_num if record else ""
        return record
    elif ws.resp_cd == DFHRESP_NOTFND:
        ws.err_flg = True
        ws.message = "Card Number NOT found..."
        return None
    else:
        ws.err_flg = True
        ws.message = "Unable to lookup Card # in XREF file..."
        return None


def startbr_transact_file_high_values(ws: WorkingStorage) -> bool:
    """Start browse from HIGH-VALUES (STARTBR-TRANSACT-FILE).
    
    Returns:
        True if successful, False otherwise
    """
    if not ws.tran_handler:
        ws.err_flg = True
        ws.message = "Unable to lookup Transaction..."
        return False
    
    resp_cd = ws.tran_handler.startbr_high_values()
    ws.resp_cd = resp_cd
    
    if resp_cd == DFHRESP_NORMAL:
        return True
    else:
        ws.err_flg = True
        ws.message = "Unable to lookup Transaction..."
        return False


def readprev_transact_file(ws: WorkingStorage) -> Optional[TranRecord]:
    """Read previous transaction (READPREV-TRANSACT-FILE).
    
    Returns:
        TranRecord if found, None if end of file
    """
    if not ws.tran_handler:
        ws.err_flg = True
        ws.message = "Unable to lookup Transaction..."
        return None
    
    record = ws.tran_handler.readprev_high_values()
    ws.resp_cd = ws.tran_handler.resp_cd
    
    if ws.resp_cd == DFHRESP_NORMAL:
        return record
    elif ws.resp_cd == DFHRESP_ENDFILE:
        # Set TRAN-ID to zeros
        ws.tran_id = "0000000000000000"
        return None
    else:
        ws.err_flg = True
        ws.message = "Unable to lookup Transaction..."
        return None


def endbr_transact_file(ws: WorkingStorage):
    """End browse on transaction file (ENDBR-TRANSACT-FILE)."""
    if ws.tran_handler:
        ws.tran_handler.endbr()


def write_transact_file(ws: WorkingStorage, record: TranRecord) -> bool:
    """Write transaction file (WRITE-TRANSACT-FILE).
    
    Returns:
        True if successful, False otherwise
    """
    if not ws.tran_handler:
        ws.err_flg = True
        ws.message = "Unable to Add Transaction..."
        return False
    
    resp_cd = ws.tran_handler.write_record(record)
    ws.resp_cd = resp_cd
    
    if resp_cd == DFHRESP_NORMAL:
        # Save to file
        ws.tran_handler.close_file()
        return True
    elif resp_cd == DFHRESP_DUPKEY:
        ws.err_flg = True
        ws.message = "Tran ID already exist..."
        return False
    else:
        ws.err_flg = True
        ws.message = "Unable to Add Transaction..."
        return False


# ============================================================================
# Validation Functions
# ============================================================================

def validate_input_key_fields(ws: WorkingStorage, map_in: TransactionAddMap, map_out: TransactionAddMap) -> bool:
    """Validate input key fields (VALIDATE-INPUT-KEY-FIELDS).
    
    Returns:
        True if valid, False otherwise
    """
    # Check if account ID is provided
    if map_in.actidini and map_in.actidini.strip():
        # Validate account ID is numeric
        if not map_in.actidini.strip().isdigit():
            ws.err_flg = True
            ws.message = "Account ID must be Numeric..."
            map_out.actidinl = -1
            send_trnadd_screen(ws, map_out)
            return False
        
        # Convert to numeric
        try:
            ws.acct_id_n = int(map_in.actidini.strip())
            ws.xref_acct_id = f"{ws.acct_id_n:011d}"
        except ValueError:
            ws.err_flg = True
            ws.message = "Account ID must be Numeric..."
            map_out.actidinl = -1
            send_trnadd_screen(ws, map_out)
            return False
        
        # Update map
        map_in.actidini = ws.xref_acct_id
        map_out.actidini = ws.xref_acct_id
        
        # Read from alternate index
        record = read_cxacaix_file(ws, ws.xref_acct_id)
        if record and not ws.err_flg:
            map_in.cardnini = record.card_num
            map_out.cardnini = record.card_num
            return True
        else:
            return False
    
    # Check if card number is provided
    elif map_in.cardnini and map_in.cardnini.strip():
        # Validate card number is numeric
        if not map_in.cardnini.strip().isdigit():
            ws.err_flg = True
            ws.message = "Card Number must be Numeric..."
            map_out.cardninl = -1
            send_trnadd_screen(ws, map_out)
            return False
        
        # Convert to numeric
        try:
            ws.card_num_n = int(map_in.cardnini.strip())
            ws.xref_card_num = f"{ws.card_num_n:016d}"
        except ValueError:
            ws.err_flg = True
            ws.message = "Card Number must be Numeric..."
            map_out.cardninl = -1
            send_trnadd_screen(ws, map_out)
            return False
        
        # Update map
        map_in.cardnini = ws.xref_card_num
        map_out.cardnini = ws.xref_card_num
        
        # Read from primary key
        record = read_ccxref_file(ws, ws.xref_card_num)
        if record and not ws.err_flg:
            map_in.actidini = record.acct_id
            map_out.actidini = record.acct_id
            return True
        else:
            return False
    
    else:
        # Neither account ID nor card number provided
        ws.err_flg = True
        ws.message = "Account or Card Number must be entered..."
        map_out.actidinl = -1
        send_trnadd_screen(ws, map_out)
        return False


def validate_input_data_fields(ws: WorkingStorage, map_in: TransactionAddMap, map_out: TransactionAddMap) -> bool:
    """Validate input data fields (VALIDATE-INPUT-DATA-FIELDS).
    
    Returns:
        True if valid, False otherwise
    """
    # If error flag is already set, clear data fields
    if ws.err_flg:
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
        return False
    
    # Validate required fields are not empty
    required_fields = [
        ("ttypcdi", "ttypcdl", "Type CD can NOT be empty..."),
        ("tcatcdi", "tcatcdl", "Category CD can NOT be empty..."),
        ("trnsrci", "trnsrcl", "Source can NOT be empty..."),
        ("tdesci", "tdescl", "Description can NOT be empty..."),
        ("trnamti", "trnamtl", "Amount can NOT be empty..."),
        ("torigdti", "torigdtl", "Orig Date can NOT be empty..."),
        ("tprocdti", "tprocdtl", "Proc Date can NOT be empty..."),
        ("midi", "midl", "Merchant ID can NOT be empty..."),
        ("mnamei", "mnamel", "Merchant Name can NOT be empty..."),
        ("mcityi", "mcityl", "Merchant City can NOT be empty..."),
        ("mzipi", "mzipl", "Merchant Zip can NOT be empty..."),
    ]
    
    for field_name, length_field, error_msg in required_fields:
        field_value = getattr(map_in, field_name, "").strip()
        if not field_value:
            ws.err_flg = True
            ws.message = error_msg
            setattr(map_out, length_field, -1)
            send_trnadd_screen(ws, map_out)
            return False
    
    # Validate numeric fields
    if not map_in.ttypcdi.strip().isdigit():
        ws.err_flg = True
        ws.message = "Type CD must be Numeric..."
        map_out.ttypcdl = -1
        send_trnadd_screen(ws, map_out)
        return False
    
    if not map_in.tcatcdi.strip().isdigit():
        ws.err_flg = True
        ws.message = "Category CD must be Numeric..."
        map_out.tcatcdl = -1
        send_trnadd_screen(ws, map_out)
        return False
    
    # Validate amount format (-99999999.99)
    trnamti = map_in.trnamti.strip()
    if len(trnamti) < 12:
        ws.err_flg = True
        ws.message = "Amount should be in format -99999999.99"
        map_out.trnamtl = -1
        send_trnadd_screen(ws, map_out)
        return False
    
    # Check format: first char is - or +, then 8 digits, then ., then 2 digits
    if trnamti[0] not in ('-', '+'):
        ws.err_flg = True
        ws.message = "Amount should be in format -99999999.99"
        map_out.trnamtl = -1
        send_trnadd_screen(ws, map_out)
        return False
    
    if not trnamti[1:9].isdigit():
        ws.err_flg = True
        ws.message = "Amount should be in format -99999999.99"
        map_out.trnamtl = -1
        send_trnadd_screen(ws, map_out)
        return False
    
    if trnamti[9] != '.':
        ws.err_flg = True
        ws.message = "Amount should be in format -99999999.99"
        map_out.trnamtl = -1
        send_trnadd_screen(ws, map_out)
        return False
    
    if not trnamti[10:12].isdigit():
        ws.err_flg = True
        ws.message = "Amount should be in format -99999999.99"
        map_out.trnamtl = -1
        send_trnadd_screen(ws, map_out)
        return False
    
    # Parse amount
    try:
        ws.tran_amt_n = Decimal(trnamti)
        ws.tran_amt_e = f"{ws.tran_amt_n:>+12.2f}"
        map_in.trnamti = ws.tran_amt_e
        map_out.trnamti = ws.tran_amt_e
    except Exception:
        ws.err_flg = True
        ws.message = "Amount should be in format -99999999.99"
        map_out.trnamtl = -1
        send_trnadd_screen(ws, map_out)
        return False
    
    # Validate date formats (YYYY-MM-DD)
    for date_field, length_field, error_msg in [
        ("torigdti", "torigdtl", "Orig Date should be in format YYYY-MM-DD"),
        ("tprocdti", "tprocdtl", "Proc Date should be in format YYYY-MM-DD"),
    ]:
        date_value = getattr(map_in, date_field, "").strip()
        if len(date_value) < 10:
            ws.err_flg = True
            ws.message = error_msg
            setattr(map_out, length_field, -1)
            send_trnadd_screen(ws, map_out)
            return False
        
        # Check format: YYYY-MM-DD
        if not date_value[0:4].isdigit():
            ws.err_flg = True
            ws.message = error_msg
            setattr(map_out, length_field, -1)
            send_trnadd_screen(ws, map_out)
            return False
        
        if date_value[4] != '-':
            ws.err_flg = True
            ws.message = error_msg
            setattr(map_out, length_field, -1)
            send_trnadd_screen(ws, map_out)
            return False
        
        if not date_value[5:7].isdigit():
            ws.err_flg = True
            ws.message = error_msg
            setattr(map_out, length_field, -1)
            send_trnadd_screen(ws, map_out)
            return False
        
        if date_value[7] != '-':
            ws.err_flg = True
            ws.message = error_msg
            setattr(map_out, length_field, -1)
            send_trnadd_screen(ws, map_out)
            return False
        
        if not date_value[8:10].isdigit():
            ws.err_flg = True
            ws.message = error_msg
            setattr(map_out, length_field, -1)
            send_trnadd_screen(ws, map_out)
            return False
    
    # Validate dates using CSUTLDTC equivalent
    orig_date = map_in.torigdti.strip()
    result = validate_date(orig_date, ws.date_format)
    if result.sev_cd != "0000":
        # msg_num "2513" appears to be for format errors, but we still want to catch invalid dates
        # Check if it's truly invalid (not just format)
        try:
            parts = orig_date.split('-')
            if len(parts) == 3:
                year = int(parts[0])
                month = int(parts[1])
                day = int(parts[2])
                datetime(year, month, day)  # This will raise ValueError for invalid dates
        except (ValueError, IndexError):
            # Invalid date - show error
            ws.err_flg = True
            ws.message = "Orig Date - Not a valid date..."
            map_out.torigdtl = -1
            send_trnadd_screen(ws, map_out)
            return False
    
    proc_date = map_in.tprocdti.strip()
    result = validate_date(proc_date, ws.date_format)
    if result.sev_cd != "0000":
        # Check if it's truly invalid
        try:
            parts = proc_date.split('-')
            if len(parts) == 3:
                year = int(parts[0])
                month = int(parts[1])
                day = int(parts[2])
                datetime(year, month, day)
        except (ValueError, IndexError):
            # Invalid date - show error
            ws.err_flg = True
            ws.message = "Proc Date - Not a valid date..."
            map_out.tprocdtl = -1
            send_trnadd_screen(ws, map_out)
            return False
    
    # Validate merchant ID is numeric (only digits, no leading zeros check needed)
    midi_clean = map_in.midi.strip()
    if not midi_clean.isdigit():
        ws.err_flg = True
        ws.message = "Merchant ID must be Numeric..."
        map_out.midl = -1
        send_trnadd_screen(ws, map_out)
        return False
    
    return True


# ============================================================================
# Process Enter Key
# ============================================================================

def process_enter_key(ws: WorkingStorage, map_in: TransactionAddMap, map_out: TransactionAddMap) -> bool:
    """Process enter key (PROCESS-ENTER-KEY).
    
    Returns:
        True if transaction added, False otherwise
    """
    # Validate key fields
    if not validate_input_key_fields(ws, map_in, map_out):
        return False
    
    # Validate data fields
    if not validate_input_data_fields(ws, map_in, map_out):
        return False
    
    # Check confirmation
    confirm_value = map_in.confirmi.strip().upper()
    
    if confirm_value == "Y":
        # Confirmed - add transaction
        return add_transaction(ws, map_in, map_out)
    elif confirm_value in ("N", ""):
        # Not confirmed
        ws.err_flg = True
        ws.message = "Confirm to add this transaction..."
        map_out.confirml = -1
        send_trnadd_screen(ws, map_out)
        return False
    else:
        # Invalid confirmation value
        ws.err_flg = True
        ws.message = "Invalid value. Valid values are (Y/N)..."
        map_out.confirml = -1
        send_trnadd_screen(ws, map_out)
        return False


# ============================================================================
# Add Transaction
# ============================================================================

def add_transaction(ws: WorkingStorage, map_in: TransactionAddMap, map_out: TransactionAddMap) -> bool:
    """Add transaction (ADD-TRANSACTION).
    
    Returns:
        True if successful, False otherwise
    """
    # Generate new transaction ID
    # Start browse from HIGH-VALUES
    if not startbr_transact_file_high_values(ws):
        return False
    
    # Read previous record (last transaction)
    last_record = readprev_transact_file(ws)
    
    # End browse
    endbr_transact_file(ws)
    
    # Calculate new transaction ID
    if last_record and last_record.tran_id and last_record.tran_id.strip():
        try:
            ws.tran_id_n = int(last_record.tran_id.strip())
            ws.tran_id_n += 1
        except ValueError:
            ws.tran_id_n = 1
    else:
        # No records found or EOF - start from 1
        ws.tran_id_n = 1
    
    # Format transaction ID (16 characters)
    new_tran_id = f"{ws.tran_id_n:016d}"
    
    # Create transaction record
    new_record = TranRecord()
    new_record.tran_id = new_tran_id
    new_record.tran_type_cd = map_in.ttypcdi.strip()
    new_record.tran_cat_cd = map_in.tcatcdi.strip()
    new_record.tran_source = map_in.trnsrci.strip()
    new_record.tran_desc = map_in.tdesci.strip()
    new_record.tran_amt = ws.tran_amt_n
    new_record.tran_card_num = map_in.cardnini.strip()
    new_record.tran_merchant_id = map_in.midi.strip()
    new_record.tran_merchant_name = map_in.mnamei.strip()
    new_record.tran_merchant_city = map_in.mcityi.strip()
    new_record.tran_merchant_zip = map_in.mzipi.strip()
    new_record.tran_orig_ts = map_in.torigdti.strip()
    new_record.tran_proc_ts = map_in.tprocdti.strip()
    
    # Write transaction
    if write_transact_file(ws, new_record):
        # Success - initialize fields and show success message
        initialize_all_fields(ws, map_in)
        map_out.actidini = ""
        map_out.cardnini = ""
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
        map_out.confirmi = ""
        
        ws.message = f"Transaction added successfully. Your Tran ID is {new_tran_id}."
        map_out.errmsgc = "GREEN"  # DFHGREEN equivalent
        send_trnadd_screen(ws, map_out)
        return True
    else:
        # Error already set in write_transact_file
        send_trnadd_screen(ws, map_out)
        return False


# ============================================================================
# Copy Last Transaction Data
# ============================================================================

def copy_last_tran_data(ws: WorkingStorage, map_in: TransactionAddMap, map_out: TransactionAddMap):
    """Copy last transaction data (COPY-LAST-TRAN-DATA)."""
    # Validate key fields first
    if not validate_input_key_fields(ws, map_in, map_out):
        return
    
    # Start browse from HIGH-VALUES
    if not startbr_transact_file_high_values(ws):
        return
    
    # Read previous record (last transaction)
    last_record = readprev_transact_file(ws)
    
    # End browse
    endbr_transact_file(ws)
    
    # Copy data if record found
    if last_record and not ws.err_flg:
        ws.tran_amt = last_record.tran_amt
        ws.tran_amt_e = f"{ws.tran_amt:>+12.2f}"
        
        map_in.ttypcdi = last_record.tran_type_cd
        map_in.tcatcdi = last_record.tran_cat_cd
        map_in.trnsrci = last_record.tran_source
        map_in.trnamti = ws.tran_amt_e
        map_in.tdesci = last_record.tran_desc
        map_in.torigdti = last_record.tran_orig_ts
        map_in.tprocdti = last_record.tran_proc_ts
        map_in.midi = last_record.tran_merchant_id
        map_in.mnamei = last_record.tran_merchant_name
        map_in.mcityi = last_record.tran_merchant_city
        map_in.mzipi = last_record.tran_merchant_zip
        
        # Also update map_out
        map_out.ttypcdi = map_in.ttypcdi
        map_out.tcatcdi = map_in.tcatcdi
        map_out.trnsrci = map_in.trnsrci
        map_out.trnamti = map_in.trnamti
        map_out.tdesci = map_in.tdesci
        map_out.torigdti = map_in.torigdti
        map_out.tprocdti = map_in.tprocdti
        map_out.midi = map_in.midi
        map_out.mnamei = map_in.mnamei
        map_out.mcityi = map_in.mcityi
        map_out.mzipi = map_in.mzipi
    
    # Process enter key (will validate and check confirmation)
    process_enter_key(ws, map_in, map_out)


# ============================================================================
# Clear Current Screen
# ============================================================================

def clear_current_screen(ws: WorkingStorage, map_in: TransactionAddMap, map_out: TransactionAddMap):
    """Clear current screen (CLEAR-CURRENT-SCREEN)."""
    initialize_all_fields(ws, map_in)
    map_out.actidini = ""
    map_out.cardnini = ""
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
    map_out.confirmi = ""
    send_trnadd_screen(ws, map_out)


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
    map_input: Optional[TransactionAddMap] = None,
    prog_commarea: Optional[ThisProgCommarea] = None,
    tran_handler: Optional[TranFileHandler] = None,
    xref_handler: Optional[XrefFileHandler] = None,
    xref_aix_handler: Optional[XrefAixFileHandler] = None
) -> Tuple[CardDemoCommarea, TransactionAddMap, Optional[str], ThisProgCommarea]:
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
    map_out = TransactionAddMap()
    if map_input is None:
        map_input = TransactionAddMap()
    
    # Initialize file handlers
    if tran_handler is None:
        ws.tran_handler = TranFileHandler(WS_TRANSACT_FILE)
        ws.tran_handler.open_file()
    else:
        ws.tran_handler = tran_handler
    
    if xref_handler is None:
        ws.xref_handler = XrefFileHandler(WS_CCXREF_FILE)
        ws.xref_handler.open_file()
    else:
        ws.xref_handler = xref_handler
    
    if xref_aix_handler is None:
        ws.xref_aix_handler = XrefAixFileHandler(WS_CXACAIX_FILE)
        ws.xref_aix_handler.open_file()
    else:
        ws.xref_aix_handler = xref_aix_handler
    
    map_out.actidinl = -1
    
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
            map_out.actidinl = -1
            
            # Copy map_input to map_out for processing
            map_out = map_input
            
            # If card number was selected from list, populate it
            if prog_commarea.trn_selected and prog_commarea.trn_selected.strip():
                map_out.cardnini = prog_commarea.trn_selected
                map_in = TransactionAddMap()
                map_in.cardnini = prog_commarea.trn_selected
                process_enter_key(ws, map_in, map_out)
            
            send_trnadd_screen(ws, map_out)
            xctl_prog = None
        else:
            # Reenter - process input
            receive_trnadd_screen(ws, map_input)
            
            # Copy map_input to map_out for processing
            map_out = map_input
            
            if eibaid == DFHENTER:
                # Process enter key
                process_enter_key(ws, map_input, map_out)
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
                # PF5 - copy last transaction data
                copy_last_tran_data(ws, map_input, map_out)
                xctl_prog = None
            else:
                # Invalid key
                ws.err_flg = True
                ws.message = CCDA_MSG_INVALID_KEY
                send_trnadd_screen(ws, map_out)
                xctl_prog = None
    
    # Set message in map
    map_out.errmsgo = ws.message
    
    return commarea, map_out, None, prog_commarea

