"""
Python Translation of COCRDSLC.cbl

Program: COCRDSLC
Layer: Business logic
Function: Accept and process credit card detail request

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, Tuple
from decimal import Decimal

# Import shared structures
from COACTVWC import CardDemoCommarea


# ============================================================================
# Constants and Literals
# ============================================================================

LIT_THISPGM = "COCRDSLC"
LIT_THISTRANID = "CCDL"
LIT_THISMAPSET = "COCRDSL "
LIT_THISMAP = "CCRDSLA"
LIT_CCLISTPGM = "COCRDLIC"
LIT_CCLISTTRANID = "CCLI"
LIT_CCLISTMAPSET = "COCRDLI"
LIT_MENUPGM = "COMEN01C"
LIT_MENUTRANID = "CM00"
LIT_CARDFILENAME = "CARDDAT "
LIT_CARDFILENAME_ACCT_PATH = "CARDAIX "

DFHENTER = "DFHENTER"
DFHPF3 = "DFHPF3"
DFHRED = "DFHRED"
DFHBMPRF = "DFHBMPRF"
DFHBMFSE = "DFHBMFSE"
DFHBMDAR = "DFHBMDAR"
DFHNEUTR = "DFHNEUTR"
DFHDFCOL = "DFHDFCOL"

# CICS Response Codes
DFHRESP_NORMAL = 0
DFHRESP_NOTFND = 12

# Default titles
CCDA_TITLE01 = "CardDemo Application"
CCDA_TITLE02 = "Credit Card Detail"


# ============================================================================
# Card Record Structure (Extended)
# ============================================================================

@dataclass
class CardRecord:
    """Card record (CARD-RECORD from COPY CVACT02Y)."""
    card_num: str = ""  # 16 chars
    card_acct_id: str = ""  # 11 chars
    card_active_status: str = ""  # 1 char
    card_embossed_name: str = ""  # 50 chars
    card_expiration_date: str = ""  # 10 chars (YYYY-MM-DD format)
    
    @property
    def card_expiry_year(self) -> str:
        """Extract expiry year from expiration date."""
        if len(self.card_expiration_date) >= 4:
            return self.card_expiration_date[:4]
        return ""
    
    @property
    def card_expiry_month(self) -> str:
        """Extract expiry month from expiration date."""
        if len(self.card_expiration_date) >= 7:
            return self.card_expiration_date[5:7]
        return ""
    
    @property
    def card_expiry_day(self) -> str:
        """Extract expiry day from expiration date."""
        if len(self.card_expiration_date) >= 10:
            return self.card_expiration_date[8:10]
        return ""


def parse_card_record(line: str) -> CardRecord:
    """Parse card record from line.
    
    Based on CVACT02Y structure with extended fields:
    - CARD-NUM: PIC X(16) - positions 0-15
    - CARD-ACCT-ID: PIC 9(11) - positions 16-26
    - CARD-ACTIVE-STATUS: PIC X(1) - position 27
    - CARD-EMBOSSED-NAME: PIC X(50) - positions 28-77
    - CARD-EXPIRATION-DATE: PIC X(10) - positions 78-87
    """
    if len(line) < 28:
        return CardRecord()
    
    card_num = line[0:16].strip() if len(line) >= 16 else ""
    card_acct_id = line[16:27].strip() if len(line) >= 27 else ""
    card_active_status = line[27:28] if len(line) >= 28 else ""
    card_embossed_name = line[28:78].strip() if len(line) >= 78 else ""
    card_expiration_date = line[78:88].strip() if len(line) >= 88 else ""
    
    return CardRecord(
        card_num=card_num,
        card_acct_id=card_acct_id,
        card_active_status=card_active_status,
        card_embossed_name=card_embossed_name,
        card_expiration_date=card_expiration_date
    )


def format_card_record(record: CardRecord) -> str:
    """Format card record for writing."""
    line = (
        f"{record.card_num:<16}"
        f"{record.card_acct_id:<11}"
        f"{record.card_active_status:<1}"
        f"{record.card_embossed_name:<50}"
        f"{record.card_expiration_date:<10}"
    )
    # Pad to appropriate length (at least 150 chars)
    return line.ljust(150)


# ============================================================================
# Map Structure (CCRDSLAO/CCRDSLAI)
# ============================================================================

@dataclass
class CardDetailMap:
    """Card detail map structure."""
    # Header fields
    title01o: str = ""
    title02o: str = ""
    trnnameo: str = ""
    pgmnameo: str = ""
    curdateo: str = ""
    curtimeo: str = ""
    
    # Input fields
    acctsidi: str = ""  # Account ID input
    acctsido: str = ""  # Account ID output
    acctsida: str = ""  # Account ID attribute
    acctsidl: int = 0  # Account ID length (cursor)
    acctsidc: str = ""  # Account ID color
    
    cardsidi: str = ""  # Card number input
    cardsido: str = ""  # Card number output
    cardsida: str = ""  # Card number attribute
    cardsidl: int = 0  # Card number length (cursor)
    cardsidc: str = ""  # Card number color
    
    # Output fields
    crdnameo: str = ""  # Card embossed name output
    expmono: str = ""  # Expiry month output
    expyearo: str = ""  # Expiry year output
    crdstcdo: str = ""  # Card status code output
    
    # Message fields
    errmsgo: str = ""
    infomsgo: str = ""
    infomsgc: str = ""


# ============================================================================
# Program-Specific Commarea
# ============================================================================

@dataclass
class ThisProgCommarea:
    """Program-specific commarea (WS-THIS-PROGCOMMAREA)."""
    from_program: str = ""
    from_tranid: str = ""


# ============================================================================
# Working Storage
# ============================================================================

class WorkingStorage:
    """Working storage for program state."""
    
    def __init__(self):
        # CICS processing variables
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self.tranid: str = LIT_THISTRANID
        
        # Input flags
        self.input_ok: bool = True
        self.input_error: bool = False
        self.input_pending: bool = False
        
        # Edit flags
        self.acctfilter_not_ok: bool = False
        self.acctfilter_isvalid: bool = False
        self.acctfilter_blank: bool = True
        
        self.cardfilter_not_ok: bool = False
        self.cardfilter_isvalid: bool = False
        self.cardfilter_blank: bool = True
        
        # PFK flag
        self.pfk_valid: bool = False
        self.pfk_invalid: bool = True
        
        # Return flag
        self.return_flag: bool = False
        
        # Messages
        self.return_msg: str = ""
        self.info_msg: str = ""
        self.long_msg: str = ""
        self.found_cards_for_account: bool = False
        
        # Card RID
        self.card_rid_cardnum: str = ""
        self.card_rid_acct_id: str = ""
        self.card_rid_acct_id_x: str = ""
        
        # File handler
        self.card_handler: Optional['CardFileHandler'] = None
        
        # Current card record
        self.card_record: Optional[CardRecord] = None
        
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
        
        # Filter values
        self.cc_acct_id: str = ""
        self.cc_acct_id_n: str = ""
        self.cc_card_num: str = ""
        self.cc_card_num_n: str = ""
        
        # Card date fields
        self.card_expiration_date_x: str = ""


# ============================================================================
# Card File Handler
# ============================================================================

class CardFileHandler:
    """Handler for CARDDAT file."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.file_status = "00"
        self._data: dict = {}
    
    def open_file(self, mode: str = "r") -> int:
        """Open file and load data."""
        try:
            if mode in ("r", "r+"):
                with open(self.filename, "r", encoding='utf-8') as f:
                    for line in f:
                        line = line.rstrip('\n\r')
                        if len(line) > 0:
                            key = self._extract_key(line)
                            if key:
                                self._data[key] = line
            self.file_status = "00"
            return 0
        except FileNotFoundError:
            self.file_status = "23"
            return 12
        except Exception as e:
            self.file_status = "99"
            return 12
    
    def _extract_key(self, line: str) -> str:
        """Extract card number (primary key)."""
        if len(line) >= 16:
            return line[:16]
        return ""
    
    def read_record(self, key: str) -> Tuple[Optional[str], int]:
        """Read record by key."""
        try:
            if key in self._data:
                self.file_status = "00"
                return self._data[key], 0
            else:
                self.file_status = "23"  # NOTFND
                return None, 12
        except Exception as e:
            self.file_status = "99"
            return None, 12


# ============================================================================
# Map Functions
# ============================================================================

def populate_header_info(ws: WorkingStorage, map_out: CardDetailMap):
    """Populate header information (1100-SCREEN-INIT)."""
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
    map_out.pgmnameo = LIT_THISPGM
    map_out.curdateo = ws.curdate_mm_dd_yy
    map_out.curtimeo = ws.curtime_hh_mm_ss


def setup_screen_vars(ws: WorkingStorage, map_out: CardDetailMap, commarea: CardDemoCommarea, eibcalen: int):
    """Setup screen variables (1200-SETUP-SCREEN-VARS)."""
    # Initialize search criteria
    if eibcalen == 0:
        ws.info_msg = "Please enter Account and Card Number"
    else:
        # Set account ID output
        if commarea.acct_id == "0" or commarea.acct_id == "00000000000" or not commarea.acct_id:
            map_out.acctsido = ""
        else:
            map_out.acctsido = ws.cc_acct_id if ws.cc_acct_id else commarea.acct_id
        
        # Set card number output
        if commarea.card_num == "0" or commarea.card_num == "0000000000000000" or not commarea.card_num:
            map_out.cardsido = ""
        else:
            map_out.cardsido = ws.cc_card_num if ws.cc_card_num else commarea.card_num
        
        # If card found, display details
        if ws.found_cards_for_account and ws.card_record:
            map_out.crdnameo = ws.card_record.card_embossed_name
            map_out.expmono = ws.card_record.card_expiry_month
            map_out.expyearo = ws.card_record.card_expiry_year
            map_out.crdstcdo = ws.card_record.card_active_status
    
    # Setup message
    if not ws.info_msg:
        ws.info_msg = "Please enter Account and Card Number"
    
    map_out.errmsgo = ws.return_msg
    map_out.infomsgo = ws.info_msg


def setup_screen_attrs(ws: WorkingStorage, map_out: CardDetailMap, commarea: CardDemoCommarea):
    """Setup screen attributes (1300-SETUP-SCREEN-ATTRS)."""
    # Protect or unprotect based on context
    if (commarea.last_mapset == LIT_CCLISTMAPSET and 
        commarea.from_program == LIT_CCLISTPGM):
        map_out.acctsida = DFHBMPRF
        map_out.cardsida = DFHBMPRF
    else:
        map_out.acctsida = DFHBMFSE
        map_out.cardsida = DFHBMFSE
    
    # Position cursor
    if ws.acctfilter_not_ok or ws.acctfilter_blank:
        map_out.acctsidl = -1
    elif ws.cardfilter_not_ok or ws.cardfilter_blank:
        map_out.cardsidl = -1
    else:
        map_out.acctsidl = -1
    
    # Setup color
    if (commarea.last_mapset == LIT_CCLISTMAPSET and 
        commarea.from_program == LIT_CCLISTPGM):
        map_out.acctsidc = DFHDFCOL
        map_out.cardsidc = DFHDFCOL
    
    if ws.acctfilter_not_ok:
        map_out.acctsidc = DFHRED
    
    if ws.cardfilter_not_ok:
        map_out.cardsidc = DFHRED
    
    if ws.acctfilter_blank and commarea.pgm_reenter:
        map_out.acctsido = "*"
        map_out.acctsidc = DFHRED
    
    if ws.cardfilter_blank and commarea.pgm_reenter:
        map_out.cardsido = "*"
        map_out.cardsidc = DFHRED
    
    # Info message color
    if not ws.info_msg or ws.info_msg == "Please enter Account and Card Number":
        map_out.infomsgc = DFHBMDAR
    else:
        map_out.infomsgc = DFHNEUTR


def send_map(ws: WorkingStorage, map_out: CardDetailMap, commarea: CardDemoCommarea, eibcalen: int):
    """Send map (1000-SEND-MAP)."""
    populate_header_info(ws, map_out)
    setup_screen_vars(ws, map_out, commarea, eibcalen)
    setup_screen_attrs(ws, map_out, commarea)


def receive_map(ws: WorkingStorage, map_in: CardDetailMap):
    """Receive map (2100-RECEIVE-MAP)."""
    ws.resp_cd = 0
    ws.reas_cd = 0
    
    # Extract input fields
    acct_id = map_in.acctsidi.strip()
    card_num = map_in.cardsidi.strip()
    
    # Replace * with empty
    if acct_id == "*" or not acct_id:
        ws.cc_acct_id = ""
    else:
        ws.cc_acct_id = acct_id
    
    if card_num == "*" or not card_num:
        ws.cc_card_num = ""
    else:
        ws.cc_card_num = card_num


# ============================================================================
# Edit Functions
# ============================================================================

def edit_account(ws: WorkingStorage, commarea: CardDemoCommarea):
    """Edit account filter (2210-EDIT-ACCOUNT)."""
    ws.acctfilter_not_ok = True
    
    acct_id = ws.cc_acct_id.strip()
    if not acct_id or acct_id == "" or acct_id == "0" or acct_id == "00000000000":
        ws.input_error = True
        ws.acctfilter_blank = True
        if not ws.return_msg:
            ws.return_msg = "Account number not provided"
        commarea.acct_id = ""
        return
    
    # Check if numeric
    if not acct_id.isdigit():
        ws.input_error = True
        ws.acctfilter_not_ok = True
        if not ws.return_msg:
            ws.return_msg = "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"
        commarea.acct_id = ""
        return
    
    # Valid
    commarea.acct_id = acct_id
    ws.cc_acct_id_n = acct_id
    ws.acctfilter_isvalid = True
    ws.acctfilter_not_ok = False
    ws.acctfilter_blank = False


def edit_card(ws: WorkingStorage, commarea: CardDemoCommarea):
    """Edit card filter (2220-EDIT-CARD)."""
    ws.cardfilter_not_ok = True
    
    card_num = ws.cc_card_num.strip()
    if not card_num or card_num == "" or card_num == "0" or card_num == "0000000000000000":
        ws.input_error = True
        ws.cardfilter_blank = True
        if not ws.return_msg:
            ws.return_msg = "Card number not provided"
        commarea.card_num = ""
        return
    
    # Check if numeric
    if not card_num.isdigit():
        ws.input_error = True
        ws.cardfilter_not_ok = True
        if not ws.return_msg:
            ws.return_msg = "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"
        commarea.card_num = ""
        return
    
    # Valid
    ws.cc_card_num_n = card_num
    commarea.card_num = card_num
    ws.cardfilter_isvalid = True
    ws.cardfilter_not_ok = False
    ws.cardfilter_blank = False


def edit_map_inputs(ws: WorkingStorage, commarea: CardDemoCommarea):
    """Edit map inputs (2200-EDIT-MAP-INPUTS)."""
    ws.input_ok = True
    ws.cardfilter_isvalid = True
    ws.acctfilter_isvalid = True
    
    # Edit individual fields
    edit_account(ws, commarea)
    edit_card(ws, commarea)
    
    # Cross field edits
    if ws.acctfilter_blank and ws.cardfilter_blank:
        ws.input_error = True
        ws.return_msg = "No input received"


def process_inputs(ws: WorkingStorage, map_in: CardDetailMap, commarea: CardDemoCommarea):
    """Process inputs (2000-PROCESS-INPUTS)."""
    receive_map(ws, map_in)
    edit_map_inputs(ws, commarea)
    
    # Set commarea fields
    commarea.error_msg = ws.return_msg
    commarea.next_prog = LIT_THISPGM
    commarea.next_mapset = LIT_THISMAPSET.rstrip()
    commarea.next_map = LIT_THISMAP


# ============================================================================
# Read Data
# ============================================================================

def getcard_byacctcard(ws: WorkingStorage):
    """Get card by account and card (9100-GETCARD-BYACCTCARD)."""
    ws.card_rid_cardnum = ws.cc_card_num
    
    if not ws.card_handler:
        ws.input_error = True
        ws.return_msg = "Error reading Card Data File"
        return
    
    record_line, resp_cd = ws.card_handler.read_record(ws.card_rid_cardnum)
    ws.resp_cd = resp_cd
    
    if resp_cd == DFHRESP_NORMAL:
        ws.found_cards_for_account = True
        ws.card_record = parse_card_record(record_line)
        ws.info_msg = "   Displaying requested details"
    elif resp_cd == DFHRESP_NOTFND:
        ws.input_error = True
        ws.acctfilter_not_ok = True
        ws.cardfilter_not_ok = True
        if not ws.return_msg:
            ws.return_msg = "Did not find cards for this search condition"
    else:
        ws.input_error = True
        if not ws.return_msg:
            ws.acctfilter_not_ok = True
            ws.return_msg = f"File Error: READ on {LIT_CARDFILENAME.strip()} returned RESP {resp_cd}"


def read_data(ws: WorkingStorage):
    """Read data (9000-READ-DATA)."""
    getcard_byacctcard(ws)


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
    map_input: Optional[CardDetailMap] = None,
    prog_commarea: Optional[ThisProgCommarea] = None,
    card_file: str = "data/CARDDAT.dat"
) -> Tuple[CardDemoCommarea, ThisProgCommarea, CardDetailMap, Optional[str]]:
    """Main program entry point (0000-MAIN).
    
    Returns:
        Tuple of (commarea, prog_commarea, map_output, xctl_program)
    """
    # Initialize working storage
    ws = WorkingStorage()
    ws.input_ok = True
    ws.return_msg = ""
    ws.info_msg = ""
    ws.found_cards_for_account = False
    
    # Initialize commarea if not provided
    if commarea is None:
        commarea = CardDemoCommarea()
    
    # Initialize program commarea
    if prog_commarea is None:
        prog_commarea = ThisProgCommarea()
    
    # Initialize maps
    map_out = CardDetailMap()
    if map_input is None:
        map_input = CardDetailMap()
    
    # Open file
    ws.card_handler = CardFileHandler(card_file)
    ws.card_handler.open_file()
    
    # Initialize commarea if first call or from menu
    if eibcalen == 0 or (commarea.from_program == LIT_MENUPGM and not commarea.pgm_reenter):
        commarea = CardDemoCommarea()
        prog_commarea = ThisProgCommarea()
    
    # Check PF key validity
    ws.pfk_invalid = True
    if eibaid in (DFHENTER, DFHPF3):
        ws.pfk_valid = True
        ws.pfk_invalid = False
    
    if ws.pfk_invalid:
        eibaid = DFHENTER
    
    xctl_prog = None
    
    # Main logic
    if eibaid == DFHPF3:
        # PF3 - Exit to calling program or menu
        if not commarea.from_tranid or commarea.from_tranid.strip() == "":
            commarea.to_tranid = LIT_MENUTRANID
        else:
            commarea.to_tranid = commarea.from_tranid
        
        if not commarea.from_program or commarea.from_program.strip() == "":
            commarea.to_program = LIT_MENUPGM
        else:
            commarea.to_program = commarea.from_program
        
        commarea.from_tranid = LIT_THISTRANID
        commarea.from_program = LIT_THISPGM
        commarea.usrtyp_user = True
        commarea.pgm_enter = True
        commarea.last_mapset = LIT_THISMAPSET.rstrip()
        commarea.last_map = LIT_THISMAP
        commarea.error_msg = "PF03 pressed.Exiting              "
        return commarea, prog_commarea, map_out, commarea.to_program
    
    elif commarea.pgm_enter and commarea.from_program == LIT_CCLISTPGM:
        # Coming from credit card list screen - criteria already validated
        ws.input_ok = True
        ws.cc_acct_id_n = commarea.acct_id
        ws.cc_card_num_n = commarea.card_num
        ws.cc_acct_id = commarea.acct_id
        ws.cc_card_num = commarea.card_num
        
        read_data(ws)
        send_map(ws, map_out, commarea, eibcalen)
        return commarea, prog_commarea, map_out, None
    
    elif commarea.pgm_enter:
        # Coming from some other context - show empty screen
        send_map(ws, map_out, commarea, eibcalen)
        return commarea, prog_commarea, map_out, None
    
    elif commarea.pgm_reenter:
        # Reenter - process inputs
        process_inputs(ws, map_input, commarea)
        
        if ws.input_error:
            send_map(ws, map_out, commarea, eibcalen)
            return commarea, prog_commarea, map_out, None
        else:
            read_data(ws)
            send_map(ws, map_out, commarea, eibcalen)
            return commarea, prog_commarea, map_out, None
    
    else:
        # Unexpected scenario
        ws.return_msg = "UNEXPECTED DATA SCENARIO"
        commarea.error_msg = ws.return_msg
        send_map(ws, map_out, commarea, eibcalen)
        return commarea, prog_commarea, map_out, None
    
    commarea.error_msg = ws.return_msg
    return commarea, prog_commarea, map_out, None

