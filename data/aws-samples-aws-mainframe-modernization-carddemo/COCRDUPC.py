"""
Python Translation of COCRDUPC.cbl

Program: COCRDUPC
Layer: Business logic
Function: Accept and process credit card detail update request

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
from COCRDSLC import CardRecord, parse_card_record, format_card_record


# ============================================================================
# Constants and Literals
# ============================================================================

LIT_THISPGM = "COCRDUPC"
LIT_THISTRANID = "CCUP"
LIT_THISMAPSET = "COCRDUP "
LIT_THISMAP = "CCRDUPA"
LIT_CCLISTPGM = "COCRDLIC"
LIT_CCLISTTRANID = "CCLI"
LIT_CCLISTMAPSET = "COCRDLI"
LIT_MENUPGM = "COMEN01C"
LIT_MENUTRANID = "CM00"
LIT_CARDFILENAME = "CARDDAT "
LIT_CARDFILENAME_ACCT_PATH = "CARDAIX "

LIT_ALL_ALPHA_FROM = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
LIT_ALL_SPACES_TO = " " * 52
LIT_UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
LIT_LOWER = "abcdefghijklmnopqrstuvwxyz"

DFHENTER = "DFHENTER"
DFHPF3 = "DFHPF3"
DFHPF5 = "DFHPF5"
DFHPF12 = "DFHPF12"
DFHRED = "DFHRED"
DFHBMPRF = "DFHBMPRF"
DFHBMFSE = "DFHBMFSE"
DFHBMDAR = "DFHBMDAR"
DFHNEUTR = "DFHNEUTR"
DFHDFCOL = "DFHDFCOL"
DFHBMBRY = "DFHBMBRY"

# CICS Response Codes
DFHRESP_NORMAL = 0
DFHRESP_NOTFND = 12

# Default titles
CCDA_TITLE01 = "CardDemo Application"
CCDA_TITLE02 = "Credit Card Update"


# ============================================================================
# Program-Specific Commarea
# ============================================================================

@dataclass
class ThisProgCommarea:
    """Program-specific commarea (WS-THIS-PROGCOMMAREA)."""
    # Change action flag
    change_action: str = ""  # 'S' = show details, 'E'/'N'/'C'/'L'/'F' = changes made
    # Old details
    old_acctid: str = ""
    old_cardid: str = ""
    old_cvv_cd: str = ""
    old_crdname: str = ""
    old_expyear: str = ""
    old_expmon: str = ""
    old_expday: str = ""
    old_crdstcd: str = ""
    # New details
    new_acctid: str = ""
    new_cardid: str = ""
    new_cvv_cd: str = ""
    new_crdname: str = ""
    new_expyear: str = ""
    new_expmon: str = ""
    new_expday: str = ""
    new_crdstcd: str = ""
    
    @property
    def details_not_fetched(self) -> bool:
        """Check if details not fetched."""
        return not self.change_action or self.change_action == ""
    
    @property
    def show_details(self) -> bool:
        """Check if show details."""
        return self.change_action == "S"
    
    @property
    def changes_made(self) -> bool:
        """Check if changes made."""
        return self.change_action in ("E", "N", "C", "L", "F")
    
    @property
    def changes_not_ok(self) -> bool:
        """Check if changes not OK."""
        return self.change_action == "E"
    
    @property
    def changes_ok_not_confirmed(self) -> bool:
        """Check if changes OK not confirmed."""
        return self.change_action == "N"
    
    @property
    def changes_okayed_and_done(self) -> bool:
        """Check if changes okayed and done."""
        return self.change_action == "C"
    
    @property
    def changes_failed(self) -> bool:
        """Check if changes failed."""
        return self.change_action in ("L", "F")
    
    @property
    def changes_okayed_lock_error(self) -> bool:
        """Check if lock error."""
        return self.change_action == "L"
    
    @property
    def changes_okayed_but_failed(self) -> bool:
        """Check if update failed."""
        return self.change_action == "F"


# ============================================================================
# Map Structure (CCRDUPAO/CCRDUPAI)
# ============================================================================

@dataclass
class CardUpdateMap:
    """Card update map structure."""
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
    
    # Card detail fields
    crdnamei: str = ""  # Card name input
    crdnameo: str = ""  # Card name output
    crdnamea: str = ""  # Card name attribute
    crdnamel: int = 0  # Card name length (cursor)
    crdnamec: str = ""  # Card name color
    
    crdstcdi: str = ""  # Card status input
    crdstcdo: str = ""  # Card status output
    crdstcda: str = ""  # Card status attribute
    crdstcdl: int = 0  # Card status length (cursor)
    crdstcdc: str = ""  # Card status color
    
    expdayi: str = ""  # Expiry day input
    expdayo: str = ""  # Expiry day output
    expdaya: str = ""  # Expiry day attribute
    expdayc: str = ""  # Expiry day color
    
    expmoni: str = ""  # Expiry month input
    expmono: str = ""  # Expiry month output
    expmona: str = ""  # Expiry month attribute
    expmonl: int = 0  # Expiry month length (cursor)
    expmonc: str = ""  # Expiry month color
    
    expyeari: str = ""  # Expiry year input
    expyearo: str = ""  # Expiry year output
    expyeara: str = ""  # Expiry year attribute
    expyearl: int = 0  # Expiry year length (cursor)
    expyearc: str = ""  # Expiry year color
    
    # Message fields
    errmsgo: str = ""
    infomsgo: str = ""
    infomsga: str = ""
    fkeysca: str = ""  # Function keys color


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
        
        self.cardname_not_ok: bool = False
        self.cardname_isvalid: bool = False
        self.cardname_blank: bool = True
        
        self.cardstatus_not_ok: bool = False
        self.cardstatus_isvalid: bool = False
        self.cardstatus_blank: bool = True
        
        self.cardexpmon_not_ok: bool = False
        self.cardexpmon_isvalid: bool = False
        self.cardexpmon_blank: bool = True
        
        self.cardexpyear_not_ok: bool = False
        self.cardexpyear_isvalid: bool = False
        self.cardexpyear_blank: bool = True
        
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
        self.no_changes_detected: bool = False
        
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
        
        # Validation helpers
        self.card_name_check: str = ""
        self.flg_yes_no_check: str = "N"
        self.card_month_check: str = ""
        self.card_year_check: str = ""


# ============================================================================
# Card File Handler
# ============================================================================

class CardFileHandler:
    """Handler for CARDDAT file with READ UPDATE and REWRITE support."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.file_status = "00"
        self._data: dict = {}
        self._locked_records: dict = {}  # Track locked records for update
    
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
    
    def read_for_update(self, key: str) -> Tuple[Optional[str], int]:
        """Read record for update (simulates CICS READ UPDATE)."""
        try:
            if key in self._data:
                # Lock the record (store original for change detection)
                self._locked_records[key] = self._data[key]
                self.file_status = "00"
                return self._data[key], 0
            else:
                self.file_status = "23"
                return None, 12
        except Exception as e:
            self.file_status = "99"
            return None, 12
    
    def rewrite_record(self, key: str, record: str) -> int:
        """Rewrite record (simulates CICS REWRITE)."""
        try:
            if key in self._data and key in self._locked_records:
                # Check if record was changed (optimistic locking)
                if self._data[key] != self._locked_records[key]:
                    # Record was changed by someone else
                    self.file_status = "99"
                    return 12
                # Update the record
                self._data[key] = record
                # Release lock
                del self._locked_records[key]
                self.file_status = "00"
                return 0
            else:
                self.file_status = "23"
                return 12
        except Exception as e:
            self.file_status = "99"
            return 12
    
    def close_file(self) -> int:
        """Close file and persist data."""
        try:
            with open(self.filename, "w", encoding='utf-8') as f:
                for key in sorted(self._data.keys()):
                    f.write(self._data[key] + '\n')
            self.file_status = "00"
            return 0
        except Exception as e:
            self.file_status = "99"
            return 12


# ============================================================================
# Map Functions
# ============================================================================

def populate_header_info(ws: WorkingStorage, map_out: CardUpdateMap):
    """Populate header information (3100-SCREEN-INIT)."""
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


def setup_screen_vars(ws: WorkingStorage, map_out: CardUpdateMap, commarea: CardDemoCommarea, prog_commarea: ThisProgCommarea):
    """Setup screen variables (3200-SETUP-SCREEN-VARS)."""
    if commarea.pgm_enter and not commarea.pgm_reenter:
        # First entry - skip
        pass
    else:
        # Set account ID output
        if ws.cc_acct_id_n == "0" or not ws.cc_acct_id_n:
            map_out.acctsido = ""
        else:
            map_out.acctsido = ws.cc_acct_id if ws.cc_acct_id else commarea.acct_id
        
        # Set card number output
        if ws.cc_card_num_n == "0" or not ws.cc_card_num_n:
            map_out.cardsido = ""
        else:
            map_out.cardsido = ws.cc_card_num if ws.cc_card_num else commarea.card_num
        
        # Set card detail fields based on state
        if prog_commarea.details_not_fetched:
            map_out.crdnameo = ""
            map_out.crdstcdo = ""
            map_out.expdayo = ""
            map_out.expmono = ""
            map_out.expyearo = ""
        elif prog_commarea.show_details:
            map_out.crdnameo = prog_commarea.old_crdname
            map_out.crdstcdo = prog_commarea.old_crdstcd
            map_out.expdayo = prog_commarea.old_expday
            map_out.expmono = prog_commarea.old_expmon
            map_out.expyearo = prog_commarea.old_expyear
        elif prog_commarea.changes_made:
            map_out.crdnameo = prog_commarea.new_crdname
            map_out.crdstcdo = prog_commarea.new_crdstcd
            map_out.expmono = prog_commarea.new_expmon
            map_out.expyearo = prog_commarea.new_expyear
            map_out.expdayo = prog_commarea.old_expday  # Day not editable
        else:
            map_out.crdnameo = prog_commarea.old_crdname
            map_out.crdstcdo = prog_commarea.old_crdstcd
            map_out.expdayo = prog_commarea.old_expday
            map_out.expmono = prog_commarea.old_expmon
            map_out.expyearo = prog_commarea.old_expyear


def setup_infomsg(ws: WorkingStorage, commarea: CardDemoCommarea, prog_commarea: ThisProgCommarea):
    """Setup info message (3250-SETUP-INFOMSG)."""
    # Check states in order of priority
    if prog_commarea.changes_okayed_and_done:
        ws.info_msg = "Changes committed to database"
    elif prog_commarea.changes_failed:
        ws.info_msg = "Changes unsuccessful. Please try again"
    elif prog_commarea.changes_ok_not_confirmed:
        ws.info_msg = "Changes validated.Press F5 to save"
    elif prog_commarea.changes_not_ok:
        ws.info_msg = "Update card details presented above."
    elif prog_commarea.show_details:
        ws.info_msg = "Details of selected card shown above"
    elif prog_commarea.details_not_fetched or (commarea.pgm_enter and not commarea.pgm_reenter):
        ws.info_msg = "Please enter Account and Card Number"
    else:
        ws.info_msg = "Please enter Account and Card Number"
    
    # Set map output
    if not ws.info_msg:
        ws.info_msg = "Please enter Account and Card Number"


def setup_screen_attrs(ws: WorkingStorage, map_out: CardUpdateMap, commarea: CardDemoCommarea, prog_commarea: ThisProgCommarea):
    """Setup screen attributes (3300-SETUP-SCREEN-ATTRS)."""
    # Protect or unprotect based on context
    if prog_commarea.details_not_fetched:
        map_out.acctsida = DFHBMFSE
        map_out.cardsida = DFHBMFSE
        map_out.crdnamea = DFHBMPRF
        map_out.crdstcda = DFHBMPRF
        map_out.expmona = DFHBMPRF
        map_out.expyeara = DFHBMPRF
    elif prog_commarea.show_details or prog_commarea.changes_not_ok:
        map_out.acctsida = DFHBMPRF
        map_out.cardsida = DFHBMPRF
        map_out.crdnamea = DFHBMFSE
        map_out.crdstcda = DFHBMFSE
        map_out.expmona = DFHBMFSE
        map_out.expyeara = DFHBMFSE
    elif prog_commarea.changes_ok_not_confirmed or prog_commarea.changes_okayed_and_done:
        map_out.acctsida = DFHBMPRF
        map_out.cardsida = DFHBMPRF
        map_out.crdnamea = DFHBMPRF
        map_out.crdstcda = DFHBMPRF
        map_out.expmona = DFHBMPRF
        map_out.expyeara = DFHBMPRF
    else:
        map_out.acctsida = DFHBMFSE
        map_out.cardsida = DFHBMFSE
        map_out.crdnamea = DFHBMPRF
        map_out.crdstcda = DFHBMPRF
        map_out.expmona = DFHBMPRF
        map_out.expyeara = DFHBMPRF
    
    # Position cursor
    if ws.found_cards_for_account or ws.no_changes_detected:
        map_out.crdnamel = -1
    elif ws.acctfilter_not_ok or ws.acctfilter_blank:
        map_out.acctsidl = -1
    elif ws.cardfilter_not_ok or ws.cardfilter_blank:
        map_out.cardsidl = -1
    elif ws.cardname_not_ok or ws.cardname_blank:
        map_out.crdnamel = -1
    elif ws.cardstatus_not_ok or ws.cardstatus_blank:
        map_out.crdstcdl = -1
    elif ws.cardexpmon_not_ok or ws.cardexpmon_blank:
        map_out.expmonl = -1
    elif ws.cardexpyear_not_ok or ws.cardexpyear_blank:
        map_out.expyearl = -1
    else:
        map_out.acctsidl = -1
    
    # Setup color
    if commarea.last_mapset == LIT_CCLISTMAPSET:
        map_out.acctsidc = DFHDFCOL
        map_out.cardsidc = DFHDFCOL
    
    if ws.acctfilter_not_ok:
        map_out.acctsidc = DFHRED
    
    if ws.acctfilter_blank and commarea.pgm_reenter:
        map_out.acctsido = "*"
        map_out.acctsidc = DFHRED
    
    if ws.cardfilter_not_ok:
        map_out.cardsidc = DFHRED
    
    if ws.cardfilter_blank and commarea.pgm_reenter:
        map_out.cardsido = "*"
        map_out.cardsidc = DFHRED
    
    if ws.cardname_not_ok and prog_commarea.changes_not_ok:
        map_out.crdnamec = DFHRED
    
    if ws.cardname_blank and prog_commarea.changes_not_ok:
        map_out.crdnameo = "*"
        map_out.crdnamec = DFHRED
    
    if ws.cardstatus_not_ok and prog_commarea.changes_not_ok:
        map_out.crdstcdc = DFHRED
    
    if ws.cardstatus_blank and prog_commarea.changes_not_ok:
        map_out.crdstcdo = "*"
        map_out.crdstcdc = DFHRED
    
    map_out.expdayc = DFHBMDAR
    
    if ws.cardexpmon_not_ok and prog_commarea.changes_not_ok:
        map_out.expmonc = DFHRED
    
    if ws.cardexpmon_blank and prog_commarea.changes_not_ok:
        map_out.expmono = "*"
        map_out.expmonc = DFHRED
    
    if ws.cardexpyear_not_ok and prog_commarea.changes_not_ok:
        map_out.expyearc = DFHRED
    
    if ws.cardexpyear_blank and prog_commarea.changes_not_ok:
        map_out.expyearo = "*"
        map_out.expyearc = DFHRED
    
    # Info message attribute
    if not ws.info_msg or ws.info_msg == "Please enter Account and Card Number":
        map_out.infomsga = DFHBMDAR
    else:
        map_out.infomsga = DFHBMBRY
    
    if prog_commarea.changes_ok_not_confirmed:
        map_out.fkeysca = DFHBMBRY


def send_map(ws: WorkingStorage, map_out: CardUpdateMap, commarea: CardDemoCommarea, prog_commarea: ThisProgCommarea):
    """Send map (3000-SEND-MAP)."""
    populate_header_info(ws, map_out)
    setup_screen_vars(ws, map_out, commarea, prog_commarea)
    setup_infomsg(ws, commarea, prog_commarea)
    setup_screen_attrs(ws, map_out, commarea, prog_commarea)
    # Set messages in map
    map_out.errmsgo = ws.return_msg
    map_out.infomsgo = ws.info_msg


def receive_map(ws: WorkingStorage, map_in: CardUpdateMap, prog_commarea: ThisProgCommarea):
    """Receive map (1100-RECEIVE-MAP)."""
    ws.resp_cd = 0
    ws.reas_cd = 0
    
    # Initialize new details
    prog_commarea.new_acctid = ""
    prog_commarea.new_cardid = ""
    prog_commarea.new_cvv_cd = ""
    prog_commarea.new_crdname = ""
    prog_commarea.new_crdstcd = ""
    prog_commarea.new_expday = ""
    prog_commarea.new_expmon = ""
    prog_commarea.new_expyear = ""
    
    # Extract input fields
    acct_id = map_in.acctsidi.strip()
    card_num = map_in.cardsidi.strip()
    card_name = map_in.crdnamei.strip()
    card_status = map_in.crdstcdi.strip()
    exp_day = map_in.expdayi.strip()
    exp_mon = map_in.expmoni.strip()
    exp_year = map_in.expyeari.strip()
    
    # Replace * with empty
    if acct_id == "*" or not acct_id:
        ws.cc_acct_id = ""
        prog_commarea.new_acctid = ""
    else:
        ws.cc_acct_id = acct_id
        prog_commarea.new_acctid = acct_id
    
    if card_num == "*" or not card_num:
        ws.cc_card_num = ""
        prog_commarea.new_cardid = ""
    else:
        ws.cc_card_num = card_num
        prog_commarea.new_cardid = card_num
    
    if card_name == "*" or not card_name:
        prog_commarea.new_crdname = ""
    else:
        prog_commarea.new_crdname = card_name
    
    if card_status == "*" or not card_status:
        prog_commarea.new_crdstcd = ""
    else:
        prog_commarea.new_crdstcd = card_status
    
    prog_commarea.new_expday = exp_day
    
    if exp_mon == "*" or not exp_mon:
        prog_commarea.new_expmon = ""
    else:
        prog_commarea.new_expmon = exp_mon
    
    if exp_year == "*" or not exp_year:
        prog_commarea.new_expyear = ""
    else:
        prog_commarea.new_expyear = exp_year


# ============================================================================
# Edit Functions
# ============================================================================

def edit_account(ws: WorkingStorage, commarea: CardDemoCommarea, prog_commarea: ThisProgCommarea):
    """Edit account filter (1210-EDIT-ACCOUNT)."""
    ws.acctfilter_not_ok = True
    
    acct_id = ws.cc_acct_id.strip()
    if not acct_id or acct_id == "" or acct_id == "0" or acct_id == "00000000000":
        ws.input_error = True
        ws.acctfilter_blank = True
        if not ws.return_msg:
            ws.return_msg = "Account number not provided"
        commarea.acct_id = ""
        prog_commarea.new_acctid = ""
        return
    
    # Check if numeric
    if not acct_id.isdigit():
        ws.input_error = True
        ws.acctfilter_not_ok = True
        if not ws.return_msg:
            ws.return_msg = "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"
        commarea.acct_id = ""
        prog_commarea.new_acctid = ""
        return
    
    # Valid
    commarea.acct_id = acct_id
    prog_commarea.new_acctid = acct_id
    ws.cc_acct_id_n = acct_id
    ws.acctfilter_isvalid = True
    ws.acctfilter_not_ok = False
    ws.acctfilter_blank = False


def edit_card(ws: WorkingStorage, commarea: CardDemoCommarea, prog_commarea: ThisProgCommarea):
    """Edit card filter (1220-EDIT-CARD)."""
    ws.cardfilter_not_ok = True
    
    card_num = ws.cc_card_num.strip()
    if not card_num or card_num == "" or card_num == "0" or card_num == "0000000000000000":
        ws.input_error = True
        ws.cardfilter_blank = True
        if not ws.return_msg:
            ws.return_msg = "Card number not provided"
        commarea.card_num = ""
        prog_commarea.new_cardid = ""
        return
    
    # Check if numeric
    if not card_num.isdigit():
        ws.input_error = True
        ws.cardfilter_not_ok = True
        if not ws.return_msg:
            ws.return_msg = "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"
        commarea.card_num = ""
        prog_commarea.new_cardid = ""
        return
    
    # Valid
    ws.cc_card_num_n = card_num
    commarea.card_num = card_num
    prog_commarea.new_cardid = card_num
    ws.cardfilter_isvalid = True
    ws.cardfilter_not_ok = False
    ws.cardfilter_blank = False


def edit_name(ws: WorkingStorage, prog_commarea: ThisProgCommarea):
    """Edit card name (1230-EDIT-NAME)."""
    ws.cardname_not_ok = True
    
    card_name = prog_commarea.new_crdname.strip()
    if not card_name or card_name == "" or card_name == "0":
        ws.input_error = True
        ws.cardname_blank = True
        if not ws.return_msg:
            ws.return_msg = "Card name not provided"
        return
    
    # Check if only alphabetic and spaces
    card_name_check = card_name
    # Convert all alphabetic to spaces
    for char in LIT_ALL_ALPHA_FROM:
        card_name_check = card_name_check.replace(char, " ")
    
    # If only spaces remain, it's valid
    if card_name_check.strip() == "":
        ws.cardname_isvalid = True
        ws.cardname_not_ok = False
        ws.cardname_blank = False
    else:
        ws.input_error = True
        ws.cardname_not_ok = True
        if not ws.return_msg:
            ws.return_msg = "Card name can only contain alphabets and spaces"


def edit_cardstatus(ws: WorkingStorage, prog_commarea: ThisProgCommarea):
    """Edit card status (1240-EDIT-CARDSTATUS)."""
    ws.cardstatus_not_ok = True
    
    card_status = prog_commarea.new_crdstcd.strip().upper()
    if not card_status or card_status == "" or card_status == "0":
        ws.input_error = True
        ws.cardstatus_blank = True
        if not ws.return_msg:
            ws.return_msg = "Card Active Status must be Y or N"
        return
    
    # Check if Y or N
    if card_status in ("Y", "N"):
        prog_commarea.new_crdstcd = card_status
        ws.cardstatus_isvalid = True
        ws.cardstatus_not_ok = False
        ws.cardstatus_blank = False
    else:
        ws.input_error = True
        ws.cardstatus_not_ok = True
        if not ws.return_msg:
            ws.return_msg = "Card Active Status must be Y or N"


def edit_expiry_mon(ws: WorkingStorage, prog_commarea: ThisProgCommarea):
    """Edit expiry month (1250-EDIT-EXPIRY-MON)."""
    ws.cardexpmon_not_ok = True
    
    exp_mon = prog_commarea.new_expmon.strip()
    if not exp_mon or exp_mon == "" or exp_mon == "0":
        ws.input_error = True
        ws.cardexpmon_blank = True
        if not ws.return_msg:
            ws.return_msg = "Card expiry month must be between 1 and 12"
        return
    
    # Check if numeric and between 1-12
    if exp_mon.isdigit():
        mon_num = int(exp_mon)
        if 1 <= mon_num <= 12:
            ws.cardexpmon_isvalid = True
            ws.cardexpmon_not_ok = False
            ws.cardexpmon_blank = False
        else:
            ws.input_error = True
            ws.cardexpmon_not_ok = True
            if not ws.return_msg:
                ws.return_msg = "Card expiry month must be between 1 and 12"
    else:
        ws.input_error = True
        ws.cardexpmon_not_ok = True
        if not ws.return_msg:
            ws.return_msg = "Card expiry month must be between 1 and 12"


def edit_expiry_year(ws: WorkingStorage, prog_commarea: ThisProgCommarea):
    """Edit expiry year (1260-EDIT-EXPIRY-YEAR)."""
    ws.cardexpyear_not_ok = True
    
    exp_year = prog_commarea.new_expyear.strip()
    if not exp_year or exp_year == "" or exp_year == "0":
        ws.input_error = True
        ws.cardexpyear_blank = True
        if not ws.return_msg:
            ws.return_msg = "Invalid card expiry year"
        return
    
    # Check if numeric and between 1950-2099
    if exp_year.isdigit():
        year_num = int(exp_year)
        if 1950 <= year_num <= 2099:
            ws.cardexpyear_isvalid = True
            ws.cardexpyear_not_ok = False
            ws.cardexpyear_blank = False
        else:
            ws.input_error = True
            ws.cardexpyear_not_ok = True
            if not ws.return_msg:
                ws.return_msg = "Invalid card expiry year"
    else:
        ws.input_error = True
        ws.cardexpyear_not_ok = True
        if not ws.return_msg:
            ws.return_msg = "Invalid card expiry year"


def edit_map_inputs(ws: WorkingStorage, commarea: CardDemoCommarea, prog_commarea: ThisProgCommarea):
    """Edit map inputs (1200-EDIT-MAP-INPUTS)."""
    ws.input_ok = True
    
    if prog_commarea.details_not_fetched:
        # Validate search keys
        edit_account(ws, commarea, prog_commarea)
        edit_card(ws, commarea, prog_commarea)
        
        # Cross field edits
        if ws.acctfilter_blank and ws.cardfilter_blank:
            ws.input_error = True
            ws.return_msg = "No input received"
        return
    
    # Search keys already validated and data fetched
    ws.found_cards_for_account = True
    ws.acctfilter_isvalid = True
    ws.cardfilter_isvalid = True
    commarea.acct_id = prog_commarea.old_acctid
    commarea.card_num = prog_commarea.old_cardid
    
    # Check if no changes detected
    old_carddata = (
        prog_commarea.old_crdname.upper() +
        prog_commarea.old_crdstcd.upper() +
        prog_commarea.old_expmon.upper() +
        prog_commarea.old_expyear.upper()
    )
    new_carddata = (
        prog_commarea.new_crdname.upper() +
        prog_commarea.new_crdstcd.upper() +
        prog_commarea.new_expmon.upper() +
        prog_commarea.new_expyear.upper()
    )
    
    if old_carddata == new_carddata:
        ws.no_changes_detected = True
        ws.cardname_isvalid = True
        ws.cardstatus_isvalid = True
        ws.cardexpmon_isvalid = True
        ws.cardexpyear_isvalid = True
        return
    
    # Changes detected - validate
    ws.no_changes_detected = False
    prog_commarea.change_action = "E"  # Changes not OK initially
    
    edit_name(ws, prog_commarea)
    edit_cardstatus(ws, prog_commarea)
    edit_expiry_mon(ws, prog_commarea)
    edit_expiry_year(ws, prog_commarea)
    
    if not ws.input_error:
        prog_commarea.change_action = "N"  # Changes OK not confirmed


def process_inputs(ws: WorkingStorage, map_in: CardUpdateMap, commarea: CardDemoCommarea, prog_commarea: ThisProgCommarea):
    """Process inputs (1000-PROCESS-INPUTS)."""
    receive_map(ws, map_in, prog_commarea)
    edit_map_inputs(ws, commarea, prog_commarea)
    
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


def read_data(ws: WorkingStorage, prog_commarea: ThisProgCommarea):
    """Read data (9000-READ-DATA)."""
    # Initialize old details
    prog_commarea.old_acctid = ws.cc_acct_id
    prog_commarea.old_cardid = ws.cc_card_num
    
    getcard_byacctcard(ws)
    
    if ws.found_cards_for_account and ws.card_record:
        prog_commarea.old_cvv_cd = ""  # CVV not in CardRecord
        prog_commarea.old_crdname = ws.card_record.card_embossed_name.upper()
        if len(ws.card_record.card_expiration_date) >= 10:
            prog_commarea.old_expyear = ws.card_record.card_expiration_date[:4]
            prog_commarea.old_expmon = ws.card_record.card_expiration_date[5:7]
            prog_commarea.old_expday = ws.card_record.card_expiration_date[8:10]
        else:
            prog_commarea.old_expyear = ""
            prog_commarea.old_expmon = ""
            prog_commarea.old_expday = ""
        prog_commarea.old_crdstcd = ws.card_record.card_active_status


# ============================================================================
# Write Processing
# ============================================================================

def check_change_in_record(ws: WorkingStorage, prog_commarea: ThisProgCommarea, current_record: CardRecord) -> bool:
    """Check if record was changed (9300-CHECK-CHANGE-IN-REC)."""
    # Compare old values with current record
    if (current_record.card_embossed_name.upper() != prog_commarea.old_crdname or
        current_record.card_active_status != prog_commarea.old_crdstcd or
        (len(current_record.card_expiration_date) >= 10 and
         current_record.card_expiration_date[:4] != prog_commarea.old_expyear) or
        (len(current_record.card_expiration_date) >= 7 and
         current_record.card_expiration_date[5:7] != prog_commarea.old_expmon) or
        (len(current_record.card_expiration_date) >= 10 and
         current_record.card_expiration_date[8:10] != prog_commarea.old_expday)):
        # Update old details with current
        prog_commarea.old_crdname = current_record.card_embossed_name.upper()
        prog_commarea.old_crdstcd = current_record.card_active_status
        if len(current_record.card_expiration_date) >= 10:
            prog_commarea.old_expyear = current_record.card_expiration_date[:4]
            prog_commarea.old_expmon = current_record.card_expiration_date[5:7]
            prog_commarea.old_expday = current_record.card_expiration_date[8:10]
        return True
    return False


def write_processing(ws: WorkingStorage, commarea: CardDemoCommarea, prog_commarea: ThisProgCommarea):
    """Write processing (9200-WRITE-PROCESSING)."""
    ws.card_rid_cardnum = ws.cc_card_num
    
    if not ws.card_handler:
        ws.input_error = True
        ws.return_msg = "Could not lock record for update"
        prog_commarea.change_action = "L"
        return
    
    # Read for update (lock)
    record_line, resp_cd = ws.card_handler.read_for_update(ws.card_rid_cardnum)
    ws.resp_cd = resp_cd
    
    if resp_cd != DFHRESP_NORMAL:
        ws.input_error = True
        if not ws.return_msg:
            ws.return_msg = "Could not lock record for update"
        prog_commarea.change_action = "L"
        return
    
    # Check if record was changed
    current_record = parse_card_record(record_line)
    if check_change_in_record(ws, prog_commarea, current_record):
        ws.return_msg = "Record changed by some one else. Please review"
        prog_commarea.change_action = "S"  # Show details again
        return
    
    # Prepare update record
    new_record = CardRecord(
        card_num=prog_commarea.new_cardid,
        card_acct_id=ws.cc_acct_id_n,
        card_active_status=prog_commarea.new_crdstcd,
        card_embossed_name=prog_commarea.new_crdname,
        card_expiration_date=f"{prog_commarea.new_expyear}-{prog_commarea.new_expmon}-{prog_commarea.new_expday}"
    )
    
    new_record_line = format_card_record(new_record)
    
    # Rewrite record
    resp_cd = ws.card_handler.rewrite_record(ws.card_rid_cardnum, new_record_line)
    ws.resp_cd = resp_cd
    
    if resp_cd == DFHRESP_NORMAL:
        # Close file to persist changes
        ws.card_handler.close_file()
        prog_commarea.change_action = "C"  # Changes okayed and done
        ws.info_msg = "Changes committed to database"
    else:
        ws.return_msg = "Update of record failed"
        prog_commarea.change_action = "F"  # Changes okayed but failed


# ============================================================================
# Decide Action
# ============================================================================

def decide_action(ws: WorkingStorage, commarea: CardDemoCommarea, prog_commarea: ThisProgCommarea, eibaid: str):
    """Decide action (2000-DECIDE-ACTION)."""
    if prog_commarea.details_not_fetched:
        # Get details and setup detail edit screen
        if ws.acctfilter_isvalid and ws.cardfilter_isvalid:
            read_data(ws, prog_commarea)
            if ws.found_cards_for_account:
                prog_commarea.change_action = "S"
    
    elif eibaid == DFHPF12:
        # User cancels - reset to show details
        if ws.acctfilter_isvalid and ws.cardfilter_isvalid:
            read_data(ws, prog_commarea)
            if ws.found_cards_for_account:
                prog_commarea.change_action = "S"
    
    elif prog_commarea.show_details:
        # Details shown - check changes and ask confirmation if good
        if ws.input_error or ws.no_changes_detected:
            pass
        else:
            prog_commarea.change_action = "N"  # Changes OK not confirmed
    
    elif prog_commarea.changes_ok_not_confirmed and eibaid == DFHPF5:
        # Confirmation given - save changes
        write_processing(ws, commarea, prog_commarea)
    
    elif prog_commarea.changes_okayed_and_done:
        # Show confirmation and reset
        prog_commarea.change_action = "S"
        if not commarea.from_tranid or commarea.from_tranid.strip() == "":
            commarea.acct_id = ""
            commarea.card_num = ""


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
    map_input: Optional[CardUpdateMap] = None,
    prog_commarea: Optional[ThisProgCommarea] = None,
    card_file: str = "data/CARDDAT.dat"
) -> Tuple[CardDemoCommarea, ThisProgCommarea, CardUpdateMap, Optional[str]]:
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
    ws.no_changes_detected = False
    
    # Initialize commarea if not provided
    if commarea is None:
        commarea = CardDemoCommarea()
    
    # Initialize program commarea
    if prog_commarea is None:
        prog_commarea = ThisProgCommarea()
    
    # Initialize maps
    map_out = CardUpdateMap()
    if map_input is None:
        map_input = CardUpdateMap()
    
    # Open file
    ws.card_handler = CardFileHandler(card_file)
    ws.card_handler.open_file()
    
    # Initialize commarea if first call or from menu
    if eibcalen == 0 or (commarea.from_program == LIT_MENUPGM and not commarea.pgm_reenter):
        commarea = CardDemoCommarea()
        prog_commarea = ThisProgCommarea()
        commarea.pgm_enter = True
        prog_commarea.change_action = ""
    
    # Check PF key validity
    ws.pfk_invalid = True
    if (eibaid in (DFHENTER, DFHPF3) or
        (eibaid == DFHPF5 and prog_commarea.changes_ok_not_confirmed) or
        (eibaid == DFHPF12 and not prog_commarea.details_not_fetched)):
        ws.pfk_valid = True
        ws.pfk_invalid = False
    
    if ws.pfk_invalid:
        eibaid = DFHENTER
    
    xctl_prog = None
    
    # Main logic
    if (eibaid == DFHPF3 or
        (prog_commarea.changes_okayed_and_done and commarea.last_mapset == LIT_CCLISTMAPSET) or
        (prog_commarea.changes_failed and commarea.last_mapset == LIT_CCLISTMAPSET)):
        # PF3 or done - exit
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
        
        if commarea.last_mapset == LIT_CCLISTMAPSET:
            commarea.acct_id = ""
            commarea.card_num = ""
        
        commarea.usrtyp_user = True
        commarea.pgm_enter = True
        commarea.last_mapset = LIT_THISMAPSET.rstrip()
        commarea.last_map = LIT_THISMAP
        commarea.error_msg = "PF03 pressed.Exiting              "
        return commarea, prog_commarea, map_out, commarea.to_program
    
    elif (commarea.pgm_enter and commarea.from_program == LIT_CCLISTPGM) or (eibaid == DFHPF12 and commarea.from_program == LIT_CCLISTPGM):
        # Coming from card list or PF12 from card list
        commarea.pgm_reenter = True
        ws.input_ok = True
        ws.acctfilter_isvalid = True
        ws.cardfilter_isvalid = True
        ws.cc_acct_id_n = commarea.acct_id
        ws.cc_card_num_n = commarea.card_num
        ws.cc_acct_id = commarea.acct_id
        ws.cc_card_num = commarea.card_num
        
        read_data(ws, prog_commarea)
        if ws.found_cards_for_account:
            prog_commarea.change_action = "S"
        send_map(ws, map_out, commarea, prog_commarea)
        return commarea, prog_commarea, map_out, None
    
    elif prog_commarea.details_not_fetched and commarea.pgm_enter:
        # Fresh entry - ask for keys
        send_map(ws, map_out, commarea, prog_commarea)
        commarea.pgm_reenter = True
        prog_commarea.change_action = ""
        return commarea, prog_commarea, map_out, None
    
    elif prog_commarea.changes_okayed_and_done or prog_commarea.changes_failed:
        # Reset and ask for fresh search
        prog_commarea = ThisProgCommarea()
        commarea.acct_id = ""
        commarea.card_num = ""
        commarea.pgm_enter = True
        send_map(ws, map_out, commarea, prog_commarea)
        commarea.pgm_reenter = True
        prog_commarea.change_action = ""
        return commarea, prog_commarea, map_out, None
    
    else:
        # Process inputs and decide action
        process_inputs(ws, map_input, commarea, prog_commarea)
        decide_action(ws, commarea, prog_commarea, eibaid)
        send_map(ws, map_out, commarea, prog_commarea)
        return commarea, prog_commarea, map_out, None
    
    commarea.error_msg = ws.return_msg
    return commarea, prog_commarea, map_out, None

