"""
Python Translation of COCRDLIC.cbl

Program: COCRDLIC
Layer: Business logic
Function: List Credit Cards
- All cards if no context passed and admin user
- Only ones associated with ACCT in COMMAREA if user is not admin

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, List, Tuple
from decimal import Decimal

# Import shared structures
from COACTVWC import CardDemoCommarea


# ============================================================================
# Constants and Literals
# ============================================================================

LIT_THISPGM = "COCRDLIC"
LIT_THISTRANID = "CCLI"
LIT_THISMAPSET = "COCRDLI"
LIT_THISMAP = "CCRDLIA"
LIT_MENUPGM = "COMEN01C"
LIT_MENUTRANID = "CM00"
LIT_CARDDTLPGM = "COCRDSLC"
LIT_CARDDTLTRANID = "CCDL"
LIT_CARDUPDPGM = "COCRDUPC"
LIT_CARDUPDTRANID = "CCUP"
LIT_CARD_FILE = "CARDDAT "
LIT_CARD_FILE_ACCT_PATH = "CARDAIX "

WS_MAX_SCREEN_LINES = 7

DFHENTER = "DFHENTER"
DFHPF3 = "DFHPF3"
DFHPF7 = "DFHPF7"
DFHPF8 = "DFHPF8"
DFHRED = "DFHRED"
DFHBMPRF = "DFHBMPRF"
DFHBMPRO = "DFHBMPRO"
DFHBMFSE = "DFHBMFSE"
DFHBMDAR = "DFHBMDAR"
DFHNEUTR = "DFHNEUTR"

# CICS Response Codes
DFHRESP_NORMAL = 0
DFHRESP_NOTFND = 12
DFHRESP_ENDFILE = 16
DFHRESP_DUPREC = 15

# Default titles
CCDA_TITLE01 = "CardDemo Application"
CCDA_TITLE02 = "Credit Card List"
CCDA_MSG_INVALID_KEY = "Invalid key pressed. Please use Enter, PF3, PF7, or PF8."


# ============================================================================
# Card Record Structure
# ============================================================================

@dataclass
class CardRecord:
    """Card record (CARD-RECORD from COPY CVACT02Y)."""
    card_num: str = ""  # 16 chars
    card_acct_id: str = ""  # 11 chars
    card_active_status: str = ""  # 1 char
    # Additional fields from CVACT02Y would be here
    # For now, we'll use a simplified structure
    
    @property
    def card_num_prop(self) -> str:
        return self.card_num
    
    @property
    def card_acct_id_prop(self) -> str:
        return self.card_acct_id
    
    @property
    def card_active_status_prop(self) -> str:
        return self.card_active_status


def parse_card_record(line: str) -> CardRecord:
    """Parse card record from line.
    
    Based on CVACT02Y structure:
    - CARD-NUM: PIC X(16) - positions 0-15
    - CARD-ACCT-ID: PIC 9(11) - positions 16-26
    - CARD-ACTIVE-STATUS: PIC X(1) - position 27
    """
    if len(line) < 28:
        return CardRecord()
    
    card_num = line[0:16].strip() if len(line) >= 16 else ""
    card_acct_id = line[16:27].strip() if len(line) >= 27 else ""
    card_active_status = line[27:28] if len(line) >= 28 else ""
    
    return CardRecord(
        card_num=card_num,
        card_acct_id=card_acct_id,
        card_active_status=card_active_status
    )


def format_card_record(record: CardRecord) -> str:
    """Format card record for writing."""
    line = (
        f"{record.card_num:<16}"
        f"{record.card_acct_id:<11}"
        f"{record.card_active_status:<1}"
    )
    # Pad to appropriate length (at least 150 chars based on other records)
    return line.ljust(150)


# ============================================================================
# Map Structure (CCRDLIAO/CCRDLIAI)
# ============================================================================

@dataclass
class CardListMap:
    """Card list map structure."""
    # Header fields
    title01o: str = ""
    title02o: str = ""
    trnnameo: str = ""
    pgmnameo: str = ""
    curdateo: str = ""
    curtimeo: str = ""
    pagenoo: str = ""
    
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
    
    # Selection fields (7 rows)
    crdsel1i: str = ""  # Selection input row 1
    crdsel1o: str = ""  # Selection output row 1
    crdsel1a: str = ""  # Selection attribute row 1
    crdsel1l: int = 0  # Selection length row 1
    crdsel1c: str = ""  # Selection color row 1
    acctno1o: str = ""  # Account number output row 1
    crdnum1o: str = ""  # Card number output row 1
    crdsts1o: str = ""  # Card status output row 1
    
    crdsel2i: str = ""
    crdsel2o: str = ""
    crdsel2a: str = ""
    crdsel2l: int = 0
    crdsel2c: str = ""
    acctno2o: str = ""
    crdnum2o: str = ""
    crdsts2o: str = ""
    
    crdsel3i: str = ""
    crdsel3o: str = ""
    crdsel3a: str = ""
    crdsel3l: int = 0
    crdsel3c: str = ""
    acctno3o: str = ""
    crdnum3o: str = ""
    crdsts3o: str = ""
    
    crdsel4i: str = ""
    crdsel4o: str = ""
    crdsel4a: str = ""
    crdsel4l: int = 0
    crdsel4c: str = ""
    acctno4o: str = ""
    crdnum4o: str = ""
    crdsts4o: str = ""
    
    crdsel5i: str = ""
    crdsel5o: str = ""
    crdsel5a: str = ""
    crdsel5l: int = 0
    crdsel5c: str = ""
    acctno5o: str = ""
    crdnum5o: str = ""
    crdsts5o: str = ""
    
    crdsel6i: str = ""
    crdsel6o: str = ""
    crdsel6a: str = ""
    crdsel6l: int = 0
    crdsel6c: str = ""
    acctno6o: str = ""
    crdnum6o: str = ""
    crdsts6o: str = ""
    
    crdsel7i: str = ""
    crdsel7o: str = ""
    crdsel7a: str = ""
    crdsel7l: int = 0
    crdsel7c: str = ""
    acctno7o: str = ""
    crdnum7o: str = ""
    crdsts7o: str = ""
    
    # Message fields
    errmsgo: str = ""
    infomsgo: str = ""
    infomsgc: str = ""


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
        
        # Edit flags
        self.acctfilter_not_ok: bool = False
        self.acctfilter_isvalid: bool = False
        self.acctfilter_blank: bool = True
        
        self.cardfilter_not_ok: bool = False
        self.cardfilter_isvalid: bool = False
        self.cardfilter_blank: bool = True
        
        # Selection flags (7 rows)
        self.edit_select_flags: List[str] = [''] * 7
        self.edit_select_error_flags: List[str] = [''] * 7
        self.i_selected: int = 0
        
        # Protect select rows flag
        self.protect_select_rows: bool = False
        
        # Screen counter
        self.scrn_counter: int = 0
        
        # Filter record flag
        self.exclude_this_record: bool = False
        
        # Records to process flag
        self.read_loop_exit: bool = False
        self.more_records_to_read: bool = True
        
        # Messages
        self.error_msg: str = ""
        self.info_msg: str = ""
        self.long_msg: str = ""
        
        # PFK flag
        self.pfk_valid: bool = False
        self.pfk_invalid: bool = True
        
        # Card RID
        self.card_rid_cardnum: str = ""
        self.card_rid_acct_id: str = ""
        
        # File handler
        self.card_handler: Optional['CardFileHandler'] = None
        
        # Screen data (7 rows)
        self.screen_rows: List[Tuple[str, str, str]] = []  # (acct_no, card_num, card_status)
        
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
        self.cc_card_num: str = ""
        self.cc_card_num_n: str = ""
        
        # Current card record
        self.card_record: Optional[CardRecord] = None


# ============================================================================
# Program-Specific Commarea
# ============================================================================

@dataclass
class ThisProgCommarea:
    """Program-specific commarea (WS-THIS-PROGCOMMAREA)."""
    last_card_num: str = ""
    last_card_acct_id: str = ""
    first_card_num: str = ""
    first_card_acct_id: str = ""
    screen_num: int = 1  # 1 = first page
    last_page_displayed: int = 9  # 0 = shown, 9 = not shown
    next_page_ind: str = ""  # 'Y' = exists, '' = not exists
    return_flag: str = ""
    # Store screen rows for selection processing
    screen_rows: List[Tuple[str, str, str]] = field(default_factory=list)  # (acct_no, card_num, card_status)


# ============================================================================
# Card File Handler
# ============================================================================

class CardFileHandler:
    """Handler for CARDDAT file with browse support."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.file_status = "00"
        self._data: dict = {}
        self._sorted_keys: List[str] = []
        self._browse_active: bool = False
        self._browse_key: Optional[str] = None
        self._browse_index: int = -1
    
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
                self._sorted_keys = sorted(self._data.keys())
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
    
    def startbr(self, key: str, gteq: bool = True) -> Tuple[int, int]:
        """Start browse (STARTBR)."""
        try:
            self._browse_active = True
            self._browse_key = key
            
            # Find starting position
            if gteq:
                # Find first key >= browse key
                for i, k in enumerate(self._sorted_keys):
                    if k >= key:
                        self._browse_index = i
                        break
                else:
                    self._browse_index = len(self._sorted_keys)
            else:
                # Find exact key
                try:
                    self._browse_index = self._sorted_keys.index(key)
                except ValueError:
                    self._browse_index = len(self._sorted_keys)
            
            self.file_status = "00"
            return 0, 0
        except Exception as e:
            self.file_status = "99"
            return 12, 0
    
    def readnext(self) -> Tuple[Optional[str], int]:
        """Read next record (READNEXT)."""
        if not self._browse_active:
            self.file_status = "99"
            return None, 12
        
        try:
            if self._browse_index >= len(self._sorted_keys):
                self.file_status = "10"  # ENDFILE
                return None, 16
            
            key = self._sorted_keys[self._browse_index]
            record = self._data[key]
            self._browse_index += 1
            self._browse_key = key
            self.file_status = "00"
            return record, 0
        except Exception as e:
            self.file_status = "99"
            return None, 12
    
    def readprev(self) -> Tuple[Optional[str], int]:
        """Read previous record (READPREV)."""
        if not self._browse_active:
            self.file_status = "99"
            return None, 12
        
        try:
            if self._browse_index <= 0:
                self.file_status = "10"  # ENDFILE
                return None, 16
            
            self._browse_index -= 1
            key = self._sorted_keys[self._browse_index]
            record = self._data[key]
            self._browse_key = key
            self.file_status = "00"
            return record, 0
        except Exception as e:
            self.file_status = "99"
            return None, 12
    
    def endbr(self) -> int:
        """End browse (ENDBR)."""
        self._browse_active = False
        self._browse_key = None
        self._browse_index = -1
        self.file_status = "00"
        return 0


# ============================================================================
# Map Functions
# ============================================================================

def populate_header_info(ws: WorkingStorage, map_out: CardListMap):
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


def setup_screen_array(ws: WorkingStorage, map_out: CardListMap, prog_commarea: ThisProgCommarea):
    """Setup screen array (1200-SCREEN-ARRAY-INIT)."""
    # Map screen rows to output fields
    row_outputs = [
        (map_out.crdsel1o, map_out.acctno1o, map_out.crdnum1o, map_out.crdsts1o),
        (map_out.crdsel2o, map_out.acctno2o, map_out.crdnum2o, map_out.crdsts2o),
        (map_out.crdsel3o, map_out.acctno3o, map_out.crdnum3o, map_out.crdsts3o),
        (map_out.crdsel4o, map_out.acctno4o, map_out.crdnum4o, map_out.crdsts4o),
        (map_out.crdsel5o, map_out.acctno5o, map_out.crdnum5o, map_out.crdsts5o),
        (map_out.crdsel6o, map_out.acctno6o, map_out.crdnum6o, map_out.crdsts6o),
        (map_out.crdsel7o, map_out.acctno7o, map_out.crdnum7o, map_out.crdsts7o),
    ]
    
    for i in range(len(row_outputs)):
        if i < len(ws.screen_rows):
            sel_o, acct_o, card_o, sts_o = row_outputs[i]
            acct_no, card_num, card_status = ws.screen_rows[i]
            
            # Set selection output
            if i < len(ws.edit_select_flags):
                sel_o = ws.edit_select_flags[i]
                if i == 0:
                    map_out.crdsel1o = sel_o
                elif i == 1:
                    map_out.crdsel2o = sel_o
                elif i == 2:
                    map_out.crdsel3o = sel_o
                elif i == 3:
                    map_out.crdsel4o = sel_o
                elif i == 4:
                    map_out.crdsel5o = sel_o
                elif i == 5:
                    map_out.crdsel6o = sel_o
                elif i == 6:
                    map_out.crdsel7o = sel_o
            
            # Set row data
            if i == 0:
                map_out.acctno1o = acct_no
                map_out.crdnum1o = card_num
                map_out.crdsts1o = card_status
            elif i == 1:
                map_out.acctno2o = acct_no
                map_out.crdnum2o = card_num
                map_out.crdsts2o = card_status
            elif i == 2:
                map_out.acctno3o = acct_no
                map_out.crdnum3o = card_num
                map_out.crdsts3o = card_status
            elif i == 3:
                map_out.acctno4o = acct_no
                map_out.crdnum4o = card_num
                map_out.crdsts4o = card_status
            elif i == 4:
                map_out.acctno5o = acct_no
                map_out.crdnum5o = card_num
                map_out.crdsts5o = card_status
            elif i == 5:
                map_out.acctno6o = acct_no
                map_out.crdnum6o = card_num
                map_out.crdsts6o = card_status
            elif i == 6:
                map_out.acctno7o = acct_no
                map_out.crdnum7o = card_num
                map_out.crdsts7o = card_status


def setup_array_attrs(ws: WorkingStorage, map_out: CardListMap):
    """Setup array attributes (1250-SETUP-ARRAY-ATTRIBS)."""
    # Map selection attributes for 7 rows
    sel_attrs = [
        (map_out.crdsel1a, map_out.crdsel1c, map_out.crdsel1l, map_out.crdsel1o),
        (map_out.crdsel2a, map_out.crdsel2c, map_out.crdsel2l, map_out.crdsel2o),
        (map_out.crdsel3a, map_out.crdsel3c, map_out.crdsel3l, map_out.crdsel3o),
        (map_out.crdsel4a, map_out.crdsel4c, map_out.crdsel4l, map_out.crdsel4o),
        (map_out.crdsel5a, map_out.crdsel5c, map_out.crdsel5l, map_out.crdsel5o),
        (map_out.crdsel6a, map_out.crdsel6c, map_out.crdsel6l, map_out.crdsel6o),
        (map_out.crdsel7a, map_out.crdsel7c, map_out.crdsel7l, map_out.crdsel7o),
    ]
    
    for i in range(len(sel_attrs)):
        sel_a, sel_c, sel_l, sel_o = sel_attrs[i]
        
        # Check if row is empty or protected
        row_empty = i >= len(ws.screen_rows)
        
        if row_empty or ws.protect_select_rows:
            # Protect field
            if i == 0:
                map_out.crdsel1a = DFHBMPRF
            elif i == 1:
                map_out.crdsel2a = DFHBMPRO
            elif i == 2:
                map_out.crdsel3a = DFHBMPRO
            elif i == 3:
                map_out.crdsel4a = DFHBMPRO
            elif i == 4:
                map_out.crdsel5a = DFHBMPRO
            elif i == 5:
                map_out.crdsel6a = DFHBMPRO
            elif i == 6:
                map_out.crdsel7a = DFHBMPRO
        else:
            # Check for errors
            if i < len(ws.edit_select_error_flags) and ws.edit_select_error_flags[i] == '1':
                if i == 0:
                    map_out.crdsel1c = DFHRED
                    if not ws.edit_select_flags[i] or ws.edit_select_flags[i] == '':
                        map_out.crdsel1o = '*'
                elif i == 1:
                    map_out.crdsel2c = DFHRED
                    map_out.crdsel2l = -1
                elif i == 2:
                    map_out.crdsel3c = DFHRED
                    map_out.crdsel3l = -1
                elif i == 3:
                    map_out.crdsel4c = DFHRED
                    map_out.crdsel4l = -1
                elif i == 4:
                    map_out.crdsel5c = DFHRED
                    map_out.crdsel5l = -1
                elif i == 5:
                    map_out.crdsel6c = DFHRED
                    map_out.crdsel6l = -1
                elif i == 6:
                    map_out.crdsel7c = DFHRED
                    map_out.crdsel7l = -1
            
            # Unprotect field
            if i == 0:
                map_out.crdsel1a = DFHBMFSE
            elif i == 1:
                map_out.crdsel2a = DFHBMFSE
            elif i == 2:
                map_out.crdsel3a = DFHBMFSE
            elif i == 3:
                map_out.crdsel4a = DFHBMFSE
            elif i == 4:
                map_out.crdsel5a = DFHBMFSE
            elif i == 5:
                map_out.crdsel6a = DFHBMFSE
            elif i == 6:
                map_out.crdsel7a = DFHBMFSE


def setup_screen_attrs(ws: WorkingStorage, map_out: CardListMap, commarea: CardDemoCommarea, eibcalen: int):
    """Setup screen attributes (1300-SETUP-SCREEN-ATTRS)."""
    # Initialize search criteria
    if eibcalen == 0 or (commarea.pgm_enter and commarea.from_program == LIT_MENUPGM):
        pass
    else:
        # Set account ID output
        if ws.acctfilter_isvalid or ws.acctfilter_not_ok:
            map_out.acctsido = ws.cc_acct_id
            map_out.acctsida = DFHBMFSE
        elif commarea.acct_id == "0" or commarea.acct_id == "00000000000":
            map_out.acctsido = ""
        else:
            map_out.acctsido = commarea.acct_id
            map_out.acctsida = DFHBMFSE
        
        # Set card number output
        if ws.cardfilter_isvalid or ws.cardfilter_not_ok:
            map_out.cardsido = ws.cc_card_num
            map_out.cardsida = DFHBMFSE
        elif commarea.card_num == "0" or commarea.card_num == "0000000000000000":
            map_out.cardsido = ""
        else:
            map_out.cardsido = commarea.card_num
            map_out.cardsida = DFHBMFSE
    
    # Position cursor
    if ws.acctfilter_not_ok:
        map_out.acctsidc = DFHRED
        map_out.acctsidl = -1
    
    if ws.cardfilter_not_ok:
        map_out.cardsidc = DFHRED
        map_out.cardsidl = -1
    
    if ws.input_ok:
        map_out.acctsidl = -1


def setup_message(ws: WorkingStorage, map_out: CardListMap, prog_commarea: ThisProgCommarea, eibaid: str):
    """Setup message (1400-SETUP-MESSAGE)."""
    ws.info_msg = ""
    
    if ws.acctfilter_not_ok or ws.cardfilter_not_ok:
        pass
    elif eibaid == DFHPF7 and prog_commarea.screen_num == 1:
        ws.error_msg = "NO PREVIOUS PAGES TO DISPLAY"
    elif eibaid == DFHPF8 and prog_commarea.next_page_ind != 'Y' and prog_commarea.last_page_displayed == 0:
        ws.error_msg = "NO MORE PAGES TO DISPLAY"
    elif eibaid == DFHPF8 and prog_commarea.next_page_ind != 'Y':
        ws.info_msg = "TYPE S FOR DETAIL, U TO UPDATE ANY RECORD"
        if prog_commarea.last_page_displayed != 0 and prog_commarea.next_page_ind != 'Y':
            prog_commarea.last_page_displayed = 0
    elif not ws.info_msg or prog_commarea.next_page_ind == 'Y':
        ws.info_msg = "TYPE S FOR DETAIL, U TO UPDATE ANY RECORD"
    
    map_out.errmsgo = ws.error_msg
    
    if ws.info_msg and ws.error_msg != "NO RECORDS FOUND FOR THIS SEARCH CONDITION.":
        map_out.infomsgo = ws.info_msg
        map_out.infomsgc = DFHNEUTR


def send_map(ws: WorkingStorage, map_out: CardListMap, commarea: CardDemoCommarea, 
             prog_commarea: ThisProgCommarea, eibcalen: int, eibaid: str):
    """Send map (1000-SEND-MAP)."""
    populate_header_info(ws, map_out)
    map_out.pagenoo = str(prog_commarea.screen_num)
    setup_screen_array(ws, map_out, prog_commarea)
    setup_array_attrs(ws, map_out)
    setup_screen_attrs(ws, map_out, commarea, eibcalen)
    setup_message(ws, map_out, prog_commarea, eibaid)


def receive_map(ws: WorkingStorage, map_in: CardListMap):
    """Receive map (2000-RECEIVE-MAP)."""
    ws.resp_cd = 0
    ws.reas_cd = 0
    
    # Extract input fields
    ws.cc_acct_id = map_in.acctsidi.strip()
    ws.cc_card_num = map_in.cardsidi.strip()
    
    # Extract selection fields
    ws.edit_select_flags = [
        map_in.crdsel1i.strip(),
        map_in.crdsel2i.strip(),
        map_in.crdsel3i.strip(),
        map_in.crdsel4i.strip(),
        map_in.crdsel5i.strip(),
        map_in.crdsel6i.strip(),
        map_in.crdsel7i.strip(),
    ]


# ============================================================================
# Edit Functions
# ============================================================================

def edit_account(ws: WorkingStorage, commarea: CardDemoCommarea):
    """Edit account filter (2210-EDIT-ACCOUNT)."""
    ws.acctfilter_blank = True
    
    acct_id = ws.cc_acct_id.strip()
    if not acct_id or acct_id == "" or acct_id == "0" or acct_id == "00000000000":
        ws.acctfilter_blank = True
        commarea.acct_id = ""
        return
    
    # Check if numeric
    if not acct_id.isdigit():
        ws.input_error = True
        ws.acctfilter_not_ok = True
        ws.protect_select_rows = True
        ws.error_msg = "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"
        commarea.acct_id = ""
        return
    
    # Valid
    commarea.acct_id = acct_id
    ws.acctfilter_isvalid = True
    ws.acctfilter_not_ok = False
    ws.acctfilter_blank = False


def edit_card(ws: WorkingStorage, commarea: CardDemoCommarea):
    """Edit card filter (2220-EDIT-CARD)."""
    ws.cardfilter_blank = True
    
    card_num = ws.cc_card_num.strip()
    if not card_num or card_num == "" or card_num == "0" or card_num == "0000000000000000":
        ws.cardfilter_blank = True
        commarea.card_num = ""
        return
    
    # Check if numeric
    if not card_num.isdigit():
        ws.input_error = True
        ws.cardfilter_not_ok = True
        ws.protect_select_rows = True
        if not ws.error_msg:
            ws.error_msg = "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"
        commarea.card_num = ""
        return
    
    # Valid
    ws.cc_card_num_n = card_num
    commarea.card_num = card_num
    ws.cardfilter_isvalid = True
    ws.cardfilter_not_ok = False
    ws.cardfilter_blank = False


def edit_array(ws: WorkingStorage):
    """Edit selection array (2250-EDIT-ARRAY)."""
    if ws.input_error:
        return
    
    # Count 'S' and 'U' selections
    select_count = 0
    for flag in ws.edit_select_flags:
        if flag.upper() in ('S', 'U'):
            select_count += 1
    
    if select_count > 1:
        ws.input_error = True
        ws.error_msg = "PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE"
        
        # Set error flags
        for i in range(len(ws.edit_select_flags)):
            if ws.edit_select_flags[i].upper() in ('S', 'U'):
                ws.edit_select_error_flags[i] = '1'
    
    # Find selected row
    ws.i_selected = 0
    for i in range(len(ws.edit_select_flags)):
        flag = ws.edit_select_flags[i].strip().upper()
        if flag in ('S', 'U'):
            ws.i_selected = i + 1
            if select_count > 1:
                ws.edit_select_error_flags[i] = '1'
        elif flag and flag not in ('', ' '):
            ws.input_error = True
            ws.edit_select_error_flags[i] = '1'
            if not ws.error_msg:
                ws.error_msg = "INVALID ACTION CODE"


def edit_inputs(ws: WorkingStorage, commarea: CardDemoCommarea):
    """Edit inputs (2200-EDIT-INPUTS)."""
    ws.input_ok = True
    ws.protect_select_rows = False
    
    edit_account(ws, commarea)
    edit_card(ws, commarea)
    edit_array(ws)


# ============================================================================
# Filter Records
# ============================================================================

def filter_records(ws: WorkingStorage, card_record: CardRecord) -> bool:
    """Filter records (9500-FILTER-RECORDS).
    
    Returns True if record should be included, False if excluded.
    """
    ws.exclude_this_record = False
    
    # Filter by account ID
    if ws.acctfilter_isvalid:
        if card_record.card_acct_id != ws.cc_acct_id:
            ws.exclude_this_record = True
            return False
    
    # Filter by card number
    if ws.cardfilter_isvalid:
        if card_record.card_num != ws.cc_card_num_n:
            ws.exclude_this_record = True
            return False
    
    return True


# ============================================================================
# Read Forward
# ============================================================================

def read_forward(ws: WorkingStorage, prog_commarea: ThisProgCommarea):
    """Read forward (9000-READ-FORWARD)."""
    # Clear screen rows
    ws.screen_rows = []
    ws.scrn_counter = 0
    prog_commarea.next_page_ind = 'Y'
    ws.read_loop_exit = False
    ws.more_records_to_read = True
    
    if not ws.card_handler:
        return
    
    # Start browse
    start_key = ws.card_rid_cardnum if ws.card_rid_cardnum else ""
    resp_cd, reas_cd = ws.card_handler.startbr(start_key, gteq=True)
    ws.resp_cd = resp_cd
    ws.reas_cd = reas_cd
    
    if resp_cd != 0:
        ws.input_error = True
        ws.error_msg = f"File Error: READ on {LIT_CARD_FILE.strip()} returned RESP {resp_cd}"
        return
    
    # Read records
    while not ws.read_loop_exit:
        record_line, resp_cd = ws.card_handler.readnext()
        ws.resp_cd = resp_cd
        
        if resp_cd == DFHRESP_NORMAL or resp_cd == DFHRESP_DUPREC:
            card_record = parse_card_record(record_line)
            ws.card_record = card_record
            
            # Filter record
            if filter_records(ws, card_record):
                ws.scrn_counter += 1
                
                # Add to screen rows
                ws.screen_rows.append((
                    card_record.card_acct_id,
                    card_record.card_num,
                    card_record.card_active_status
                ))
                
                # Store first card key
                if ws.scrn_counter == 1:
                    prog_commarea.first_card_acct_id = card_record.card_acct_id
                    prog_commarea.first_card_num = card_record.card_num
                    if prog_commarea.screen_num == 0:
                        prog_commarea.screen_num = 1
                
                # Check if we've filled the screen
                if ws.scrn_counter >= WS_MAX_SCREEN_LINES:
                    ws.read_loop_exit = True
                    
                    # Store last card key
                    prog_commarea.last_card_acct_id = card_record.card_acct_id
                    prog_commarea.last_card_num = card_record.card_num
                    
                    # Check if there's a next record
                    next_record_line, next_resp_cd = ws.card_handler.readnext()
                    if next_resp_cd == DFHRESP_NORMAL or next_resp_cd == DFHRESP_DUPREC:
                        prog_commarea.next_page_ind = 'Y'
                        next_card = parse_card_record(next_record_line)
                        prog_commarea.last_card_acct_id = next_card.card_acct_id
                        prog_commarea.last_card_num = next_card.card_num
                    elif next_resp_cd == DFHRESP_ENDFILE:
                        prog_commarea.next_page_ind = ""
                        if not ws.error_msg:
                            ws.error_msg = "NO MORE RECORDS TO SHOW"
                    else:
                        ws.read_loop_exit = True
                        ws.input_error = True
                        ws.error_msg = f"File Error: READ on {LIT_CARD_FILE.strip()} returned RESP {next_resp_cd}"
        
        elif resp_cd == DFHRESP_ENDFILE:
            ws.read_loop_exit = True
            prog_commarea.next_page_ind = ""
            if ws.scrn_counter > 0:
                prog_commarea.last_card_acct_id = ws.card_record.card_acct_id if ws.card_record else ""
                prog_commarea.last_card_num = ws.card_record.card_num if ws.card_record else ""
            if not ws.error_msg:
                ws.error_msg = "NO MORE RECORDS TO SHOW"
            if prog_commarea.screen_num == 1 and ws.scrn_counter == 0:
                ws.error_msg = "NO RECORDS FOUND FOR THIS SEARCH CONDITION."
        
        else:
            ws.read_loop_exit = True
            ws.input_error = True
            ws.error_msg = f"File Error: READ on {LIT_CARD_FILE.strip()} returned RESP {resp_cd}"
    
    # End browse
    ws.card_handler.endbr()


# ============================================================================
# Read Backwards
# ============================================================================

def read_backwards(ws: WorkingStorage, prog_commarea: ThisProgCommarea):
    """Read backwards (9100-READ-BACKWARDS)."""
    # Clear screen rows
    ws.screen_rows = []
    
    # Set last card key to first card key
    prog_commarea.last_card_num = prog_commarea.first_card_num
    prog_commarea.last_card_acct_id = prog_commarea.first_card_acct_id
    
    # Start browse
    start_key = ws.card_rid_cardnum if ws.card_rid_cardnum else ""
    resp_cd, reas_cd = ws.card_handler.startbr(start_key, gteq=True)
    ws.resp_cd = resp_cd
    ws.reas_cd = reas_cd
    
    if resp_cd != 0:
        ws.input_error = True
        ws.error_msg = f"File Error: READ on {LIT_CARD_FILE.strip()} returned RESP {resp_cd}"
        return
    
    # Initialize counter to MAX + 1
    ws.scrn_counter = WS_MAX_SCREEN_LINES + 1
    prog_commarea.next_page_ind = 'Y'
    ws.read_loop_exit = False
    ws.more_records_to_read = True
    
    # Read previous (skip one)
    record_line, resp_cd = ws.card_handler.readprev()
    ws.resp_cd = resp_cd
    
    if resp_cd == DFHRESP_NORMAL or resp_cd == DFHRESP_DUPREC:
        ws.scrn_counter -= 1
    else:
        ws.read_loop_exit = True
        ws.input_error = True
        ws.error_msg = f"File Error: READ on {LIT_CARD_FILE.strip()} returned RESP {resp_cd}"
        ws.card_handler.endbr()
        return
    
    # Read backwards to fill screen
    while not ws.read_loop_exit:
        record_line, resp_cd = ws.card_handler.readprev()
        ws.resp_cd = resp_cd
        
        if resp_cd == DFHRESP_NORMAL or resp_cd == DFHRESP_DUPREC:
            card_record = parse_card_record(record_line)
            ws.card_record = card_record
            
            # Filter record
            if filter_records(ws, card_record):
                # Insert at beginning (backwards fill)
                ws.screen_rows.insert(0, (
                    card_record.card_acct_id,
                    card_record.card_num,
                    card_record.card_active_status
                ))
                
                ws.scrn_counter -= 1
                if ws.scrn_counter == 0:
                    ws.read_loop_exit = True
                    prog_commarea.first_card_acct_id = card_record.card_acct_id
                    prog_commarea.first_card_num = card_record.card_num
        else:
            ws.read_loop_exit = True
            ws.input_error = True
            ws.error_msg = f"File Error: READ on {LIT_CARD_FILE.strip()} returned RESP {resp_cd}"
    
    # End browse
    ws.card_handler.endbr()


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
    map_input: Optional[CardListMap] = None,
    prog_commarea: Optional[ThisProgCommarea] = None,
    card_file: str = "data/CARDDAT.dat"
) -> Tuple[CardDemoCommarea, ThisProgCommarea, CardListMap, Optional[str]]:
    """Main program entry point (0000-MAIN).
    
    Returns:
        Tuple of (commarea, prog_commarea, map_output, xctl_program)
    """
    # Initialize working storage
    ws = WorkingStorage()
    ws.input_ok = True
    ws.error_msg = ""
    
    # Initialize commarea if not provided
    if commarea is None:
        commarea = CardDemoCommarea()
    
    # Initialize program commarea
    if prog_commarea is None:
        prog_commarea = ThisProgCommarea()
    
    # Initialize maps
    map_out = CardListMap()
    if map_input is None:
        map_input = CardListMap()
    
    # Open file
    ws.card_handler = CardFileHandler(card_file)
    ws.card_handler.open_file()
    
    # Initialize commarea if first call
    if eibcalen == 0:
        commarea = CardDemoCommarea()
        commarea.from_tranid = LIT_THISTRANID
        commarea.from_program = LIT_THISPGM
        commarea.usrtyp_user = True
        commarea.pgm_enter = True
        commarea.last_map = LIT_THISMAP
        commarea.last_mapset = LIT_THISMAPSET
        prog_commarea.screen_num = 1
        prog_commarea.last_page_displayed = 9
    
    # Check if coming from menu
    if commarea.pgm_enter and commarea.from_program != LIT_THISPGM:
        prog_commarea = ThisProgCommarea()
        commarea.pgm_enter = True
        commarea.last_map = LIT_THISMAP
        prog_commarea.screen_num = 1
        prog_commarea.last_page_displayed = 9
    
    # Check PF key validity
    ws.pfk_invalid = True
    if eibaid in (DFHENTER, DFHPF3, DFHPF7, DFHPF8):
        ws.pfk_valid = True
        ws.pfk_invalid = False
    
    if ws.pfk_invalid:
        eibaid = DFHENTER
    
    # Handle PF3 exit
    if eibaid == DFHPF3 and commarea.from_program == LIT_THISPGM:
        commarea.from_tranid = LIT_THISTRANID
        commarea.from_program = LIT_THISPGM
        commarea.usrtyp_user = True
        commarea.pgm_enter = True
        commarea.last_mapset = LIT_THISMAPSET
        commarea.last_map = LIT_THISMAP
        commarea.to_program = LIT_MENUPGM
        commarea.error_msg = "PF03 PRESSED.EXITING"
        return commarea, prog_commarea, map_out, LIT_MENUPGM
    
    # Reset last page flag if not PF8
    if eibaid != DFHPF8:
        prog_commarea.last_page_displayed = 9
    
    # Receive map if reenter
    if eibcalen > 0 and commarea.from_program == LIT_THISPGM:
        receive_map(ws, map_input)
        edit_inputs(ws, commarea)
    
    xctl_prog = None
    
    # Main logic
    if ws.input_error:
        # Error handling
        commarea.error_msg = ws.error_msg
        commarea.from_program = LIT_THISPGM
        commarea.last_mapset = LIT_THISMAPSET
        commarea.last_map = LIT_THISMAP
        commarea.next_prog = LIT_THISPGM
        commarea.next_mapset = LIT_THISMAPSET
        commarea.next_map = LIT_THISMAP
        
        if not ws.acctfilter_not_ok and not ws.cardfilter_not_ok:
            # Set start key and read
            ws.card_rid_cardnum = prog_commarea.first_card_num if prog_commarea.first_card_num else ""
            read_forward(ws, prog_commarea)
        
        send_map(ws, map_out, commarea, prog_commarea, eibcalen, eibaid)
        # Store screen_rows in prog_commarea for future use
        prog_commarea.screen_rows = ws.screen_rows.copy()
        return commarea, prog_commarea, map_out, None
    
    elif eibaid == DFHPF7 and prog_commarea.screen_num == 1:
        # PF7 on first page - show first page again
        ws.card_rid_cardnum = prog_commarea.first_card_num if prog_commarea.first_card_num else ""
        read_forward(ws, prog_commarea)
        send_map(ws, map_out, commarea, prog_commarea, eibcalen, eibaid)
        # Store screen_rows in prog_commarea for future use
        prog_commarea.screen_rows = ws.screen_rows.copy()
        return commarea, prog_commarea, map_out, None
    
    elif eibaid == DFHPF3 or (commarea.pgm_reenter and commarea.from_program != LIT_THISPGM):
        # PF3 or return from other program
        commarea = CardDemoCommarea()
        prog_commarea = ThisProgCommarea()
        commarea.from_tranid = LIT_THISTRANID
        commarea.from_program = LIT_THISPGM
        commarea.usrtyp_user = True
        commarea.pgm_enter = True
        commarea.last_map = LIT_THISMAP
        commarea.last_mapset = LIT_THISMAPSET
        prog_commarea.screen_num = 1
        prog_commarea.last_page_displayed = 9
        
        ws.card_rid_cardnum = prog_commarea.first_card_num if prog_commarea.first_card_num else ""
        read_forward(ws, prog_commarea)
        send_map(ws, map_out, commarea, prog_commarea, eibcalen, eibaid)
        # Store screen_rows in prog_commarea for future use
        prog_commarea.screen_rows = ws.screen_rows.copy()
        return commarea, prog_commarea, map_out, None
    
    elif eibaid == DFHPF8 and prog_commarea.next_page_ind == 'Y':
        # Page down
        ws.card_rid_cardnum = prog_commarea.last_card_num if prog_commarea.last_card_num else ""
        prog_commarea.screen_num += 1
        read_forward(ws, prog_commarea)
        send_map(ws, map_out, commarea, prog_commarea, eibcalen, eibaid)
        # Store screen_rows in prog_commarea for future use
        prog_commarea.screen_rows = ws.screen_rows.copy()
        return commarea, prog_commarea, map_out, None
    
    elif eibaid == DFHPF7 and prog_commarea.screen_num != 1:
        # Page up
        ws.card_rid_cardnum = prog_commarea.first_card_num if prog_commarea.first_card_num else ""
        prog_commarea.screen_num -= 1
        read_backwards(ws, prog_commarea)
        send_map(ws, map_out, commarea, prog_commarea, eibcalen, eibaid)
        # Store screen_rows in prog_commarea for future use
        prog_commarea.screen_rows = ws.screen_rows.copy()
        return commarea, prog_commarea, map_out, None
    
    elif eibaid == DFHENTER and ws.i_selected > 0 and commarea.from_program == LIT_THISPGM:
        # View or update selected record
        # Use screen_rows from prog_commarea if available, otherwise use ws.screen_rows
        screen_rows_to_use = prog_commarea.screen_rows if prog_commarea.screen_rows else ws.screen_rows
        
        if ws.i_selected <= len(screen_rows_to_use):
            row_idx = ws.i_selected - 1
            acct_no, card_num, card_status = screen_rows_to_use[row_idx]
            
            commarea.from_tranid = LIT_THISTRANID
            commarea.from_program = LIT_THISPGM
            commarea.usrtyp_user = True
            commarea.pgm_enter = True
            commarea.last_mapset = LIT_THISMAPSET
            commarea.last_map = LIT_THISMAP
            commarea.acct_id = acct_no
            commarea.card_num = card_num
            
            # Determine which program to call
            selected_flag = ws.edit_select_flags[row_idx].upper() if row_idx < len(ws.edit_select_flags) else ""
            if selected_flag == 'S':
                # View detail
                xctl_prog = LIT_CARDDTLPGM
            elif selected_flag == 'U':
                # Update
                xctl_prog = LIT_CARDUPDPGM
            
            return commarea, prog_commarea, map_out, xctl_prog
    
    else:
        # Default - read forward
        ws.card_rid_cardnum = prog_commarea.first_card_num if prog_commarea.first_card_num else ""
        read_forward(ws, prog_commarea)
        send_map(ws, map_out, commarea, prog_commarea, eibcalen, eibaid)
        # Store screen_rows in prog_commarea for future use
        prog_commarea.screen_rows = ws.screen_rows.copy()
        return commarea, prog_commarea, map_out, None
    
    commarea.from_program = LIT_THISPGM
    # Store screen_rows in prog_commarea for future use
    prog_commarea.screen_rows = ws.screen_rows.copy()
    return commarea, prog_commarea, map_out, None

