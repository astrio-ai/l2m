"""
Python Translation of COACTVWC.cbl

Program: COACTVWC
Layer: Business logic
Function: Accept and process Account View request

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, Dict
from decimal import Decimal

# Import shared record structures and functions from COACTUPC
from COACTUPC import (
    CardXrefRecord,
    AccountRecord,
    CustomerRecord,
    parse_xref_record,
    parse_account_record,
    parse_customer_record,
    IndexedFileHandler,
    XrefFileHandler,
    AcctFileHandler,
    CustFileHandler,
    edit_account_id
)


# ============================================================================
# CICS Response Codes
# ============================================================================

DFHRESP_NORMAL = 0
DFHRESP_NOTFND = 12
DFHRESP_ERROR = 13


# ============================================================================
# Commarea Structure (simplified)
# ============================================================================

@dataclass
class CardDemoCommarea:
    """Simplified commarea structure."""
    from_program: str = ""
    from_tranid: str = ""
    to_program: str = ""
    to_tranid: str = ""
    pgm_enter: bool = False
    pgm_reenter: bool = False
    usrtyp_user: bool = True
    last_mapset: str = ""
    last_map: str = ""
    acct_id: str = ""
    cust_id: str = ""
    card_num: str = ""
    error_msg: str = ""
    aid_enter: bool = False
    aid_pfk03: bool = False
    next_mapset: str = ""
    next_map: str = ""
    next_prog: str = ""
    
    # Map fields
    acctsidi: str = ""  # Account ID input field
    acctsido: str = ""  # Account ID output field
    acstsido: str = ""  # Account status output
    acurbalo: str = ""  # Current balance output
    acrdlimo: str = ""  # Credit limit output
    acshlimo: str = ""  # Cash credit limit output
    acrcycro: str = ""  # Current cycle credit output
    acrcydbo: str = ""  # Current cycle debit output
    adtopeno: str = ""  # Open date output
    aexpdto: str = ""  # Expiration date output
    areisdto: str = ""  # Reissue date output
    aaddgrpo: str = ""  # Group ID output
    acstnumo: str = ""  # Customer ID output
    acstssno: str = ""  # Customer SSN output
    acstfcoo: str = ""  # FICO score output
    acstdobo: str = ""  # Date of birth output
    acsfnamo: str = ""  # First name output
    acsmnamo: str = ""  # Middle name output
    acslnamo: str = ""  # Last name output
    acsadl1o: str = ""  # Address line 1 output
    acsadl2o: str = ""  # Address line 2 output
    acscityo: str = ""  # Address line 3 (city) output
    acsstteo: str = ""  # State output
    acszipco: str = ""  # Zip code output
    acsctryo: str = ""  # Country output
    acsphn1o: str = ""  # Phone 1 output
    acsphn2o: str = ""  # Phone 2 output
    acsgovto: str = ""  # Government ID output
    acseftco: str = ""  # EFT account output
    acspflgo: str = ""  # Primary card flag output
    errmsgo: str = ""  # Error message output
    infomsgo: str = ""  # Info message output
    
    # Map attribute fields
    acctsida: str = ""  # Account ID attribute
    acctsidl: int = 0  # Account ID length
    acctsidc: str = ""  # Account ID color
    infomsgc: str = ""  # Info message color


# ============================================================================
# Working Storage
# ============================================================================

class WorkingStorage:
    """Working storage for program state."""
    
    def __init__(self):
        # CICS processing variables
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self.tranid: str = "CAVW"
        
        # Input flags
        self.input_ok: bool = True
        self.input_error: bool = False
        self.input_pending: bool = False
        
        # PFK flags
        self.pfk_valid: bool = False
        self.pfk_invalid: bool = True
        
        # Edit flags
        self.acctfilter_not_ok: bool = False
        self.acctfilter_isvalid: bool = True
        self.acctfilter_blank: bool = False
        
        self.custfilter_not_ok: bool = False
        self.custfilter_isvalid: bool = False
        self.custfilter_blank: bool = True
        
        # File read flags
        self.found_acct_in_master: bool = False
        self.found_cust_in_master: bool = False
        
        # Messages
        self.return_msg: str = ""
        self.info_msg: str = ""
        self.long_msg: str = ""
        
        # Account ID being searched
        self.card_rid_acct_id: str = ""
        self.card_rid_acct_id_x: str = ""
        self.card_rid_cust_id: str = ""
        self.card_rid_cust_id_x: str = ""
        
        # File handlers
        self.xref_handler: Optional[XrefFileHandler] = None
        self.acct_handler: Optional[AcctFileHandler] = None
        self.cust_handler: Optional[CustFileHandler] = None
        
        # Records
        self.xref_record: Optional[CardXrefRecord] = None
        self.acct_record: Optional[AccountRecord] = None
        self.cust_record: Optional[CustomerRecord] = None


# ============================================================================
# Literals
# ============================================================================

LIT_THISPGM = "COACTVWC"
LIT_THISTRANID = "CAVW"
LIT_THISMAPSET = "COACTVW "
LIT_THISMAP = "CACTVWA"
LIT_MENUPGM = "COMEN01C"
LIT_MENUTRANID = "CM00"
LIT_ACCTFILENAME = "ACCTDAT "
LIT_CUSTFILENAME = "CUSTDAT "
LIT_CARDXREFNAME_ACCT_PATH = "CXACAIX "  # Alternate index path


# ============================================================================
# Main Program Functions
# ============================================================================

def read_cardxref_by_acct(ws: WorkingStorage, commarea: CardDemoCommarea) -> bool:
    """Read CARDXREF by account ID using alternate index (9200-GETCARDXREF-BYACCT)."""
    try:
        if not ws.xref_handler:
            return False
        
        # Read using alternate index (by account ID)
        record_line, resp_cd = ws.xref_handler.read_record(
            ws.card_rid_acct_id_x,
            use_alt_index=True
        )
        
        ws.resp_cd = resp_cd
        ws.reas_cd = 0
        
        if resp_cd == DFHRESP_NORMAL:
            ws.xref_record = parse_xref_record(record_line)
            commarea.cust_id = ws.xref_record.cust_id
            commarea.card_num = ws.xref_record.card_num
            ws.card_rid_cust_id = ws.xref_record.cust_id
            ws.card_rid_cust_id_x = ws.xref_record.cust_id
            return True
        elif resp_cd == DFHRESP_NOTFND:
            ws.input_error = True
            ws.acctfilter_not_ok = True
            if not ws.return_msg:
                error_msg = (
                    f"Account:{ws.card_rid_acct_id_x} not found in "
                    f"Cross ref file.  Resp:{resp_cd} Reas:{ws.reas_cd}"
                )
                ws.return_msg = error_msg
            return False
        else:
            ws.input_error = True
            ws.acctfilter_not_ok = True
            error_msg = (
                f"File Error: READ on {LIT_CARDXREFNAME_ACCT_PATH.strip()} "
                f"returned RESP {resp_cd},RESP2 {ws.reas_cd}"
            )
            ws.return_msg = error_msg
            return False
    except Exception as e:
        ws.input_error = True
        ws.acctfilter_not_ok = True
        ws.return_msg = f"Error reading card xref: {str(e)}"
        return False


def read_acctdata_by_acct(ws: WorkingStorage, commarea: CardDemoCommarea) -> bool:
    """Read ACCTDAT by account ID (9300-GETACCTDATA-BYACCT)."""
    try:
        if not ws.acct_handler:
            return False
        
        record_line, resp_cd = ws.acct_handler.read_record(ws.card_rid_acct_id_x)
        ws.resp_cd = resp_cd
        ws.reas_cd = 0
        
        if resp_cd == DFHRESP_NORMAL:
            ws.acct_record = parse_account_record(record_line)
            ws.found_acct_in_master = True
            return True
        elif resp_cd == DFHRESP_NOTFND:
            ws.input_error = True
            ws.acctfilter_not_ok = True
            if not ws.return_msg:
                error_msg = (
                    f"Account:{ws.card_rid_acct_id_x} not found in "
                    f"Acct Master file.Resp:{resp_cd} Reas:{ws.reas_cd}"
                )
                ws.return_msg = error_msg
            return False
        else:
            ws.input_error = True
            ws.acctfilter_not_ok = True
            error_msg = (
                f"File Error: READ on {LIT_ACCTFILENAME.strip()} "
                f"returned RESP {resp_cd},RESP2 {ws.reas_cd}"
            )
            ws.return_msg = error_msg
            return False
    except Exception as e:
        ws.input_error = True
        ws.acctfilter_not_ok = True
        ws.return_msg = f"Error reading account data: {str(e)}"
        return False


def read_custdata_by_cust(ws: WorkingStorage, commarea: CardDemoCommarea) -> bool:
    """Read CUSTDAT by customer ID (9400-GETCUSTDATA-BYCUST)."""
    try:
        if not ws.cust_handler:
            return False
        
        record_line, resp_cd = ws.cust_handler.read_record(ws.card_rid_cust_id_x)
        ws.resp_cd = resp_cd
        ws.reas_cd = 0
        
        if resp_cd == DFHRESP_NORMAL:
            ws.cust_record = parse_customer_record(record_line)
            ws.found_cust_in_master = True
            return True
        elif resp_cd == DFHRESP_NOTFND:
            ws.input_error = True
            ws.custfilter_not_ok = True
            if not ws.return_msg:
                error_msg = (
                    f"CustId:{ws.card_rid_cust_id_x} not found "
                    f"in customer master.Resp: {resp_cd} REAS:{ws.reas_cd}"
                )
                ws.return_msg = error_msg
            return False
        else:
            ws.input_error = True
            ws.custfilter_not_ok = True
            error_msg = (
                f"File Error: READ on {LIT_CUSTFILENAME.strip()} "
                f"returned RESP {resp_cd},RESP2 {ws.reas_cd}"
            )
            ws.return_msg = error_msg
            return False
    except Exception as e:
        ws.input_error = True
        ws.custfilter_not_ok = True
        ws.return_msg = f"Error reading customer data: {str(e)}"
        return False


def read_account(ws: WorkingStorage, commarea: CardDemoCommarea):
    """Main read account routine (9000-READ-ACCT)."""
    ws.info_msg = ""
    ws.card_rid_acct_id = commarea.acct_id
    ws.card_rid_acct_id_x = commarea.acct_id.strip()
    
    # Step 1: Read CARDXREF by account ID
    if not read_cardxref_by_acct(ws, commarea):
        return
    
    # Step 2: Read ACCTDAT by account ID
    if not read_acctdata_by_acct(ws, commarea):
        return
    
    # Step 3: Read CUSTDAT by customer ID
    if not read_custdata_by_cust(ws, commarea):
        return
    
    # Success - set info message
    ws.info_msg = "Displaying details of given Account"


def edit_account_input(ws: WorkingStorage, commarea: CardDemoCommarea):
    """Edit account input field (2210-EDIT-ACCOUNT)."""
    ws.acctfilter_not_ok = True
    
    # Get account ID from input
    acct_id_input = commarea.acctsidi.strip()
    
    # Replace * with empty
    if acct_id_input == '*' or acct_id_input == '':
        acct_id_input = ''
    
    # Check if blank
    if not acct_id_input or acct_id_input == '':
        ws.input_error = True
        ws.acctfilter_blank = True
        if not ws.return_msg:
            ws.return_msg = "Account number not provided"
        commarea.acct_id = ""
        return
    
    # Check if numeric and 11 digits and not zero
    if not acct_id_input.isdigit():
        ws.input_error = True
        ws.acctfilter_not_ok = True
        if not ws.return_msg:
            ws.return_msg = "Account Filter must  be a non-zero 11 digit number"
        commarea.acct_id = ""
        return
    
    if len(acct_id_input) != 11:
        ws.input_error = True
        ws.acctfilter_not_ok = True
        if not ws.return_msg:
            ws.return_msg = "Account Filter must  be a non-zero 11 digit number"
        commarea.acct_id = ""
        return
    
    if acct_id_input == "00000000000":
        ws.input_error = True
        ws.acctfilter_not_ok = True
        if not ws.return_msg:
            ws.return_msg = "Account Filter must  be a non-zero 11 digit number"
        commarea.acct_id = ""
        return
    
    # Valid
    commarea.acct_id = acct_id_input
    ws.acctfilter_isvalid = True
    ws.acctfilter_not_ok = False
    ws.acctfilter_blank = False


def process_inputs(ws: WorkingStorage, commarea: CardDemoCommarea):
    """Process inputs from screen (2000-PROCESS-INPUTS)."""
    ws.input_ok = True
    ws.acctfilter_isvalid = True
    
    # Replace * with empty
    acct_id_input = commarea.acctsidi.strip()
    if acct_id_input == '*' or acct_id_input == '':
        commarea.acct_id = ""
    else:
        commarea.acct_id = acct_id_input
    
    # Edit account field
    edit_account_input(ws, commarea)
    
    # Cross field edits
    if ws.acctfilter_blank:
        ws.return_msg = "No input received"
    
    commarea.error_msg = ws.return_msg
    commarea.next_prog = LIT_THISPGM
    commarea.next_mapset = LIT_THISMAPSET
    commarea.next_map = LIT_THISMAP


def setup_screen_vars(ws: WorkingStorage, commarea: CardDemoCommarea, eibcalen: int = 0):
    """Setup screen variables (1200-SETUP-SCREEN-VARS)."""
    # Initialize account ID output
    if ws.acctfilter_blank:
        commarea.acctsido = ""
    else:
        commarea.acctsido = commarea.acct_id
    
    # Populate account fields if found
    if ws.found_acct_in_master and ws.acct_record:
        commarea.acstsido = ws.acct_record.active_status
        commarea.acurbalo = f"{ws.acct_record.curr_bal:>12.2f}"
        commarea.acrdlimo = f"{ws.acct_record.credit_limit:>12.2f}"
        commarea.acshlimo = f"{ws.acct_record.cash_credit_limit:>12.2f}"
        commarea.acrcycro = f"{ws.acct_record.curr_cyc_credit:>12.2f}"
        commarea.acrcydbo = f"{ws.acct_record.curr_cyc_debit:>12.2f}"
        commarea.adtopeno = ws.acct_record.open_date
        commarea.aexpdto = ws.acct_record.expiration_date
        commarea.areisdto = ws.acct_record.reissue_date
        commarea.aaddgrpo = ws.acct_record.group_id.strip()
    
    # Populate customer fields if found
    if ws.found_cust_in_master and ws.cust_record:
        commarea.acstnumo = ws.cust_record.cust_id
        # Format SSN as XXX-XX-XXXX
        if ws.cust_record.ssn and len(ws.cust_record.ssn) >= 9:
            commarea.acstssno = (
                f"{ws.cust_record.ssn[0:3]}-"
                f"{ws.cust_record.ssn[3:5]}-"
                f"{ws.cust_record.ssn[5:9]}"
            )
        else:
            commarea.acstssno = ""
        commarea.acstfcoo = str(ws.cust_record.fico_credit_score)
        commarea.acstdobo = ws.cust_record.dob_yyyy_mm_dd
        commarea.acsfnamo = ws.cust_record.first_name.strip()
        commarea.acsmnamo = ws.cust_record.middle_name.strip()
        commarea.acslnamo = ws.cust_record.last_name.strip()
        commarea.acsadl1o = ws.cust_record.addr_line_1.strip()
        commarea.acsadl2o = ws.cust_record.addr_line_2.strip()
        commarea.acscityo = ws.cust_record.addr_line_3.strip()
        commarea.acsstteo = ws.cust_record.addr_state_cd
        commarea.acszipco = ws.cust_record.addr_zip.strip()
        commarea.acsctryo = ws.cust_record.addr_country_cd
        commarea.acsphn1o = ws.cust_record.phone_num_1.strip()
        commarea.acsphn2o = ws.cust_record.phone_num_2.strip()
        commarea.acsgovto = ws.cust_record.govt_issued_id.strip()
        commarea.acseftco = ws.cust_record.eft_account_id.strip()
        commarea.acspflgo = ws.cust_record.pri_card_ind
    
    # Setup message
    if not ws.info_msg:
        ws.info_msg = "Enter or update id of account to display"
    
    commarea.errmsgo = ws.return_msg
    commarea.infomsgo = ws.info_msg


def setup_screen_attrs(ws: WorkingStorage, commarea: CardDemoCommarea):
    """Setup screen attributes (1300-SETUP-SCREEN-ATTRS)."""
    # Protect account ID field
    commarea.acctsida = "DFHBMFSE"  # Unprotected, normal intensity
    
    # Position cursor
    commarea.acctsidl = -1  # Cursor position
    
    # Setup color
    commarea.acctsidc = "DFHDFCOL"  # Default color
    
    if ws.acctfilter_not_ok:
        commarea.acctsidc = "DFHRED"  # Red
    
    if ws.acctfilter_blank and commarea.pgm_reenter:
        commarea.acctsido = "*"
        commarea.acctsidc = "DFHRED"
    
    # Info message color
    if not ws.info_msg:
        commarea.infomsgc = "DFHBMDAR"  # Dark
    else:
        commarea.infomsgc = "DFHNEUTR"  # Neutral


def send_map(ws: WorkingStorage, commarea: CardDemoCommarea, eibcalen: int = 0):
    """Send map to screen (1000-SEND-MAP)."""
    # Initialize screen
    # In real CICS, this would clear the map output area
    
    # Setup screen variables
    setup_screen_vars(ws, commarea, eibcalen)
    
    # Setup screen attributes
    setup_screen_attrs(ws, commarea)
    
    # In real CICS, this would send the map
    # For simulation, we just update commarea
    commarea.next_mapset = LIT_THISMAPSET
    commarea.next_map = LIT_THISMAP
    commarea.pgm_reenter = True


def abend_program(abend_code: str = "9999", reason: str = ""):
    """Abend program (ABEND-ROUTINE)."""
    print(f"ABEND: {abend_code} - {reason}")
    sys.exit(1)


def main(
    xref_file: str = "data/CARDXREF.dat",
    acct_file: str = "data/ACCTDAT.dat",
    cust_file: str = "data/CUSTDAT.dat",
    commarea: Optional[CardDemoCommarea] = None,
    eibcalen: int = 0
) -> CardDemoCommarea:
    """Main program entry point (0000-MAIN)."""
    
    # Initialize working storage
    ws = WorkingStorage()
    
    # Initialize commarea if not provided
    if commarea is None:
        commarea = CardDemoCommarea()
    
    # Initialize commarea if first call
    # Preserve aid flags if they were set
    saved_aid_enter = commarea.aid_enter
    saved_aid_pfk03 = commarea.aid_pfk03
    
    is_first_call = False
    if eibcalen == 0 or (commarea.from_program == LIT_MENUPGM and not commarea.pgm_reenter):
        commarea = CardDemoCommarea()
        commarea.from_program = ""
        commarea.from_tranid = ""
        # Restore aid flags
        commarea.aid_enter = saved_aid_enter
        commarea.aid_pfk03 = saved_aid_pfk03
        is_first_call = True
        if not commarea.aid_pfk03:  # Only set pgm_enter if not exiting
            commarea.pgm_enter = True  # Treat as first entry
    
    # Clear return message
    ws.return_msg = ""
    
    # Store PF key
    # In real CICS, this would map PF keys
    if not commarea.aid_enter and not commarea.aid_pfk03:
        commarea.aid_enter = True
    
    # Check PF key validity
    ws.pfk_invalid = True
    if commarea.aid_enter or commarea.aid_pfk03:
        ws.pfk_valid = True
        ws.pfk_invalid = False
    
    if ws.pfk_invalid:
        commarea.aid_enter = True
    
    # Main logic
    if commarea.aid_pfk03:
        # Exit - return to calling program or menu
        if not commarea.from_tranid or commarea.from_tranid == "":
            commarea.to_tranid = LIT_MENUTRANID
        else:
            commarea.to_tranid = commarea.from_tranid
        
        if not commarea.from_program or commarea.from_program == "":
            commarea.to_program = LIT_MENUPGM
        else:
            commarea.to_program = commarea.from_program
        
        commarea.from_tranid = LIT_THISTRANID
        commarea.from_program = LIT_THISPGM
        commarea.usrtyp_user = True
        commarea.pgm_enter = True
        commarea.last_mapset = LIT_THISMAPSET
        commarea.last_map = LIT_THISMAP
        
        # In real CICS, this would XCTL to the target program
        # For simulation, we just return the commarea
        return commarea
    
    elif commarea.pgm_enter:
        # First entry - show screen
        send_map(ws, commarea, eibcalen)
        return commarea
    
    elif commarea.pgm_reenter:
        # Reenter - process inputs
        # Open files
        ws.xref_handler = XrefFileHandler(xref_file)
        ws.acct_handler = AcctFileHandler(acct_file)
        ws.cust_handler = CustFileHandler(cust_file)
        
        ws.xref_handler.open_file()
        ws.acct_handler.open_file()
        ws.cust_handler.open_file()
        
        process_inputs(ws, commarea)
        
        if ws.input_error:
            send_map(ws, commarea, eibcalen)
            return commarea
        else:
            read_account(ws, commarea)
            send_map(ws, commarea, eibcalen)
            return commarea
    
    else:
        # Unexpected scenario
        abend_program("0001", "UNEXPECTED DATA SCENARIO")
    
    # Set error message in commarea
    commarea.error_msg = ws.return_msg
    return commarea

