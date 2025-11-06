"""
Python Translation of TRADERBL.cbl

Program: TRADERBL
Type: IMS DL/I Program
Function: Business logic module for Trader ITSO IMS sample

This program:
1. Receives request via DL/I GET-UNIQUE from IOPCB
2. Processes three request types: GET_COMPANY, SHARE_VALUE, BUY_SELL
3. Performs DL/I operations on CUSTOMER and COMPANY databases
4. Calculates share values and handles buy/sell operations
5. Returns response via DL/I ISRT
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, List, Tuple, Dict, Any
from datetime import datetime
from decimal import Decimal, ROUND_HALF_UP


# ============================================================================
# Constants
# ============================================================================

# DL/I Function Codes
GET_UNIQUE = "GU  "
GET_HOLD_UNIQUE = "GHU "
GET_NEXT = "GN  "
ISRT = "ISRT"
DLET = "DLET"
REPL = "REPL"

# DL/I Status Codes
NORMAL = "  "
NOTFND = "GE"
END_OF_DATABASE = "GB"

# Return Values
CLEAN_RETURN = "00"
UNKNOWN_REQUEST = "01"
UNKNOWN_SUBTYPE = "01"
BAD_CUST_READ = "02"
BAD_CUST_WRITE = "02"
BAD_CUST_REWRITE = "02"
BAD_COMP_READ = "03"
OVERFLOW_RC = "04"
COMPANY_NOT_FOUND = "05"
INVALID_SALE = "06"
INVALID_BUY = "06"
PGM_LOGIC_ERROR = "98"
CUSTOMER_NOT_FOUND = "99"

# Request Types
GET_COMPANY_REQ = "GET_COMPANY    "
GET_COMPANY_REQ1 = "Get_Company    "
SHARE_VALUE_REQ = "SHARE_VALUE    "
SHARE_VALUE_REQ1 = "Share_Value    "
BUY_SELL_REQ = "BUY_SELL       "
BUY_SELL_REQ1 = "Buy_Sell       "

# Subtypes
SUBTYPE_UPDATE = "0"
SUBTYPE_BUY = "1"
SUBTYPE_SELL = "2"

# Misc
OVERFLOW_VALUE = "XXXXXXXXX.XX"

# MODNAME transformations
MODNAME_TRANSFORMATIONS = {
    "TRDRQUO": "TRDOQUO",
    "TRDRCMP": "TRDOCMP",
    "TRDRBUY": "TRDOBUY",
    "TRDRSEL": "TRDOSEL"
}


# ============================================================================
# Commarea Structure (used in IN-BUFFER and OUT-BUFFER)
# ============================================================================

@dataclass
class Commarea:
    """Commarea structure for request/response."""
    request_type: str = ""  # 15 chars
    return_value: str = ""  # 2 chars
    userid: str = ""  # 60 chars
    user_password: str = ""  # 10 chars
    company_name: str = ""  # 20 chars
    correlid: str = ""  # 32 chars
    unit_share_price: str = ""  # 8 chars
    unit_value_7_days: str = ""  # 8 chars
    unit_value_6_days: str = ""  # 8 chars
    unit_value_5_days: str = ""  # 8 chars
    unit_value_4_days: str = ""  # 8 chars
    unit_value_3_days: str = ""  # 8 chars
    unit_value_2_days: str = ""  # 8 chars
    unit_value_1_days: str = ""  # 8 chars
    commission_cost_sell: str = ""  # 3 chars
    commission_cost_buy: str = ""  # 3 chars
    no_of_shares: str = ""  # 4 chars (string)
    no_of_shares_dec: int = 0  # 4 digits (numeric - REDEFINES)
    total_share_value: str = ""  # 12 chars
    buy_sell1: str = ""  # 4 chars
    buy_sell_price1: str = ""  # 8 chars
    buy_sell2: str = ""  # 4 chars
    buy_sell_price2: str = ""  # 8 chars
    buy_sell3: str = ""  # 4 chars
    buy_sell_price3: str = ""  # 8 chars
    buy_sell4: str = ""  # 4 chars
    buy_sell_price4: str = ""  # 8 chars
    alarm_change: str = ""  # 3 chars
    update_buy_sell: str = ""  # 1 char
    filler: str = ""  # 15 chars
    company_name_tab: List[str] = field(default_factory=lambda: [""] * 4)  # 4 occurrences of 20 chars
    imsuserid: str = ""  # 8 chars (OUT-BUFFER only)


# ============================================================================
# IN-BUFFER Structure
# ============================================================================

@dataclass
class InBuffer:
    """Input buffer structure (IN-BUFFER)."""
    ll: int = 0  # S9(3) COMP
    zz: int = 0  # S9(3) COMP
    trcd: str = ""  # 10 chars
    commarea: Commarea = field(default_factory=Commarea)


# ============================================================================
# OUT-BUFFER Structure
# ============================================================================

@dataclass
class OutBuffer:
    """Output buffer structure (OUT-BUFFER)."""
    ll: int = 384  # S9(3) COMP (default 384)
    zz: int = 0  # S9(3) COMP
    commarea: Commarea = field(default_factory=Commarea)
    segno: int = 0  # 9(4)


# ============================================================================
# CUSTOMER-IO-BUFFER Structure
# ============================================================================

@dataclass
class CustomerIOBuffer:
    """Customer I/O buffer structure."""
    customer: str = ""  # 60 chars
    keyrec_dot: str = "."  # 1 char
    company: str = ""  # 20 chars
    no_shares: str = ""  # 4 chars (string)
    dec_no_shares: int = 0  # 4 digits (numeric - REDEFINES)
    buy_from: str = ""  # 8 chars
    buy_from_no: str = ""  # 4 chars
    buy_to: str = ""  # 8 chars
    buy_to_no: str = ""  # 4 chars
    sell_from: str = ""  # 8 chars
    sell_from_no: str = ""  # 4 chars
    sell_to: str = ""  # 8 chars
    sell_to_no: str = ""  # 4 chars
    alarm_percent: str = ""  # 3 chars
    
    def sync_shares(self):
        """Synchronize shares: update no_shares from dec_no_shares."""
        self.no_shares = str(self.dec_no_shares).zfill(4)
    
    def sync_dec_shares(self):
        """Synchronize dec_shares: update dec_no_shares from no_shares."""
        try:
            self.dec_no_shares = int(self.no_shares.strip() or "0")
        except ValueError:
            self.dec_no_shares = 0


# ============================================================================
# COMPANY-IO-BUFFER Structure
# ============================================================================

@dataclass
class CompanyIOBuffer:
    """Company I/O buffer structure."""
    company: str = ""  # 20 chars
    share_value_int_part: str = ""  # 5 chars
    share_value_dec_part: str = ""  # 2 chars
    value_1: str = ""  # 8 chars
    value_2: str = ""  # 8 chars
    value_3: str = ""  # 8 chars
    value_4: str = ""  # 8 chars
    value_5: str = ""  # 8 chars
    value_6: str = ""  # 8 chars
    value_7: str = ""  # 8 chars
    commission_buy: str = ""  # 3 chars
    commission_sell: str = ""  # 3 chars
    
    def get_share_value_str(self) -> str:
        """Get share value as formatted string (e.g., '12345.67')."""
        int_part = self.share_value_int_part.strip() or "00000"
        dec_part = self.share_value_dec_part.strip() or "00"
        return f"{int_part}.{dec_part}"
    
    def get_share_value_decimal(self) -> Decimal:
        """Get share value as Decimal."""
        try:
            share_str = self.get_share_value_str()
            return Decimal(share_str)
        except:
            return Decimal("0.00")
    
    def set_share_value_from_str(self, value: str):
        """Set share value from string (e.g., '12345.67')."""
        parts = value.split(".")
        if len(parts) == 2:
            self.share_value_int_part = parts[0].rjust(5, "0")[:5]
            self.share_value_dec_part = parts[1].ljust(2, "0")[:2]
        else:
            self.share_value_int_part = value.rjust(5, "0")[:5]
            self.share_value_dec_part = "00"


# ============================================================================
# PCB Structures
# ============================================================================

@dataclass
class IOPCB:
    """I/O Program Communication Block."""
    lterm_name: str = ""  # 8 chars
    io_reserve_ims: str = ""  # 2 chars
    io_status: str = ""  # 2 chars
    curr_date: str = ""  # 4 chars
    curr_time: str = ""  # 4 chars
    in_msn: str = ""  # 4 chars
    modname: str = ""  # 8 chars
    userid: str = ""  # 8 chars


@dataclass
class ALTPCB:
    """Alternate PCB."""
    dbd_name: str = ""  # 8 chars
    seg_level: str = ""  # 2 chars
    alt_status: str = ""  # 2 chars
    proc_options: str = ""  # 4 chars
    reserve_dli: str = ""  # 4 chars
    seg_name_fb: str = ""  # 8 chars
    length_fb_key: int = 0  # 9(4)
    numb_sens_segs: int = 0  # 9(4)
    key_fb_area: str = ""  # 17 chars


@dataclass
class DBPCBCust:
    """Database PCB for Customer."""
    dbd_name: str = ""  # 8 chars
    seg_level: str = ""  # 2 chars
    db_cust_status: str = ""  # 2 chars
    proc_options: str = ""  # 4 chars
    reserve_dli: str = ""  # 4 chars
    seg_name_fb: str = ""  # 8 chars
    length_fb_key: int = 0  # 9(4)
    numb_sens_segs: int = 0  # 9(4)
    key_fb_area: str = ""  # 17 chars


@dataclass
class DBPCBComp:
    """Database PCB for Company."""
    dbd_name: str = ""  # 8 chars
    seg_level: str = ""  # 2 chars
    db_comp_status: str = ""  # 2 chars
    proc_options: str = ""  # 4 chars
    reserve_dli: str = ""  # 4 chars
    seg_name_fb: str = ""  # 8 chars
    length_fb_key: int = 0  # 9(4)
    numb_sens_segs: int = 0  # 9(4)
    key_fb_area: str = ""  # 17 chars


# ============================================================================
# SSA (Segment Search Argument) Structures
# ============================================================================

@dataclass
class SSACompany:
    """SSA for Company segment."""
    segment_name: str = "COMPSEG "  # 8 chars
    seg_key_name: str = "(COMPANY GE"  # 11 chars
    company_key: str = ""  # 20 chars
    filler: str = ")"  # 1 char
    
    def to_string(self) -> str:
        """Convert SSA to string format."""
        return f"{self.segment_name}{self.seg_key_name}{self.company_key:<20}{self.filler}"


@dataclass
class SSACustomer:
    """SSA for Customer segment."""
    segment_name: str = "CUSTSEG "  # 8 chars
    seg_key_name: str = "(KEYREC  EQ"  # 11 chars
    customer_key: str = ""  # 60 chars
    filler1: str = "."  # 1 char
    customer_comp: str = ""  # 20 chars
    filler2: str = ")"  # 1 char
    
    def to_string(self) -> str:
        """Convert SSA to string format."""
        return f"{self.segment_name}{self.seg_key_name}{self.customer_key:<60}{self.filler1}{self.customer_comp:<20}{self.filler2}"


@dataclass
class SSACustomerIsrt:
    """SSA for Customer insert."""
    segment_name: str = "CUSTSEG  "  # 9 chars
    
    def to_string(self) -> str:
        """Convert SSA to string format."""
        return self.segment_name


# ============================================================================
# DL/I Database Simulation
# ============================================================================

class DLIDatabase:
    """Mock DL/I database for testing."""
    
    def __init__(self):
        self.companies: Dict[str, CompanyIOBuffer] = {}
        self.customers: Dict[str, CustomerIOBuffer] = {}  # Key: "customer.company"
        self.company_browse_position: int = 0
        self.company_keys: List[str] = []
    
    def add_company(self, company: CompanyIOBuffer):
        """Add a company to the database."""
        self.companies[company.company] = company
        self._update_company_keys()
    
    def add_customer(self, customer: CustomerIOBuffer):
        """Add a customer to the database."""
        key = f"{customer.customer}.{customer.company}"
        self.customers[key] = customer
    
    def _update_company_keys(self):
        """Update sorted list of company keys."""
        self.company_keys = sorted(self.companies.keys())
    
    def get_company(self, company_key: str) -> Optional[CompanyIOBuffer]:
        """Get company by key."""
        return self.companies.get(company_key)
    
    def get_customer(self, customer_key: str, company: str) -> Optional[CustomerIOBuffer]:
        """Get customer by key."""
        key = f"{customer_key}.{company}"
        return self.customers.get(key)
    
    def start_company_browse(self, company_key: str = ""):
        """Start browsing companies."""
        if company_key:
            # Find first company >= company_key
            for i, key in enumerate(self.company_keys):
                if key >= company_key:
                    self.company_browse_position = i
                    return
            self.company_browse_position = len(self.company_keys)
        else:
            self.company_browse_position = 0
    
    def get_next_company(self) -> Optional[CompanyIOBuffer]:
        """Get next company in browse."""
        if self.company_browse_position < len(self.company_keys):
            key = self.company_keys[self.company_browse_position]
            self.company_browse_position += 1
            return self.companies[key]
        return None
    
    def update_customer(self, customer: CustomerIOBuffer):
        """Update customer in database."""
        key = f"{customer.customer}.{customer.company}"
        self.customers[key] = customer
    
    def insert_customer(self, customer: CustomerIOBuffer):
        """Insert customer into database."""
        key = f"{customer.customer}.{customer.company}"
        if key in self.customers:
            return False  # Duplicate key
        self.customers[key] = customer
        return True


# ============================================================================
# DL/I Call Simulation
# ============================================================================

def cbltdli(
    function_code: str,
    pcb,
    io_buffer,
    ssa: Optional[str] = None,
    db: Optional[DLIDatabase] = None
) -> Tuple[str, Optional[Any]]:
    """Simulate CBLTDLI call.
    
    Args:
        function_code: DL/I function code (GU, GHU, GN, ISRT, REPL, etc.)
        pcb: PCB structure (IOPCB, DBPCBCust, DBPCBComp)
        io_buffer: I/O buffer (InBuffer, OutBuffer, CustomerIOBuffer, CompanyIOBuffer)
        ssa: Optional SSA string
        db: Optional DLIDatabase instance for testing
    
    Returns:
        Tuple of (status_code, updated_buffer)
    """
    if db is None:
        # Default mock database
        db = DLIDatabase()
    
    if isinstance(pcb, IOPCB):
        if function_code == GET_UNIQUE:
            # GET-UNIQUE from IOPCB - return input buffer as-is
            pcb.io_status = NORMAL
            return NORMAL, io_buffer
        elif function_code == ISRT:
            # ISRT to IOPCB - return output buffer as-is
            pcb.io_status = NORMAL
            return NORMAL, io_buffer
    elif isinstance(pcb, DBPCBCust):
        if function_code == GET_UNIQUE or function_code == GET_HOLD_UNIQUE:
            # GET-UNIQUE or GET-HOLD-UNIQUE from DBPCB-CUST
            if isinstance(io_buffer, CustomerIOBuffer) and ssa:
                # Parse SSA to extract customer key and company
                # Format: "CUSTSEG (KEYREC  EQ{customer_key:60}.{company:20})"
                customer_key = ssa[19:79].strip() if len(ssa) > 79 else ""
                company = ssa[80:100].strip() if len(ssa) > 100 else ""
                
                customer = db.get_customer(customer_key, company)
                if customer:
                    pcb.db_cust_status = NORMAL
                    # Copy customer data to buffer
                    io_buffer.customer = customer.customer
                    io_buffer.company = customer.company
                    io_buffer.no_shares = customer.no_shares
                    io_buffer.dec_no_shares = customer.dec_no_shares
                    io_buffer.buy_from = customer.buy_from
                    io_buffer.buy_from_no = customer.buy_from_no
                    io_buffer.buy_to = customer.buy_to
                    io_buffer.buy_to_no = customer.buy_to_no
                    io_buffer.sell_from = customer.sell_from
                    io_buffer.sell_from_no = customer.sell_from_no
                    io_buffer.sell_to = customer.sell_to
                    io_buffer.sell_to_no = customer.sell_to_no
                    io_buffer.alarm_percent = customer.alarm_percent
                    return NORMAL, io_buffer
                else:
                    pcb.db_cust_status = NOTFND
                    return NOTFND, None
            else:
                pcb.db_cust_status = NOTFND
                return NOTFND, None
        elif function_code == ISRT:
            # ISRT to DBPCB-CUST
            if isinstance(io_buffer, CustomerIOBuffer):
                if db.insert_customer(io_buffer):
                    pcb.db_cust_status = NORMAL
                    return NORMAL, io_buffer
                else:
                    pcb.db_cust_status = "DU"  # Duplicate key
                    return "DU", None
            else:
                pcb.db_cust_status = "ER"
                return "ER", None
        elif function_code == REPL:
            # REPL (replace) to DBPCB-CUST
            if isinstance(io_buffer, CustomerIOBuffer):
                db.update_customer(io_buffer)
                pcb.db_cust_status = NORMAL
                return NORMAL, io_buffer
            else:
                pcb.db_cust_status = "ER"
                return "ER", None
    elif isinstance(pcb, DBPCBComp):
        if function_code == GET_UNIQUE:
            # GET-UNIQUE from DBPCB-COMP
            if isinstance(io_buffer, CompanyIOBuffer) and ssa:
                # Parse SSA to extract company key
                # Format: "COMPSEG (COMPANY GE{company_key:20})"
                company_key = ssa[19:39].strip() if len(ssa) > 39 else ""
                
                if company_key:
                    # Specific key - use exact match
                    company = db.get_company(company_key)
                    if company:
                        pcb.db_comp_status = NORMAL
                        # Copy company data to buffer
                        io_buffer.company = company.company
                        io_buffer.share_value_int_part = company.share_value_int_part
                        io_buffer.share_value_dec_part = company.share_value_dec_part
                        io_buffer.value_1 = company.value_1
                        io_buffer.value_2 = company.value_2
                        io_buffer.value_3 = company.value_3
                        io_buffer.value_4 = company.value_4
                        io_buffer.value_5 = company.value_5
                        io_buffer.value_6 = company.value_6
                        io_buffer.value_7 = company.value_7
                        io_buffer.commission_buy = company.commission_buy
                        io_buffer.commission_sell = company.commission_sell
                        return NORMAL, io_buffer
                    else:
                        pcb.db_comp_status = NOTFND
                        return NOTFND, None
                else:
                    # Empty key - start browse from first company
                    db.start_company_browse("")
                    company = db.get_next_company()
                    if company:
                        pcb.db_comp_status = NORMAL
                        # Copy company data to buffer
                        io_buffer.company = company.company
                        io_buffer.share_value_int_part = company.share_value_int_part
                        io_buffer.share_value_dec_part = company.share_value_dec_part
                        io_buffer.value_1 = company.value_1
                        io_buffer.value_2 = company.value_2
                        io_buffer.value_3 = company.value_3
                        io_buffer.value_4 = company.value_4
                        io_buffer.value_5 = company.value_5
                        io_buffer.value_6 = company.value_6
                        io_buffer.value_7 = company.value_7
                        io_buffer.commission_buy = company.commission_buy
                        io_buffer.commission_sell = company.commission_sell
                        return NORMAL, io_buffer
                    else:
                        pcb.db_comp_status = END_OF_DATABASE
                        return END_OF_DATABASE, None
            else:
                pcb.db_comp_status = NOTFND
                return NOTFND, None
        elif function_code == GET_NEXT:
            # GET-NEXT from DBPCB-COMP
            if isinstance(io_buffer, CompanyIOBuffer):
                company = db.get_next_company()
                if company:
                    pcb.db_comp_status = NORMAL
                    # Copy company data to buffer
                    io_buffer.company = company.company
                    io_buffer.share_value_int_part = company.share_value_int_part
                    io_buffer.share_value_dec_part = company.share_value_dec_part
                    io_buffer.value_1 = company.value_1
                    io_buffer.value_2 = company.value_2
                    io_buffer.value_3 = company.value_3
                    io_buffer.value_4 = company.value_4
                    io_buffer.value_5 = company.value_5
                    io_buffer.value_6 = company.value_6
                    io_buffer.value_7 = company.value_7
                    io_buffer.commission_buy = company.commission_buy
                    io_buffer.commission_sell = company.commission_sell
                    return NORMAL, io_buffer
                else:
                    pcb.db_comp_status = END_OF_DATABASE
                    return END_OF_DATABASE, None
            else:
                pcb.db_comp_status = END_OF_DATABASE
                return END_OF_DATABASE, None
    
    # Unknown function/PCB combination
    return "ER", None


# ============================================================================
# Business Logic Functions
# ============================================================================

def get_company(
    out_buffer: OutBuffer,
    dbpcb_comp: DBPCBComp,
    company_io_buffer: CompanyIOBuffer,
    ssa_company: SSACompany,
    db: Optional[DLIDatabase] = None
):
    """GET-COMPANY: Get all company names from COMPFILE."""
    ssa_company.company_key = ""
    ssa_str = ssa_company.to_string()
    
    company_idx = 0
    
    # GET-UNIQUE gets first company (and starts browse)
    status, _ = cbltdli(GET_UNIQUE, dbpcb_comp, company_io_buffer, ssa_str, db)
    
    # Get companies until END-OF-DATABASE or 4 companies
    while (dbpcb_comp.db_comp_status == NORMAL or dbpcb_comp.db_comp_status == "") and company_idx < 4:
        if dbpcb_comp.db_comp_status == NORMAL or status == NORMAL:
            out_buffer.commarea.company_name_tab[company_idx] = company_io_buffer.company
            company_idx += 1
        
        # Get next company
        if company_idx < 4:
            status, _ = cbltdli(GET_NEXT, dbpcb_comp, company_io_buffer, None, db)
            if status != NORMAL:
                break


def read_custfile(
    out_buffer: OutBuffer,
    dbpcb_cust: DBPCBCust,
    customer_io_buffer: CustomerIOBuffer,
    ssa_customer: SSACustomer,
    db: Optional[DLIDatabase] = None
):
    """READ-CUSTFILE: Read customer record."""
    ssa_customer.customer_key = out_buffer.commarea.userid
    ssa_customer.customer_comp = out_buffer.commarea.company_name
    ssa_str = ssa_customer.to_string()
    
    status, _ = cbltdli(GET_UNIQUE, dbpcb_cust, customer_io_buffer, ssa_str, db)
    
    if status == NORMAL:
        dbpcb_cust.db_cust_status = NORMAL
        out_buffer.commarea.return_value = CLEAN_RETURN
    elif status == NOTFND:
        dbpcb_cust.db_cust_status = NOTFND
        out_buffer.commarea.return_value = CUSTOMER_NOT_FOUND
    else:
        dbpcb_cust.db_cust_status = status
        out_buffer.commarea.return_value = BAD_CUST_READ


def read_custfile_for_update(
    out_buffer: OutBuffer,
    dbpcb_cust: DBPCBCust,
    customer_io_buffer: CustomerIOBuffer,
    ssa_customer: SSACustomer,
    db: Optional[DLIDatabase] = None
):
    """READ-CUSTFILE-FOR-UPDATE: Read customer record for update (GHU)."""
    ssa_customer.customer_key = out_buffer.commarea.userid
    ssa_customer.customer_comp = out_buffer.commarea.company_name
    ssa_str = ssa_customer.to_string()
    
    status, _ = cbltdli(GET_HOLD_UNIQUE, dbpcb_cust, customer_io_buffer, ssa_str, db)
    
    if status == NORMAL:
        customer_io_buffer.sync_dec_shares()  # Sync numeric value
        dbpcb_cust.db_cust_status = NORMAL
        out_buffer.commarea.return_value = CLEAN_RETURN
    elif status == NOTFND:
        dbpcb_cust.db_cust_status = NOTFND
        out_buffer.commarea.return_value = CUSTOMER_NOT_FOUND
    else:
        dbpcb_cust.db_cust_status = status
        out_buffer.commarea.return_value = BAD_CUST_READ


def write_custfile(
    out_buffer: OutBuffer,
    dbpcb_cust: DBPCBCust,
    customer_io_buffer: CustomerIOBuffer,
    ssa_customer_isrt: SSACustomerIsrt,
    db: Optional[DLIDatabase] = None
):
    """WRITE-CUSTFILE: Insert new customer record."""
    customer_io_buffer.sync_shares()  # Sync string value
    ssa_str = ssa_customer_isrt.to_string()
    
    status, _ = cbltdli(ISRT, dbpcb_cust, customer_io_buffer, ssa_str, db)
    
    if status == NORMAL:
        dbpcb_cust.db_cust_status = NORMAL
    else:
        dbpcb_cust.db_cust_status = status
        out_buffer.commarea.return_value = BAD_CUST_WRITE


def rewrite_custfile(
    out_buffer: OutBuffer,
    dbpcb_cust: DBPCBCust,
    customer_io_buffer: CustomerIOBuffer,
    db: Optional[DLIDatabase] = None
):
    """REWRITE-CUSTFILE: Update existing customer record."""
    customer_io_buffer.sync_shares()  # Sync string value
    
    status, _ = cbltdli(REPL, dbpcb_cust, customer_io_buffer, None, db)
    
    if status == NORMAL:
        dbpcb_cust.db_cust_status = NORMAL
    else:
        dbpcb_cust.db_cust_status = status
        out_buffer.commarea.return_value = BAD_CUST_REWRITE


def read_compfile(
    out_buffer: OutBuffer,
    dbpcb_comp: DBPCBComp,
    company_io_buffer: CompanyIOBuffer,
    ssa_company: SSACompany,
    db: Optional[DLIDatabase] = None
):
    """READ-COMPFILE: Read company record."""
    ssa_company.company_key = out_buffer.commarea.company_name
    ssa_str = ssa_company.to_string()
    
    status, _ = cbltdli(GET_UNIQUE, dbpcb_comp, company_io_buffer, ssa_str, db)
    
    if status == NORMAL:
        dbpcb_comp.db_comp_status = NORMAL
        out_buffer.commarea.return_value = CLEAN_RETURN
    elif status == NOTFND:
        dbpcb_comp.db_comp_status = NOTFND
        out_buffer.commarea.return_value = COMPANY_NOT_FOUND
    else:
        dbpcb_comp.db_comp_status = status
        out_buffer.commarea.return_value = BAD_COMP_READ


def build_new_customer(
    out_buffer: OutBuffer,
    customer_io_buffer: CustomerIOBuffer
):
    """BUILD-NEW-CUSTOMER: Create a new customer record."""
    customer_io_buffer.customer = out_buffer.commarea.userid
    customer_io_buffer.keyrec_dot = "."
    customer_io_buffer.company = out_buffer.commarea.company_name
    customer_io_buffer.no_shares = "0000"
    customer_io_buffer.dec_no_shares = 0
    customer_io_buffer.buy_from = "00000.00"
    customer_io_buffer.buy_from_no = "0000"
    customer_io_buffer.buy_to = "00000.00"
    customer_io_buffer.buy_to_no = "0000"
    customer_io_buffer.sell_from = "00000.00"
    customer_io_buffer.sell_from_no = "0000"
    customer_io_buffer.sell_to = "00000.00"
    customer_io_buffer.sell_to_no = "0000"
    customer_io_buffer.alarm_percent = "000"


def set_dummy_cust_record(customer_io_buffer: CustomerIOBuffer):
    """SET-DUMMY-CUST-RECORD: Set dummy customer record."""
    customer_io_buffer.customer = " "
    customer_io_buffer.company = " "
    customer_io_buffer.no_shares = "0000"
    customer_io_buffer.dec_no_shares = 0
    customer_io_buffer.buy_from = "        "
    customer_io_buffer.buy_to = "        "
    customer_io_buffer.sell_from = "        "
    customer_io_buffer.sell_to = "        "


def update_buy_sell_fields(
    out_buffer: OutBuffer,
    customer_io_buffer: CustomerIOBuffer
):
    """UPDATE-BUY-SELL-FIELDS: Update buy/sell fields in customer buffer."""
    customer_io_buffer.buy_from_no = out_buffer.commarea.buy_sell1
    customer_io_buffer.buy_from = out_buffer.commarea.buy_sell_price1
    customer_io_buffer.buy_to_no = out_buffer.commarea.buy_sell2
    customer_io_buffer.buy_to = out_buffer.commarea.buy_sell_price2
    customer_io_buffer.sell_from_no = out_buffer.commarea.buy_sell3
    customer_io_buffer.sell_from = out_buffer.commarea.buy_sell_price3
    customer_io_buffer.sell_to_no = out_buffer.commarea.buy_sell4
    customer_io_buffer.sell_to = out_buffer.commarea.buy_sell_price4
    customer_io_buffer.alarm_percent = out_buffer.commarea.alarm_change


def calculate_shares_bought(
    out_buffer: OutBuffer,
    customer_io_buffer: CustomerIOBuffer
):
    """CALCULATE-SHARES-BOUGHT: Calculate new number of shares after buy."""
    # Convert shares to numeric
    shares_to_buy = out_buffer.commarea.no_of_shares_dec
    current_shares = customer_io_buffer.dec_no_shares
    
    # Add shares
    new_shares = shares_to_buy + current_shares
    
    # Check for overflow (max 9999)
    if new_shares > 9999:
        out_buffer.commarea.return_value = INVALID_BUY
        return
    
    # Update shares
    customer_io_buffer.dec_no_shares = new_shares
    customer_io_buffer.sync_shares()
    out_buffer.commarea.no_of_shares_dec = new_shares
    out_buffer.commarea.no_of_shares = customer_io_buffer.no_shares
    # Ensure return value is set to CLEAN_RETURN on success
    if out_buffer.commarea.return_value != INVALID_BUY:
        out_buffer.commarea.return_value = CLEAN_RETURN


def calculate_shares_sold(
    out_buffer: OutBuffer,
    customer_io_buffer: CustomerIOBuffer
):
    """CALCULATE-SHARES-SOLD: Calculate new number of shares after sell."""
    # Convert shares to numeric
    shares_to_sell = out_buffer.commarea.no_of_shares_dec
    current_shares = customer_io_buffer.dec_no_shares
    
    # Subtract shares
    new_shares = current_shares - shares_to_sell
    
    # Update shares
    customer_io_buffer.dec_no_shares = new_shares
    customer_io_buffer.sync_shares()
    out_buffer.commarea.no_of_shares_dec = new_shares
    out_buffer.commarea.no_of_shares = customer_io_buffer.no_shares


def calculate_share_value(
    out_buffer: OutBuffer,
    company_io_buffer: CompanyIOBuffer,
    customer_io_buffer: CustomerIOBuffer
):
    """CALCULATE-SHARE-VALUE: Calculate total share value."""
    # Get share value as Decimal
    share_value = company_io_buffer.get_share_value_decimal()
    num_shares = Decimal(customer_io_buffer.dec_no_shares)
    
    # Multiply: total = share_value * num_shares
    total_value = share_value * num_shares
    
    # Check for overflow (max 999999999.99)
    max_value = Decimal("999999999.99")
    if total_value > max_value:
        out_buffer.commarea.total_share_value = OVERFLOW_VALUE
        out_buffer.commarea.return_value = OVERFLOW_RC
        return
    
    # Format as string: "999999999.99"
    total_str = f"{total_value:.2f}"
    
    # Pad to 12 characters
    if len(total_str) <= 12:
        out_buffer.commarea.total_share_value = total_str.rjust(12)
    else:
        # Truncate if too long
        out_buffer.commarea.total_share_value = total_str[:12]
    
    # Ensure return value is set to CLEAN_RETURN on success (if not already set to error)
    if out_buffer.commarea.return_value != OVERFLOW_RC:
        out_buffer.commarea.return_value = CLEAN_RETURN


def build_resp_commarea(
    out_buffer: OutBuffer,
    company_io_buffer: CompanyIOBuffer,
    customer_io_buffer: CustomerIOBuffer
):
    """BUILD-RESP-COMMAREA: Build response commarea."""
    # Calculate share value
    calculate_share_value(out_buffer, company_io_buffer, customer_io_buffer)
    
    # Return no of shares and unit value today
    share_value_str = company_io_buffer.get_share_value_str()
    out_buffer.commarea.unit_share_price = share_value_str.rjust(8)
    out_buffer.commarea.no_of_shares = customer_io_buffer.no_shares
    out_buffer.commarea.no_of_shares_dec = customer_io_buffer.dec_no_shares
    
    # Return old unit share prices
    out_buffer.commarea.unit_value_7_days = company_io_buffer.value_7.rjust(8)
    out_buffer.commarea.unit_value_6_days = company_io_buffer.value_6.rjust(8)
    out_buffer.commarea.unit_value_5_days = company_io_buffer.value_5.rjust(8)
    out_buffer.commarea.unit_value_4_days = company_io_buffer.value_4.rjust(8)
    out_buffer.commarea.unit_value_3_days = company_io_buffer.value_3.rjust(8)
    out_buffer.commarea.unit_value_2_days = company_io_buffer.value_2.rjust(8)
    out_buffer.commarea.unit_value_1_days = company_io_buffer.value_1.rjust(8)
    
    # Return commission figures
    out_buffer.commarea.commission_cost_sell = company_io_buffer.commission_sell.rjust(3)
    out_buffer.commarea.commission_cost_buy = company_io_buffer.commission_buy.rjust(3)
    
    # Fill in buy/sell numbers
    out_buffer.commarea.buy_sell1 = customer_io_buffer.buy_from_no.rjust(4)
    out_buffer.commarea.buy_sell_price1 = customer_io_buffer.buy_from.rjust(8)
    out_buffer.commarea.buy_sell2 = customer_io_buffer.buy_to_no.rjust(4)
    out_buffer.commarea.buy_sell_price2 = customer_io_buffer.buy_to.rjust(8)
    out_buffer.commarea.buy_sell3 = customer_io_buffer.sell_from_no.rjust(4)
    out_buffer.commarea.buy_sell_price3 = customer_io_buffer.sell_from.rjust(8)
    out_buffer.commarea.buy_sell4 = customer_io_buffer.sell_to_no.rjust(4)
    out_buffer.commarea.buy_sell_price4 = customer_io_buffer.sell_to.rjust(8)
    
    # Fill in alarm value
    out_buffer.commarea.alarm_change = customer_io_buffer.alarm_percent.rjust(3)


def validate_company_exists(
    out_buffer: OutBuffer,
    dbpcb_comp: DBPCBComp,
    company_io_buffer: CompanyIOBuffer,
    ssa_company: SSACompany,
    db: Optional[DLIDatabase] = None
):
    """VALIDATE-COMPANY-EXISTS: Validate that company exists."""
    read_compfile(out_buffer, dbpcb_comp, company_io_buffer, ssa_company, db)


def get_share_value(
    out_buffer: OutBuffer,
    dbpcb_cust: DBPCBCust,
    dbpcb_comp: DBPCBComp,
    customer_io_buffer: CustomerIOBuffer,
    company_io_buffer: CompanyIOBuffer,
    ssa_customer: SSACustomer,
    ssa_company: SSACompany,
    db: Optional[DLIDatabase] = None
):
    """GET-SHARE-VALUE: Get share value for customer/company."""
    read_custfile(out_buffer, dbpcb_cust, customer_io_buffer, ssa_customer, db)
    
    if out_buffer.commarea.return_value == CUSTOMER_NOT_FOUND:
        out_buffer.commarea.return_value = CLEAN_RETURN
        set_dummy_cust_record(customer_io_buffer)
    
    if out_buffer.commarea.return_value == CLEAN_RETURN:
        read_compfile(out_buffer, dbpcb_comp, company_io_buffer, ssa_company, db)
        if out_buffer.commarea.return_value == CLEAN_RETURN:
            build_resp_commarea(out_buffer, company_io_buffer, customer_io_buffer)


def buy_sell_update_function(
    out_buffer: OutBuffer,
    dbpcb_cust: DBPCBCust,
    customer_io_buffer: CustomerIOBuffer,
    ssa_customer: SSACustomer,
    ssa_customer_isrt: SSACustomerIsrt,
    db: Optional[DLIDatabase] = None
):
    """BUY-SELL-UPDATE-FUNCTION: Update buy/sell fields."""
    read_custfile_for_update(out_buffer, dbpcb_cust, customer_io_buffer, ssa_customer, db)
    
    if out_buffer.commarea.return_value == CLEAN_RETURN:
        update_buy_sell_fields(out_buffer, customer_io_buffer)
        rewrite_custfile(out_buffer, dbpcb_cust, customer_io_buffer, db)
    elif out_buffer.commarea.return_value == CUSTOMER_NOT_FOUND:
        out_buffer.commarea.return_value = CLEAN_RETURN
        build_new_customer(out_buffer, customer_io_buffer)
        update_buy_sell_fields(out_buffer, customer_io_buffer)
        write_custfile(out_buffer, dbpcb_cust, customer_io_buffer, ssa_customer_isrt, db)
    else:
        out_buffer.commarea.return_value = BAD_CUST_READ


def buy_sell_buy_function(
    out_buffer: OutBuffer,
    dbpcb_cust: DBPCBCust,
    dbpcb_comp: DBPCBComp,
    customer_io_buffer: CustomerIOBuffer,
    company_io_buffer: CompanyIOBuffer,
    ssa_customer: SSACustomer,
    ssa_customer_isrt: SSACustomerIsrt,
    ssa_company: SSACompany,
    db: Optional[DLIDatabase] = None
):
    """BUY-SELL-BUY-FUNCTION: Process buy operation."""
    read_custfile_for_update(out_buffer, dbpcb_cust, customer_io_buffer, ssa_customer, db)
    
    if out_buffer.commarea.return_value == CLEAN_RETURN:
        calculate_shares_bought(out_buffer, customer_io_buffer)
        if out_buffer.commarea.return_value == CLEAN_RETURN:
            update_buy_sell_fields(out_buffer, customer_io_buffer)
            rewrite_custfile(out_buffer, dbpcb_cust, customer_io_buffer, db)
            build_resp_commarea(out_buffer, company_io_buffer, customer_io_buffer)
    elif out_buffer.commarea.return_value == CUSTOMER_NOT_FOUND:
        out_buffer.commarea.return_value = CLEAN_RETURN
        build_new_customer(out_buffer, customer_io_buffer)
        calculate_shares_bought(out_buffer, customer_io_buffer)
        if out_buffer.commarea.return_value == CLEAN_RETURN:
            update_buy_sell_fields(out_buffer, customer_io_buffer)
            write_custfile(out_buffer, dbpcb_cust, customer_io_buffer, ssa_customer_isrt, db)
            build_resp_commarea(out_buffer, company_io_buffer, customer_io_buffer)
    else:
        out_buffer.commarea.return_value = BAD_CUST_READ


def buy_sell_sell_function(
    out_buffer: OutBuffer,
    dbpcb_cust: DBPCBCust,
    dbpcb_comp: DBPCBComp,
    customer_io_buffer: CustomerIOBuffer,
    company_io_buffer: CompanyIOBuffer,
    ssa_customer: SSACustomer,
    ssa_company: SSACompany,
    db: Optional[DLIDatabase] = None
):
    """BUY-SELL-SELL-FUNCTION: Process sell operation."""
    read_custfile_for_update(out_buffer, dbpcb_cust, customer_io_buffer, ssa_customer, db)
    
    if out_buffer.commarea.return_value == CLEAN_RETURN:
        shares_to_sell = out_buffer.commarea.no_of_shares_dec
        current_shares = customer_io_buffer.dec_no_shares
        
        if shares_to_sell > current_shares:
            out_buffer.commarea.return_value = INVALID_SALE
        else:
            calculate_shares_sold(out_buffer, customer_io_buffer)
            update_buy_sell_fields(out_buffer, customer_io_buffer)
            rewrite_custfile(out_buffer, dbpcb_cust, customer_io_buffer, db)
            build_resp_commarea(out_buffer, company_io_buffer, customer_io_buffer)
    elif out_buffer.commarea.return_value == CUSTOMER_NOT_FOUND:
        out_buffer.commarea.return_value = INVALID_SALE
    else:
        out_buffer.commarea.return_value = BAD_CUST_READ


def buy_sell(
    out_buffer: OutBuffer,
    dbpcb_cust: DBPCBCust,
    dbpcb_comp: DBPCBComp,
    customer_io_buffer: CustomerIOBuffer,
    company_io_buffer: CompanyIOBuffer,
    ssa_customer: SSACustomer,
    ssa_customer_isrt: SSACustomerIsrt,
    ssa_company: SSACompany,
    db: Optional[DLIDatabase] = None
):
    """BUY-SELL: Process buy/sell request."""
    update_buy_sell = out_buffer.commarea.update_buy_sell
    
    if update_buy_sell == SUBTYPE_UPDATE:
        validate_company_exists(out_buffer, dbpcb_comp, company_io_buffer, ssa_company, db)
        if out_buffer.commarea.return_value == CLEAN_RETURN:
            buy_sell_update_function(out_buffer, dbpcb_cust, customer_io_buffer, ssa_customer, ssa_customer_isrt, db)
    elif update_buy_sell == SUBTYPE_BUY:
        validate_company_exists(out_buffer, dbpcb_comp, company_io_buffer, ssa_company, db)
        if out_buffer.commarea.return_value == CLEAN_RETURN:
            buy_sell_buy_function(out_buffer, dbpcb_cust, dbpcb_comp, customer_io_buffer, company_io_buffer, ssa_customer, ssa_customer_isrt, ssa_company, db)
    elif update_buy_sell == SUBTYPE_SELL:
        validate_company_exists(out_buffer, dbpcb_comp, company_io_buffer, ssa_company, db)
        if out_buffer.commarea.return_value == CLEAN_RETURN:
            buy_sell_sell_function(out_buffer, dbpcb_cust, dbpcb_comp, customer_io_buffer, company_io_buffer, ssa_customer, ssa_company, db)
    else:
        out_buffer.commarea.return_value = UNKNOWN_SUBTYPE


# ============================================================================
# Main Program
# ============================================================================

def main(
    iopcb: IOPCB,
    altpcb: Optional[ALTPCB] = None,
    dbpcb_cust: Optional[DBPCBCust] = None,
    dbpcb_comp: Optional[DBPCBComp] = None,
    in_buffer: Optional[InBuffer] = None,
    mock_cbltdli: Optional[callable] = None,
    db: Optional[DLIDatabase] = None,
    segno: int = 0
) -> Tuple[OutBuffer, int]:
    """Main program entry point (MAINLINE).
    
    Args:
        iopcb: I/O PCB
        altpcb: Alternate PCB (optional)
        dbpcb_cust: Database PCB for Customer (optional)
        dbpcb_comp: Database PCB for Company (optional)
        in_buffer: Input buffer (created if None)
        mock_cbltdli: Optional mock function for CBLTDLI (for testing)
        db: Optional DLIDatabase instance (for testing)
        segno: Current segment number (default 0, will be incremented)
    
    Returns:
        Tuple of (out_buffer, new_segno)
    """
    # Initialize structures
    if in_buffer is None:
        in_buffer = InBuffer()
    
    if dbpcb_cust is None:
        dbpcb_cust = DBPCBCust()
    
    if dbpcb_comp is None:
        dbpcb_comp = DBPCBComp()
    
    # Initialize customer and company buffers
    customer_io_buffer = CustomerIOBuffer()
    company_io_buffer = CompanyIOBuffer()
    
    out_buffer = OutBuffer()
    out_buffer.ll = 384
    
    # Use mock if provided
    if mock_cbltdli:
        # For testing, we'll use the mock function
        pass
    else:
        # Use default cbltdli
        mock_cbltdli = None  # Will use module-level cbltdli
    
    # GET-UNIQUE from IOPCB to get input
    if mock_cbltdli:
        status, result = mock_cbltdli(GET_UNIQUE, iopcb, in_buffer)
    else:
        status, result = cbltdli(GET_UNIQUE, iopcb, in_buffer, None, db)
    
    # Get timestamp
    timestamp = datetime.now().strftime("%Y%m%d%H%M%S")[:14]
    
    # Display debug information
    print(f"{timestamp} IOPCB ModNAME: {iopcb.modname}")
    print(f"{timestamp} IOPCB Userid: {iopcb.userid}")
    print(f"{timestamp} IN-LL       : {in_buffer.ll}")
    print(f"{timestamp} REQUEST-TYPE: {in_buffer.commarea.request_type}")
    print(f"{timestamp} USERID      : {in_buffer.commarea.userid}")
    print(f"{timestamp} COMPANY-NAME: {in_buffer.commarea.company_name}")
    print(f"{timestamp} IN-COMMAREA : {in_buffer.commarea}")
    
    # Move IN-COMMAREA to OUT-COMMAREA
    out_buffer.commarea = Commarea(
        request_type=in_buffer.commarea.request_type,
        return_value=in_buffer.commarea.return_value,
        userid=in_buffer.commarea.userid,
        user_password=in_buffer.commarea.user_password,
        company_name=in_buffer.commarea.company_name,
        correlid=in_buffer.commarea.correlid,
        unit_share_price=in_buffer.commarea.unit_share_price,
        unit_value_7_days=in_buffer.commarea.unit_value_7_days,
        unit_value_6_days=in_buffer.commarea.unit_value_6_days,
        unit_value_5_days=in_buffer.commarea.unit_value_5_days,
        unit_value_4_days=in_buffer.commarea.unit_value_4_days,
        unit_value_3_days=in_buffer.commarea.unit_value_3_days,
        unit_value_2_days=in_buffer.commarea.unit_value_2_days,
        unit_value_1_days=in_buffer.commarea.unit_value_1_days,
        commission_cost_sell=in_buffer.commarea.commission_cost_sell,
        commission_cost_buy=in_buffer.commarea.commission_cost_buy,
        no_of_shares=in_buffer.commarea.no_of_shares,
        no_of_shares_dec=in_buffer.commarea.no_of_shares_dec,
        total_share_value=in_buffer.commarea.total_share_value,
        buy_sell1=in_buffer.commarea.buy_sell1,
        buy_sell_price1=in_buffer.commarea.buy_sell_price1,
        buy_sell2=in_buffer.commarea.buy_sell2,
        buy_sell_price2=in_buffer.commarea.buy_sell_price2,
        buy_sell3=in_buffer.commarea.buy_sell3,
        buy_sell_price3=in_buffer.commarea.buy_sell_price3,
        buy_sell4=in_buffer.commarea.buy_sell4,
        buy_sell_price4=in_buffer.commarea.buy_sell_price4,
        alarm_change=in_buffer.commarea.alarm_change,
        update_buy_sell=in_buffer.commarea.update_buy_sell,
        filler=in_buffer.commarea.filler,
        company_name_tab=in_buffer.commarea.company_name_tab.copy()
    )
    
    # Move USERID from IOPCB to IMSUSERID
    out_buffer.commarea.imsuserid = iopcb.userid
    
    # Transform MODNAME if needed
    if iopcb.modname in MODNAME_TRANSFORMATIONS:
        iopcb.modname = MODNAME_TRANSFORMATIONS[iopcb.modname]
    
    # Get request code
    request_code = in_buffer.commarea.request_type
    
    # Initialize SSAs
    ssa_company = SSACompany()
    ssa_customer = SSACustomer()
    ssa_customer_isrt = SSACustomerIsrt()
    
    # Process request
    if request_code == GET_COMPANY_REQ or request_code == GET_COMPANY_REQ1:
        get_company(out_buffer, dbpcb_comp, company_io_buffer, ssa_company, db)
    elif request_code == SHARE_VALUE_REQ or request_code == SHARE_VALUE_REQ1:
        get_share_value(out_buffer, dbpcb_cust, dbpcb_comp, customer_io_buffer, company_io_buffer, ssa_customer, ssa_company, db)
    elif request_code == BUY_SELL_REQ or request_code == BUY_SELL_REQ1:
        buy_sell(out_buffer, dbpcb_cust, dbpcb_comp, customer_io_buffer, company_io_buffer, ssa_customer, ssa_customer_isrt, ssa_company, db)
    else:
        out_buffer.commarea.return_value = UNKNOWN_REQUEST
        print("REQUEST CODE OF " + request_code + " INVALID")
    
    # Increment segment number
    new_segno = segno + 1
    out_buffer.segno = new_segno
    
    # Display output
    timestamp = datetime.now().strftime("%Y%m%d%H%M%S")[:14]
    print(f"{timestamp} OUT-LL      : {out_buffer.ll}")
    print(f"{timestamp} OUT-BUFFER  : {out_buffer}")
    
    # ISRT to IOPCB
    if not iopcb.modname or iopcb.modname.strip() == "":
        if mock_cbltdli:
            status, _ = mock_cbltdli(ISRT, iopcb, out_buffer)
        else:
            status, _ = cbltdli(ISRT, iopcb, out_buffer, None, db)
    else:
        print(f"{timestamp} MODNAME     : {iopcb.modname}")
        # ISRT with MODNAME (simplified - MODNAME is passed but not used in mock)
        if mock_cbltdli:
            status, _ = mock_cbltdli(ISRT, iopcb, out_buffer, iopcb.modname)
        else:
            status, _ = cbltdli(ISRT, iopcb, out_buffer, None, db)
        iopcb.modname = ""
    
    return out_buffer, new_segno


if __name__ == "__main__":
    # For testing/standalone execution
    iopcb = IOPCB(modname="", userid="TESTUSER")
    in_buffer = InBuffer()
    in_buffer.commarea.request_type = GET_COMPANY_REQ
    
    out_buffer, segno = main(iopcb=iopcb, in_buffer=in_buffer)
    print(f"Segment Number: {segno}")

