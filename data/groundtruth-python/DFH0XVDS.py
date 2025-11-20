"""
Python Translation of DFH0XVDS.cbl

Program: DFH0XVDS
Type: CICS Program
Function: VSAM Data Store for catalog operations

This program:
1. Reads VSAM file configuration
2. Performs catalog inquire (browse multiple items)
3. Performs catalog inquire single (read one item)
4. Places orders (updates stock levels)
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, List, Tuple
from datetime import datetime
from decimal import Decimal


# ============================================================================
# Constants
# ============================================================================

DFHRESP_NORMAL = 0
DFHRESP_NOTFND = 12
DFHRESP_ENDFILE = 13
DFHRESP_DUPKEY = 22
DFHRESP_NOSPACE = 24

# CICS Response Codes
DFHRESP_NORMAL_STR = "DFHRESP(NORMAL)"
DFHRESP_NOTFND_STR = "DFHRESP(NOTFND)"
DFHRESP_ENDFILE_STR = "DFHRESP(ENDFILE)"
DFHRESP_DUPKEY_STR = "DFHRESP(DUPKEY)"
DFHRESP_NOSPACE_STR = "DFHRESP(NOSPACE)"

# Request IDs
REQ_ID_INQUIRE = "01INQC"
REQ_ID_INQUIRE_SINGLE = "01INQS"
REQ_ID_PLACE_ORDER = "01ORDR"

# Return Codes
RETURN_CODE_SUCCESS = "00"
RETURN_CODE_NOT_FOUND = "20"
RETURN_CODE_FILE_ERROR = "21"
RETURN_CODE_UPDATE_ERROR = "22"
RETURN_CODE_INSUFFICIENT_STOCK = "97"
RETURN_CODE_INVALID_QUANTITY = "98"
RETURN_CODE_UNKNOWN_REQUEST = "99"

# Configuration
CONF_FILE = "EXMPCONF"
CONF_KEY = "VSAM-NAME"
DEFAULT_FILENAME = "EXMPCAT "
MAX_ITEMS = 15


# ============================================================================
# Data Structures
# ============================================================================

@dataclass
class CatalogItem:
    """Catalog item structure (WS-CAT-ITEM)."""
    item_ref: str = "0000"  # PIC 9(4)
    description: str = ""  # PIC X(40)
    department: str = "000"  # PIC 9(3)
    cost: str = "0.00"  # PIC ZZZ.99 (display format)
    in_stock: int = 0  # PIC 9(4)
    on_order: int = 0  # PIC 9(3)
    filler: str = ""  # PIC X(20)
    
    def to_string(self) -> str:
        """Convert to fixed-width string (80 bytes)."""
        return (
            f"{self.item_ref:>4}"
            f"{self.description:<40}"
            f"{self.department:>3}"
            f"{self.cost:>6}"
            f"{self.in_stock:>4}"
            f"{self.on_order:>3}"
            f"{self.filler:<20}"
        )
    
    @classmethod
    def from_string(cls, data: str) -> "CatalogItem":
        """Create from fixed-width string."""
        if len(data) < 80:
            data = data.ljust(80)
        return cls(
            item_ref=data[0:4].strip() or "0000",
            description=data[4:44].strip(),
            department=data[44:47].strip() or "000",
            cost=data[47:53].strip() or "0.00",
            in_stock=int(data[53:57].strip() or "0"),
            on_order=int(data[57:60].strip() or "0"),
            filler=data[60:80].strip()
        )


@dataclass
class Commarea:
    """DFHCOMMAREA structure (DFH0XCP1)."""
    # Request fields
    request_id: str = ""  # CA-REQUEST-ID
    item_ref_req: str = ""  # CA-ITEM-REF-REQ
    item_ref_number: str = ""  # CA-ITEM-REF-NUMBER
    quantity_req: int = 0  # CA-QUANTITY-REQ
    list_start_ref: str = "0000"  # CA-LIST-START-REF
    
    # Response fields
    return_code: str = "00"  # CA-RETURN-CODE
    response_message: str = ""  # CA-RESPONSE-MESSAGE
    item_count: int = 0  # CA-ITEM-COUNT
    last_item_ref: str = ""  # CA-LAST-ITEM-REF
    
    # Catalog items (array of 15)
    cat_items: List[CatalogItem] = field(default_factory=lambda: [CatalogItem() for _ in range(15)])
    
    # Single item response
    single_item: CatalogItem = field(default_factory=CatalogItem)


@dataclass
class ErrorMessage:
    """Error message structure."""
    date: str = ""  # EM-DATE PIC X(8)
    time: str = ""  # EM-TIME PIC X(6)
    request_id: str = ""  # EM-REQUEST-ID PIC X(6)
    detail: str = ""  # EM-DETAIL PIC X(50)
    
    def to_string(self) -> str:
        """Convert to string format."""
        return f"{self.date} {self.time} EXMPCMAN REQUESTID={self.request_id} {self.detail}"


# ============================================================================
# VSAM File Handler
# ============================================================================

class VSAMFileHandler:
    """Mock VSAM file handler for CICS file operations."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self._data: dict = {}  # key -> CatalogItem
        self._sorted_keys: List[str] = []
        self._browse_key: Optional[str] = None
        self._browse_index: int = -1
        self._locked_key: Optional[str] = None
        self._locked_record: Optional[CatalogItem] = None
    
    def add_record(self, key: str, item: CatalogItem):
        """Add a record to the file."""
        self._data[key] = item
        self._sorted_keys = sorted(self._data.keys())
    
    def startbr(self, key: str) -> Tuple[int, str]:
        """Start browse operation (STARTBR)."""
        if not self._data:
            return DFHRESP_NOTFND, ""
        
        if key in self._data:
            self._browse_key = key
            self._browse_index = self._sorted_keys.index(key)
            return DFHRESP_NORMAL, key
        
        # Find closest key >= key
        for k in self._sorted_keys:
            if k >= key:
                self._browse_key = k
                self._browse_index = self._sorted_keys.index(k)
                return DFHRESP_NORMAL, k
        
        # No key >= key found
        return DFHRESP_NOTFND, ""
    
    def readnext(self) -> Tuple[int, Optional[CatalogItem], str]:
        """Read next record (READNEXT)."""
        if self._browse_index < 0 or self._browse_index >= len(self._sorted_keys):
            return DFHRESP_ENDFILE, None, ""
        
        # Read current record
        key = self._sorted_keys[self._browse_index]
        item = self._data[key]
        
        # Move to next
        self._browse_index += 1
        
        # Check if we've reached the end (after increment)
        if self._browse_index >= len(self._sorted_keys):
            return DFHRESP_ENDFILE, item, key
        
        return DFHRESP_NORMAL, item, key
    
    def read(self, key: str) -> Tuple[int, Optional[CatalogItem]]:
        """Read record (READ)."""
        if key in self._data:
            return DFHRESP_NORMAL, self._data[key]
        return DFHRESP_NOTFND, None
    
    def read_update(self, key: str) -> Tuple[int, Optional[CatalogItem]]:
        """Read record for update (READ UPDATE)."""
        if key in self._data:
            item = self._data[key]
            self._locked_key = key
            self._locked_record = CatalogItem(
                item.item_ref, item.description, item.department,
                item.cost, item.in_stock, item.on_order, item.filler
            )
            return DFHRESP_NORMAL, item
        return DFHRESP_NOTFND, None
    
    def rewrite(self, key: str, item: CatalogItem) -> int:
        """Rewrite record (REWRITE)."""
        if self._locked_key != key or self._locked_record is None:
            return DFHRESP_NOTFND
        
        self._data[key] = item
        self._locked_key = None
        self._locked_record = None
        return DFHRESP_NORMAL
    
    def unlock(self, key: str) -> int:
        """Unlock record (UNLOCK)."""
        if self._locked_key == key:
            self._locked_key = None
            self._locked_record = None
        return DFHRESP_NORMAL
    
    def endbr(self) -> int:
        """End browse operation (ENDBR)."""
        self._browse_key = None
        self._browse_index = -1
        return DFHRESP_NORMAL
    
    def clear(self):
        """Clear all records."""
        self._data.clear()
        self._sorted_keys.clear()
        self._browse_key = None
        self._browse_index = -1
        self._locked_key = None
        self._locked_record = None


# ============================================================================
# Configuration File Handler
# ============================================================================

class ConfigFileHandler:
    """Mock configuration file handler."""
    
    def __init__(self):
        self._data: dict = {}
    
    def set_config(self, key: str, filename: str):
        """Set configuration value."""
        self._data[key] = filename
    
    def read(self, key: str) -> str:
        """Read configuration value."""
        return self._data.get(key, DEFAULT_FILENAME)


# ============================================================================
# CICS Operations Simulation
# ============================================================================

class CICSOperations:
    """Simulate CICS operations."""
    
    def __init__(self):
        self.files: dict = {}  # filename -> VSAMFileHandler
        self.config_file = ConfigFileHandler()
        self.error_queue: List[str] = []
    
    def get_file(self, filename: str) -> VSAMFileHandler:
        """Get or create file handler."""
        if filename not in self.files:
            self.files[filename] = VSAMFileHandler(filename)
        return self.files[filename]
    
    def read_config_file(self, filename: str, key: str) -> str:
        """Read configuration file."""
        return self.config_file.read(key)
    
    def write_error_message(self, error_msg: ErrorMessage):
        """Write error message to TDQ CSMT."""
        self.error_queue.append(error_msg.to_string())
    
    def get_current_time(self) -> Tuple[str, str]:
        """Get current time and date (ASKTIME/FORMATTIME)."""
        now = datetime.now()
        date_str = now.strftime("%m%d%Y")
        time_str = now.strftime("%H%M%S")
        return date_str, time_str


# ============================================================================
# Main Program
# ============================================================================

def main(
    commarea: Commarea,
    eibcalen: int = 0,
    eibtrnid: str = "",
    eibtrmid: str = "",
    eibtaskn: int = 0,
    cics_ops: Optional[CICSOperations] = None
) -> Commarea:
    """Main program entry point (MAINLINE).
    
    Args:
        commarea: DFHCOMMAREA structure
        eibcalen: EIBCALEN (commarea length)
        eibtrnid: EIBTRNID (transaction ID)
        eibtrmid: EIBTRMID (terminal ID)
        eibtaskn: EIBTASKN (task number)
        cics_ops: CICS operations handler (created if None)
    
    Returns:
        Updated commarea
    """
    # Initialize CICS operations
    if cics_ops is None:
        cics_ops = CICSOperations()
    
    # Check commarea
    if eibcalen == 0:
        error_msg = ErrorMessage()
        error_msg.detail = " NO COMMAREA RECEIVED"
        cics_ops.write_error_message(error_msg)
        raise SystemExit("ABEND EXCA")  # Simulate ABEND
    
    # Initialize return code
    commarea.return_code = RETURN_CODE_SUCCESS
    
    # Read configuration file
    try:
        filename_conf = cics_ops.read_config_file(CONF_FILE, CONF_KEY)
        filename = filename_conf.strip() or DEFAULT_FILENAME
    except:
        filename = DEFAULT_FILENAME
    
    # Get VSAM file handler (use default if config fails)
    try:
        vsam_file = cics_ops.get_file(filename)
    except:
        vsam_file = cics_ops.get_file(DEFAULT_FILENAME)
    
    # Uppercase request ID
    commarea.request_id = commarea.request_id.upper()
    
    # Route to appropriate operation
    if commarea.request_id == REQ_ID_INQUIRE:
        catalog_inquire(commarea, vsam_file, cics_ops)
    elif commarea.request_id == REQ_ID_INQUIRE_SINGLE:
        catalog_inquire_single(commarea, vsam_file, cics_ops)
    elif commarea.request_id == REQ_ID_PLACE_ORDER:
        place_order(commarea, vsam_file, cics_ops)
    else:
        request_not_recognised(commarea, cics_ops)
    
    return commarea


def catalog_inquire(commarea: Commarea, vsam_file: VSAMFileHandler, cics_ops: CICSOperations):
    """CATALOG-INQUIRE: Browse catalog items."""
    # Initialize response
    commarea.response_message = "EXDSVSAM: CATALOG-INQUIRE"
    commarea.item_count = 0
    commarea.cat_items = [CatalogItem() for _ in range(MAX_ITEMS)]
    
    # Start browse
    current_item_ref = commarea.list_start_ref
    resp_code, ridfld = vsam_file.startbr(current_item_ref)
    
    if resp_code == DFHRESP_NOTFND:
        commarea.return_code = RETURN_CODE_NOT_FOUND
        commarea.response_message = "ITEM NOT FOUND"
        return
    elif resp_code != DFHRESP_NORMAL:
        commarea.return_code = RETURN_CODE_FILE_ERROR
        commarea.response_message = f"ERROR OPENING FILE {vsam_file.filename}"
        return
    
    # Read records
    loop_counter = 0
    catalog_eof = False
    
    while not catalog_eof and loop_counter < MAX_ITEMS:
        resp_code, item, key = vsam_file.readnext()
        
        if resp_code == DFHRESP_NORMAL:
            loop_counter += 1
            commarea.cat_items[loop_counter - 1] = item
            commarea.item_count = loop_counter
            commarea.last_item_ref = key
            current_item_ref = key
        elif resp_code == DFHRESP_ENDFILE:
            # If we got an item but it's the last one, still count it
            if item is not None:
                loop_counter += 1
                commarea.cat_items[loop_counter - 1] = item
                commarea.item_count = loop_counter
                commarea.last_item_ref = key
            catalog_eof = True
        else:
            commarea.return_code = RETURN_CODE_FILE_ERROR
            commarea.response_message = "ERROR OCCURED READING FILE"
            return
    
    # Format response message
    if commarea.item_count > 0:
        commarea.response_message = f"{commarea.item_count:+03d} ITEMS RETURNED"
    else:
        commarea.response_message = ""
    
    # End browse
    resp_code = vsam_file.endbr()
    if resp_code != DFHRESP_NORMAL:
        commarea.return_code = RETURN_CODE_FILE_ERROR
        commarea.response_message = "ERROR ENDING BROWSE SESSION"
        return


def catalog_inquire_single(commarea: Commarea, vsam_file: VSAMFileHandler, cics_ops: CICSOperations):
    """CATALOG-INQUIRE-SINGLE: Read single catalog item."""
    resp_code, item = vsam_file.read(commarea.item_ref_req)
    
    if resp_code == DFHRESP_NOTFND:
        commarea.return_code = RETURN_CODE_NOT_FOUND
        commarea.response_message = "ITEM NOT FOUND"
        return
    elif resp_code != DFHRESP_NORMAL:
        commarea.return_code = RETURN_CODE_FILE_ERROR
        commarea.response_message = f"ERROR OPENING FILE {vsam_file.filename}"
        return
    
    # Return single item
    commarea.single_item = item
    commarea.response_message = f"RETURNED ITEM: REF ={commarea.item_ref_req}"


def place_order(commarea: Commarea, vsam_file: VSAMFileHandler, cics_ops: CICSOperations):
    """PLACE-ORDER: Update stock levels."""
    commarea.response_message = "PLACE-ORDER"
    
    # Check quantity validity
    if commarea.quantity_req <= 0:
        commarea.return_code = RETURN_CODE_INVALID_QUANTITY
        commarea.response_message = "ORDER QUANTITY MUST BE POSITIVE"
        return
    
    # Read for update
    resp_code, item = vsam_file.read_update(commarea.item_ref_number)
    
    if resp_code == DFHRESP_NOTFND:
        commarea.return_code = RETURN_CODE_NOT_FOUND
        commarea.response_message = f"ITEM - {commarea.item_ref_number} NOT FOUND"
        return
    elif resp_code != DFHRESP_NORMAL:
        commarea.return_code = RETURN_CODE_FILE_ERROR
        commarea.response_message = "ERROR OCCURED READING FILE"
        return
    
    # Update file
    update_file(commarea, vsam_file, item)


def update_file(commarea: Commarea, vsam_file: VSAMFileHandler, item: CatalogItem):
    """UPDATE-FILE: Update stock and rewrite."""
    # Check stock availability
    if commarea.quantity_req > item.in_stock:
        commarea.return_code = RETURN_CODE_INSUFFICIENT_STOCK
        commarea.response_message = "INSUFFICENT STOCK TO COMPLETE ORDER"
        vsam_file.unlock(commarea.item_ref_number)
        return
    
    # Update stock
    item.in_stock -= commarea.quantity_req
    
    # Rewrite record
    resp_code = vsam_file.rewrite(commarea.item_ref_number, item)
    
    if resp_code == DFHRESP_NORMAL:
        commarea.response_message = "ORDER SUCCESSFULLY PLACED"
    else:
        commarea.return_code = RETURN_CODE_UPDATE_ERROR
        commarea.response_message = "ERROR UPDATING FILE"
        return


def request_not_recognised(commarea: Commarea, cics_ops: CICSOperations):
    """REQUEST-NOT-RECOGNISED: Handle unknown requests."""
    commarea.return_code = RETURN_CODE_UNKNOWN_REQUEST
    commarea.response_message = "OPERATION UNKNOWN"
    
    # Write error message
    error_msg = ErrorMessage()
    date_str, time_str = cics_ops.get_current_time()
    error_msg.date = date_str
    error_msg.time = time_str
    error_msg.request_id = commarea.request_id
    error_msg.detail = f" UNKNOWN REQUEST ID RECEIVED - {commarea.request_id}"
    cics_ops.write_error_message(error_msg)


if __name__ == "__main__":
    # For testing/standalone execution
    commarea = Commarea(
        request_id="01INQC",
        list_start_ref="0001"
    )
    result = main(commarea, eibcalen=100)
    print(f"Return Code: {result.return_code}")
    print(f"Response: {result.response_message}")

