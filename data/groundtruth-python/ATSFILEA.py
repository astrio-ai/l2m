"""
Python Translation of ATSFILEA.cbl

Program: ATSFILEA
Type: WOLA (WebSphere on z/OS Liberty) Service
Function: VSAM file operations via REST API (GET, POST, PUT, DELETE)

This program registers with a WOLA daemon and provides REST operations
on a VSAM indexed file.
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, Tuple, Dict, List
import os
import json


# ============================================================================
# FileA Record Structure
# ============================================================================

@dataclass
class FileARecord:
    """FileA VSAM record structure."""
    stat: str = ""  # 1 char
    numb: str = ""  # 6 chars (key)
    name: str = ""  # 20 chars
    addrx: str = ""  # 20 chars
    phone: str = ""  # 8 chars
    datex: str = ""  # 8 chars
    amount: str = ""  # 8 chars
    comment: str = ""  # 9 chars
    
    def to_string(self) -> str:
        """Convert record to fixed-width string (80 chars total)."""
        return (
            f"{self.stat:1s}"
            f"{self.numb:6s}"
            f"{self.name:20s}"
            f"{self.addrx:20s}"
            f"{self.phone:8s}"
            f"{self.datex:8s}"
            f"{self.amount:8s}"
            f"{self.comment:9s}"
        )[:80].ljust(80)
    
    @classmethod
    def from_string(cls, record_str: str) -> 'FileARecord':
        """Create record from fixed-width string."""
        record_str = record_str.ljust(80)
        return cls(
            stat=record_str[0:1].strip(),
            numb=record_str[1:7].strip(),
            name=record_str[7:27].strip(),
            addrx=record_str[27:47].strip(),
            phone=record_str[47:55].strip(),
            datex=record_str[55:63].strip(),
            amount=record_str[63:71].strip(),
            comment=record_str[71:80].strip()
        )
    
    def to_dict(self) -> dict:
        """Convert to dictionary."""
        return {
            'stat': self.stat,
            'numb': self.numb,
            'name': self.name,
            'addrx': self.addrx,
            'phone': self.phone,
            'datex': self.datex,
            'amount': self.amount,
            'comment': self.comment
        }
    
    @classmethod
    def from_dict(cls, data: dict) -> 'FileARecord':
        """Create from dictionary."""
        return cls(
            stat=data.get('stat', ''),
            numb=data.get('numb', ''),
            name=data.get('name', ''),
            addrx=data.get('addrx', ''),
            phone=data.get('phone', ''),
            datex=data.get('datex', ''),
            amount=data.get('amount', ''),
            comment=data.get('comment', '')
        )


# ============================================================================
# Request/Response Data Structures
# ============================================================================

@dataclass
class RequestData:
    """Request data structure (FILEAREQ)."""
    request_type: str = ""  # HTTP verb: 'G' (GET), 'P' (POST), 'U' (PUT), 'D' (DELETE)
    stat: str = ""
    numb: str = ""
    name: str = ""
    addrx: str = ""
    phone: str = ""
    datex: str = ""
    amount: str = ""
    comment: str = ""
    
    def to_filea_record(self) -> FileARecord:
        """Convert to FileA record."""
        return FileARecord(
            stat=self.stat,
            numb=self.numb,
            name=self.name,
            addrx=self.addrx,
            phone=self.phone,
            datex=self.datex,
            amount=self.amount,
            comment=self.comment
        )
    
    @classmethod
    def from_filea_record(cls, record: FileARecord, request_type: str = "") -> 'RequestData':
        """Create from FileA record."""
        return cls(
            request_type=request_type,
            stat=record.stat,
            numb=record.numb,
            name=record.name,
            addrx=record.addrx,
            phone=record.phone,
            datex=record.datex,
            amount=record.amount,
            comment=record.comment
        )


@dataclass
class ResponseData:
    """Response data structure (FILEARSP)."""
    vsam_status_code: str = "00"  # VSAM status code
    results_message: str = ""  # Results message
    stat: str = ""
    numb: str = ""
    name: str = ""
    addrx: str = ""
    phone: str = ""
    datex: str = ""
    amount: str = ""
    comment: str = ""
    
    @classmethod
    def from_filea_record(cls, record: FileARecord, status_code: str = "00", message: str = "") -> 'ResponseData':
        """Create from FileA record."""
        return cls(
            vsam_status_code=status_code,
            results_message=message,
            stat=record.stat,
            numb=record.numb,
            name=record.name,
            addrx=record.addrx,
            phone=record.phone,
            datex=record.datex,
            amount=record.amount,
            comment=record.comment
        )


# ============================================================================
# VSAM File Handler
# ============================================================================

class FileAVSAMHandler:
    """Handler for FileA VSAM indexed file operations."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.status_code: str = "00"
        self.extended_return_code: int = 0
        self.extended_function_code: int = 0
        self.extended_feedback_code: int = 0
        self._data: Dict[str, FileARecord] = {}  # Key: numb
        self._sorted_keys: List[str] = []
        self._file_open: bool = False
    
    def open_file(self, mode: str = "I-O") -> bool:
        """Open VSAM file (I-O mode for read/write).
        
        Args:
            mode: File mode ('I-O' for input-output)
        
        Returns:
            True if successful, False otherwise
        """
        try:
            # Load existing data if file exists
            if os.path.exists(self.filename):
                with open(self.filename, "r", encoding='utf-8') as f:
                    for line in f:
                        line = line.rstrip('\n\r')
                        if line:
                            record = FileARecord.from_string(line)
                            if record.numb:
                                self._data[record.numb] = record
            
            self._sorted_keys = sorted(self._data.keys())
            self.status_code = "00"
            self._file_open = True
            return True
        except Exception as e:
            self.status_code = "92"  # I/O error
            self._file_open = False
            return False
    
    def close_file(self):
        """Close VSAM file and save data."""
        if not self._file_open:
            return
        
        try:
            with open(self.filename, "w", encoding='utf-8') as f:
                for key in self._sorted_keys:
                    record = self._data[key]
                    f.write(record.to_string() + '\n')
            self._file_open = False
            self.status_code = "00"
        except Exception as e:
            self.status_code = "92"
    
    def read_record(self, key: str) -> Optional[FileARecord]:
        """Read record by key.
        
        Args:
            key: Record key (numb field)
        
        Returns:
            FileARecord if found, None otherwise
        """
        if not self._file_open:
            self.status_code = "92"
            return None
        
        key = key.strip()
        if key in self._data:
            self.status_code = "00"
            return self._data[key]
        else:
            self.status_code = "23"  # NOTFOUND
            return None
    
    def write_record(self, record: FileARecord) -> bool:
        """Write new record.
        
        Args:
            record: FileARecord to write
        
        Returns:
            True if successful, False otherwise
        """
        if not self._file_open:
            self.status_code = "92"
            return False
        
        key = record.numb.strip()
        if not key:
            self.status_code = "92"
            return False
        
        if key in self._data:
            self.status_code = "22"  # DUPLICATE
            return False
        
        self._data[key] = record
        self._sorted_keys = sorted(self._data.keys())
        self.status_code = "00"
        return True
    
    def rewrite_record(self, record: FileARecord) -> bool:
        """Rewrite existing record.
        
        Args:
            record: FileARecord to update
        
        Returns:
            True if successful, False otherwise
        """
        if not self._file_open:
            self.status_code = "92"
            return False
        
        key = record.numb.strip()
        if not key:
            self.status_code = "92"
            return False
        
        if key not in self._data:
            self.status_code = "23"  # NOTFOUND
            return False
        
        self._data[key] = record
        self.status_code = "00"
        return True
    
    def delete_record(self, key: str) -> bool:
        """Delete record by key.
        
        Args:
            key: Record key (numb field)
        
        Returns:
            True if successful, False otherwise
        """
        if not self._file_open:
            self.status_code = "92"
            return False
        
        key = key.strip()
        if key not in self._data:
            self.status_code = "23"  # NOTFOUND
            return False
        
        del self._data[key]
        self._sorted_keys = sorted(self._data.keys())
        self.status_code = "00"
        return True
    
    def clear_records(self):
        """Clear all records (for testing)."""
        self._data = {}
        self._sorted_keys = []
    
    def is_normal(self) -> bool:
        """Check if status is normal."""
        return self.status_code == "00"
    
    def is_duplicate(self) -> bool:
        """Check if status is duplicate."""
        return self.status_code == "22"
    
    def is_notfound(self) -> bool:
        """Check if status is not found."""
        return self.status_code == "23"


# ============================================================================
# WOLA API Simulation
# ============================================================================

class WOLARegistration:
    """WOLA registration structure."""
    
    def __init__(self):
        self.daemon_grp: str = ""
        self.node: str = ""
        self.svr_name: str = ""
        self.reg_name: str = ""
        self.min_conn: int = 1
        self.max_conn: int = 10
        self.flags: int = 0
        self.urg_flags: int = 0


class WOLAService:
    """WOLA service structure."""
    
    def __init__(self):
        self.service_name: str = ""
        self.service_name_length: int = 0
        self.rqst_data_addr: Optional[bytes] = None
        self.rqst_data_length: int = 0
        self.resp_data_addr: Optional[bytes] = None
        self.resp_data_length: int = 0
        self.connect_handle: str = ""
        self.wait_time: int = 0


class WOLAResponse:
    """WOLA API response."""
    
    def __init__(self):
        self.rc: int = 0  # Return code
        self.rsn: int = 0  # Reason code
        self.rv: int = 0  # Return value


# Mock WOLA API functions (for testing)
def bboa1reg(reg: WOLARegistration, rsp: WOLAResponse) -> int:
    """Register with WOLA daemon (BBOA1REG).
    
    Returns:
        Response code (0 = success)
    """
    # Mock implementation - in real system this would call the WOLA API
    rsp.rc = 0
    rsp.rsn = 0
    return 0


def bboa1srv(reg: WOLARegistration, svc: WOLAService, rsp: WOLAResponse) -> int:
    """Wait for service request (BBOA1SRV).
    
    Returns:
        Response code (0 = success)
    """
    # Mock implementation - in real system this would wait for requests
    rsp.rc = 0
    rsp.rsn = 0
    rsp.rv = 0
    return 0


def bboa1srp(svc: WOLAService, rsp: WOLAResponse) -> int:
    """Send response (BBOA1SRP).
    
    Returns:
        Response code (0 = success)
    """
    # Mock implementation
    rsp.rc = 0
    rsp.rsn = 0
    return 0


def bboa1cnr(svc: WOLAService, rsp: WOLAResponse) -> int:
    """Release connection (BBOA1CNR).
    
    Returns:
        Response code (0 = success)
    """
    # Mock implementation
    rsp.rc = 0
    rsp.rsn = 0
    return 0


def bboa1urg(reg: WOLARegistration, rsp: WOLAResponse) -> int:
    """Unregister service (BBOA1URG).
    
    Returns:
        Response code (0 = success)
    """
    # Mock implementation
    rsp.rc = 0
    rsp.rsn = 0
    return 0


# ============================================================================
# Main Program
# ============================================================================

def clear_fields(request_data: RequestData, response_data: ResponseData):
    """Clear request and response fields (CLEAR-FIELDS)."""
    # Initialize with low values (empty strings)
    request_data.request_type = ""
    request_data.stat = ""
    request_data.numb = ""
    request_data.name = ""
    request_data.addrx = ""
    request_data.phone = ""
    request_data.datex = ""
    request_data.amount = ""
    request_data.comment = ""
    
    response_data.vsam_status_code = "00"
    response_data.results_message = ""
    response_data.stat = ""
    response_data.numb = ""
    response_data.name = ""
    response_data.addrx = ""
    response_data.phone = ""
    response_data.datex = ""
    response_data.amount = ""
    response_data.comment = ""


def process_request(
    http_verb: str,
    request_data: RequestData,
    file_handler: FileAVSAMHandler
) -> Tuple[FileARecord, str, str]:
    """Process HTTP request and return result.
    
    Args:
        http_verb: HTTP verb ('G'=GET, 'P'=POST, 'U'=PUT, 'D'=DELETE)
        request_data: Request data
        file_handler: VSAM file handler
    
    Returns:
        Tuple of (FileARecord, status_code, message)
    """
    filea_record = request_data.to_filea_record()
    status_code = "00"
    message = ""
    
    if http_verb == 'U':  # PUT - Update
        # Read existing record
        existing = file_handler.read_record(filea_record.numb)
        if file_handler.is_notfound():
            message = 'PUT unsuccessful'
            status_code = file_handler.status_code
        else:
            # Update record
            file_handler.rewrite_record(filea_record)
            if file_handler.is_normal():
                message = 'PUT successful'
            else:
                message = 'PUT unsuccessful'
            status_code = file_handler.status_code
    
    elif http_verb == 'G':  # GET - Read
        record = file_handler.read_record(filea_record.numb)
        if file_handler.is_normal() and record:
            filea_record = record
            message = 'GET successful'
        else:
            message = 'No record found'
            status_code = file_handler.status_code
    
    elif http_verb == 'P':  # POST - Create
        file_handler.write_record(filea_record)
        if file_handler.is_normal():
            message = 'POST successful'
        elif file_handler.is_duplicate():
            message = 'Duplicate record'
        else:
            message = 'POST unsuccessful'
        status_code = file_handler.status_code
    
    elif http_verb == 'D':  # DELETE - Delete
        file_handler.delete_record(filea_record.numb)
        if file_handler.is_normal():
            message = 'DELETE successful'
        else:
            message = 'No record found'
            status_code = file_handler.status_code
    
    else:
        message = 'Unknown action'
        status_code = "99"
    
    return filea_record, status_code, message


def main(
    filea_filename: str = "FILEA",
    reg_name: str = "FILEAZCON",
    daemon_grp: str = "ZCEESRVR",
    node: str = "ZCEESRVR",
    svr_name: str = "ZCEESRVR",
    service_name: str = "ATSFILEA",
    stop_flag: bool = False,
    mock_request_callback = None
) -> int:
    """Main program entry point (MAIN-CONTROL).
    
    Args:
        filea_filename: VSAM file name
        reg_name: Registration name
        daemon_grp: Daemon group
        node: Node name
        svr_name: Server name
        service_name: Service name
        stop_flag: Stop flag (for testing)
        mock_request_callback: Optional callback to simulate requests (for testing)
    
    Returns:
        Exit code (0 = success)
    """
    # Initialize file handler
    file_handler = FileAVSAMHandler(filea_filename)
    
    # Open file
    if not file_handler.open_file():
        print(f"ERROR: Failed to open VSAM file {filea_filename}")
        print(f"VSAM status code: {file_handler.status_code}")
        return 1
    
    # Initialize WOLA registration
    reg = WOLARegistration()
    reg.reg_name = reg_name
    reg.daemon_grp = daemon_grp.replace(' ', '\x00')  # Convert spaces to low values
    reg.node = node
    reg.svr_name = svr_name
    reg.min_conn = 1
    reg.max_conn = 10
    reg.flags = 0
    
    # Initialize WOLA service
    svc = WOLAService()
    svc.service_name = service_name.replace(' ', '\x00')  # Convert spaces to low values
    svc.service_name_length = len(service_name)
    svc.wait_time = 0
    
    # Initialize response
    rsp = WOLAResponse()
    
    # Register with WOLA
    bboa1reg(reg, rsp)
    if rsp.rc > 0:
        print("ERROR: Call to BBOA1REG failed")
        print(f"Return Code = {rsp.rc}")
        print(f"Reason Code = {rsp.rsn}")
        file_handler.close_file()
        return 1
    
    print("======================================")
    print(f" Register Name : {reg_name}")
    print("======================================")
    print(" Successfully registered into")
    print(f" {daemon_grp} {node} {svr_name}")
    print("======================================")
    
    # Set request data length
    svc.rqst_data_length = 255  # Approximate length of REQUEST-DATA
    
    # Main service loop
    stop_loop = stop_flag
    request_count = 0
    
    while not stop_loop:
        # Initialize request/response
        request_data = RequestData()
        response_data = ResponseData()
        
        clear_fields(request_data, response_data)
        
        # If mock callback provided (for testing), use it
        if mock_request_callback:
            request_data = mock_request_callback(request_count)
            if request_data is None:
                stop_loop = True
                break
            request_count += 1
        else:
            # Wait for service request (BBOA1SRV)
            bboa1srv(reg, svc, rsp)
            if rsp.rc > 0:
                print("ERROR: Call to BBOA1SRV failed")
                stop_loop = True
                break
        
        print("")
        print(f" Service Name        : {service_name}")
        print(f" Data length         : {svc.rqst_data_length}")
        print(f" Return value length : {rsp.rv}")
        print("")
        
        # Process request
        http_verb = request_data.request_type
        filea_record, status_code, message = process_request(
            http_verb,
            request_data,
            file_handler
        )
        
        # Build response
        response_data = ResponseData.from_filea_record(
            filea_record,
            status_code,
            message
        )
        
        # Handle duplicate/notfound
        if file_handler.is_duplicate():
            response_data.results_message = 'Duplicate record'
        elif file_handler.is_notfound():
            response_data.results_message = 'No record found'
        
        print(response_data.results_message)
        print(f"STATUS CODE = {status_code}")
        
        # Set response data length
        svc.resp_data_length = 255  # Approximate length of RESPONSE-DATA
        
        # Send response (BBOA1SRP)
        if not mock_request_callback:
            bboa1srp(svc, rsp)
            if rsp.rc > 0:
                print("ERROR: Call to BBOA1SRP failed")
                stop_loop = True
                break
            
            # Release connection (BBOA1CNR)
            bboa1cnr(svc, rsp)
            if rsp.rc > 0:
                print("ERROR: Call to BBOA1CNR failed")
                stop_loop = True
                break
        
        # Check for stop condition
        if http_verb not in ['G', 'P', 'U', 'D']:
            print("-> Unknown action was specified")
            print("   Program will terminate ...")
            stop_loop = True
    
    # Close file
    file_handler.close_file()
    
    # Unregister service (BBOA1URG)
    bboa1urg(reg, rsp)
    if rsp.rc > 0:
        print("ERROR: Call to BBOA1URG failed")
        return 1
    
    print("")
    print(" Successfully unregistered from")
    print(f" {daemon_grp} {node} {svr_name}")
    print("")
    
    return 0

