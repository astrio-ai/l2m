"""
Python Translation of CSCVDLTI.cbl

Program: CSCVDLTI
Type: IMS DL/I Program
Function: DELETE operation via z/OS Connect EE API

This program:
1. Receives DELETE request via DL/I GET-UNIQUE
2. Calls z/OS Connect EE API via BAQCSTUB
3. Returns response via DL/I ISRT
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, Tuple, Dict
from datetime import datetime


# ============================================================================
# Constants
# ============================================================================

GET_UNIQUE = "GU  "
ISRT = "ISRT"
NORMAL = "  "
COMM_STUB_PGM_NAME = "BAQCSTUB"

# BAQ Return Codes
BAQ_SUCCESS = "BAQ-SUCCESS"
BAQ_ERROR_IN_API = "BAQ-ERROR-IN-API"
BAQ_ERROR_IN_ZCEE = "BAQ-ERROR-IN-ZCEE"
BAQ_ERROR_IN_STUB = "BAQ-ERROR-IN-STUB"


# ============================================================================
# Input Buffer Structure
# ============================================================================

@dataclass
class InBuffer:
    """Input buffer structure (IN-BUFFER)."""
    ll: int = 0  # Length field
    zz: int = 0  # Reserved field
    trcd: str = ""  # Transaction code (10 chars)
    numb: str = ""  # Number (6 chars)
    name: str = ""  # Name (20 chars)
    addrx: str = ""  # Address (20 chars)
    phone: str = ""  # Phone (8 chars)
    datex: str = ""  # Date (8 chars)
    amount: str = ""  # Amount (8 chars)
    
    def to_bytes(self) -> bytes:
        """Convert to byte representation (for simulation)."""
        # Not used in Python version, but kept for compatibility
        return b""


# ============================================================================
# Output Buffer Structure
# ============================================================================

@dataclass
class OutBuffer:
    """Output buffer structure (OUT-BUFFER)."""
    ll: int = 400  # Length field
    zz: int = 0  # Reserved field
    numb: str = ""  # Number (6 chars)
    name: str = ""  # Name (20 chars)
    addrx: str = ""  # Address (20 chars)
    phone: str = ""  # Phone (8 chars)
    datex: str = ""  # Date (8 chars)
    amount: str = ""  # Amount (8 chars)
    userid: str = ""  # User ID (8 chars)
    httpcode: int = 0  # HTTP code (10 digits)
    msg1: str = ""  # Message 1 (75 chars)
    msg2: str = ""  # Message 2 (75 chars)
    msg3: str = ""  # Message 3 (75 chars)
    msg4: str = ""  # Message 4 (75 chars)
    segno: int = 0  # Segment number


# ============================================================================
# IOPCB Structure
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


# ============================================================================
# ALTPCB Structure
# ============================================================================

@dataclass
class ALTPCB:
    """Alternate Program Communication Block."""
    dbd_name: str = ""  # 8 chars
    seg_level: str = ""  # 2 chars
    alt_status: str = ""  # 2 chars


# ============================================================================
# DELETE Request Structure (IMS02Q01)
# ============================================================================

class DeleteRequest:
    """DELETE request structure (IMS02Q01)."""
    def __init__(self, employee: str = "", employee_length: int = 0):
        self._employee = employee
        self._employee_length = employee_length if employee_length > 0 else len(employee)
    
    @property
    def employee(self) -> str:
        """Get employee number."""
        return self._employee
    
    @employee.setter
    def employee(self, value: str):
        """Set employee number and update length."""
        self._employee = value
        self._employee_length = len(value) if value else 0
    
    @property
    def employee_length(self) -> int:
        """Get employee length."""
        return self._employee_length
    
    @employee_length.setter
    def employee_length(self, value: int):
        """Set employee length."""
        self._employee_length = value


# ============================================================================
# DELETE Response Structure (IMS02P01)
# ============================================================================

@dataclass
class DeleteResponse:
    """DELETE response structure (IMS02P01)."""
    userid2: str = ""  # User ID from response
    # Other response fields would be here


# ============================================================================
# DELETE Info Structure (IMS02I01)
# ============================================================================

@dataclass
class DeleteInfoOper1:
    """DELETE info structure (IMS02I01)."""
    operation_name: str = "DELETE"
    # Other info fields would be here


# ============================================================================
# BAQ Request/Response Info (BAQRINFO)
# ============================================================================

@dataclass
class BAQRequestInfo:
    """BAQ request info structure."""
    api_name: str = ""
    api_version: str = ""
    # Other request info fields


@dataclass
class BAQResponseInfo:
    """BAQ response info structure."""
    return_code: str = BAQ_SUCCESS
    status_code: int = 200
    status_message: str = ""
    
    def is_success(self) -> bool:
        """Check if return code indicates success."""
        return self.return_code == BAQ_SUCCESS
    
    def is_error_in_api(self) -> bool:
        """Check if error is in API."""
        return self.return_code == BAQ_ERROR_IN_API
    
    def is_error_in_zcee(self) -> bool:
        """Check if error is in zCEE."""
        return self.return_code == BAQ_ERROR_IN_ZCEE
    
    def is_error_in_stub(self) -> bool:
        """Check if error is in stub."""
        return self.return_code == BAQ_ERROR_IN_STUB


# ============================================================================
# Error Message Structure
# ============================================================================

@dataclass
class ErrorMsg:
    """Error message structure."""
    origin: str = ""  # EM-ORIGIN (8 chars)
    code: int = 0  # EM-CODE
    detail: str = ""  # EM-DETAIL (1024 chars)
    timestamp: str = ""  # TIMESTAMP (14 chars)


# ============================================================================
# DL/I Operations Simulation
# ============================================================================

def cbltdli_get_unique(iopcb: IOPCB, in_buffer: InBuffer) -> str:
    """Simulate DL/I GET-UNIQUE call.
    
    Args:
        iopcb: I/O PCB
        in_buffer: Input buffer
    
    Returns:
        Status code
    """
    # In real IMS, this would call CBLTDLI
    # For simulation, we'll return NORMAL
    iopcb.io_status = NORMAL
    return NORMAL


def cbltdli_isrt(iopcb: IOPCB, out_buffer: OutBuffer, modname: str) -> str:
    """Simulate DL/I ISRT call.
    
    Args:
        iopcb: I/O PCB
        out_buffer: Output buffer
        modname: Module name
    
    Returns:
        Status code
    """
    # In real IMS, this would call CBLTDLI
    # For simulation, we'll return NORMAL
    iopcb.io_status = NORMAL
    return NORMAL


# ============================================================================
# BAQCSTUB Communication Stub Simulation
# ============================================================================

def baqcstub(
    delete_info: DeleteInfoOper1,
    baq_request_info: BAQRequestInfo,
    baq_request_ptr: bytes,
    baq_request_len: int,
    baq_response_info: BAQResponseInfo,
    baq_response_ptr: bytes,
    baq_response_len: int,
    delete_request: DeleteRequest,
    delete_response: DeleteResponse
) -> BAQResponseInfo:
    """Simulate BAQCSTUB communication stub call.
    
    This is a mock implementation. In a real z/OS environment, this would
    call the actual BAQCSTUB to communicate with z/OS Connect EE.
    
    Args:
        delete_info: DELETE info structure
        baq_request_info: BAQ request info
        baq_request_ptr: Request pointer (not used in Python)
        baq_request_len: Request length
        baq_response_info: BAQ response info (output)
        baq_response_ptr: Response pointer (not used in Python)
        baq_response_len: Response length
        delete_request: DELETE request
        delete_response: DELETE response (output)
    
    Returns:
        BAQResponseInfo with return code and status
    """
    # Mock implementation: Simulate successful DELETE
    # In real system, this would call z/OS Connect EE API
    
    if not delete_request.employee:
        baq_response_info.return_code = BAQ_ERROR_IN_API
        baq_response_info.status_code = 400
        baq_response_info.status_message = "Employee number is required"
        return baq_response_info
    
    # Simulate successful DELETE
    baq_response_info.return_code = BAQ_SUCCESS
    baq_response_info.status_code = 200
    baq_response_info.status_message = "DELETE successful"
    delete_response.userid2 = "SYSTEM"  # Mock user ID
    
    return baq_response_info


# ============================================================================
# Main Program
# ============================================================================

def main(
    iopcb: Optional[IOPCB] = None,
    altpcb: Optional[ALTPCB] = None,
    in_buffer: Optional[InBuffer] = None,
    mock_baqcstub: Optional[callable] = None
) -> Tuple[OutBuffer, str, ErrorMsg]:
    """Main program entry point (MAINLINE).
    
    Args:
        iopcb: I/O PCB (created if None)
        altpcb: Alternate PCB (created if None)
        in_buffer: Input buffer (created if None)
        mock_baqcstub: Optional mock function for BAQCSTUB (for testing)
    
    Returns:
        Tuple of (out_buffer, status_code, error_msg)
    """
    # Initialize structures
    if iopcb is None:
        iopcb = IOPCB()
    if altpcb is None:
        altpcb = ALTPCB()
    if in_buffer is None:
        in_buffer = InBuffer()
    
    # Initialize error message
    error_msg = ErrorMsg()
    timestamp = datetime.now().strftime("%Y%m%d%H%M%S")
    error_msg.timestamp = timestamp
    
    # Initialize output buffer
    out_buffer = OutBuffer()
    segno = 0
    
    # Initialize DELETE request/response
    delete_request = DeleteRequest()
    delete_response = DeleteResponse()
    delete_info = DeleteInfoOper1()
    
    # Initialize BAQ structures
    baq_request_info = BAQRequestInfo()
    baq_response_info = BAQResponseInfo()
    
    # Simulate DL/I GET-UNIQUE
    status = cbltdli_get_unique(iopcb, in_buffer)
    
    print(f"{timestamp} IOPCB ModNAME: {iopcb.modname}")
    print(f"{timestamp} IOPCB Userid: {iopcb.userid}")
    print(f"{timestamp} IN-LL: {in_buffer.ll}")
    print(f"{timestamp} IN-BUFFER: {in_buffer}")
    
    # Set up DELETE request
    delete_request.employee = in_buffer.numb
    delete_request.employee_length = len(in_buffer.numb) if in_buffer.numb else 0
    
    # Copy NUMB to output buffer
    out_buffer.numb = in_buffer.numb
    
    # Set up pointers and lengths (simulated)
    baq_request_ptr = b""  # Not used in Python
    baq_request_len = len(str(delete_request))
    baq_response_ptr = b""  # Not used in Python
    baq_response_len = len(str(delete_response))
    
    # Call BAQCSTUB (or mock)
    if mock_baqcstub:
        baq_response_info = mock_baqcstub(
            delete_info,
            baq_request_info,
            baq_request_ptr,
            baq_request_len,
            baq_response_info,
            baq_response_ptr,
            baq_response_len,
            delete_request,
            delete_response
        )
    else:
        baq_response_info = baqcstub(
            delete_info,
            baq_request_info,
            baq_request_ptr,
            baq_request_len,
            baq_response_info,
            baq_response_ptr,
            baq_response_len,
            delete_request,
            delete_response
        )
    
    # Handle MODNAME change
    if iopcb.modname == "CSCVDLT":
        iopcb.modname = "CSCODLT"
    
    # Process response
    if baq_response_info.is_success():
        # Success - copy user ID from response
        out_buffer.userid = delete_response.userid2
    else:
        # Error - populate error messages
        status_msg = baq_response_info.status_message
        out_buffer.msg1 = status_msg[:75] if len(status_msg) > 0 else ""
        out_buffer.msg2 = status_msg[75:150] if len(status_msg) > 75 else ""
        out_buffer.msg3 = status_msg[150:225] if len(status_msg) > 150 else ""
        out_buffer.msg4 = status_msg[225:300] if len(status_msg) > 225 else ""
        
        error_msg.code = baq_response_info.status_code
        error_msg.detail = status_msg
        
        # Determine error origin
        if baq_response_info.is_error_in_api():
            error_msg.origin = "API"
        elif baq_response_info.is_error_in_zcee():
            error_msg.origin = "ZCEE"
        elif baq_response_info.is_error_in_stub():
            error_msg.origin = "STUB"
        
        print(f"{timestamp} Error code: {baq_response_info.status_code}")
        print(f"{timestamp} Error msg: {status_msg}")
        print(f"{timestamp} Error origin: {error_msg.origin}")
    
    # Set HTTP code
    print(f"{timestamp} HTTP CODE: {baq_response_info.status_code}")
    out_buffer.httpcode = baq_response_info.status_code
    
    # Increment segment number
    segno += 1
    out_buffer.segno = segno
    
    # Simulate DL/I ISRT
    status = cbltdli_isrt(iopcb, out_buffer, iopcb.modname)
    
    return out_buffer, status, error_msg


if __name__ == "__main__":
    # For testing/standalone execution
    iopcb = IOPCB(modname="CSCVDLT", userid="TESTUSER")
    in_buffer = InBuffer(
        ll=100,
        numb="000001",
        name="John Doe"
    )
    out_buffer, status, error_msg = main(iopcb=iopcb, in_buffer=in_buffer)
    print(f"Status: {status}")
    print(f"Output: {out_buffer}")

