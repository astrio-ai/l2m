"""
Python Translation of CSCVGETI.cbl

Program: CSCVGETI (Note: Program ID in COBOL is CSCVINCI, but handles GET)
Type: IMS DL/I Program
Function: GET operation via z/OS Connect EE API

This program:
1. Receives GET request via DL/I GET-UNIQUE
2. Calls z/OS Connect EE API via BAQCSTUB
3. Returns response via DL/I ISRT
"""

import sys
from dataclasses import dataclass
from typing import Optional, Tuple
from datetime import datetime

# Import common structures from CSCVDLTI
from CSCVDLTI import (
    InBuffer,
    OutBuffer,
    IOPCB,
    ALTPCB,
    BAQRequestInfo,
    BAQResponseInfo,
    ErrorMsg,
    cbltdli_get_unique,
    cbltdli_isrt,
    BAQ_SUCCESS,
    BAQ_ERROR_IN_API,
    BAQ_ERROR_IN_ZCEE,
    BAQ_ERROR_IN_STUB,
    NORMAL
)


# ============================================================================
# Constants
# ============================================================================

GET_UNIQUE = "GU  "
ISRT = "ISRT"
COMM_STUB_PGM_NAME = "BAQCSTUB"


# ============================================================================
# GET Request Structure (IMS00Q01)
# ============================================================================

class GetRequest:
    """GET request structure (IMS00Q01)."""
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
# GET Response Structure (IMS00P01)
# ============================================================================

@dataclass
class GetResponse:
    """GET response structure (IMS00P01)."""
    name2: str = ""  # Name from response
    address2: str = ""  # Address from response (Xaddress2 in COBOL)
    phoneNumber2: str = ""  # Phone number from response
    date2: str = ""  # Date from response (Xdate2 in COBOL)
    amount2: str = ""  # Amount from response
    userid2: str = ""  # User ID from response


# ============================================================================
# GET Info Structure (IMS00I01)
# ============================================================================

@dataclass
class GetInfoOper1:
    """GET info structure (IMS00I01)."""
    operation_name: str = "GET"
    # Other info fields would be here


# ============================================================================
# BAQCSTUB Communication Stub Simulation
# ============================================================================

def baqcstub(
    get_info: GetInfoOper1,
    baq_request_info: BAQRequestInfo,
    baq_request_ptr: bytes,
    baq_request_len: int,
    baq_response_info: BAQResponseInfo,
    baq_response_ptr: bytes,
    baq_response_len: int,
    get_request: GetRequest,
    get_response: GetResponse
) -> BAQResponseInfo:
    """Simulate BAQCSTUB communication stub call for GET operation.
    
    This is a mock implementation. In a real z/OS environment, this would
    call the actual BAQCSTUB to communicate with z/OS Connect EE.
    
    Args:
        get_info: GET info structure
        baq_request_info: BAQ request info
        baq_request_ptr: Request pointer (not used in Python)
        baq_request_len: Request length
        baq_response_info: BAQ response info (output)
        baq_response_ptr: Response pointer (not used in Python)
        baq_response_len: Response length
        get_request: GET request
        get_response: GET response (output)
    
    Returns:
        BAQResponseInfo with return code and status
    """
    # Mock implementation: Simulate successful GET
    # In real system, this would call z/OS Connect EE API
    
    if not get_request.employee:
        baq_response_info.return_code = BAQ_ERROR_IN_API
        baq_response_info.status_code = 400
        baq_response_info.status_message = "Employee number is required"
        return baq_response_info
    
    # Simulate successful GET - return employee data
    # In real system, this would retrieve data from the API
    baq_response_info.return_code = BAQ_SUCCESS
    baq_response_info.status_code = 200
    baq_response_info.status_message = "GET successful"
    
    # Mock response data
    get_response.name2 = "John Doe"
    get_response.address2 = "123 Main St"
    get_response.phoneNumber2 = "5551234"
    get_response.date2 = "20250101"
    get_response.amount2 = "100.00"
    get_response.userid2 = "SYSTEM"
    
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
    
    # Initialize GET request/response
    get_request = GetRequest()
    get_response = GetResponse()
    get_info = GetInfoOper1()
    
    # Initialize BAQ structures
    baq_request_info = BAQRequestInfo()
    baq_response_info = BAQResponseInfo()
    
    # Simulate DL/I GET-UNIQUE
    status = cbltdli_get_unique(iopcb, in_buffer)
    
    print(f"{timestamp} IOPCB ModNAME: {iopcb.modname}")
    print(f"{timestamp} IOPCB Userid: {iopcb.userid}")
    print(f"{timestamp} IN-LL: {in_buffer.ll}")
    print(f"{timestamp} IN-BUFFER: {in_buffer}")
    
    # Set up GET request
    get_request.employee = in_buffer.numb
    get_request.employee_length = len(in_buffer.numb) if in_buffer.numb else 0
    
    # Copy NUMB to output buffer
    out_buffer.numb = in_buffer.numb
    
    # Set up pointers and lengths (simulated)
    baq_request_ptr = b""  # Not used in Python
    baq_request_len = len(str(get_request))
    baq_response_ptr = b""  # Not used in Python
    baq_response_len = len(str(get_response))
    
    # Call BAQCSTUB (or mock)
    if mock_baqcstub:
        baq_response_info = mock_baqcstub(
            get_info,
            baq_request_info,
            baq_request_ptr,
            baq_request_len,
            baq_response_info,
            baq_response_ptr,
            baq_response_len,
            get_request,
            get_response
        )
    else:
        baq_response_info = baqcstub(
            get_info,
            baq_request_info,
            baq_request_ptr,
            baq_request_len,
            baq_response_info,
            baq_response_ptr,
            baq_response_len,
            get_request,
            get_response
        )
    
    # Handle MODNAME change
    if iopcb.modname == "CSCVGET":
        iopcb.modname = "CSCOGET"
    
    # Process response
    if baq_response_info.is_success():
        # Success - copy all fields from response to output buffer
        out_buffer.name = get_response.name2
        out_buffer.addrx = get_response.address2
        out_buffer.phone = get_response.phoneNumber2
        out_buffer.datex = get_response.date2
        out_buffer.amount = get_response.amount2
        out_buffer.userid = get_response.userid2
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
    iopcb = IOPCB(modname="CSCVGET", userid="TESTUSER")
    in_buffer = InBuffer(
        ll=100,
        numb="000001",
        name="John Doe"
    )
    out_buffer, status, error_msg = main(iopcb=iopcb, in_buffer=in_buffer)
    print(f"Status: {status}")
    print(f"Output: {out_buffer}")

