"""
Python Translation of CSCVINCI.cbl

Program: CSCVINCI (Note: Entry point comment says CSCVINCI, Program ID is CSCVINCI)
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

# Import GET structures from CSCVGETI
from CSCVGETI import (
    GetRequest,
    GetResponse,
    GetInfoOper1,
    baqcstub as baqcstub_get
)


# ============================================================================
# Constants
# ============================================================================

GET_UNIQUE = "GU  "
ISRT = "ISRT"
COMM_STUB_PGM_NAME = "BAQCSTUB"


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
        baq_response_info = baqcstub_get(
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
    
    # Handle MODNAME change (CSCRGET -> CSCOGET)
    if iopcb.modname == "CSCRGET":
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
    iopcb = IOPCB(modname="CSCRGET", userid="TESTUSER")
    in_buffer = InBuffer(
        ll=100,
        numb="000001",
        name="John Doe"
    )
    out_buffer, status, error_msg = main(iopcb=iopcb, in_buffer=in_buffer)
    print(f"Status: {status}")
    print(f"Output: {out_buffer}")

