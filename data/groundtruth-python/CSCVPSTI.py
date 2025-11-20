"""
Python Translation of CSCVPSTI.cbl

Program: CSCVPSTI
Type: IMS DL/I Program
Function: POST operation via z/OS Connect EE API

This program:
1. Receives POST request via DL/I GET-UNIQUE
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
# POST Request Structure (IMS03Q01)
# ============================================================================

@dataclass
class PostRequest:
    """POST request structure (IMS03Q01)."""
    # Array index/count fields (set to 1 in COBOL)
    cscvinc_insert_service_op_num: int = 1
    request2_num: int = 1
    filea2_num: int = 1
    employee_number_num: int = 1
    name_num: int = 1
    xaddress_num: int = 1
    phone_number_num: int = 1
    xdate_num: int = 1
    amount_num: int = 1
    
    # Data fields
    employee_number2: str = ""
    employee_number2_length: int = 0
    name2: str = ""
    name2_length: int = 0
    xaddress2: str = ""  # Xaddress2 in COBOL
    xaddress2_length: int = 0
    phone_number2: str = ""
    phone_number2_length: int = 0
    xdate2: str = ""  # Xdate2 in COBOL
    xdate2_length: int = 0  # xDate2-length in COBOL
    amount2: str = ""
    amount2_length: int = 0
    
    def set_employee_number2(self, value: str):
        """Set employee number and update length."""
        self.employee_number2 = value
        self.employee_number2_length = len(value) if value else 0
    
    def set_name2(self, value: str):
        """Set name and update length."""
        self.name2 = value
        self.name2_length = len(value) if value else 0
    
    def set_xaddress2(self, value: str):
        """Set address and update length."""
        self.xaddress2 = value
        self.xaddress2_length = len(value) if value else 0
    
    def set_phone_number2(self, value: str):
        """Set phone number and update length."""
        self.phone_number2 = value
        self.phone_number2_length = len(value) if value else 0
    
    def set_xdate2(self, value: str):
        """Set date and update length."""
        self.xdate2 = value
        self.xdate2_length = len(value) if value else 0
    
    def set_amount2(self, value: str):
        """Set amount and update length."""
        self.amount2 = value
        self.amount2_length = len(value) if value else 0


# ============================================================================
# POST Response Structure (IMS03P01)
# ============================================================================

@dataclass
class PostResponse:
    """POST response structure (IMS03P01)."""
    userid2: str = ""  # User ID from response (USERID2 in COBOL)


# ============================================================================
# POST Info Structure (IMS03I01)
# ============================================================================

@dataclass
class PostInfoOper1:
    """POST info structure (IMS03I01)."""
    operation_name: str = "POST"
    # Other info fields would be here


# ============================================================================
# BAQCSTUB Communication Stub Simulation
# ============================================================================

def baqcstub(
    post_info: PostInfoOper1,
    baq_request_info: BAQRequestInfo,
    baq_request_ptr: bytes,
    baq_request_len: int,
    baq_response_info: BAQResponseInfo,
    baq_response_ptr: bytes,
    baq_response_len: int,
    post_request: PostRequest,
    post_response: PostResponse
) -> BAQResponseInfo:
    """Simulate BAQCSTUB communication stub call for POST operation.
    
    This is a mock implementation. In a real z/OS environment, this would
    call the actual BAQCSTUB to communicate with z/OS Connect EE.
    
    Args:
        post_info: POST info structure
        baq_request_info: BAQ request info
        baq_request_ptr: Request pointer (not used in Python)
        baq_request_len: Request length
        baq_response_info: BAQ response info (output)
        baq_response_ptr: Response pointer (not used in Python)
        baq_response_len: Response length
        post_request: POST request
        post_response: POST response (output)
    
    Returns:
        BAQResponseInfo with return code and status
    """
    # Mock implementation: Simulate successful POST
    # In real system, this would call z/OS Connect EE API
    
    if not post_request.employee_number2:
        baq_response_info.return_code = BAQ_ERROR_IN_API
        baq_response_info.status_code = 400
        baq_response_info.status_message = "Employee number is required"
        return baq_response_info
    
    # Simulate successful POST - return user ID
    # In real system, this would insert data via the API
    baq_response_info.return_code = BAQ_SUCCESS
    baq_response_info.status_code = 201  # Created
    baq_response_info.status_message = "Record created successfully"
    
    # Set response user ID
    post_response.userid2 = "SYSTEM"  # Mock user ID
    
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
    
    # Initialize POST request/response
    post_request = PostRequest()
    post_response = PostResponse()
    post_info = PostInfoOper1()
    
    # Initialize BAQ structures
    baq_request_info = BAQRequestInfo()
    baq_response_info = BAQResponseInfo()
    
    # Simulate DL/I GET-UNIQUE
    status = cbltdli_get_unique(iopcb, in_buffer)
    
    print(f"{timestamp} IOPCB ModNAME: {iopcb.modname}")
    print(f"{timestamp} IOPCB Userid: {iopcb.userid}")
    print(f"{timestamp} IN-LL: {in_buffer.ll}")
    print(f"{timestamp} IN-BUFFER: {in_buffer}")
    
    # Copy NUMB to output buffer
    out_buffer.numb = in_buffer.numb
    
    # Set array index/count fields to 1
    post_request.cscvinc_insert_service_op_num = 1
    post_request.request2_num = 1
    post_request.filea2_num = 1
    post_request.employee_number_num = 1
    post_request.name_num = 1
    post_request.xaddress_num = 1
    post_request.phone_number_num = 1
    post_request.xdate_num = 1
    post_request.amount_num = 1
    
    # Set up POST request fields from input buffer
    post_request.set_employee_number2(in_buffer.numb)
    post_request.set_name2(in_buffer.name)
    post_request.set_xaddress2(in_buffer.addrx)
    post_request.set_phone_number2(in_buffer.phone)
    post_request.set_xdate2(in_buffer.datex)
    post_request.set_amount2(in_buffer.amount)
    
    # Set up pointers and lengths (simulated)
    baq_request_ptr = b""  # Not used in Python
    baq_request_len = len(str(post_request))
    baq_response_ptr = b""  # Not used in Python
    baq_response_len = len(str(post_response))
    
    # Call BAQCSTUB (or mock)
    if mock_baqcstub:
        baq_response_info = mock_baqcstub(
            post_info,
            baq_request_info,
            baq_request_ptr,
            baq_request_len,
            baq_response_info,
            baq_response_ptr,
            baq_response_len,
            post_request,
            post_response
        )
    else:
        baq_response_info = baqcstub(
            post_info,
            baq_request_info,
            baq_request_ptr,
            baq_request_len,
            baq_response_info,
            baq_response_ptr,
            baq_response_len,
            post_request,
            post_response
        )
    
    # Handle MODNAME change (CSCVPST -> CSCOPST)
    if iopcb.modname == "CSCVPST":
        iopcb.modname = "CSCOPST"
    
    # Process response
    if baq_response_info.is_success():
        # Success - copy USERID2 to output buffer
        out_buffer.userid = post_response.userid2
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
    iopcb = IOPCB(modname="CSCVPST", userid="TESTUSER")
    in_buffer = InBuffer(
        ll=100,
        numb="000001",
        name="John Doe",
        addrx="123 Main St",
        phone="5551234",
        datex="20250101",
        amount="100.00"
    )
    out_buffer, status, error_msg = main(iopcb=iopcb, in_buffer=in_buffer)
    print(f"Status: {status}")
    print(f"Output: {out_buffer}")

