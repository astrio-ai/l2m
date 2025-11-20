"""
Python Translation of POSTAPI.cbl

Program: POSTAPI
Type: CICS Program
Function: POST operation via z/OS Connect EE API

This program:
1. Receives POST request via PARM-BUFFER (employee number)
2. Sets up POST request with employee number and default values
3. Calls z/OS Connect EE API via BAQCSTUB
4. Displays response fields (EIBRESP, EIBRESP2, USERID, HTTP CODE)
5. Returns status code as return code
"""

import sys
from dataclasses import dataclass
from typing import Optional, Tuple

# Import common structures from CSCVDLTI
from CSCVDLTI import (
    BAQRequestInfo,
    BAQResponseInfo,
    ErrorMsg,
    BAQ_SUCCESS,
    BAQ_ERROR_IN_API,
    BAQ_ERROR_IN_ZCEE,
    BAQ_ERROR_IN_STUB,
)


# ============================================================================
# Constants
# ============================================================================

COMM_STUB_PGM_NAME = "BAQCSTUB"


# ============================================================================
# PARM Buffer Structure
# ============================================================================

@dataclass
class ParmBuffer:
    """PARM-BUFFER structure for CICS program parameters."""
    parm_length: int = 0
    employee_number: str = ""  # employeeNumber in PARM-DATA
    filler: str = ""  # Remaining 250 bytes


# ============================================================================
# POST Request Structure (CSC00Q01)
# ============================================================================

@dataclass
class PostRequest:
    """POST request structure (CSC00Q01)."""
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
    employee_number2: str = ""  # employeeNumber2 in POST-REQUEST
    employee_number2_length: int = 0  # employeeNumber2-length in POST-REQUEST
    name2: str = ""  # name2 in POST-REQUEST
    name2_length: int = 0  # name2-length in POST-REQUEST
    xaddress2: str = ""  # Xaddress2 in POST-REQUEST
    xaddress2_length: int = 0  # Xaddress2-length in POST-REQUEST
    phone_number2: str = ""  # phoneNumber2 in POST-REQUEST
    phone_number2_length: int = 0  # phoneNumber2-length in POST-REQUEST
    xdate2: str = ""  # Xdate2 in POST-REQUEST
    xdate2_length: int = 0  # Xdate2-length in POST-REQUEST
    amount2: str = ""  # amount2 in POST-REQUEST
    amount2_length: int = 0  # amount2-length in POST-REQUEST
    
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
# POST Response Structure (CSC00P01)
# ============================================================================

@dataclass
class PostResponse:
    """POST response structure (CSC00P01)."""
    ceibresp: str = ""  # CEIBRESP in POST-RESPONSE
    ceibresp2: str = ""  # CEIBRESP2 in POST-RESPONSE
    userid2: str = ""  # USERID2 in POST-RESPONSE (not USERID2, just USERID2)


# ============================================================================
# POST Info Structure (CSC00I01)
# ============================================================================

@dataclass
class PostInfoOper1:
    """POST info structure (CSC00I01)."""
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
    
    # Simulate successful POST - return response data
    # In real system, this would process the POST request
    baq_response_info.return_code = BAQ_SUCCESS
    baq_response_info.status_code = 200
    baq_response_info.status_message = "POST successful"
    
    # Set response fields with mock data
    post_response.ceibresp = "DFHRESP(NORMAL)"
    post_response.ceibresp2 = "DFHRESP2(NORMAL)"
    post_response.userid2 = "SYSTEM"
    
    return baq_response_info


# ============================================================================
# Main Program
# ============================================================================

def main(
    parm_buffer: Optional[ParmBuffer] = None,
    mock_baqcstub: Optional[callable] = None,
    custom_data: Optional[dict] = None
) -> Tuple[int, ErrorMsg]:
    """Main program entry point (MAINLINE).
    
    Args:
        parm_buffer: PARM buffer with employee number (created if None)
        mock_baqcstub: Optional mock function for BAQCSTUB (for testing)
        custom_data: Optional dict with custom data to use instead of defaults
                     Keys: name2, xaddress2, phone_number2, xdate2, amount2
    
    Returns:
        Tuple of (return_code, error_msg)
    """
    # Initialize structures
    if parm_buffer is None:
        parm_buffer = ParmBuffer()
    
    # Initialize error message
    error_msg = ErrorMsg()
    
    # Initialize POST request/response
    post_request = PostRequest()
    post_response = PostResponse()
    post_info = PostInfoOper1()
    
    # Initialize BAQ structures
    baq_request_info = BAQRequestInfo()
    baq_response_info = BAQResponseInfo()
    
    # Initialize EIBRESP fields
    eibresp = ""
    eibresp2 = ""
    
    # Set up the data for the API Requester call
    # Set all array indices to 1
    post_request.cscvinc_insert_service_op_num = 1
    post_request.request2_num = 1
    post_request.filea2_num = 1
    post_request.employee_number_num = 1
    post_request.name_num = 1
    post_request.xaddress_num = 1
    post_request.phone_number_num = 1
    post_request.xdate_num = 1
    post_request.amount_num = 1
    
    # Set employee number from PARM buffer
    post_request.set_employee_number2(parm_buffer.employee_number)
    
    # Set default values (from COBOL code) or use custom data
    if custom_data:
        name2 = custom_data.get("name2", "John")
        xaddress2 = custom_data.get("xaddress2", "Apex")
        phone_number2 = custom_data.get("phone_number2", "0065")
        xdate2 = custom_data.get("xdate2", "11 22 65")
        amount2 = custom_data.get("amount2", "$1000.65")
    else:
        name2 = "John"
        xaddress2 = "Apex"
        phone_number2 = "0065"
        xdate2 = "11 22 65"
        amount2 = "$1000.65"
    
    # Set all fields with auto-updating lengths
    post_request.set_name2(name2)
    post_request.set_xaddress2(xaddress2)
    post_request.set_phone_number2(phone_number2)
    post_request.set_xdate2(xdate2)
    post_request.set_amount2(amount2)
    
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
    
    # Process response
    if baq_response_info.is_success():
        # Success - display response fields
        eibresp = post_response.ceibresp
        eibresp2 = post_response.ceibresp2
        
        print(f"EIBRESP:    {eibresp}")
        print(f"EIBRESP2:   {eibresp2}")
        print(f"USERID:     {post_response.userid2}")
        print(f"HTTP CODE:  {baq_response_info.status_code}")
    else:
        # Error - populate error messages
        error_msg.code = baq_response_info.status_code
        error_msg.detail = baq_response_info.status_message
        
        # Determine error origin
        if baq_response_info.is_error_in_api():
            error_msg.origin = "API"
        elif baq_response_info.is_error_in_zcee():
            error_msg.origin = "ZCEE"
        elif baq_response_info.is_error_in_stub():
            error_msg.origin = "STUB"
        
        print(f"Error code: {baq_response_info.status_code}")
        print(f"Error msg: {baq_response_info.status_message}")
        print(f"Error origin: {error_msg.origin}")
    
    # Return BAQ-STATUS-CODE as return code
    return_code = baq_response_info.status_code
    
    return return_code, error_msg


if __name__ == "__main__":
    # For testing/standalone execution
    parm_buffer = ParmBuffer(
        parm_length=6,
        employee_number="000001"
    )
    return_code, error_msg = main(parm_buffer=parm_buffer)
    print(f"Return Code: {return_code}")

