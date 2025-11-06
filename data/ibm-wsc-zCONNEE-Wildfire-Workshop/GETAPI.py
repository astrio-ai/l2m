"""
Python Translation of GETAPI.cbl

Program: GETAPI
Type: CICS Program
Function: GET operation via z/OS Connect EE API

This program:
1. Receives GET request via PARM-BUFFER
2. Calls z/OS Connect EE API via BAQCSTUB
3. Returns status code as return code
"""

import sys
from dataclasses import dataclass
from typing import Optional, Tuple
from datetime import datetime

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
    employee: str = ""  # employee in PARM-DATA
    filler: str = ""  # Remaining 250 bytes


# ============================================================================
# GET Request Structure (CSC01Q01)
# ============================================================================

@dataclass
class GetRequest:
    """GET request structure (CSC01Q01)."""
    employee: str = ""
    employee_length: int = 0
    
    def set_employee(self, value: str):
        """Set employee number and update length."""
        self.employee = value
        self.employee_length = len(value) if value else 0


# ============================================================================
# GET Response Structure (CSC01P01)
# ============================================================================

@dataclass
class GetResponse:
    """GET response structure (CSC01P01)."""
    employee_number2: str = ""  # employeeNumber2 in COBOL
    name2: str = ""  # name2 in COBOL
    xaddress2: str = ""  # Xaddress2 in COBOL
    phone_number2: str = ""  # phoneNumber2 in COBOL
    xdate2: str = ""  # Xdate2 in COBOL
    amount2: str = ""  # amount2 in COBOL
    ceibresp: str = ""  # EIBRESP equivalent
    ceibresp2: str = ""  # EIBRESP2 equivalent
    userid2: str = ""  # USERID2 in COBOL


# ============================================================================
# GET Info Structure (CSC01I01)
# ============================================================================

@dataclass
class GetInfoOper1:
    """GET info structure (CSC01I01)."""
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
    
    # Set response fields with mock data
    get_response.employee_number2 = get_request.employee
    get_response.name2 = "John Doe"
    get_response.xaddress2 = "123 Main St"
    get_response.phone_number2 = "5551234"
    get_response.xdate2 = "20250101"
    get_response.amount2 = "100.00"
    get_response.ceibresp = "DFHRESP(NORMAL)"
    get_response.ceibresp2 = "DFHRESP2(NORMAL)"
    get_response.userid2 = "SYSTEM"
    
    return baq_response_info


# ============================================================================
# Main Program
# ============================================================================

def main(
    parm_buffer: Optional[ParmBuffer] = None,
    mock_baqcstub: Optional[callable] = None
) -> Tuple[int, ErrorMsg]:
    """Main program entry point (MAINLINE).
    
    Args:
        parm_buffer: PARM buffer with employee number (created if None)
        mock_baqcstub: Optional mock function for BAQCSTUB (for testing)
    
    Returns:
        Tuple of (return_code, error_msg)
    """
    # Initialize structures
    if parm_buffer is None:
        parm_buffer = ParmBuffer()
    
    # Initialize error message
    error_msg = ErrorMsg()
    
    # Initialize GET request/response
    get_request = GetRequest()
    get_response = GetResponse()
    get_info = GetInfoOper1()
    
    # Initialize BAQ structures
    baq_request_info = BAQRequestInfo()
    baq_response_info = BAQResponseInfo()
    
    # Initialize EIBRESP fields
    eibresp = ""
    eibresp2 = ""
    
    # Set up GET request from PARM buffer
    get_request.set_employee(parm_buffer.employee)
    
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
    
    # Process response
    if baq_response_info.is_success():
        # Success - display all fields
        eibresp = get_response.ceibresp
        eibresp2 = get_response.ceibresp2
        
        print(f"EmployeeNumber: {get_response.employee_number2}")
        print(f"EmployeeName:   {get_response.name2}")
        print(f"Address:        {get_response.xaddress2}")
        print(f"Phone:          {get_response.phone_number2}")
        print(f"Date:           {get_response.xdate2}")
        print(f"Amount:         {get_response.amount2}")
        print(f"EIBRESP:        {eibresp}")
        print(f"EIBRESP2:       {eibresp2}")
        print(f"USERID:         {get_response.userid2}")
        print(f"HTTP CODE:      {baq_response_info.status_code}")
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
        employee="000001"
    )
    return_code, error_msg = main(parm_buffer=parm_buffer)
    print(f"Return Code: {return_code}")

