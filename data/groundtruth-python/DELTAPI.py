"""
Python Translation of DELTAPI.cbl

Program: DELTAPI
Type: CICS Program
Function: DELETE operation via z/OS Connect EE API

This program:
1. Receives DELETE request via PARM-BUFFER
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
    employee_number: str = ""  # employeeNumber in COBOL
    filler: str = ""  # Remaining 250 bytes


# ============================================================================
# DELETE Request Structure (CSC03Q01)
# ============================================================================

@dataclass
class DeleteRequest:
    """DELETE request structure (CSC03Q01)."""
    # Array index/count fields (commented out in COBOL, but included for completeness)
    cscvinc_delete_service_op_num: int = 1
    request_container2_num: int = 1
    file_area2_num: int = 1
    employee_number_num: int = 1
    
    # Data fields
    employee: str = ""
    employee_length: int = 0
    
    def set_employee(self, value: str):
        """Set employee number and update length."""
        self.employee = value
        self.employee_length = len(value) if value else 0


# ============================================================================
# DELETE Response Structure (CSC03P01)
# ============================================================================

@dataclass
class DeleteResponse:
    """DELETE response structure (CSC03P01)."""
    ceibresp: str = ""  # EIBRESP equivalent
    ceibresp2: str = ""  # EIBRESP2 equivalent
    userid2: str = ""  # User ID from response


# ============================================================================
# DELETE Info Structure (CSC03I01)
# ============================================================================

@dataclass
class DeleteInfoOper1:
    """DELETE info structure (CSC03I01)."""
    operation_name: str = "DELETE"
    # Other info fields would be here


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
    """Simulate BAQCSTUB communication stub call for DELETE operation.
    
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
    
    # Simulate successful DELETE - return CICS response codes
    # In real system, this would delete data via the API
    baq_response_info.return_code = BAQ_SUCCESS
    baq_response_info.status_code = 200  # OK (deleted)
    baq_response_info.status_message = "Record deleted successfully"
    
    # Set response fields
    delete_response.ceibresp = "DFHRESP(NORMAL)"
    delete_response.ceibresp2 = "DFHRESP2(NORMAL)"
    delete_response.userid2 = "SYSTEM"  # Mock user ID
    
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
    
    # Initialize DELETE request/response
    delete_request = DeleteRequest()
    delete_response = DeleteResponse()
    delete_info = DeleteInfoOper1()
    
    # Initialize BAQ structures
    baq_request_info = BAQRequestInfo()
    baq_response_info = BAQResponseInfo()
    
    # Initialize EIBRESP fields
    eibresp = ""
    eibresp2 = ""
    
    # Set up DELETE request from PARM buffer
    delete_request.set_employee(parm_buffer.employee_number)
    
    # Set array index fields to 1 (as per commented COBOL code)
    delete_request.cscvinc_delete_service_op_num = 1
    delete_request.request_container2_num = 1
    delete_request.file_area2_num = 1
    delete_request.employee_number_num = 1
    
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
    
    # Process response
    if baq_response_info.is_success():
        # Success - copy CICS response codes
        eibresp = delete_response.ceibresp
        eibresp2 = delete_response.ceibresp2
        
        print(f"EIBRESP:    {eibresp}")
        print(f"EIBRESP2:   {eibresp2}")
        print(f"USERID:   {delete_response.userid2}")
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

