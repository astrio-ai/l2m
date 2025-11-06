"""
Python Translation of MQPUT.cbl

Program: MQPUT
Type: CICS Program
Function: PUT operation for MQ (Message Queue) via z/OS Connect EE API

This program:
1. Sets up PUT request data (numb2, name2, addrx2, phone2, datex2, amount2)
2. Calls z/OS Connect EE API via BAQCSTUB to put message to MQ
3. Displays HTTP status code on success
4. Returns status code as return code
"""

import sys
import os
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
# PUT Request Structure (MQ001Q01)
# ============================================================================

@dataclass
class PutRequest:
    """PUT request structure (MQ001Q01) for MQ message."""
    # Array index/count fields
    mqmessage2_num: int = 1
    
    # Data fields with lengths
    numb2: str = ""  # NUMB2 in PUT-REQUEST
    numb2_length: int = 0  # NUMB2-length in PUT-REQUEST
    name2: str = ""  # NAME2 in PUT-REQUEST
    name2_length: int = 0  # NAME2-length in PUT-REQUEST
    addrx2: str = ""  # ADDRX2 in PUT-REQUEST
    addrx2_length: int = 0  # ADDRX2-length in PUT-REQUEST
    phone2: str = ""  # PHONE2 in PUT-REQUEST
    phone2_length: int = 0  # PHONE2-length in PUT-REQUEST
    datex2: str = ""  # DATEX2 in PUT-REQUEST
    datex2_length: int = 0  # DATEX2-length in PUT-REQUEST
    amount2: str = ""  # AMOUNT2 in PUT-REQUEST
    amount2_length: int = 0  # AMOUNT2-length in PUT-REQUEST
    
    def set_numb2(self, value: str):
        """Set numb2 and update length."""
        self.numb2 = value
        self.numb2_length = len(value) if value else 0
    
    def set_name2(self, value: str):
        """Set name2 and update length."""
        self.name2 = value
        self.name2_length = len(value) if value else 0
    
    def set_addrx2(self, value: str):
        """Set addrx2 and update length."""
        self.addrx2 = value
        self.addrx2_length = len(value) if value else 0
    
    def set_phone2(self, value: str):
        """Set phone2 and update length."""
        self.phone2 = value
        self.phone2_length = len(value) if value else 0
    
    def set_datex2(self, value: str):
        """Set datex2 and update length."""
        self.datex2 = value
        self.datex2_length = len(value) if value else 0
    
    def set_amount2(self, value: str):
        """Set amount2 and update length."""
        self.amount2 = value
        self.amount2_length = len(value) if value else 0


# ============================================================================
# PUT Response Structure (Empty - just filler)
# ============================================================================

@dataclass
class PutResponse:
    """PUT response structure (essentially empty - just filler for MQ PUT)."""
    filler: str = ""  # FILLER PIC X(1) - empty response


# ============================================================================
# PUT Info Structure (MQ001I01)
# ============================================================================

@dataclass
class PutInfoOper1:
    """PUT info structure (MQ001I01)."""
    operation_name: str = "PUT"
    # Other info fields would be here


# ============================================================================
# BAQCSTUB Communication Stub Simulation
# ============================================================================

def baqcstub(
    put_info: PutInfoOper1,
    baq_request_info: BAQRequestInfo,
    baq_request_ptr: bytes,
    baq_request_len: int,
    baq_response_info: BAQResponseInfo,
    baq_response_ptr: bytes,
    baq_response_len: int,
    put_request: PutRequest,
    put_response: PutResponse
) -> BAQResponseInfo:
    """Simulate BAQCSTUB communication stub call for MQ PUT operation.
    
    This is a mock implementation. In a real z/OS environment, this would
    call the actual BAQCSTUB to communicate with z/OS Connect EE and
    put a message to MQ.
    
    Args:
        put_info: PUT info structure
        baq_request_info: BAQ request info
        baq_request_ptr: Request pointer (not used in Python)
        baq_request_len: Request length
        baq_response_info: BAQ response info (output)
        baq_response_ptr: Response pointer (not used in Python)
        baq_response_len: Response length
        put_request: PUT request
        put_response: PUT response (output, empty)
    
    Returns:
        BAQResponseInfo with return code and status
    """
    # Mock implementation: Simulate successful MQ PUT
    # In real system, this would put a message to MQ via z/OS Connect EE
    
    # Validate required fields
    if not put_request.numb2 or not put_request.name2:
        baq_response_info.return_code = BAQ_ERROR_IN_API
        baq_response_info.status_code = 400
        baq_response_info.status_message = "numb2 and name2 are required"
        return baq_response_info
    
    # Simulate successful PUT - message sent to MQ
    baq_response_info.return_code = BAQ_SUCCESS
    baq_response_info.status_code = 200
    baq_response_info.status_message = "MQ PUT successful"
    
    return baq_response_info


# ============================================================================
# Main Program
# ============================================================================

def main(
    mock_baqcstub: Optional[callable] = None,
    custom_data: Optional[dict] = None
) -> Tuple[int, ErrorMsg]:
    """Main program entry point (MAINLINE).
    
    Args:
        mock_baqcstub: Optional mock function for BAQCSTUB (for testing)
        custom_data: Optional dict with custom data to use instead of defaults
                     Keys: numb2, name2, addrx2, phone2, datex2, amount2
    
    Returns:
        Tuple of (return_code, error_msg)
    """
    # Initialize structures
    error_msg = ErrorMsg()
    
    # Initialize PUT request/response
    put_request = PutRequest()
    put_response = PutResponse()
    put_info = PutInfoOper1()
    
    # Initialize BAQ structures
    baq_request_info = BAQRequestInfo()
    baq_response_info = BAQResponseInfo()
    
    # Initialize working storage variables
    # Set up the data for the API Requester call
    put_request.mqmessage2_num = 1
    
    # Set default values (from COBOL code) or use custom data
    if custom_data:
        numb2 = custom_data.get("numb2", "837367")
        name2 = custom_data.get("name2", "John")
        addrx2 = custom_data.get("addrx2", "Apex")
        phone2 = custom_data.get("phone2", "0065")
        datex2 = custom_data.get("datex2", "11 22 65")
        amount2 = custom_data.get("amount2", "$1000.65")
    else:
        numb2 = "837367"
        name2 = "John"
        addrx2 = "Apex"
        phone2 = "0065"
        datex2 = "11 22 65"
        amount2 = "$1000.65"
    
    # Set all fields with auto-updating lengths
    put_request.set_numb2(numb2)
    put_request.set_name2(name2)
    put_request.set_addrx2(addrx2)
    put_request.set_phone2(phone2)
    put_request.set_datex2(datex2)
    put_request.set_amount2(amount2)
    
    # Set up pointers and lengths (simulated)
    baq_request_ptr = b""  # Not used in Python
    baq_request_len = len(str(put_request))
    baq_response_ptr = b""  # Not used in Python
    baq_response_len = 1  # Length of PUT-RESPONSE (1 byte filler)
    
    # Call BAQCSTUB (or mock)
    if mock_baqcstub:
        baq_response_info = mock_baqcstub(
            put_info,
            baq_request_info,
            baq_request_ptr,
            baq_request_len,
            baq_response_info,
            baq_response_ptr,
            baq_response_len,
            put_request,
            put_response
        )
    else:
        baq_response_info = baqcstub(
            put_info,
            baq_request_info,
            baq_request_ptr,
            baq_request_len,
            baq_response_info,
            baq_response_ptr,
            baq_response_len,
            put_request,
            put_response
        )
    
    # Process response
    if baq_response_info.is_success():
        # Success - display HTTP CODE
        print(f"HTTP CODE: {baq_response_info.status_code}")
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
    return_code, error_msg = main()
    print(f"Return Code: {return_code}")

