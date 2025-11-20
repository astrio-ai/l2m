"""
Python Translation of MQGET.cbl

Program: MQGET
Type: CICS Program
Function: GET operation for MQ (Message Queue) via z/OS Connect EE API

This program:
1. Receives GET request (empty request - just retrieves next message)
2. Calls z/OS Connect EE API via BAQCSTUB to get message from MQ
3. Displays message fields (numb, name, addrx, phone, datex, amount)
4. Returns status code as return code
"""

from dataclasses import dataclass
from typing import Callable, Optional, Tuple

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

StubCallable = Callable[
    [
        "GetInfoOper1",
        BAQRequestInfo,
        bytes,
        int,
        BAQResponseInfo,
        bytes,
        int,
        "GetRequest",
        "GetResponse",
    ],
    BAQResponseInfo,
]
OutputFunc = Callable[[str], None]


# ============================================================================
# GET Request Structure (Empty - just filler)
# ============================================================================

@dataclass
class GetRequest:
    """GET request structure (essentially empty - just filler for MQ GET)."""
    filler: str = ""  # FILLER PIC X(1) - empty request


# ============================================================================
# GET Response Structure (MQ000P01)
# ============================================================================

@dataclass
class GetResponse:
    """GET response structure (MQ000P01) for MQ message."""
    numb: str = ""  # NUMB in GET-RESPONSE
    name: str = ""  # NAME in GET-RESPONSE
    addrx: str = ""  # ADDRX in GET-RESPONSE
    phone: str = ""  # PHONE in GET-RESPONSE
    datex: str = ""  # DATEX in GET-RESPONSE
    amount: str = ""  # AMOUNT in GET-RESPONSE


# ============================================================================
# GET Info Structure (MQ000I01)
# ============================================================================

@dataclass
class GetInfoOper1:
    """GET info structure (MQ000I01)."""
    operation_name: str = "GET"
    # Other info fields would be here


# ============================================================================
# Result container
# ============================================================================

@dataclass
class MQGetResult:
    """Container for the outcome of an MQ GET request."""
    response: GetResponse
    baq_response_info: BAQResponseInfo
    error_msg: ErrorMsg


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
    """Simulate BAQCSTUB communication stub call for MQ GET operation.
    
    This is a mock implementation. In a real z/OS environment, this would
    call the actual BAQCSTUB to communicate with z/OS Connect EE and
    retrieve a message from MQ.
    
    Args:
        get_info: GET info structure
        baq_request_info: BAQ request info
        baq_request_ptr: Request pointer (not used in Python)
        baq_request_len: Request length
        baq_response_info: BAQ response info (output)
        baq_response_ptr: Response pointer (not used in Python)
        baq_response_len: Response length
        get_request: GET request (empty for MQ GET)
        get_response: GET response (output)
    
    Returns:
        BAQResponseInfo with return code and status
    """
    # Mock implementation: Simulate successful MQ GET
    # In real system, this would retrieve a message from MQ via z/OS Connect EE
    
    # Simulate successful GET - return mock message data
    # In real system, this would retrieve data from MQ
    baq_response_info.return_code = BAQ_SUCCESS
    baq_response_info.status_code = 200
    baq_response_info.status_message = "MQ GET successful"
    
    # Set response fields with mock data
    get_response.numb = "000001"
    get_response.name = "John Doe"
    get_response.addrx = "123 Main St"
    get_response.phone = "5551234"
    get_response.datex = "20250101"
    get_response.amount = "100.00"
    
    return baq_response_info


# ============================================================================
# Helper functions
# ============================================================================

def perform_mq_get(
    mock_baqcstub: Optional[StubCallable] = None,
) -> MQGetResult:
    """Execute the MQ GET call and return the raw structures."""
    error_msg = ErrorMsg()
    get_request = GetRequest()
    get_response = GetResponse()
    get_info = GetInfoOper1()
    baq_request_info = BAQRequestInfo()
    baq_response_info = BAQResponseInfo()
    
    baq_request_ptr = b""
    baq_request_len = max(len(get_request.filler), 1)
    baq_response_ptr = b""
    baq_response_len = len(str(get_response))
    
    stub = mock_baqcstub or baqcstub
    baq_response_info = stub(
        get_info,
        baq_request_info,
        baq_request_ptr,
        baq_request_len,
        baq_response_info,
        baq_response_ptr,
        baq_response_len,
        get_request,
        get_response,
    )
    
    if not baq_response_info.is_success():
        error_msg.code = baq_response_info.status_code
        error_msg.detail = baq_response_info.status_message
        if baq_response_info.is_error_in_api():
            error_msg.origin = "API"
        elif baq_response_info.is_error_in_zcee():
            error_msg.origin = "ZCEE"
        elif baq_response_info.is_error_in_stub():
            error_msg.origin = "STUB"
    
    return MQGetResult(
        response=get_response,
        baq_response_info=baq_response_info,
        error_msg=error_msg,
    )


def _print_success(response: GetResponse, status_code: int, output_func: OutputFunc) -> None:
    """Pretty-print the successful response fields."""
    output_func(f"NUMB:   {response.numb}")
    output_func(f"NAME:   {response.name}")
    output_func(f"ADDRX:  {response.addrx}")
    output_func(f"PHONE:  {response.phone}")
    output_func(f"DATEX:  {response.datex}")
    output_func(f"AMOUNT: {response.amount}")
    output_func(f"HTTP CODE: {status_code}")


def _print_error(
    baq_response_info: BAQResponseInfo,
    error_msg: ErrorMsg,
    output_func: OutputFunc,
) -> None:
    """Pretty-print error information."""
    output_func(f"Error code: {baq_response_info.status_code}")
    output_func(f"Error msg: {baq_response_info.status_message}")
    output_func(f"Error origin: {error_msg.origin}")


# ============================================================================
# Main Program
# ============================================================================

def main(
    mock_baqcstub: Optional[StubCallable] = None,
    output_func: OutputFunc = print,
) -> Tuple[int, ErrorMsg]:
    """Main program entry point (MAINLINE)."""
    result = perform_mq_get(mock_baqcstub=mock_baqcstub)
    
    if result.baq_response_info.is_success():
        _print_success(result.response, result.baq_response_info.status_code, output_func)
    else:
        _print_error(result.baq_response_info, result.error_msg, output_func)
    
    return result.baq_response_info.status_code, result.error_msg


if __name__ == "__main__":
    RETURN_CODE, ERROR_MSG = main()
    print(f"Return Code: {RETURN_CODE}")
