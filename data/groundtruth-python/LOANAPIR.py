"""
Python Translation of LOANAPIR.cbl

Program: LOANAPIR
Type: CICS Program
Function: Loan Application Processing via z/OS Connect EE API

This program:
1. Displays loan application screen (MINIMAP)
2. Receives loan application data from user
3. Calls z/OS Connect EE API via BAQCSTUB (POST operation)
4. Displays loan approval status and messages
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, List, Tuple
from decimal import Decimal

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
TRANSID = "APIR"

# AID Keys
DFHENTER = "ENTER"
DFHPF3 = "PF3"
DFHPF12 = "PF12"
DFHCLEAR = "CLEAR"


# ============================================================================
# MINIMAP Structure (Input/Output Map)
# ============================================================================

@dataclass
class Minimap:
    """MINIMAP structure for loan application screen."""
    # Input fields
    namei: str = ""  # NAMEI
    crdscrei: str = ""  # CRDSCREI (credit score)
    incomei: str = ""  # INCOMEI (yearly income)
    agei: str = ""  # AGEI
    amounti: str = ""  # AMOUNTI
    yrpaymnti: str = ""  # YRPAYMNTI (yearly repayment)
    
    # Output fields
    approvedo: str = ""  # APPROVEDO
    uido: str = ""  # UIDO
    msg1o: str = ""  # MSG1O
    msg2o: str = ""  # MSG2O
    msg3o: str = ""  # MSG3O
    msg4o: str = ""  # MSG4O
    msg5o: str = ""  # MSG5O
    msg6o: str = ""  # MSG6O
    msg7o: str = ""  # MSG7O
    msg8o: str = ""  # MSG8O
    msg9o: str = ""  # MSG9O
    msgao: str = ""  # MSGAO (10th message)


# ============================================================================
# Commarea Buffer Structure
# ============================================================================

@dataclass
class CommareaBuffer:
    """COMMAREA-BUFFER structure for state management."""
    name: str = ""  # 20 bytes
    credit_score: str = ""  # 18 digits
    yearly_income: str = ""  # 18 digits
    age: str = ""  # 10 digits
    amount: str = ""  # 18 digits
    approved: str = ""  # 1 byte ('T' for approved)
    effect_date: str = ""  # 8 bytes
    yearly_interest_rate: str = ""  # S9(5) - signed
    yearly_repayment: str = ""  # 18 digits
    uid: str = ""  # 8 bytes
    messages_num: str = ""  # 9 digits
    messages: List[str] = field(default_factory=lambda: [""] * 10)  # 10 occurrences of 60 bytes


# ============================================================================
# POST Request Structure (MIN00Q01)
# ============================================================================

@dataclass
class PostRequest:
    """POST request structure (MIN00Q01) for loan application."""
    # Array index/count fields (set to 1 in COBOL)
    miniloan_commarea2_num: int = 1
    name_num: int = 1
    creditscore_num: int = 1
    yearlyincome_num: int = 1
    age_num: int = 1
    amount_num: int = 1
    yearlyrepayment_num: int = 1
    
    # Data fields
    name2: str = ""  # NAME2 in POST-REQUEST
    name2_length: int = 0  # NAME2-length in POST-REQUEST
    age: str = ""  # AGE in POST-REQUEST
    creditscore: str = ""  # CREDITSCORE in POST-REQUEST
    yearlyincome: str = ""  # YEARLYINCOME in POST-REQUEST
    amount: str = ""  # AMOUNT in POST-REQUEST
    yearlyrepayment: str = ""  # YEARLYREPAYMENT in POST-REQUEST
    
    def set_name2(self, value: str):
        """Set name and update length."""
        self.name2 = value
        self.name2_length = len(value) if value else 0


# ============================================================================
# POST Response Structure (MIN00P01)
# ============================================================================

@dataclass
class PostResponse:
    """POST response structure (MIN00P01) for loan application."""
    approvedx2: str = ""  # APPROVEDX2 in POST-RESPONSE ('T' for approved)
    uid2: str = ""  # UID2 in POST-RESPONSE
    messages2: List[str] = field(default_factory=lambda: [""] * 10)  # MESSAGES2 occurs 10 times


# ============================================================================
# POST Info Structure (MIN00I01)
# ============================================================================

@dataclass
class PostInfoOper1:
    """POST info structure (MIN00I01)."""
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
    # Mock implementation: Simulate loan approval logic
    # In real system, this would call z/OS Connect EE API
    
    if not post_request.name2 or not post_request.creditscore:
        baq_response_info.return_code = BAQ_ERROR_IN_API
        baq_response_info.status_code = 400
        baq_response_info.status_message = "Name and credit score are required"
        return baq_response_info
    
    # Simulate loan approval logic
    try:
        credit_score = int(post_request.creditscore) if post_request.creditscore else 0
        yearly_income = int(post_request.yearlyincome) if post_request.yearlyincome else 0
        age = int(post_request.age) if post_request.age else 0
        amount = int(post_request.amount) if post_request.amount else 0
        
        # Simple approval logic: approve if credit score >= 650 and age >= 18
        if credit_score >= 650 and age >= 18:
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "Loan processed successfully"
            
            post_response.approvedx2 = "T"
            post_response.uid2 = "LOAN" + str(hash(post_request.name2))[:4].zfill(4)
            post_response.messages2[0] = "Loan application approved"
            post_response.messages2[1] = f"Credit score: {credit_score}"
            post_response.messages2[2] = f"Approved amount: {amount}"
            post_response.messages2[3] = "Interest rate: 5.5%"
            post_response.messages2[4] = "Processing completed"
        else:
            baq_response_info.return_code = BAQ_SUCCESS
            baq_response_info.status_code = 200
            baq_response_info.status_message = "Loan processed successfully"
            
            post_response.approvedx2 = "F"
            post_response.uid2 = "LOAN" + str(hash(post_request.name2))[:4].zfill(4)
            post_response.messages2[0] = "Loan application not approved"
            if credit_score < 650:
                post_response.messages2[1] = f"Credit score too low: {credit_score} (minimum: 650)"
            if age < 18:
                post_response.messages2[2] = f"Age requirement not met: {age} (minimum: 18)"
            post_response.messages2[3] = "Please contact customer service"
    except ValueError:
        baq_response_info.return_code = BAQ_ERROR_IN_API
        baq_response_info.status_code = 400
        baq_response_info.status_message = "Invalid numeric values"
        return baq_response_info
    
    return baq_response_info


# ============================================================================
# CICS Command Simulation
# ============================================================================

@dataclass
class CICSResponse:
    """Response structure for CICS commands."""
    resp: int = 0  # DFHRESP (0 = NORMAL)
    resp2: int = 0  # DFHRESP2


def send_map(
    map_name: str,
    mapset_name: str,
    map_only: bool = False,
    freekb: bool = False,
    erase: bool = False,
    map_data: Optional[Minimap] = None
) -> CICSResponse:
    """Simulate CICS SEND MAP command."""
    # In real CICS, this would send the map to the terminal
    return CICSResponse()


def receive_map(
    map_name: str,
    mapset_name: str,
    nohandle: bool = False
) -> Tuple[Optional[Minimap], CICSResponse]:
    """Simulate CICS RECEIVE MAP command."""
    # In real CICS, this would receive map data from the terminal
    # For testing, return None to indicate no map received
    return None, CICSResponse()


def send_control(erase: bool = False) -> CICSResponse:
    """Simulate CICS SEND CONTROL command."""
    return CICSResponse()


def send_text(text: str, erase: bool = False, freekb: bool = False) -> CICSResponse:
    """Simulate CICS SEND TEXT command."""
    # In real CICS, this would send text to the terminal
    print(text)
    return CICSResponse()


def return_transid(
    transid: str,
    commarea: Optional[CommareaBuffer] = None,
    length: Optional[int] = None
) -> None:
    """Simulate CICS RETURN TRANSID command."""
    # In real CICS, this would return control to CICS with the transaction ID
    pass


def return_normal() -> None:
    """Simulate CICS RETURN command."""
    # In real CICS, this would return control to CICS
    pass


# ============================================================================
# Main Program
# ============================================================================

def main(
    eibcalen: int = 0,
    eibaid: str = "",
    dfhcommarea: Optional[CommareaBuffer] = None,
    map_in: Optional[Minimap] = None,
    mock_baqcstub: Optional[callable] = None,
    mock_send_map: Optional[callable] = None,
    mock_receive_map: Optional[callable] = None,
    mock_send_control: Optional[callable] = None,
    mock_send_text: Optional[callable] = None,
    mock_return_transid: Optional[callable] = None,
    mock_return_normal: Optional[callable] = None
) -> Tuple[Optional[Minimap], Optional[CommareaBuffer], ErrorMsg]:
    """Main program entry point (MAIN-PROCESS).
    
    Args:
        eibcalen: EIB CALEN (commarea length, 0 = first time)
        eibaid: EIB AID (attention identifier: ENTER, PF3, PF12, CLEAR)
        dfhcommarea: DFHCOMMAREA (commarea buffer)
        map_in: Input map data (optional, for testing)
        mock_baqcstub: Optional mock function for BAQCSTUB
        mock_send_map: Optional mock function for SEND MAP
        mock_receive_map: Optional mock function for RECEIVE MAP
        mock_send_control: Optional mock function for SEND CONTROL
        mock_send_text: Optional mock function for SEND TEXT
        mock_return_transid: Optional mock function for RETURN TRANSID
        mock_return_normal: Optional mock function for RETURN
    
    Returns:
        Tuple of (map_out, commarea_buffer, error_msg)
        - map_out: Output map with results
        - commarea_buffer: Updated commarea buffer
        - error_msg: Error message structure
    """
    # Initialize structures
    commarea_buffer = CommareaBuffer()
    error_msg = ErrorMsg()
    
    # Initialize POST request/response
    post_request = PostRequest()
    post_response = PostResponse()
    post_info = PostInfoOper1()
    
    # Initialize BAQ structures
    baq_request_info = BAQRequestInfo()
    baq_response_info = BAQResponseInfo()
    
    # Initialize map
    map_out = Minimap()
    
    # Use mocks if provided
    if mock_send_map is None:
        mock_send_map = send_map
    if mock_receive_map is None:
        mock_receive_map = receive_map
    if mock_send_control is None:
        mock_send_control = send_control
    if mock_send_text is None:
        mock_send_text = send_text
    if mock_return_transid is None:
        mock_return_transid = return_transid
    if mock_return_normal is None:
        mock_return_normal = return_normal
    
    # FIRST TIME PROCESSING
    if eibcalen == 0:
        # Send map only (initial screen display)
        mock_send_map('MINIMAP', 'MINIMAP', map_only=True, freekb=True, erase=True)
        
        # Return with commarea
        mock_return_transid(TRANSID, commarea_buffer, len(str(commarea_buffer)))
        
        return map_out, commarea_buffer, error_msg
    
    # MOVE DFHCOMMAREA TO COMMAREA-BUFFER
    if dfhcommarea is not None:
        commarea_buffer = dfhcommarea
    
    # Initialize working storage
    post_request = PostRequest()
    post_response = PostResponse()
    
    # RECEIVE MAP
    if map_in is None:
        map_received, resp = mock_receive_map('MINIMAP', 'MINIMAP', nohandle=True)
        if map_received is not None:
            map_in = map_received
        else:
            map_in = Minimap()
    else:
        map_in = map_in
    
    # EVALUATE EIBAID
    if eibaid == DFHPF3 or eibaid == DFHPF12:
        # EXIT-TRANSACTION
        mock_send_control(erase=True)
        mock_send_text('LOANAPIR Session Over', erase=True, freekb=True)
        mock_return_normal()
        return map_out, commarea_buffer, error_msg
    elif eibaid == DFHCLEAR:
        # Continue processing
        pass
    elif eibaid == DFHENTER:
        # Process loan application
        # Move map input to POST request
        post_request.set_name2(map_in.namei)
        post_request.age = map_in.agei
        post_request.creditscore = map_in.crdscrei
        post_request.yearlyincome = map_in.incomei
        post_request.amount = map_in.amounti
        post_request.yearlyrepayment = map_in.yrpaymnti
        
        # Set array indices to 1
        post_request.miniloan_commarea2_num = 1
        post_request.name_num = 1
        post_request.creditscore_num = 1
        post_request.yearlyincome_num = 1
        post_request.age_num = 1
        post_request.amount_num = 1
        post_request.yearlyrepayment_num = 1
        
        # INVOKE-API
        baq_request_ptr = b""  # Not used in Python
        baq_request_len = len(str(post_request))
        baq_response_ptr = b""  # Not used in Python
        baq_response_len = len(str(post_response))
        
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
            # Success - display approval status
            print(f"Approved: {post_response.approvedx2}")
            
            if post_response.approvedx2 == 'T':
                map_out.approvedo = 'Loan approved'
            else:
                map_out.approvedo = 'Loan not approved'
            
            map_out.uido = post_response.uid2
            map_out.msg1o = post_response.messages2[0] if len(post_response.messages2) > 0 else ""
            map_out.msg2o = post_response.messages2[1] if len(post_response.messages2) > 1 else ""
            map_out.msg3o = post_response.messages2[2] if len(post_response.messages2) > 2 else ""
            map_out.msg4o = post_response.messages2[3] if len(post_response.messages2) > 3 else ""
            map_out.msg5o = post_response.messages2[4] if len(post_response.messages2) > 4 else ""
            map_out.msg6o = post_response.messages2[5] if len(post_response.messages2) > 5 else ""
            map_out.msg7o = post_response.messages2[6] if len(post_response.messages2) > 6 else ""
            map_out.msg8o = post_response.messages2[7] if len(post_response.messages2) > 7 else ""
            map_out.msg9o = post_response.messages2[8] if len(post_response.messages2) > 8 else ""
        else:
            # Error - CHECK-API-ERROR
            print(f"Error code: {baq_response_info.status_code}")
            print(f"Error msg: {baq_response_info.status_message}")
            
            error_msg.code = baq_response_info.status_code
            error_msg.detail = baq_response_info.status_message
            
            # Determine error origin
            if baq_response_info.is_error_in_api():
                error_msg.origin = "API"
            elif baq_response_info.is_error_in_zcee():
                error_msg.origin = "ZCEE"
            elif baq_response_info.is_error_in_stub():
                error_msg.origin = "STUB"
            
            print(f"Error origin: {error_msg.origin}")
            
            # Move error message to map output (split across 10 message fields)
            status_msg = baq_response_info.status_message
            map_out.msg1o = str(baq_response_info.status_code)
            map_out.msg2o = status_msg[0:60] if len(status_msg) > 0 else ""
            map_out.msg3o = status_msg[60:120] if len(status_msg) > 60 else ""
            map_out.msg4o = status_msg[120:180] if len(status_msg) > 120 else ""
            map_out.msg5o = status_msg[180:240] if len(status_msg) > 180 else ""
            map_out.msg6o = status_msg[240:300] if len(status_msg) > 240 else ""
            map_out.msg7o = status_msg[300:360] if len(status_msg) > 300 else ""
            map_out.msg8o = status_msg[360:420] if len(status_msg) > 360 else ""
            map_out.msg9o = status_msg[420:480] if len(status_msg) > 420 else ""
            map_out.msgao = status_msg[480:540] if len(status_msg) > 480 else ""
    
    # SEND CONTROL ERASE
    mock_send_control(erase=True)
    
    # SEND MAP
    mock_send_map('MINIMAP', 'MINIMAP', freekb=True, erase=True, map_data=map_out)
    
    # RETURN TRANSID
    mock_return_transid(TRANSID, commarea_buffer)
    
    return map_out, commarea_buffer, error_msg


if __name__ == "__main__":
    # For testing/standalone execution
    map_out, commarea, error = main(
        eibcalen=0,
        eibaid="",
        dfhcommarea=None
    )
    print("Initial screen sent")

