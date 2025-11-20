"""
Python Translation of LOANCICS.cbl

Program: LOANCICS
Type: CICS Program
Function: Loan Application Processing via CICS LINK

This program:
1. Displays loan application screen (MINIMAP)
2. Receives loan application data from user
3. Calls MINICICS program via CICS LINK with COMMAREA
4. Displays loan approval status and messages from linked program
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, List, Tuple
from decimal import Decimal

# Import ErrorMsg for consistency
from CSCVDLTI import ErrorMsg


# ============================================================================
# Constants
# ============================================================================

TRANSID = "LOAN"
LINKED_PROGRAM = "MINICICS"

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
    efdtei: str = ""  # EFDATEI (effect date)
    interesti: str = ""  # INTERESTI (interest rate)
    
    # Output fields
    nameo: str = ""  # NAMEO
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


# ============================================================================
# Commarea Buffer Structure
# ============================================================================

@dataclass
class CommareaBuffer:
    """COMMAREA-BUFFER structure for state management and inter-program communication."""
    name: str = ""  # 20 bytes
    credit_score: str = ""  # 18 digits
    yearly_income: str = ""  # 18 digits
    age: str = ""  # 10 digits
    amount: str = ""  # 18 digits
    approved: str = ""  # 1 byte ('T' for approved, 'F' for not approved)
    effect_date: str = ""  # 8 bytes
    yearly_interest_rate: str = ""  # S9(5) - signed
    yearly_repayment: str = ""  # 18 digits
    uid: str = ""  # 8 bytes
    messages_num: str = ""  # 9 digits
    messages: List[str] = field(default_factory=lambda: [""] * 10)  # 10 occurrences of 60 bytes
    
    def bool_value(self) -> bool:
        """Check if approved is 'T' (88 BoolValue in COBOL)."""
        return self.approved == 'T'


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


def link_program(
    program_name: str,
    commarea: CommareaBuffer
) -> Tuple[CommareaBuffer, CICSResponse]:
    """Simulate CICS LINK command.
    
    In a real CICS environment, this would call the specified program
    and pass the COMMAREA. The called program would process the data
    and return results in the COMMAREA.
    
    For this mock implementation, we simulate the MINICICS program
    processing the loan application.
    
    Args:
        program_name: Name of the program to link to
        commarea: COMMAREA buffer (input/output)
    
    Returns:
        Tuple of (updated_commarea, response)
    """
    # Mock implementation: Simulate MINICICS loan processing
    # In real system, this would call the actual MINICICS program
    
    if program_name != LINKED_PROGRAM:
        # Return unchanged commarea if program name doesn't match
        return commarea, CICSResponse()
    
    # Simulate loan processing logic
    try:
        # Extract numeric values
        credit_score = int(commarea.credit_score) if commarea.credit_score else 0
        yearly_income = int(commarea.yearly_income) if commarea.yearly_income else 0
        age = int(commarea.age) if commarea.age else 0
        amount = int(commarea.amount) if commarea.amount else 0
        
        # Simple approval logic: approve if credit score >= 650 and age >= 18
        if credit_score >= 650 and age >= 18:
            commarea.approved = 'T'
            commarea.uid = "LOAN" + str(hash(commarea.name))[:4].zfill(4)
            commarea.messages_num = "1"
            commarea.messages[0] = "Loan application approved"
            commarea.messages[1] = f"Credit score: {credit_score}"
            commarea.messages[2] = f"Approved amount: {amount}"
            commarea.messages[3] = "Interest rate: 5.5%"
            commarea.messages[4] = "Processing completed"
        else:
            commarea.approved = 'F'
            commarea.uid = "LOAN" + str(hash(commarea.name))[:4].zfill(4)
            commarea.messages_num = "1"
            commarea.messages[0] = "Loan application not approved"
            if credit_score < 650:
                commarea.messages[1] = f"Credit score too low: {credit_score} (minimum: 650)"
            if age < 18:
                commarea.messages[2] = f"Age requirement not met: {age} (minimum: 18)"
            commarea.messages[3] = "Please contact customer service"
    except ValueError:
        # If numeric conversion fails, mark as not approved
        commarea.approved = 'F'
        commarea.uid = "LOAN0000"
        commarea.messages_num = "1"
        commarea.messages[0] = "Invalid numeric values in application"
    
    return commarea, CICSResponse()


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
    mock_link_program: Optional[callable] = None,
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
        mock_link_program: Optional mock function for LINK PROGRAM
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
        - error_msg: Error message structure (always empty for this program)
    """
    # Initialize structures
    commarea_buffer = CommareaBuffer()
    error_msg = ErrorMsg()
    
    # Initialize map
    map_out = Minimap()
    
    # Use mocks if provided
    if mock_link_program is None:
        mock_link_program = link_program
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
        mock_send_text('LOANCICS Session Over', erase=True, freekb=True)
        mock_return_normal()
        return map_out, commarea_buffer, error_msg
    elif eibaid == DFHCLEAR:
        # Continue processing
        pass
    elif eibaid == DFHENTER:
        # Process loan application
        # Move map input to COMMAREA-BUFFER
        commarea_buffer.age = map_in.agei
        commarea_buffer.credit_score = map_in.crdscrei
        commarea_buffer.name = map_in.namei
        commarea_buffer.yearly_income = map_in.incomei
        commarea_buffer.amount = map_in.amounti
        commarea_buffer.approved = 'F'  # Initialize to 'F'
        commarea_buffer.effect_date = map_in.efdtei
        commarea_buffer.messages_num = "0"
        commarea_buffer.yearly_interest_rate = map_in.interesti
        commarea_buffer.yearly_repayment = map_in.yrpaymnti
        
        # LINK to MINICICS program
        commarea_buffer, link_resp = mock_link_program(LINKED_PROGRAM, commarea_buffer)
        
        # Move results from COMMAREA-BUFFER to map output
        map_out.nameo = commarea_buffer.name
        map_out.uido = commarea_buffer.uid
        
        if commarea_buffer.approved == 'T':
            map_out.approvedo = 'Loan approved'
        else:
            map_out.approvedo = 'Loan not approved'
        
        map_out.msg1o = commarea_buffer.messages[0] if len(commarea_buffer.messages) > 0 else ""
        map_out.msg2o = commarea_buffer.messages[1] if len(commarea_buffer.messages) > 1 else ""
        map_out.msg3o = commarea_buffer.messages[2] if len(commarea_buffer.messages) > 2 else ""
        map_out.msg4o = commarea_buffer.messages[3] if len(commarea_buffer.messages) > 3 else ""
        map_out.msg5o = commarea_buffer.messages[4] if len(commarea_buffer.messages) > 4 else ""
        map_out.msg6o = commarea_buffer.messages[5] if len(commarea_buffer.messages) > 5 else ""
        map_out.msg7o = commarea_buffer.messages[6] if len(commarea_buffer.messages) > 6 else ""
        map_out.msg8o = commarea_buffer.messages[7] if len(commarea_buffer.messages) > 7 else ""
        map_out.msg9o = commarea_buffer.messages[8] if len(commarea_buffer.messages) > 8 else ""
    
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

