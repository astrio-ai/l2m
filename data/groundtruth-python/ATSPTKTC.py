"""
Python Translation of ATSPTKTC.cbl

Program: ATSPTKTC
Type: RACF Pass Ticket Generation
Function: Generate RACF pass ticket using environment variables

This program:
1. Gets BAQUSERNAME from environment
2. Gets ATSAPPLID from environment
3. Calls RACF service IRRSPK00 to obtain a pass ticket
4. Sets BAQPASSWORD environment variable with the pass ticket
"""

import os
import sys
from dataclasses import dataclass
from typing import Optional, Tuple


# ============================================================================
# Structures
# ============================================================================

@dataclass
class FeedbackCode:
    """Feedback code structure from CEEENV."""
    severity: int = 0
    msg_no: int = 0
    condition_token_value: bytes = b'\x00' * 16
    i_s_info: int = 0


@dataclass
class IRRArea:
    """IRRSPK00 area structure."""
    workarea: bytes = b'\x00' * 1024
    safrc: int = 0  # SAF return code
    racfrc: int = 0  # RACF return code
    racfrsn: int = 0  # RACF reason code
    alet: int = 0
    function_code: bytes = b'\x00\x03'  # X'0003' = request pass ticket
    option_word: int = 0
    ticket_options: bytes = b'\x00\x00\x00\x01'  # X'00000001'
    ticket_options_ptr: Optional[bytes] = None


@dataclass
class IRRApplid:
    """Application ID structure."""
    applid_length: int = 0
    applid: str = ""  # 8 chars


@dataclass
class IRRIdentity:
    """Identity structure."""
    identity_length: int = 0
    identity: str = ""  # Up to 240 chars


@dataclass
class IRRPassticket:
    """Pass ticket structure."""
    pass_ticket_length: int = 8
    pass_ticket: str = ""  # 8 chars


# ============================================================================
# CEEENV API Simulation
# ============================================================================

def ceeenv_get(
    env_variable_name: str,
    feedback_code: Optional[FeedbackCode] = None
) -> Tuple[Optional[str], int, FeedbackCode]:
    """Get environment variable (CEEENV function code 1).
    
    Args:
        env_variable_name: Environment variable name
        feedback_code: Optional feedback code (created if None)
    
    Returns:
        Tuple of (value, length, feedback_code)
        - value: Environment variable value (None if not found)
        - length: Length of value (0 if not found)
        - feedback_code: Feedback code
    """
    if feedback_code is None:
        feedback_code = FeedbackCode()
    
    # Remove trailing spaces from variable name
    env_variable_name = env_variable_name.rstrip()
    env_variable_name_length = len(env_variable_name)
    
    # Get from environment
    value = os.environ.get(env_variable_name)
    
    if value is not None:
        value_length = len(value)
        feedback_code.severity = 0
        feedback_code.msg_no = 0
        return value, value_length, feedback_code
    else:
        feedback_code.severity = 0
        feedback_code.msg_no = 0
        return None, 0, feedback_code


def ceeenv_set(
    env_variable_name: str,
    env_variable_value: str,
    feedback_code: Optional[FeedbackCode] = None
) -> Tuple[int, FeedbackCode]:
    """Set environment variable (CEEENV function code 5).
    
    Args:
        env_variable_name: Environment variable name
        env_variable_value: Environment variable value
        feedback_code: Optional feedback code (created if None)
    
    Returns:
        Tuple of (return_code, feedback_code)
        - return_code: 0 = success
        - feedback_code: Feedback code
    """
    if feedback_code is None:
        feedback_code = FeedbackCode()
    
    try:
        # Remove trailing spaces from variable name
        env_variable_name = env_variable_name.rstrip()
        
        # Set environment variable
        os.environ[env_variable_name] = env_variable_value
        
        feedback_code.severity = 0
        feedback_code.msg_no = 0
        return 0, feedback_code
    except Exception as e:
        feedback_code.severity = 8
        feedback_code.msg_no = 1
        return 1, feedback_code


# ============================================================================
# RACF IRRSPK00 Service Simulation
# ============================================================================

def irrspk00(
    irr_area: IRRArea,
    irr_passticket: IRRPassticket,
    irr_identity: IRRIdentity,
    irr_applid: IRRApplid
) -> Tuple[int, int, int]:
    """Call RACF service IRRSPK00 to obtain a pass ticket.
    
    This is a mock implementation. In a real z/OS environment, this would
    call the actual RACF service.
    
    Args:
        irr_area: IRR area structure
        irr_passticket: Pass ticket structure (output)
        irr_identity: Identity structure (input)
        irr_applid: Application ID structure (input)
    
    Returns:
        Tuple of (safrc, racfrc, racfrsn)
        - safrc: SAF return code (0 = success)
        - racfrc: RACF return code (0 = success)
        - racfrsn: RACF reason code
    """
    # Mock implementation: Generate a simple pass ticket
    # In real system, this would call RACF to generate the ticket
    
    # Validate inputs
    if not irr_identity.identity or irr_identity.identity_length == 0:
        irr_area.safrc = 8
        irr_area.racfrc = 8
        irr_area.racfrsn = 1
        return 8, 8, 1
    
    if not irr_applid.applid or irr_applid.applid_length == 0:
        irr_area.safrc = 8
        irr_area.racfrc = 8
        irr_area.racfrsn = 2
        return 8, 8, 2
    
    # Generate a mock pass ticket (8 characters)
    # In real system, RACF would generate this based on identity and applid
    identity_clean = irr_identity.identity.strip()[:8].upper()
    applid_clean = irr_applid.applid.strip()[:8].upper()
    
    # Simple mock: use first chars of identity and applid
    # Pad to 8 characters with 'X'
    pass_ticket = (identity_clean[:4] + applid_clean[:4]).ljust(8, 'X')
    
    irr_passticket.pass_ticket = pass_ticket
    irr_passticket.pass_ticket_length = 8
    
    irr_area.safrc = 0
    irr_area.racfrc = 0
    irr_area.racfrsn = 0
    
    return 0, 0, 0


# ============================================================================
# Helper Functions
# ============================================================================

def trim_trailing_spaces(s: str) -> Tuple[str, int]:
    """Trim trailing spaces and return string with length.
    
    Args:
        s: String to trim
    
    Returns:
        Tuple of (trimmed_string, length)
    """
    trimmed = s.rstrip()
    length = len(trimmed)
    return trimmed, length


# ============================================================================
# Main Program
# ============================================================================

def main() -> int:
    """Main program entry point.
    
    Returns:
        Exit code (0 = success)
    """
    # Initialize IRR area
    irr_area = IRRArea()
    irr_area.function_code = b'\x00\x03'  # X'0003' = request pass ticket
    irr_area.ticket_options = b'\x00\x00\x00\x01'  # X'00000001'
    
    # Initialize structures
    irr_identity = IRRIdentity()
    irr_applid = IRRApplid()
    irr_passticket = IRRPassticket()
    irr_passticket.pass_ticket_length = 8
    irr_passticket.pass_ticket = " " * 8
    
    # Get BAQUSERNAME environment variable
    env_variable_name = "BAQUSERNAME"
    identity_value, identity_length, feedback = ceeenv_get(env_variable_name)
    
    if identity_length != 0 and identity_value:
        # Trim trailing spaces
        identity_trimmed, identity_len = trim_trailing_spaces(identity_value)
        irr_identity.identity = identity_trimmed
        irr_identity.identity_length = identity_len
    else:
        print(f"{env_variable_name} NOT FOUND {identity_length}")
        return 1
    
    # Get ATSAPPLID environment variable
    env_variable_name = "ATSAPPLID"
    applid_value, applid_length, feedback = ceeenv_get(env_variable_name)
    
    if applid_length != 0 and applid_value:
        # Trim trailing spaces
        applid_trimmed, applid_len = trim_trailing_spaces(applid_value)
        irr_applid.applid = applid_trimmed
        irr_applid.applid_length = applid_len
    else:
        print(f"{env_variable_name} NOT FOUND {applid_length}")
        return 1
    
    print(f"ATSPTKTC-BAQUSERNAME: {irr_identity.identity[:8]}")
    print(f"ATSPTKTC-ATSAPPLID:   {irr_applid.applid}")
    
    # Call RACF service IRRSPK00 to obtain pass ticket
    safrc, racfrc, racfrsn = irrspk00(
        irr_area,
        irr_passticket,
        irr_identity,
        irr_applid
    )
    
    if irr_area.safrc != 0:
        print(f"SAF_return_code:     {irr_area.safrc}")
        print(f"RACF_return_code:    {irr_area.racfrc}")
        print(f"RACF_reason_code:    {irr_area.racfrsn}")
        return 1
    
    print(f"ATSPTKTC-BAQPASSWORD: {irr_passticket.pass_ticket}")
    
    # Set BAQPASSWORD environment variable
    env_variable_name = "BAQPASSWORD"
    env_variable_value = irr_passticket.pass_ticket
    return_code, feedback = ceeenv_set(env_variable_name, env_variable_value)
    
    if return_code != 0:
        print(f"ERROR: Failed to set {env_variable_name}")
        return 1
    
    return 0


if __name__ == "__main__":
    sys.exit(main())

