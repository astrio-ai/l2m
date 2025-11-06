"""
Python Translation of CSUTLDTC.cbl

Program: CSUTLDTC
Application: CardDemo
Type: COBOL Subprogram
Function: Date validation using CEEDAYS API

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass
from typing import Tuple
from datetime import datetime


# ============================================================================
# Constants
# ============================================================================

# Feedback code constants (simulated CEEDAYS feedback codes)
# These are represented as hex values in COBOL, but we'll use constants
FC_INVALID_DATE = b'\x00\x00\x00\x00\x00\x00\x00\x00'  # Date is valid
FC_INSUFFICIENT_DATA = b'\x00\x03\x09\xCB\x59\xC3\xC5\xC5'
FC_BAD_DATE_VALUE = b'\x00\x03\x09\xCC\x59\xC3\xC5\xC5'
FC_INVALID_ERA = b'\x00\x03\x09\xCD\x59\xC3\xC5\xC5'
FC_UNSUPP_RANGE = b'\x00\x03\x09\xD1\x59\xC3\xC5\xC5'
FC_INVALID_MONTH = b'\x00\x03\x09\xD5\x59\xC3\xC5\xC5'
FC_BAD_PIC_STRING = b'\x00\x03\x09\xD6\x59\xC3\xC5\xC5'
FC_NON_NUMERIC_DATA = b'\x00\x03\x09\xD8\x59\xC3\xC5\xC5'
FC_YEAR_IN_ERA_ZERO = b'\x00\x03\x09\xD9\x59\xC3\xC5\xC5'


# ============================================================================
# Feedback Code Structure
# ============================================================================

@dataclass
class FeedbackCode:
    """Feedback code structure from CEEDAYS API."""
    severity: int = 0  # Severity code
    msg_no: int = 0  # Message number
    feedback_token: bytes = b'\x00' * 8  # Feedback token value


# ============================================================================
# CEEDAYS API Simulation
# ============================================================================

def ceedays(date_str: str, date_format: str) -> Tuple[int, FeedbackCode]:
    """Simulate CEEDAYS API for date validation.
    
    Args:
        date_str: Date string to validate (10 chars)
        date_format: Date format mask (10 chars, e.g., "YYYY-MM-DD")
    
    Returns:
        Tuple of (lillian_date, feedback_code)
        - lillian_date: Lillian date (not used in output, set to 0)
        - feedback_code: FeedbackCode with severity and message number
    """
    date_str = date_str.strip() if date_str else ""
    date_format = date_format.strip() if date_format else ""
    
    # Initialize feedback code
    feedback = FeedbackCode()
    
    # Check for empty date
    if not date_str:
        feedback.severity = 8  # Error
        feedback.msg_no = 309  # Insufficient data
        feedback.feedback_token = FC_INSUFFICIENT_DATA
        return 0, feedback
    
    # Check for empty format
    if not date_format:
        feedback.severity = 8  # Error
        feedback.msg_no = 309  # Bad PIC string
        feedback.feedback_token = FC_BAD_PIC_STRING
        return 0, feedback
    
    # Normalize format (common formats)
    format_normalized = date_format.upper().strip()
    
    # Remove separators from date for analysis
    date_clean = date_str.replace("-", "").replace("/", "").strip()
    has_separators = "-" in date_str or "/" in date_str
    
    # Handle CCYYMMDD/YYYYMMDD format (8 digits, no separators)
    if format_normalized in ("YYYYMMDD", "CCYYMMDD") or (len(date_clean) == 8 and not has_separators and date_clean.isdigit()):
        try:
            if len(date_clean) != 8:
                feedback.severity = 8
                feedback.msg_no = 309
                feedback.feedback_token = FC_BAD_DATE_VALUE
                return 0, feedback
            
            if not date_clean.isdigit():
                feedback.severity = 8
                feedback.msg_no = 309
                feedback.feedback_token = FC_NON_NUMERIC_DATA
                return 0, feedback
            
            year = int(date_clean[0:4])
            month = int(date_clean[4:6])
            day = int(date_clean[6:8])
            
            # Validate month
            if month < 1 or month > 12:
                feedback.severity = 8
                feedback.msg_no = 309
                feedback.feedback_token = FC_INVALID_MONTH
                return 0, feedback
            
            # Try to create a date
            try:
                datetime(year, month, day)
                feedback.severity = 0
                feedback.msg_no = 0
                feedback.feedback_token = FC_INVALID_DATE
                return 0, feedback
            except ValueError:
                feedback.severity = 8
                feedback.msg_no = 309
                feedback.feedback_token = FC_BAD_DATE_VALUE
                return 0, feedback
        except (ValueError, IndexError):
            feedback.severity = 8
            feedback.msg_no = 309
            feedback.feedback_token = FC_BAD_DATE_VALUE
            return 0, feedback
    
    # Handle YYYY-MM-DD format
    elif format_normalized == "YYYY-MM-DD" or (has_separators and format_normalized.startswith("YYYY")):
        try:
            # Try to parse the date
            parts = date_str.split("-")
            if len(parts) != 3:
                feedback.severity = 8
                feedback.msg_no = 309
                feedback.feedback_token = FC_BAD_DATE_VALUE
                return 0, feedback
            
            year_str = parts[0].strip()
            month_str = parts[1].strip()
            day_str = parts[2].strip()
            
            # Check for non-numeric data
            if not (year_str.isdigit() and month_str.isdigit() and day_str.isdigit()):
                feedback.severity = 8
                feedback.msg_no = 309
                feedback.feedback_token = FC_NON_NUMERIC_DATA
                return 0, feedback
            
            year = int(year_str)
            month = int(month_str)
            day = int(day_str)
            
            # Validate month
            if month < 1 or month > 12:
                feedback.severity = 8
                feedback.msg_no = 309
                feedback.feedback_token = FC_INVALID_MONTH
                return 0, feedback
            
            # Validate day (basic check)
            if day < 1 or day > 31:
                feedback.severity = 8
                feedback.msg_no = 309
                feedback.feedback_token = FC_BAD_DATE_VALUE
                return 0, feedback
            
            # Try to create a date to validate (catches invalid dates like Feb 30)
            try:
                datetime(year, month, day)
                # Date is valid
                feedback.severity = 0
                feedback.msg_no = 0
                feedback.feedback_token = FC_INVALID_DATE  # This means "date is valid" in the COBOL
                return 0, feedback
            except ValueError:
                # Invalid date (e.g., Feb 30)
                feedback.severity = 8
                feedback.msg_no = 309
                feedback.feedback_token = FC_BAD_DATE_VALUE
                return 0, feedback
                
        except (ValueError, IndexError):
            feedback.severity = 8
            feedback.msg_no = 309
            feedback.feedback_token = FC_BAD_DATE_VALUE
            return 0, feedback
    
    # Unsupported format
    feedback.severity = 8
    feedback.msg_no = 309
    feedback.feedback_token = FC_BAD_PIC_STRING
    return 0, feedback


# ============================================================================
# Main Function (Subprogram Entry Point)
# ============================================================================

def main(date_str: str, date_format: str) -> Tuple[str, int]:
    """Main program entry point (A000-MAIN).
    
    This is a subprogram that validates a date using CEEDAYS API.
    
    Args:
        date_str: Date string to validate (10 chars)
        date_format: Date format mask (10 chars, e.g., "YYYY-MM-DD")
    
    Returns:
        Tuple of (result_message, return_code)
        - result_message: Formatted result message (80 chars)
        - return_code: Return code (severity as integer)
    """
    # Initialize message
    ws_message = ""
    ws_severity_n = 0
    ws_msg_no_n = 0
    ws_result = ""
    ws_date = date_str.strip() if date_str else ""
    ws_date_fmt = date_format.strip() if date_format else ""
    
    # Truncate to 10 chars (COBOL PIC X(10))
    if len(ws_date) > 10:
        ws_date = ws_date[:10]
    if len(ws_date_fmt) > 10:
        ws_date_fmt = ws_date_fmt[:10]
    
    # Call CEEDAYS API
    output_lillian, feedback_code = ceedays(ws_date, ws_date_fmt)
    
    # Extract severity and message number
    ws_severity_n = feedback_code.severity
    ws_msg_no_n = feedback_code.msg_no
    
    # Determine result message based on feedback token
    feedback_token = feedback_code.feedback_token
    
    if feedback_token == FC_INVALID_DATE:
        ws_result = "Date is valid"
    elif feedback_token == FC_INSUFFICIENT_DATA:
        ws_result = "Insufficient"
    elif feedback_token == FC_BAD_DATE_VALUE:
        ws_result = "Datevalue error"
    elif feedback_token == FC_INVALID_ERA:
        ws_result = "Invalid Era    "
    elif feedback_token == FC_UNSUPP_RANGE:
        ws_result = "Unsupp. Range  "
    elif feedback_token == FC_INVALID_MONTH:
        ws_result = "Invalid month  "
    elif feedback_token == FC_BAD_PIC_STRING:
        ws_result = "Bad Pic String "
    elif feedback_token == FC_NON_NUMERIC_DATA:
        ws_result = "Nonnumeric data"
    elif feedback_token == FC_YEAR_IN_ERA_ZERO:
        ws_result = "YearInEra is 0 "
    else:
        ws_result = "Date is invalid"
    
    # Format result message (80 chars total)
    # Format: "SEV Mesg Code:MSG RESULT           TstDate:DATE      Mask used:FORMAT"
    # SEV = 4 chars, "Mesg Code:" = 11 chars, MSG = 4 chars, space = 1 char
    # RESULT = 15 chars, "TstDate:" = 9 chars, DATE = 10 chars, space = 1 char
    # "Mask used:" = 10 chars, FORMAT = 10 chars, spaces = 3 chars
    # Total: 4 + 11 + 4 + 1 + 15 + 9 + 10 + 1 + 10 + 10 + 3 = 78 chars + 2 padding = 80
    
    ws_severity_str = f"{ws_severity_n:04d}"
    ws_msg_no_str = f"{ws_msg_no_n:04d}"
    ws_result_padded = f"{ws_result:<15}"
    ws_date_padded = f"{ws_date:<10}"
    ws_date_fmt_padded = f"{ws_date_fmt:<10}"
    
    ws_message = (
        f"{ws_severity_str}Mesg Code:{ws_msg_no_str} {ws_result_padded}TstDate:{ws_date_padded} "
        f"Mask used:{ws_date_fmt_padded}   "
    )
    
    # Ensure exactly 80 characters
    ws_message = ws_message[:80].ljust(80)
    
    return ws_message, ws_severity_n


# ============================================================================
# Convenience Function
# ============================================================================

def validate_date(date_str: str, date_format: str = "YYYY-MM-DD") -> Tuple[str, int]:
    """Convenience function to validate a date.
    
    Args:
        date_str: Date string to validate
        date_format: Date format mask (default: "YYYY-MM-DD")
    
    Returns:
        Tuple of (result_message, return_code)
    """
    return main(date_str, date_format)

