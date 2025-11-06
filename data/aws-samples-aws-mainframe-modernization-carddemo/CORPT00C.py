"""
Python Translation of CORPT00C.cbl

Program: CORPT00C
Application: CardDemo
Type: CICS COBOL Program
Function: Print Transaction reports by submitting batch job from online using extra partition TDQ.

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, List, Tuple
from datetime import datetime, timedelta
from calendar import monthrange

# Import shared structures
from COACTVWC import CardDemoCommarea


# ============================================================================
# Constants and Literals
# ============================================================================

WS_PGMNAME = "CORPT00C"
WS_TRANID = "CR00"
WS_TRANSACT_FILE = "TRANSACT"
COSGN00C = "COSGN00C"
COMEN01C = "COMEN01C"
DFHENTER = "DFHENTER"
DFHPF3 = "DFHPF3"
DFHGREEN = "DFHGREEN"

# Default titles (from COTTL01Y)
CCDA_TITLE01 = "CardDemo Application"
CCDA_TITLE02 = "Transaction Report"
CCDA_MSG_INVALID_KEY = "Invalid key pressed. Please use Enter or PF3."

# TDQ Queue name
TDQ_QUEUE_NAME = "JOBS"

# CICS Response Codes
DFHRESP_NORMAL = 0


# ============================================================================
# Date Validation Result Structure
# ============================================================================

@dataclass
class DateValidationResult:
    """Date validation result (CSUTLDTC-RESULT)."""
    sev_cd: str = "0000"  # Severity code: '0000' = valid
    msg_num: str = ""  # Message number
    msg: str = ""  # Message text


# ============================================================================
# Map Structure (CORPT0AO/CORPT0AI)
# ============================================================================

@dataclass
class ReportMap:
    """Report map structure (CORPT0A)."""
    # Header fields
    title01o: str = ""
    title02o: str = ""
    trnnameo: str = ""
    pgmnameo: str = ""
    curdateo: str = ""
    curtimeo: str = ""
    
    # Report type input fields
    monthlyi: str = ""  # Monthly report input
    monthlyo: str = ""  # Monthly report output
    monthlyl: int = 0  # Monthly report length (cursor)
    
    yearlyi: str = ""  # Yearly report input
    yearlyo: str = ""  # Yearly report output
    
    customi: str = ""  # Custom report input
    customo: str = ""  # Custom report output
    
    # Start date fields
    sdtmmi: str = ""  # Start date month input
    sdtmmo: str = ""  # Start date month output
    sdtmml: int = 0  # Start date month length (cursor)
    
    sdtddi: str = ""  # Start date day input
    sdtddo: str = ""  # Start date day output
    sdtddl: int = 0  # Start date day length (cursor)
    
    sdtyyyyi: str = ""  # Start date year input
    sdtyyyyo: str = ""  # Start date year output
    sdtyyyyl: int = 0  # Start date year length (cursor)
    
    # End date fields
    edtmmi: str = ""  # End date month input
    edtmmo: str = ""  # End date month output
    edtmml: int = 0  # End date month length (cursor)
    
    edtddi: str = ""  # End date day input
    edtddo: str = ""  # End date day output
    edtddl: int = 0  # End date day length (cursor)
    
    edtyyyyi: str = ""  # End date year input
    edtyyyyo: str = ""  # End date year output
    edtyyyyl: int = 0  # End date year length (cursor)
    
    # Confirmation field
    confirmy: str = ""  # Confirmation input
    confirmo: str = ""  # Confirmation output
    confirml: int = 0  # Confirmation length (cursor)
    
    # Message fields
    errmsgo: str = ""
    errmsgc: str = ""  # Color attribute


# ============================================================================
# Working Storage
# ============================================================================

class WorkingStorage:
    """Working storage for program state."""
    
    def __init__(self):
        self.pgmname: str = WS_PGMNAME
        self.tranid: str = WS_TRANID
        self.message: str = ""
        self.transact_file: str = WS_TRANSACT_FILE
        self.err_flg: bool = False  # False = 'N', True = 'Y'
        self.transact_eof: bool = False
        self.send_erase_flg: bool = True
        self.end_loop: bool = False
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self.rec_count: int = 0
        self.idx: int = 0
        self.report_name: str = ""
        
        # Date fields
        self.start_date_yyyy: str = ""
        self.start_date_mm: str = ""
        self.start_date_dd: str = ""
        self.end_date_yyyy: str = ""
        self.end_date_mm: str = ""
        self.end_date_dd: str = ""
        self.date_format: str = "YYYY-MM-DD"
        
        # Numeric fields
        self.num_99: int = 0
        self.num_9999: int = 0
        
        # Current date/time
        self.curdate_data: str = ""
        self.curdate_month: str = ""
        self.curdate_day: str = ""
        self.curdate_year: str = ""
        self.curdate_yy: str = ""
        self.curdate_mm_dd_yy: str = ""
        self.curtime_hours: str = ""
        self.curtime_minute: str = ""
        self.curtime_second: str = ""
        self.curtime_hh_mm_ss: str = ""
        
        # JCL data
        self.job_lines: List[str] = []
        self.jcl_record: str = ""
        
        # TDQ handler
        self.tdq_handler: Optional['TDQHandler'] = None


# ============================================================================
# TDQ Handler
# ============================================================================

class TDQHandler:
    """Handler for Transient Data Queue (TDQ) operations."""
    
    def __init__(self, queue_name: str):
        self.queue_name = queue_name
        self.resp_cd: int = 0
        self.reas_cd: int = 0
        self._queue: List[str] = []
    
    def writeq_td(self, record: str) -> int:
        """Write record to TDQ (CICS WRITEQ TD).
        
        Returns:
            Response code (0 = normal)
        """
        try:
            self._queue.append(record)
            self.resp_cd = DFHRESP_NORMAL
            return 0
        except Exception as e:
            self.resp_cd = 12
            return 12
    
    def get_queue_contents(self) -> List[str]:
        """Get all records in queue."""
        return self._queue.copy()
    
    def clear_queue(self):
        """Clear queue."""
        self._queue = []


# ============================================================================
# Date Validation
# ============================================================================

def validate_date(date_str: str, date_format: str = "YYYY-MM-DD") -> DateValidationResult:
    """Validate date (CSUTLDTC equivalent).
    
    Args:
        date_str: Date string in YYYY-MM-DD format
        date_format: Date format (default: YYYY-MM-DD)
    
    Returns:
        DateValidationResult with validation result
    """
    result = DateValidationResult()
    
    try:
        # Parse date
        if date_format == "YYYY-MM-DD":
            parts = date_str.split('-')
            if len(parts) != 3:
                result.sev_cd = "0001"
                result.msg_num = "2513"
                result.msg = "Date is invalid"
                return result
            
            year = int(parts[0])
            month = int(parts[1])
            day = int(parts[2])
            
            # Validate date
            datetime(year, month, day)
            result.sev_cd = "0000"
            result.msg = "Date is valid"
        else:
            result.sev_cd = "0001"
            result.msg_num = "2513"
            result.msg = "Date is invalid"
    except (ValueError, IndexError):
        result.sev_cd = "0001"
        result.msg_num = "2513"
        result.msg = "Date is invalid"
    
    return result


# ============================================================================
# JCL Generation
# ============================================================================

def build_jcl_template(start_date: str, end_date: str) -> List[str]:
    """Build JCL template with date parameters.
    
    Args:
        start_date: Start date in YYYY-MM-DD format
        end_date: End date in YYYY-MM-DD format
    
    Returns:
        List of JCL lines
    """
    jcl_lines = [
        "//TRNRPT00 JOB 'TRAN REPORT',CLASS=A,MSGCLASS=0,",
        "// NOTIFY=&SYSUID",
        "//*",
        "//JOBLIB JCLLIB ORDER=('AWS.M2.CARDDEMO.PROC')",
        "//*",
        "//STEP10 EXEC PROC=TRANREPT",
        "//*",
        "//STEP05R.SYMNAMES DD *",
        "TRAN-CARD-NUM,263,16,ZD",
        "TRAN-PROC-DT,305,10,CH",
        f"PARM-START-DATE,C'{start_date}'",
        f"PARM-END-DATE,C'{end_date}'",
        "/*",
        "//STEP10R.DATEPARM DD *",
        f"{start_date} {end_date}",
        "/*",
        "/*EOF"
    ]
    return jcl_lines


# ============================================================================
# Map Functions
# ============================================================================

def populate_header_info(ws: WorkingStorage, map_out: ReportMap):
    """Populate header information (POPULATE-HEADER-INFO)."""
    now = datetime.now()
    ws.curdate_data = now.strftime("%Y%m%d%H%M%S")
    
    ws.curdate_month = f"{now.month:02d}"
    ws.curdate_day = f"{now.day:02d}"
    ws.curdate_year = now.strftime("%Y")
    ws.curdate_yy = now.strftime("%y")
    ws.curdate_mm_dd_yy = f"{ws.curdate_month}/{ws.curdate_day}/{ws.curdate_yy}"
    
    ws.curtime_hours = f"{now.hour:02d}"
    ws.curtime_minute = f"{now.minute:02d}"
    ws.curtime_second = f"{now.second:02d}"
    ws.curtime_hh_mm_ss = f"{ws.curtime_hours}:{ws.curtime_minute}:{ws.curtime_second}"
    
    map_out.title01o = CCDA_TITLE01
    map_out.title02o = CCDA_TITLE02
    map_out.trnnameo = ws.tranid
    map_out.pgmnameo = ws.pgmname
    map_out.curdateo = ws.curdate_mm_dd_yy
    map_out.curtimeo = ws.curtime_hh_mm_ss


def send_trnrpt_screen(ws: WorkingStorage, map_out: ReportMap):
    """Send report screen (SEND-TRNRPT-SCREEN)."""
    populate_header_info(ws, map_out)
    map_out.errmsgo = ws.message


def receive_trnrpt_screen(ws: WorkingStorage, map_in: ReportMap):
    """Receive report screen (RECEIVE-TRNRPT-SCREEN)."""
    ws.resp_cd = 0
    ws.reas_cd = 0
    # Extract input fields from map
    # (In real CICS, this would be done automatically by RECEIVE MAP)


def initialize_all_fields(ws: WorkingStorage, map_out: ReportMap):
    """Initialize all fields (INITIALIZE-ALL-FIELDS)."""
    map_out.monthlyl = -1
    map_out.monthlyi = ""
    map_out.yearlyi = ""
    map_out.customi = ""
    map_out.sdtmmi = ""
    map_out.sdtddi = ""
    map_out.sdtyyyyi = ""
    map_out.edtmmi = ""
    map_out.edtddi = ""
    map_out.edtyyyyi = ""
    map_out.confirmy = ""
    ws.message = ""


# ============================================================================
# Process Enter Key
# ============================================================================

def process_monthly_report(ws: WorkingStorage):
    """Process monthly report selection."""
    ws.report_name = "Monthly"
    now = datetime.now()
    
    # Start date: first day of current month
    ws.start_date_yyyy = now.strftime("%Y")
    ws.start_date_mm = f"{now.month:02d}"
    ws.start_date_dd = "01"
    start_date = f"{ws.start_date_yyyy}-{ws.start_date_mm}-{ws.start_date_dd}"
    
    # End date: last day of current month
    last_day = monthrange(now.year, now.month)[1]
    ws.end_date_yyyy = now.strftime("%Y")
    ws.end_date_mm = f"{now.month:02d}"
    ws.end_date_dd = f"{last_day:02d}"
    end_date = f"{ws.end_date_yyyy}-{ws.end_date_mm}-{ws.end_date_dd}"
    
    return start_date, end_date


def process_yearly_report(ws: WorkingStorage):
    """Process yearly report selection."""
    ws.report_name = "Yearly"
    now = datetime.now()
    
    # Start date: January 1 of current year
    ws.start_date_yyyy = now.strftime("%Y")
    ws.start_date_mm = "01"
    ws.start_date_dd = "01"
    start_date = f"{ws.start_date_yyyy}-{ws.start_date_mm}-{ws.start_date_dd}"
    
    # End date: December 31 of current year
    ws.end_date_yyyy = now.strftime("%Y")
    ws.end_date_mm = "12"
    ws.end_date_dd = "31"
    end_date = f"{ws.end_date_yyyy}-{ws.end_date_mm}-{ws.end_date_dd}"
    
    return start_date, end_date


def process_custom_report(ws: WorkingStorage, map_in: ReportMap, map_out: ReportMap) -> Optional[Tuple[str, str]]:
    """Process custom report selection.
    
    Returns:
        Tuple of (start_date, end_date) if valid, None if error
    """
    # Validate all fields are present
    if not map_in.sdtmmi or map_in.sdtmmi.strip() == "":
        ws.message = "Start Date - Month can NOT be empty..."
        ws.err_flg = True
        map_out.sdtmml = -1
        return None
    
    if not map_in.sdtddi or map_in.sdtddi.strip() == "":
        ws.message = "Start Date - Day can NOT be empty..."
        ws.err_flg = True
        map_out.sdtddl = -1
        return None
    
    if not map_in.sdtyyyyi or map_in.sdtyyyyi.strip() == "":
        ws.message = "Start Date - Year can NOT be empty..."
        ws.err_flg = True
        map_out.sdtyyyyl = -1
        return None
    
    if not map_in.edtmmi or map_in.edtmmi.strip() == "":
        ws.message = "End Date - Month can NOT be empty..."
        ws.err_flg = True
        map_out.edtmml = -1
        return None
    
    if not map_in.edtddi or map_in.edtddi.strip() == "":
        ws.message = "End Date - Day can NOT be empty..."
        ws.err_flg = True
        map_out.edtddl = -1
        return None
    
    if not map_in.edtyyyyi or map_in.edtyyyyi.strip() == "":
        ws.message = "End Date - Year can NOT be empty..."
        ws.err_flg = True
        map_out.edtyyyyl = -1
        return None
    
    # Convert to numeric (COBOL NUMVAL-C equivalent)
    try:
        sdtmm = int(map_in.sdtmmi.strip())
        sdtdd = int(map_in.sdtddi.strip())
        sdtyyyy = int(map_in.sdtyyyyi.strip())
        edtmm = int(map_in.edtmmi.strip())
        edtdd = int(map_in.edtddi.strip())
        edtyyyy = int(map_in.edtyyyyi.strip())
    except ValueError:
        ws.message = "Date fields must be numeric..."
        ws.err_flg = True
        map_out.sdtmml = -1
        return None
    
    # Validate month
    if sdtmm < 1 or sdtmm > 12:
        ws.message = "Start Date - Not a valid Month..."
        ws.err_flg = True
        map_out.sdtmml = -1
        return None
    
    if edtmm < 1 or edtmm > 12:
        ws.message = "End Date - Not a valid Month..."
        ws.err_flg = True
        map_out.edtmml = -1
        return None
    
    # Validate day (basic check - max 31)
    if sdtdd < 1 or sdtdd > 31:
        ws.message = "Start Date - Not a valid Day..."
        ws.err_flg = True
        map_out.sdtddl = -1
        return None
    
    if edtdd < 1 or edtdd > 31:
        ws.message = "End Date - Not a valid Day..."
        ws.err_flg = True
        map_out.edtddl = -1
        return None
    
    # Format dates
    ws.start_date_yyyy = f"{sdtyyyy:04d}"
    ws.start_date_mm = f"{sdtmm:02d}"
    ws.start_date_dd = f"{sdtdd:02d}"
    start_date = f"{ws.start_date_yyyy}-{ws.start_date_mm}-{ws.start_date_dd}"
    
    ws.end_date_yyyy = f"{edtyyyy:04d}"
    ws.end_date_mm = f"{edtmm:02d}"
    ws.end_date_dd = f"{edtdd:02d}"
    end_date = f"{ws.end_date_yyyy}-{ws.end_date_mm}-{ws.end_date_dd}"
    
    # Validate dates using date validation
    result_start = validate_date(start_date, ws.date_format)
    if result_start.sev_cd != "0000":
        ws.message = "Start Date - Not a valid date..."
        ws.err_flg = True
        map_out.sdtmml = -1
        return None
    
    result_end = validate_date(end_date, ws.date_format)
    if result_end.sev_cd != "0000":
        ws.message = "End Date - Not a valid date..."
        ws.err_flg = True
        map_out.edtmml = -1
        return None
    
    ws.report_name = "Custom"
    return start_date, end_date


def process_enter_key(ws: WorkingStorage, map_in: ReportMap, map_out: ReportMap) -> Optional[Tuple[str, str]]:
    """Process enter key (PROCESS-ENTER-KEY).
    
    Returns:
        Tuple of (start_date, end_date) if valid, None if error
    """
    # Check which report type is selected
    if map_in.monthlyi and map_in.monthlyi.strip() != "":
        return process_monthly_report(ws)
    elif map_in.yearlyi and map_in.yearlyi.strip() != "":
        return process_yearly_report(ws)
    elif map_in.customi and map_in.customi.strip() != "":
        return process_custom_report(ws, map_in, map_out)
    else:
        ws.message = "Select a report type to print report..."
        ws.err_flg = True
        map_out.monthlyl = -1
        return None


# ============================================================================
# Submit Job to Internal Reader
# ============================================================================

def write_jobsub_tdq(ws: WorkingStorage, jcl_record: str) -> bool:
    """Write JCL record to TDQ (WIRTE-JOBSUB-TDQ).
    
    Returns:
        True if successful, False otherwise
    """
    if not ws.tdq_handler:
        ws.err_flg = True
        ws.message = "Unable to Write TDQ (JOBS)..."
        return False
    
    resp_cd = ws.tdq_handler.writeq_td(jcl_record)
    ws.resp_cd = resp_cd
    
    if resp_cd == DFHRESP_NORMAL:
        return True
    else:
        ws.err_flg = True
        ws.message = "Unable to Write TDQ (JOBS)..."
        return False


def submit_job_to_intrdr(ws: WorkingStorage, map_in: ReportMap, map_out: ReportMap, start_date: str, end_date: str) -> bool:
    """Submit job to internal reader (SUBMIT-JOB-TO-INTRDR).
    
    Returns:
        True if successful, False otherwise
    """
    # Check confirmation
    if not map_in.confirmy or map_in.confirmy.strip() == "":
        ws.message = f"Please confirm to print the {ws.report_name} report..."
        ws.err_flg = True
        map_out.confirml = -1
        return False
    
    confirm = map_in.confirmy.strip().upper()
    
    if confirm == "N":
        # User cancelled
        initialize_all_fields(ws, map_out)
        ws.err_flg = True
        return False
    
    if confirm != "Y":
        ws.message = f'"{map_in.confirmy}" is not a valid value to confirm...'
        ws.err_flg = True
        map_out.confirml = -1
        return False
    
    # Build JCL
    jcl_lines = build_jcl_template(start_date, end_date)
    
    # Write JCL to TDQ
    ws.end_loop = False
    for idx, jcl_line in enumerate(jcl_lines):
        if jcl_line == "/*EOF" or not jcl_line or jcl_line.strip() == "":
            ws.end_loop = True
            break
        
        # Pad to 80 characters
        jcl_record = jcl_line.ljust(80)
        
        if not write_jobsub_tdq(ws, jcl_record):
            return False
    
    return True


# ============================================================================
# Return to Previous Screen
# ============================================================================

def return_to_prev_screen(commarea: CardDemoCommarea) -> str:
    """Return to previous screen (RETURN-TO-PREV-SCREEN).
    
    Returns:
        Program name to XCTL to
    """
    if not commarea.to_program or commarea.to_program.strip() == "":
        return COSGN00C
    return commarea.to_program


# ============================================================================
# Main Program
# ============================================================================

def main(
    commarea: Optional[CardDemoCommarea] = None,
    eibcalen: int = 0,
    eibaid: str = DFHENTER,
    map_input: Optional[ReportMap] = None,
    tdq_queue: Optional[TDQHandler] = None
) -> Tuple[CardDemoCommarea, ReportMap, Optional[str]]:
    """Main program entry point (MAIN-PARA).
    
    Returns:
        Tuple of (commarea, map_output, xctl_program)
    """
    # Initialize working storage
    ws = WorkingStorage()
    ws.err_flg = False
    ws.transact_eof = False
    ws.send_erase_flg = True
    ws.message = ""
    
    # Initialize commarea if not provided
    if commarea is None:
        commarea = CardDemoCommarea()
    
    # Initialize maps
    map_out = ReportMap()
    if map_input is None:
        map_input = ReportMap()
    
    # Initialize TDQ handler
    if tdq_queue is None:
        ws.tdq_handler = TDQHandler(TDQ_QUEUE_NAME)
    else:
        ws.tdq_handler = tdq_queue
    
    # Main logic
    if eibcalen == 0:
        # First call - return to signon
        commarea.to_program = COSGN00C
        xctl_prog = return_to_prev_screen(commarea)
        return commarea, map_out, xctl_prog
    else:
        # Process commarea
        if not commarea.pgm_reenter:
            # First entry - show screen
            commarea.pgm_reenter = True
            map_out.monthlyl = -1
            send_trnrpt_screen(ws, map_out)
        else:
            # Reenter - process input
            receive_trnrpt_screen(ws, map_input)
            
            if eibaid == DFHENTER:
                # Process enter key
                date_range = process_enter_key(ws, map_input, map_out)
                
                if date_range and not ws.err_flg:
                    # Submit job
                    success = submit_job_to_intrdr(ws, map_input, map_out, date_range[0], date_range[1])
                    
                    if success and not ws.err_flg:
                        # Success - initialize fields and show success message
                        initialize_all_fields(ws, map_out)
                        map_out.errmsgc = DFHGREEN
                        ws.message = f"{ws.report_name} report submitted for printing ..."
                        map_out.monthlyl = -1
                        send_trnrpt_screen(ws, map_out)
                    else:
                        # Error - show error message
                        send_trnrpt_screen(ws, map_out)
                else:
                    # Error - show error message
                    send_trnrpt_screen(ws, map_out)
                
                xctl_prog = None
            elif eibaid == DFHPF3:
                # PF3 - return to menu
                commarea.to_program = COMEN01C
                xctl_prog = return_to_prev_screen(commarea)
                return commarea, map_out, xctl_prog
            else:
                # Invalid key
                ws.err_flg = True
                ws.message = CCDA_MSG_INVALID_KEY
                map_out.monthlyl = -1
                send_trnrpt_screen(ws, map_out)
                xctl_prog = None
    
    # Set message in map
    map_out.errmsgo = ws.message
    
    return commarea, map_out, None

