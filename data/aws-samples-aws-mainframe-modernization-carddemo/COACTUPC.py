"""
Python Translation of COACTUPC.cbl

Program: COACTUPC
Layer: Business logic
Function: Accept and process ACCOUNT UPDATE

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
import re
from dataclasses import dataclass, field
from typing import Optional, Dict
from decimal import Decimal, ROUND_HALF_UP


# ============================================================================
# Record Structures
# ============================================================================

@dataclass
class CardXrefRecord:
    """Card cross-reference record (CARD-XREF-RECORD from COPY CVACT03Y)."""
    card_num: str = ""  # 16 chars
    cust_id: str = ""  # 9 chars
    acct_id: str = ""  # 11 chars
    
    @property
    def xref_card_num(self) -> str:
        return self.card_num
    
    @property
    def xref_cust_id(self) -> str:
        return self.cust_id
    
    @property
    def xref_acct_id(self) -> str:
        return self.acct_id


@dataclass
class AccountRecord:
    """Account record (ACCOUNT-RECORD from COPY CVACT01Y)."""
    acct_id: str = ""  # 11 chars
    active_status: str = ""  # 1 char
    curr_bal: Decimal = Decimal('0')  # S9(10)V99
    credit_limit: Decimal = Decimal('0')  # S9(10)V99
    cash_credit_limit: Decimal = Decimal('0')  # S9(10)V99
    open_date: str = ""  # 10 chars (YYYY-MM-DD)
    expiration_date: str = ""  # 10 chars (YYYY-MM-DD)
    reissue_date: str = ""  # 10 chars (YYYY-MM-DD)
    curr_cyc_credit: Decimal = Decimal('0')  # S9(10)V99
    curr_cyc_debit: Decimal = Decimal('0')  # S9(10)V99
    group_id: str = ""  # 10 chars
    
    @property
    def acct_active_status(self) -> str:
        return self.active_status
    
    @property
    def acct_curr_bal(self) -> Decimal:
        return self.curr_bal
    
    @property
    def acct_credit_limit(self) -> Decimal:
        return self.credit_limit
    
    @property
    def acct_cash_credit_limit(self) -> Decimal:
        return self.cash_credit_limit
    
    @property
    def acct_open_date(self) -> str:
        return self.open_date
    
    @property
    def acct_expiration_date(self) -> str:
        return self.expiration_date
    
    @property
    def acct_reissue_date(self) -> str:
        return self.reissue_date
    
    @property
    def acct_curr_cyc_credit(self) -> Decimal:
        return self.curr_cyc_credit
    
    @property
    def acct_curr_cyc_debit(self) -> Decimal:
        return self.curr_cyc_debit
    
    @property
    def acct_group_id(self) -> str:
        return self.group_id


@dataclass
class CustomerRecord:
    """Customer record (CUSTOMER-RECORD from COPY CVCUS01Y)."""
    cust_id: str = ""  # 9 chars
    first_name: str = ""  # 25 chars
    middle_name: str = ""  # 25 chars
    last_name: str = ""  # 25 chars
    addr_line_1: str = ""  # 50 chars
    addr_line_2: str = ""  # 50 chars
    addr_line_3: str = ""  # 50 chars
    addr_state_cd: str = ""  # 2 chars
    addr_country_cd: str = ""  # 3 chars
    addr_zip: str = ""  # 10 chars
    phone_num_1: str = ""  # 15 chars
    phone_num_2: str = ""  # 15 chars
    ssn: str = ""  # 9 chars
    govt_issued_id: str = ""  # 20 chars
    dob_yyyy_mm_dd: str = ""  # 10 chars (YYYY-MM-DD)
    eft_account_id: str = ""  # 10 chars
    pri_card_ind: str = ""  # 1 char
    fico_credit_score: int = 0  # 9(03)
    
    @property
    def cust_first_name(self) -> str:
        return self.first_name
    
    @property
    def cust_middle_name(self) -> str:
        return self.middle_name
    
    @property
    def cust_last_name(self) -> str:
        return self.last_name
    
    @property
    def cust_addr_line_1(self) -> str:
        return self.addr_line_1
    
    @property
    def cust_addr_line_2(self) -> str:
        return self.addr_line_2
    
    @property
    def cust_addr_line_3(self) -> str:
        return self.addr_line_3
    
    @property
    def cust_addr_state_cd(self) -> str:
        return self.addr_state_cd
    
    @property
    def cust_addr_country_cd(self) -> str:
        return self.addr_country_cd
    
    @property
    def cust_addr_zip(self) -> str:
        return self.addr_zip
    
    @property
    def cust_phone_num_1(self) -> str:
        return self.phone_num_1
    
    @property
    def cust_phone_num_2(self) -> str:
        return self.phone_num_2
    
    @property
    def cust_ssn(self) -> str:
        return self.ssn
    
    @property
    def cust_govt_issued_id(self) -> str:
        return self.govt_issued_id
    
    @property
    def cust_dob_yyyy_mm_dd(self) -> str:
        return self.dob_yyyy_mm_dd
    
    @property
    def cust_eft_account_id(self) -> str:
        return self.eft_account_id
    
    @property
    def cust_pri_card_ind(self) -> str:
        return self.pri_card_ind
    
    @property
    def cust_fico_credit_score(self) -> int:
        return self.fico_credit_score


# ============================================================================
# File Handlers
# ============================================================================

class IndexedFileHandler:
    """Handler for indexed files (simulated with dictionary)."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.file_status = "00"
        self._data: Dict[str, str] = {}
        self._alt_index_data: Dict[str, str] = {}  # For alternate index access
    
    def open_file(self, mode: str = "r") -> int:
        """Open file and load data."""
        try:
            if mode in ("r", "r+"):
                with open(self.filename, "r", encoding='utf-8') as f:
                    for line in f:
                        line = line.rstrip('\n\r')
                        if len(line) > 0:
                            key = self._extract_key(line)
                            if key:
                                self._data[key] = line
                            # Also store by alternate key if applicable
                            alt_key = self._extract_alt_key(line)
                            if alt_key:
                                self._alt_index_data[alt_key] = line
            self.file_status = "00"
            return 0
        except FileNotFoundError:
            self.file_status = "23"
            return 12
        except Exception as e:
            self.file_status = "99"
            return 12
    
    def _extract_key(self, line: str) -> str:
        """Extract primary key from record line."""
        # Default: first 16 chars for card num (CARDXREF)
        if len(line) >= 16:
            return line[:16]
        return ""
    
    def _extract_alt_key(self, line: str) -> str:
        """Extract alternate key from record line."""
        # For CARDXREF alternate index by ACCT-ID: starts at position 25
        if len(line) >= 36:
            return line[25:36]  # ACCT-ID is at position 25-35
        return ""
    
    def read_record(self, key: str, use_alt_index: bool = False) -> tuple[Optional[str], int]:
        """Read record by key."""
        try:
            if use_alt_index:
                record = self._alt_index_data.get(key)
            else:
                record = self._data.get(key)
            
            if record:
                self.file_status = "00"
                return record, 0
            else:
                self.file_status = "23"  # Not found
                return None, 12
        except Exception as e:
            self.file_status = "99"
            return None, 12
    
    def read_for_update(self, key: str, use_alt_index: bool = False) -> tuple[Optional[str], int]:
        """Read record for update (simulates CICS READ UPDATE)."""
        return self.read_record(key, use_alt_index)
    
    def rewrite_record(self, key: str, record: str) -> int:
        """Rewrite record (simulates CICS REWRITE)."""
        try:
            if key in self._data:
                self._data[key] = record
                # Update alternate index if applicable
                alt_key = self._extract_alt_key(record)
                if alt_key:
                    self._alt_index_data[alt_key] = record
                self.file_status = "00"
                return 0
            else:
                self.file_status = "23"
                return 12
        except Exception as e:
            self.file_status = "99"
            return 12
    
    def close_file(self) -> int:
        """Close file and persist data."""
        try:
            with open(self.filename, "w", encoding='utf-8') as f:
                for key in sorted(self._data.keys()):
                    f.write(self._data[key] + '\n')
            self.file_status = "00"
            return 0
        except Exception as e:
            self.file_status = "99"
            return 12
    
    def display_io_status(self):
        """Display I/O status."""
        io_status = self.file_status
        if len(io_status) != 2:
            io_status = "00"
        
        if io_status[0].isdigit() and io_status[1].isdigit() and io_status[0] != '9':
            status_04 = f"00{io_status}"
            print(f'FILE STATUS IS: NNNN {status_04}')
        else:
            left = io_status[0] if len(io_status) > 0 else '0'
            right = io_status[1] if len(io_status) > 1 else '0'
            status_04 = f"{left}00{right}"
            print(f'FILE STATUS IS: NNNN {status_04}')


class XrefFileHandler(IndexedFileHandler):
    """Handler for CARDXREF file."""
    
    def _extract_key(self, line: str) -> str:
        """Extract card number (primary key)."""
        if len(line) >= 16:
            return line[:16]
        return ""
    
    def _extract_alt_key(self, line: str) -> str:
        """Extract account ID (alternate key)."""
        if len(line) >= 36:
            return line[25:36]  # ACCT-ID is at position 25-35
        return ""


class AcctFileHandler(IndexedFileHandler):
    """Handler for ACCTDAT file."""
    
    def _extract_key(self, line: str) -> str:
        """Extract account ID (primary key)."""
        if len(line) >= 11:
            return line[:11]
        return ""
    
    def _extract_alt_key(self, line: str) -> str:
        """No alternate index for ACCTDAT."""
        return ""


class CustFileHandler(IndexedFileHandler):
    """Handler for CUSTDAT file."""
    
    def _extract_key(self, line: str) -> str:
        """Extract customer ID (primary key)."""
        if len(line) >= 9:
            return line[:9]
        return ""
    
    def _extract_alt_key(self, line: str) -> str:
        """No alternate index for CUSTDAT."""
        return ""


# ============================================================================
# Record Parsing Functions
# ============================================================================

def parse_xref_record(line: str) -> CardXrefRecord:
    """Parse CARDXREF record."""
    # Extract what's available even if line is shorter than expected
    return CardXrefRecord(
        card_num=line[:16] if len(line) >= 16 else line[:len(line)],
        cust_id=line[16:25] if len(line) >= 25 else "",
        acct_id=line[25:36] if len(line) >= 36 else ""
    )


def parse_account_record(line: str) -> AccountRecord:
    """Parse ACCOUNT record.
    
    Format based on COPY CVACT01Y:
    - ACCT-ID: PIC 9(11) - positions 0-10
    - ACTIVE-STATUS: PIC X(01) - position 11
    - CURR-BAL: PIC S9(10)V99 - positions 12-23 (12 chars, signed numeric)
    - CREDIT-LIMIT: PIC S9(10)V99 - positions 24-35
    - CASH-CREDIT-LIMIT: PIC S9(10)V99 - positions 36-47
    - OPEN-DATE: PIC X(10) - positions 48-57 (YYYY-MM-DD format)
    - EXPIRAION-DATE: PIC X(10) - positions 58-67
    - REISSUE-DATE: PIC X(10) - positions 68-77
    - CURR-CYC-CREDIT: PIC S9(10)V99 - positions 78-89
    - CURR-CYC-DEBIT: PIC S9(10)V99 - positions 90-101
    - GROUP-ID: PIC X(10) - positions 102-111
    - FILLER: PIC X(188) - positions 112-299
    """
    # Extract what's available even if line is shorter than expected
    
    # Parse fixed fields
    acct_id = line[:11].strip() if len(line) >= 11 else ""
    active_status = line[11:12] if len(line) >= 12 else ""
    
    # Parse numeric fields (assuming they're stored as strings with decimal point)
    try:
        curr_bal_str = line[12:24].strip() if len(line) >= 24 else "0"
        # Remove any leading/trailing spaces and parse
        curr_bal = Decimal(curr_bal_str) if curr_bal_str and curr_bal_str.replace('.', '').replace('-', '').strip().isdigit() else Decimal('0')
    except:
        curr_bal = Decimal('0')
    
    try:
        credit_limit_str = line[24:36].strip() if len(line) >= 36 else "0"
        credit_limit = Decimal(credit_limit_str) if credit_limit_str and credit_limit_str.replace('.', '').replace('-', '').strip().isdigit() else Decimal('0')
    except:
        credit_limit = Decimal('0')
    
    try:
        cash_credit_limit_str = line[36:48].strip() if len(line) >= 48 else "0"
        cash_credit_limit = Decimal(cash_credit_limit_str) if cash_credit_limit_str and cash_credit_limit_str.replace('.', '').replace('-', '').strip().isdigit() else Decimal('0')
    except:
        cash_credit_limit = Decimal('0')
    
    open_date = line[48:58].strip() if len(line) >= 58 else ""
    expiration_date = line[58:68].strip() if len(line) >= 68 else ""
    reissue_date = line[68:78].strip() if len(line) >= 78 else ""
    
    try:
        curr_cyc_credit_str = line[78:90].strip() if len(line) >= 90 else "0"
        curr_cyc_credit = Decimal(curr_cyc_credit_str) if curr_cyc_credit_str and curr_cyc_credit_str.replace('.', '').replace('-', '').strip().isdigit() else Decimal('0')
    except:
        curr_cyc_credit = Decimal('0')
    
    try:
        curr_cyc_debit_str = line[90:102].strip() if len(line) >= 102 else "0"
        curr_cyc_debit = Decimal(curr_cyc_debit_str) if curr_cyc_debit_str and curr_cyc_debit_str.replace('.', '').replace('-', '').strip().isdigit() else Decimal('0')
    except:
        curr_cyc_debit = Decimal('0')
    
    group_id = line[102:112].strip() if len(line) >= 112 else ""
    
    return AccountRecord(
        acct_id=acct_id,
        active_status=active_status,
        curr_bal=curr_bal,
        credit_limit=credit_limit,
        cash_credit_limit=cash_credit_limit,
        open_date=open_date,
        expiration_date=expiration_date,
        reissue_date=reissue_date,
        curr_cyc_credit=curr_cyc_credit,
        curr_cyc_debit=curr_cyc_debit,
        group_id=group_id
    )


def parse_customer_record(line: str) -> CustomerRecord:
    """Parse CUSTOMER record."""
    if len(line) < 300:
        return CustomerRecord()
    
    # Parse fixed fields based on COPY CVCUS01Y structure
    cust_id = line[:9] if len(line) >= 9 else ""
    first_name = line[9:34].strip() if len(line) >= 34 else ""
    middle_name = line[34:59].strip() if len(line) >= 59 else ""
    last_name = line[59:84].strip() if len(line) >= 84 else ""
    addr_line_1 = line[84:134].strip() if len(line) >= 134 else ""
    addr_line_2 = line[134:184].strip() if len(line) >= 184 else ""
    addr_line_3 = line[184:234].strip() if len(line) >= 234 else ""
    addr_state_cd = line[234:236].strip() if len(line) >= 236 else ""
    addr_country_cd = line[236:239].strip() if len(line) >= 239 else ""
    addr_zip = line[239:249].strip() if len(line) >= 249 else ""
    phone_num_1 = line[249:264].strip() if len(line) >= 264 else ""
    phone_num_2 = line[264:279].strip() if len(line) >= 279 else ""
    ssn = line[279:288] if len(line) >= 288 else ""
    govt_issued_id = line[288:308].strip() if len(line) >= 308 else ""
    dob_yyyy_mm_dd = line[308:318] if len(line) >= 318 else ""
    eft_account_id = line[318:328].strip() if len(line) >= 328 else ""
    pri_card_ind = line[328:329] if len(line) >= 329 else ""
    
    try:
        fico_credit_score = int(line[329:332]) if len(line) >= 332 and line[329:332].strip().isdigit() else 0
    except:
        fico_credit_score = 0
    
    return CustomerRecord(
        cust_id=cust_id,
        first_name=first_name,
        middle_name=middle_name,
        last_name=last_name,
        addr_line_1=addr_line_1,
        addr_line_2=addr_line_2,
        addr_line_3=addr_line_3,
        addr_state_cd=addr_state_cd,
        addr_country_cd=addr_country_cd,
        addr_zip=addr_zip,
        phone_num_1=phone_num_1,
        phone_num_2=phone_num_2,
        ssn=ssn,
        govt_issued_id=govt_issued_id,
        dob_yyyy_mm_dd=dob_yyyy_mm_dd,
        eft_account_id=eft_account_id,
        pri_card_ind=pri_card_ind,
        fico_credit_score=fico_credit_score
    )


def format_account_record(record: AccountRecord) -> str:
    """Format account record for writing."""
    # Format as fixed-width record
    # Format numeric fields as strings with proper width (always format, even if zero)
    curr_bal_str = f"{record.curr_bal:>12.2f}"
    credit_limit_str = f"{record.credit_limit:>12.2f}"
    cash_credit_limit_str = f"{record.cash_credit_limit:>12.2f}"
    curr_cyc_credit_str = f"{record.curr_cyc_credit:>12.2f}"
    curr_cyc_debit_str = f"{record.curr_cyc_debit:>12.2f}"
    
    line = (
        f"{record.acct_id:<11}"
        f"{record.active_status:<1}"
        f"{curr_bal_str:>12}"
        f"{credit_limit_str:>12}"
        f"{cash_credit_limit_str:>12}"
        f"{record.open_date:<10}"
        f"{record.expiration_date:<10}"
        f"{record.reissue_date:<10}"
        f"{curr_cyc_credit_str:>12}"
        f"{curr_cyc_debit_str:>12}"
        f"{record.group_id:<10}"
    )
    # Pad to 300 characters
    line = line.ljust(300)
    return line[:300]


def format_customer_record(record: CustomerRecord) -> str:
    """Format customer record for writing."""
    # Format as fixed-width record
    line = (
        f"{record.cust_id:<9}"
        f"{record.first_name:<25}"
        f"{record.middle_name:<25}"
        f"{record.last_name:<25}"
        f"{record.addr_line_1:<50}"
        f"{record.addr_line_2:<50}"
        f"{record.addr_line_3:<50}"
        f"{record.addr_state_cd:<2}"
        f"{record.addr_country_cd:<3}"
        f"{record.addr_zip:<10}"
        f"{record.phone_num_1:<15}"
        f"{record.phone_num_2:<15}"
        f"{record.ssn:<9}"
        f"{record.govt_issued_id:<20}"
        f"{record.dob_yyyy_mm_dd:<10}"
        f"{record.eft_account_id:<10}"
        f"{record.pri_card_ind:<1}"
        f"{record.fico_credit_score:>3}"
    )
    # Pad to 500 characters
    line = line.ljust(500)
    return line[:500]


# ============================================================================
# Validation and Edit Routines
# ============================================================================

def edit_yes_no(value: str, field_name: str) -> tuple[bool, Optional[str]]:
    """Edit Yes/No field (Y or N)."""
    if not value or value.strip() == '':
        return False, f"{field_name} must be supplied."
    value_upper = value.strip().upper()
    if value_upper not in ('Y', 'N'):
        return False, f"{field_name} must be Y or N."
    return True, None


def edit_signed_number(value: str, field_name: str) -> tuple[bool, Optional[str], Optional[Decimal]]:
    """Edit signed numeric field (S9(10)V99 format)."""
    if not value or value.strip() == '':
        return False, f"{field_name} must be supplied.", None
    
    try:
        # Remove commas and parse
        cleaned = value.strip().replace(',', '')
        num_value = Decimal(cleaned)
        return True, None, num_value
    except:
        return False, f"{field_name} is not valid", None


def edit_date_ccyymmdd(year: str, month: str, day: str, field_name: str) -> tuple[bool, Optional[str], Optional[str]]:
    """Edit date in CCYYMMDD format."""
    errors = []
    
    # Validate year
    if not year or not year.strip() or not year.strip().isdigit():
        errors.append(f"{field_name} year must be supplied.")
    elif len(year.strip()) != 4:
        errors.append(f"{field_name} year must be 4 digits.")
    
    # Validate month
    if not month or not month.strip() or not month.strip().isdigit():
        errors.append(f"{field_name} month must be supplied.")
    elif len(month.strip()) != 2:
        errors.append(f"{field_name} month must be 2 digits.")
    else:
        try:
            month_num = int(month.strip())
            if month_num < 1 or month_num > 12:
                errors.append(f"{field_name} month must be between 1 and 12.")
        except:
            errors.append(f"{field_name} month is not valid.")
    
    # Validate day
    if not day or not day.strip() or not day.strip().isdigit():
        errors.append(f"{field_name} day must be supplied.")
    elif len(day.strip()) != 2:
        errors.append(f"{field_name} day must be 2 digits.")
    else:
        try:
            day_num = int(day.strip())
            if day_num < 1 or day_num > 31:
                errors.append(f"{field_name} day must be between 1 and 31.")
        except:
            errors.append(f"{field_name} day is not valid.")
    
    if errors:
        return False, "; ".join(errors), None
    
    # Format date as YYYY-MM-DD
    date_str = f"{year.strip()}-{month.strip()}-{day.strip()}"
    return True, None, date_str


def edit_alpha_required(value: str, field_name: str, max_length: int = 25) -> tuple[bool, Optional[str]]:
    """Edit required alphabetic field (alphabets and spaces only)."""
    if not value or value.strip() == '':
        return False, f"{field_name} must be supplied."
    
    # Check if contains only alphabets and spaces
    if not re.match(r'^[A-Za-z\s]+$', value):
        return False, f"{field_name} can have alphabets only."
    
    if len(value) > max_length:
        return False, f"{field_name} exceeds maximum length of {max_length}."
    
    return True, None


def edit_ssn(ssn_part1: str, ssn_part2: str, ssn_part3: str) -> tuple[bool, Optional[str], Optional[str]]:
    """Edit US SSN (xxx-xx-xxxx format)."""
    errors = []
    
    # Validate part 1 (3 digits)
    if not ssn_part1 or not ssn_part1.strip() or not ssn_part1.strip().isdigit():
        errors.append("SSN: First 3 chars must be supplied.")
    elif len(ssn_part1.strip()) != 3:
        errors.append("SSN: First 3 chars must be 3 digits.")
    else:
        part1_num = int(ssn_part1.strip())
        if part1_num == 0 or part1_num == 666 or (900 <= part1_num <= 999):
            errors.append("SSN: First 3 chars should not be 000, 666, or between 900 and 999")
    
    # Validate part 2 (2 digits)
    if not ssn_part2 or not ssn_part2.strip() or not ssn_part2.strip().isdigit():
        errors.append("SSN 4th & 5th chars must be supplied.")
    elif len(ssn_part2.strip()) != 2:
        errors.append("SSN 4th & 5th chars must be 2 digits.")
    else:
        part2_num = int(ssn_part2.strip())
        if part2_num == 0:
            errors.append("SSN 4th & 5th chars must not be zero.")
    
    # Validate part 3 (4 digits)
    if not ssn_part3 or not ssn_part3.strip() or not ssn_part3.strip().isdigit():
        errors.append("SSN Last 4 chars must be supplied.")
    elif len(ssn_part3.strip()) != 4:
        errors.append("SSN Last 4 chars must be 4 digits.")
    else:
        part3_num = int(ssn_part3.strip())
        if part3_num == 0:
            errors.append("SSN Last 4 chars must not be zero.")
    
    if errors:
        return False, "; ".join(errors), None
    
    ssn_str = f"{ssn_part1.strip()}{ssn_part2.strip()}{ssn_part3.strip()}"
    return True, None, ssn_str


def edit_phone_number(area_code: str, prefix: str, line_num: str, field_name: str) -> tuple[bool, Optional[str], Optional[str]]:
    """Edit US phone number (xxx)xxx-xxxx format."""
    # All parts blank is OK (optional field)
    if (not area_code or area_code.strip() == '') and \
       (not prefix or prefix.strip() == '') and \
       (not line_num or line_num.strip() == ''):
        return True, None, ""
    
    errors = []
    
    # Validate area code
    if not area_code or not area_code.strip() or not area_code.strip().isdigit():
        errors.append(f"{field_name}: Area code must be supplied.")
    elif len(area_code.strip()) != 3:
        errors.append(f"{field_name}: Area code must be a 3 digit number.")
    else:
        area_code_num = int(area_code.strip())
        if area_code_num == 0:
            errors.append(f"{field_name}: Area code cannot be zero")
    
    # Validate prefix
    if not prefix or not prefix.strip() or not prefix.strip().isdigit():
        errors.append(f"{field_name}: Prefix code must be supplied.")
    elif len(prefix.strip()) != 3:
        errors.append(f"{field_name}: Prefix code must be a 3 digit number.")
    else:
        prefix_num = int(prefix.strip())
        if prefix_num == 0:
            errors.append(f"{field_name}: Prefix code cannot be zero")
    
    # Validate line number
    if not line_num or not line_num.strip() or not line_num.strip().isdigit():
        errors.append(f"{field_name}: Line number code must be supplied.")
    elif len(line_num.strip()) != 4:
        errors.append(f"{field_name}: Line number code must be a 4 digit number.")
    else:
        line_num_val = int(line_num.strip())
        if line_num_val == 0:
            errors.append(f"{field_name}: Line number code cannot be zero")
    
    if errors:
        return False, "; ".join(errors), None
    
    # Format as (xxx)xxx-xxxx
    phone_str = f"({area_code.strip()}){prefix.strip()}-{line_num.strip()}"
    return True, None, phone_str


def edit_fico_score(score_str: str, field_name: str) -> tuple[bool, Optional[str], Optional[int]]:
    """Edit FICO score (300-850 range)."""
    if not score_str or not score_str.strip() or not score_str.strip().isdigit():
        return False, f"{field_name} must be supplied.", None
    
    try:
        score = int(score_str.strip())
        if score < 300 or score > 850:
            return False, f"{field_name}: should be between 300 and 850", None
        return True, None, score
    except:
        return False, f"{field_name} must be all numeric.", None


# ============================================================================
# Main Program Logic - Read and Update Functions
# ============================================================================

def read_account_by_acct_id(acct_id: str, xref_file: XrefFileHandler, 
                             acct_file: AcctFileHandler, 
                             cust_file: CustFileHandler) -> tuple[Optional[AccountRecord], Optional[CustomerRecord], Optional[str]]:
    """Read account data by account ID.
    
    Returns:
        tuple: (AccountRecord, CustomerRecord, error_message)
               If error occurs, returns (None, None, error_message)
    """
    # Read XREF by alternate index (ACCT-ID)
    xref_line, result = xref_file.read_record(acct_id, use_alt_index=True)
    if result != 0 or not xref_line:
        return None, None, f"Account:{acct_id} not found in Cross ref file."
    
    xref_record = parse_xref_record(xref_line)
    cust_id = xref_record.xref_cust_id
    
    # Read account record
    acct_line, result = acct_file.read_record(acct_id)
    if result != 0 or not acct_line:
        return None, None, f"Account:{acct_id} not found in Acct Master file."
    
    acct_record = parse_account_record(acct_line)
    
    # Read customer record
    cust_line, result = cust_file.read_record(cust_id)
    if result != 0 or not cust_line:
        return None, None, f"CustId:{cust_id} not found in customer master."
    
    cust_record = parse_customer_record(cust_line)
    
    return acct_record, cust_record, None


def update_account_and_customer(acct_id: str, cust_id: str,
                                new_acct: AccountRecord, new_cust: CustomerRecord,
                                old_acct: AccountRecord, old_cust: CustomerRecord,
                                acct_file: AcctFileHandler,
                                cust_file: CustFileHandler) -> tuple[bool, Optional[str]]:
    """Update account and customer records with change detection.
    
    Returns:
        tuple: (success, error_message)
    """
    # Read for update (simulates CICS READ UPDATE)
    acct_line, result = acct_file.read_for_update(acct_id)
    if result != 0:
        return False, "Could not lock account record for update"
    
    cust_line, result = cust_file.read_for_update(cust_id)
    if result != 0:
        return False, "Could not lock customer record for update"
    
    # Check if records were changed by someone else
    current_acct = parse_account_record(acct_line)
    current_cust = parse_customer_record(cust_line)
    
    if not records_match(current_acct, old_acct, current_cust, old_cust):
        return False, "Record changed by some one else. Please review"
    
    # Update account record
    new_acct_line = format_account_record(new_acct)
    result = acct_file.rewrite_record(acct_id, new_acct_line)
    if result != 0:
        return False, "Update of account record failed"
    
    # Update customer record
    new_cust_line = format_customer_record(new_cust)
    result = cust_file.rewrite_record(cust_id, new_cust_line)
    if result != 0:
        # Rollback account update would go here in real CICS
        return False, "Update of customer record failed"
    
    return True, None


def records_match(current_acct: AccountRecord, old_acct: AccountRecord,
                  current_cust: CustomerRecord, old_cust: CustomerRecord) -> bool:
    """Check if current records match old records."""
    # Normalize empty strings for comparison
    def norm_str(s):
        return s.strip().upper() if s else ""
    
    # Check account fields
    if (current_acct.acct_id.strip() != old_acct.acct_id.strip() or
        norm_str(current_acct.active_status) != norm_str(old_acct.active_status) or
        current_acct.curr_bal != old_acct.curr_bal or
        current_acct.credit_limit != old_acct.credit_limit or
        current_acct.cash_credit_limit != old_acct.cash_credit_limit or
        norm_str(current_acct.open_date) != norm_str(old_acct.open_date) or
        norm_str(current_acct.expiration_date) != norm_str(old_acct.expiration_date) or
        norm_str(current_acct.reissue_date) != norm_str(old_acct.reissue_date) or
        current_acct.curr_cyc_credit != old_acct.curr_cyc_credit or
        current_acct.curr_cyc_debit != old_acct.curr_cyc_debit or
        norm_str(current_acct.group_id) != norm_str(old_acct.group_id)):
        return False
    
    # Check customer fields - handle empty strings properly
    if (norm_str(current_cust.cust_id) != norm_str(old_cust.cust_id) or
        norm_str(current_cust.first_name) != norm_str(old_cust.first_name) or
        norm_str(current_cust.middle_name) != norm_str(old_cust.middle_name) or
        norm_str(current_cust.last_name) != norm_str(old_cust.last_name) or
        norm_str(current_cust.addr_line_1) != norm_str(old_cust.addr_line_1) or
        norm_str(current_cust.addr_line_2) != norm_str(old_cust.addr_line_2) or
        norm_str(current_cust.addr_line_3) != norm_str(old_cust.addr_line_3) or
        norm_str(current_cust.addr_state_cd) != norm_str(old_cust.addr_state_cd) or
        norm_str(current_cust.addr_country_cd) != norm_str(old_cust.addr_country_cd) or
        norm_str(current_cust.addr_zip) != norm_str(old_cust.addr_zip) or
        (current_cust.phone_num_1 or "").strip() != (old_cust.phone_num_1 or "").strip() or
        (current_cust.phone_num_2 or "").strip() != (old_cust.phone_num_2 or "").strip() or
        (current_cust.ssn or "").strip() != (old_cust.ssn or "").strip() or
        norm_str(current_cust.govt_issued_id) != norm_str(old_cust.govt_issued_id) or
        (current_cust.dob_yyyy_mm_dd or "").strip() != (old_cust.dob_yyyy_mm_dd or "").strip() or
        (current_cust.eft_account_id or "").strip() != (old_cust.eft_account_id or "").strip() or
        norm_str(current_cust.pri_card_ind) != norm_str(old_cust.pri_card_ind) or
        current_cust.fico_credit_score != old_cust.fico_credit_score):
        return False
    
    return True


def edit_account_id(acct_id: str) -> tuple[bool, Optional[str]]:
    """Edit account ID (must be 11-digit non-zero number)."""
    if not acct_id or acct_id.strip() == '':
        return False, "Account number not provided"
    
    acct_id = acct_id.strip()
    if not acct_id.isdigit():
        return False, "Account number must be a non zero 11 digit number"
    
    if len(acct_id) != 11:
        return False, "Account number must be a non zero 11 digit number"
    
    if int(acct_id) == 0:
        return False, "Account number must be a non zero 11 digit number"
    
    return True, None


# ============================================================================
# Main Program Logic
# ============================================================================

def main():
    """Main procedure equivalent to COBOL PROCEDURE DIVISION."""
    print('START OF EXECUTION OF PROGRAM COACTUPC')
    
    # For testing purposes, this is a simplified version
    # In a real CICS environment, this would handle commarea and maps
    print('END OF EXECUTION OF PROGRAM COACTUPC')
    return 0


if __name__ == '__main__':
    sys.exit(main())

