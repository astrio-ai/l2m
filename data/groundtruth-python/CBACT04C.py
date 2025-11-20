"""
Ground Truth Python Equivalent for CBACT04C.cbl

Program: CBACT04C
Application: CardDemo
Type: BATCH Program
Function: This is a interest calculator program.

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, Dict, TextIO
from datetime import datetime
from decimal import Decimal, ROUND_HALF_UP


@dataclass
class TranCatBalRecord:
    """Transaction Category Balance Record (FD-TRAN-CAT-BAL-RECORD)."""
    trancat_acct_id: str  # PIC 9(11) - 11 chars
    trancat_type_cd: str  # PIC X(02) - 2 chars
    trancat_cd: str  # PIC 9(04) - 4 chars
    tran_cat_data: str = ""  # PIC X(33) - 33 chars
    
    @property
    def tran_cat_bal(self) -> Decimal:
        """Extract balance from tran_cat_data (simplified - actual would parse from COBOL structure)."""
        # In real implementation, this would parse the actual COBOL structure
        # For now, assume it's a numeric value in the first part
        try:
            # Simplified: assume balance is stored as a number in the data field
            parts = self.tran_cat_data.split()
            if parts:
                return Decimal(parts[0])
            return Decimal('0')
        except:
            return Decimal('0')


@dataclass
class XRefRecord:
    """Cross Reference Record (FD-XREFFILE-REC)."""
    xref_card_num: str  # PIC X(16) - 16 chars
    xref_cust_num: str  # PIC 9(09) - 9 chars
    xref_acct_id: str  # PIC 9(11) - 11 chars
    xref_filler: str = ""  # PIC X(14) - 14 chars


@dataclass
class DisGroupRecord:
    """Disclosure Group Record (FD-DISCGRP-REC)."""
    dis_acct_group_id: str  # PIC X(10) - 10 chars
    dis_tran_type_cd: str  # PIC X(02) - 2 chars
    dis_tran_cat_cd: str  # PIC 9(04) - 4 chars
    discgrp_data: str = ""  # PIC X(34) - 34 chars
    
    @property
    def int_rate(self) -> Decimal:
        """Extract interest rate from discgrp_data (simplified)."""
        # In real implementation, this would parse from COBOL structure
        try:
            parts = self.discgrp_data.split()
            if parts:
                return Decimal(parts[0])
            return Decimal('0')
        except:
            return Decimal('0')


@dataclass
class AccountRecord:
    """Account Record (FD-ACCTFILE-REC)."""
    acct_id: str  # PIC 9(11) - 11 chars
    acct_data: str = ""  # PIC X(289) - 289 chars
    
    @property
    def acct_group_id(self) -> str:
        """Extract account group ID from acct_data (simplified)."""
        # In real implementation, parse from COBOL structure
        if len(self.acct_data) >= 10:
            return self.acct_data[:10].strip()
        return "DEFAULT"
    
    @property
    def curr_bal(self) -> Decimal:
        """Extract current balance from acct_data (simplified)."""
        try:
            # Assume balance is stored somewhere in the data
            parts = self.acct_data.split()
            for part in parts:
                try:
                    return Decimal(part)
                except:
                    continue
            return Decimal('0')
        except:
            return Decimal('0')
    
    def set_curr_bal(self, value: Decimal):
        """Set current balance in acct_data (simplified)."""
        # In real implementation, update the specific field in the COBOL structure
        # For now, we'll just store it as a string representation
        parts = self.acct_data.split()
        if parts:
            parts[0] = str(value)
            self.acct_data = ' '.join(parts)


@dataclass
class TranRecord:
    """Transaction Record (FD-TRANFILE-REC)."""
    trans_id: str  # PIC X(16) - 16 chars
    tran_data: str = ""  # PIC X(334) - 334 chars
    
    # Parsed fields from TRAN-RECORD (from COPY CVTRA05Y)
    tran_type_cd: str = ""  # 2 chars
    tran_cat_cd: str = ""  # 4 chars
    tran_source: str = ""  # variable
    tran_desc: str = ""  # variable
    tran_amt: Decimal = Decimal('0')
    tran_merchant_id: str = ""
    tran_merchant_name: str = ""
    tran_merchant_city: str = ""
    tran_merchant_zip: str = ""
    tran_card_num: str = ""  # 16 chars
    tran_orig_ts: str = ""  # 26 chars
    tran_proc_ts: str = ""  # 26 chars


class FileStatusError(Exception):
    """Represent a file I/O error."""
    def __init__(self, status: str, message: str = ""):
        super().__init__(message or f"File status: {status}")
        self.status = status


class FileHandler:
    """Base file handler with common functionality."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.file_handle: Optional[TextIO] = None
        self.file_status = "00"
    
    def check_status(self) -> int:
        """Check file status and return APPL-RESULT code."""
        if self.file_status == "00":
            return 0  # APPL-AOK
        elif self.file_status == "10":
            return 16  # APPL-EOF
        else:
            return 12  # Error
    
    def display_io_status(self):
        """Display I/O status information."""
        io_status = self.file_status
        if len(io_status) != 2:
            io_status = "00"
        
        if io_status[0].isdigit() and io_status[1].isdigit() and io_status[0] != '9':
            status_04 = f"00{io_status}"
            print(f'FILE STATUS IS: NNNN {status_04}')
        else:
            # Non-numeric or 9xx status
            left = io_status[0] if len(io_status) > 0 else '0'
            right = io_status[1] if len(io_status) > 1 else '0'
            status_04 = f"{left}00{right}"
            print(f'FILE STATUS IS: NNNN {status_04}')


class SequentialFileHandler(FileHandler):
    """Handler for sequential files."""
    
    def __init__(self, filename: str):
        super().__init__(filename)
        self.end_of_file = False
    
    def open_file(self, mode: str = "r") -> int:
        """Open file for reading."""
        try:
            self.file_handle = open(self.filename, mode, encoding='utf-8')
            self.file_status = "00"
            self.end_of_file = False
            return 0
        except FileNotFoundError:
            self.file_status = "23"
            return 12
        except Exception as e:
            self.file_status = "99"
            print(f"ERROR: {e}")
            return 12
    
    def read_next(self) -> tuple[Optional[object], int]:
        """Read next record. Returns (record, APPL-RESULT)."""
        if self.end_of_file:
            return None, 16
        
        try:
            line = self.file_handle.readline()
            if not line:
                self.end_of_file = True
                self.file_status = "10"
                return None, 16
            
            line = line.rstrip('\n\r')
            self.file_status = "00"
            return line, 0
        except Exception as e:
            self.file_status = "99"
            return None, 12
    
    def write_record(self, record: str) -> int:
        """Write a record to file."""
        try:
            self.file_handle.write(record + '\n')
            self.file_handle.flush()
            self.file_status = "00"
            return 0
        except Exception as e:
            self.file_status = "99"
            return 12
    
    def close_file(self) -> int:
        """Close the file."""
        try:
            if self.file_handle:
                self.file_handle.close()
                self.file_handle = None
            self.file_status = "00"
            return 0
        except Exception as e:
            self.file_status = "99"
            return 12


class IndexedFileHandler(FileHandler):
    """Handler for indexed files (simulated with dictionary)."""
    
    def __init__(self, filename: str):
        super().__init__(filename)
        self._data: Dict[str, str] = {}
        self._loaded = False
    
    def open_file(self, mode: str = "r") -> int:
        """Open file and load data into memory."""
        try:
            if mode in ("r", "r+"):
                # Load existing data
                try:
                    with open(self.filename, "r", encoding='utf-8') as f:
                        for line in f:
                            line = line.rstrip('\n\r')
                            if len(line) >= 11:  # Minimum key length
                                key = line[:11].strip()
                                self._data[key] = line
                    self._loaded = True
                except FileNotFoundError:
                    # File doesn't exist yet, start with empty data
                    self._data = {}
                    self._loaded = True
            else:
                self._data = {}
                self._loaded = True
            
            self.file_status = "00"
            return 0
        except Exception as e:
            self.file_status = "99"
            return 12
    
    def read_record(self, key: str) -> tuple[Optional[str], int]:
        """Read a record by key."""
        if key in self._data:
            self.file_status = "00"
            return self._data[key], 0
        else:
            self.file_status = "23"  # Record not found
            return None, 12
    
    def rewrite_record(self, key: str, record: str) -> int:
        """Rewrite/update a record."""
        if key in self._data:
            self._data[key] = record
            self.file_status = "00"
            return 0
        else:
            self.file_status = "23"
            return 12
    
    def close_file(self) -> int:
        """Close file and save data."""
        try:
            # Save data to file
            with open(self.filename, "w", encoding='utf-8') as f:
                for key, value in sorted(self._data.items()):
                    f.write(value + '\n')
            
            self.file_status = "00"
            return 0
        except Exception as e:
            self.file_status = "99"
            return 12


def parse_tran_cat_bal_record(line: str) -> Optional[TranCatBalRecord]:
    """Parse a transaction category balance record from line."""
    if len(line) < 17:
        return None
    return TranCatBalRecord(
        trancat_acct_id=line[:11].strip(),
        trancat_type_cd=line[11:13],
        trancat_cd=line[13:17],
        tran_cat_data=line[17:50] if len(line) > 17 else ""
    )


def parse_xref_record(line: str) -> Optional[XRefRecord]:
    """Parse an XREF record from line."""
    if len(line) < 40:
        return None
    return XRefRecord(
        xref_card_num=line[:16],
        xref_cust_num=line[16:25],
        xref_acct_id=line[25:36],
        xref_filler=line[36:50] if len(line) > 36 else ""
    )


def parse_dis_group_record(line: str) -> Optional[DisGroupRecord]:
    """Parse a disclosure group record from line."""
    if len(line) < 16:
        return None
    return DisGroupRecord(
        dis_acct_group_id=line[:10].strip(),
        dis_tran_type_cd=line[10:12],
        dis_tran_cat_cd=line[12:16],
        discgrp_data=line[16:50] if len(line) > 16 else ""
    )


def parse_account_record(line: str) -> Optional[AccountRecord]:
    """Parse an account record from line."""
    if len(line) < 11:
        return None
    return AccountRecord(
        acct_id=line[:11].strip(),
        acct_data=line[11:300] if len(line) > 11 else ""
    )


def format_db2_timestamp() -> str:
    """Generate DB2 format timestamp: YYYY-MM-DD-HH.MM.SS.MM0000"""
    now = datetime.now()
    return f"{now.year:04d}-{now.month:02d}-{now.day:02d}-{now.hour:02d}.{now.minute:02d}.{now.second:02d}.{now.microsecond//10000:02d}0000"


def abend_program():
    """Abend the program."""
    print('ABENDING PROGRAM')
    sys.exit(999)


def main(parm_date: str = "20240101"):
    """Main procedure equivalent to COBOL PROCEDURE DIVISION."""
    print('START OF EXECUTION OF PROGRAM CBACT04C')
    
    # File handlers
    tcatbal_file = SequentialFileHandler("TCATBALF")
    xreffile = IndexedFileHandler("XREFFILE")
    discgrp_file = IndexedFileHandler("DISCGRP")
    acctfile = IndexedFileHandler("ACCTFILE")
    tranfile = SequentialFileHandler("TRANSACT")
    
    # Working variables
    last_acct_num = ""
    monthly_int = Decimal('0')
    total_int = Decimal('0')
    first_time = True
    record_count = 0
    tranid_suffix = 0
    end_of_file = False
    
    # Current records
    tran_cat_bal_record: Optional[TranCatBalRecord] = None
    account_record: Optional[AccountRecord] = None
    xref_record: Optional[XRefRecord] = None
    dis_group_record: Optional[DisGroupRecord] = None
    
    try:
        # Open files
        if tcatbal_file.open_file("r") != 0:
            print('ERROR OPENING TRANSACTION CATEGORY BALANCE')
            tcatbal_file.display_io_status()
            abend_program()
        
        if xreffile.open_file("r") != 0:
            print('ERROR OPENING CROSS REF FILE')
            xreffile.display_io_status()
            abend_program()
        
        if discgrp_file.open_file("r") != 0:
            print('ERROR OPENING DALY REJECTS FILE')
            discgrp_file.display_io_status()
            abend_program()
        
        if acctfile.open_file("r+") != 0:
            print('ERROR OPENING ACCOUNT MASTER FILE')
            acctfile.display_io_status()
            abend_program()
        
        if tranfile.open_file("w") != 0:
            print('ERROR OPENING TRANSACTION FILE')
            tranfile.display_io_status()
            abend_program()
        
        # Main processing loop
        while not end_of_file:
            if not end_of_file:
                # Read next transaction category balance record
                line, appl_result = tcatbal_file.read_next()
                
                if appl_result == 0 and line:
                    record_count += 1
                    tran_cat_bal_record = parse_tran_cat_bal_record(line)
                    
                    if tran_cat_bal_record:
                        print(f"{tran_cat_bal_record.trancat_acct_id}{tran_cat_bal_record.trancat_type_cd}{tran_cat_bal_record.trancat_cd}{tran_cat_bal_record.tran_cat_data}")
                        
                        # Check if this is a new account
                        if tran_cat_bal_record.trancat_acct_id != last_acct_num:
                            if not first_time:
                                # Update previous account
                                if account_record:
                                    account_record.set_curr_bal(account_record.curr_bal + total_int)
                                    acct_key = account_record.acct_id
                                    acct_line = f"{acct_key:11s}{account_record.acct_data}"
                                    acctfile.rewrite_record(acct_key, acct_line)
                            else:
                                first_time = False
                            
                            # Reset for new account
                            total_int = Decimal('0')
                            last_acct_num = tran_cat_bal_record.trancat_acct_id
                            
                            # Get account data
                            acct_key = tran_cat_bal_record.trancat_acct_id
                            acct_line, appl_result = acctfile.read_record(acct_key)
                            if appl_result == 0 and acct_line:
                                account_record = parse_account_record(acct_line)
                            else:
                                print(f'ACCOUNT NOT FOUND: {acct_key}')
                                account_record = None
                            
                            # Get XREF data
                            xref_line, appl_result = xreffile.read_record(acct_key)
                            if appl_result == 0 and xref_line:
                                xref_record = parse_xref_record(xref_line)
                            else:
                                print(f'ACCOUNT NOT FOUND: {acct_key}')
                                xref_record = None
                        
                        # Get interest rate
                        if account_record:
                            acct_group_id = account_record.acct_group_id
                            discgrp_key = f"{acct_group_id:10s}{tran_cat_bal_record.trancat_type_cd}{tran_cat_bal_record.trancat_cd}"
                            
                            discgrp_line, appl_result = discgrp_file.read_record(discgrp_key)
                            if appl_result == 0 and discgrp_line:
                                dis_group_record = parse_dis_group_record(discgrp_line)
                            else:
                                print('DISCLOSURE GROUP RECORD MISSING')
                                print('TRY WITH DEFAULT GROUP CODE')
                                dis_group_record = None
                            
                            # Try default if not found
                            if not dis_group_record:
                                default_key = f"{'DEFAULT':10s}{tran_cat_bal_record.trancat_type_cd}{tran_cat_bal_record.trancat_cd}"
                                discgrp_line, appl_result = discgrp_file.read_record(default_key)
                                if appl_result == 0 and discgrp_line:
                                    dis_group_record = parse_dis_group_record(discgrp_line)
                            
                            # Calculate interest if rate > 0
                            if dis_group_record and dis_group_record.int_rate != 0:
                                # Compute monthly interest
                                tran_cat_bal = tran_cat_bal_record.tran_cat_bal
                                int_rate = dis_group_record.int_rate
                                monthly_int = (tran_cat_bal * int_rate) / Decimal('1200')
                                total_int += monthly_int
                                
                                # Write transaction record
                                tranid_suffix += 1
                                trans_id = f"{parm_date}{tranid_suffix:06d}"
                                
                                tran_record = TranRecord(
                                    trans_id=trans_id[:16],
                                    tran_type_cd="01",
                                    tran_cat_cd="05",
                                    tran_source="System",
                                    tran_desc=f"Int. for a/c {account_record.acct_id}",
                                    tran_amt=monthly_int,
                                    tran_merchant_id="0",
                                    tran_merchant_name="",
                                    tran_merchant_city="",
                                    tran_merchant_zip="",
                                    tran_card_num=xref_record.xref_card_num if xref_record else " " * 16,
                                    tran_orig_ts=format_db2_timestamp(),
                                    tran_proc_ts=format_db2_timestamp()
                                )
                                
                                # Format transaction record for writing
                                tran_line = f"{tran_record.trans_id:16s}{tran_record.tran_type_cd:2s}{tran_record.tran_cat_cd:4s}{tran_record.tran_source:20s}{tran_record.tran_desc:50s}{str(tran_record.tran_amt):15s}{tran_record.tran_merchant_id:10s}{tran_record.tran_merchant_name:30s}{tran_record.tran_merchant_city:20s}{tran_record.tran_merchant_zip:10s}{tran_record.tran_card_num:16s}{tran_record.tran_orig_ts:26s}{tran_record.tran_proc_ts:26s}"
                                
                                if tranfile.write_record(tran_line) != 0:
                                    print('ERROR WRITING TRANSACTION RECORD')
                                    tranfile.display_io_status()
                                    abend_program()
                
                elif appl_result == 16:
                    # EOF
                    end_of_file = True
                    # Update last account
                    if account_record and not first_time:
                        account_record.set_curr_bal(account_record.curr_bal + total_int)
                        acct_key = account_record.acct_id
                        acct_line = f"{acct_key:11s}{account_record.acct_data}"
                        acctfile.rewrite_record(acct_key, acct_line)
                else:
                    print('ERROR READING TRANSACTION CATEGORY FILE')
                    tcatbal_file.display_io_status()
                    abend_program()
            else:
                # Update last account
                if account_record and not first_time:
                    account_record.set_curr_bal(account_record.curr_bal + total_int)
                    acct_key = account_record.acct_id
                    acct_line = f"{acct_key:11s}{account_record.acct_data}"
                    acctfile.rewrite_record(acct_key, acct_line)
        
        # Close files
        if tcatbal_file.close_file() != 0:
            print('ERROR CLOSING TRANSACTION BALANCE FILE')
            tcatbal_file.display_io_status()
            abend_program()
        
        if xreffile.close_file() != 0:
            print('ERROR CLOSING CROSS REF FILE')
            xreffile.display_io_status()
            abend_program()
        
        if discgrp_file.close_file() != 0:
            print('ERROR CLOSING DISCLOSURE GROUP FILE')
            discgrp_file.display_io_status()
            abend_program()
        
        if acctfile.close_file() != 0:
            print('ERROR CLOSING ACCOUNT FILE')
            acctfile.display_io_status()
            abend_program()
        
        if tranfile.close_file() != 0:
            print('ERROR CLOSING TRANSACTION FILE')
            tranfile.display_io_status()
            abend_program()
        
        print('END OF EXECUTION OF PROGRAM CBACT04C')
        return 0
        
    except Exception as e:
        print(f'ERROR: {e}')
        abend_program()


if __name__ == '__main__':
    parm_date = sys.argv[1] if len(sys.argv) > 1 else "20240101"
    sys.exit(main(parm_date))

