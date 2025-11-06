"""
Ground Truth Python Equivalent for CBACT01C.cbl

Program: CBACT01C
Application: CardDemo
Type: BATCH Program
Function: Read and print account data file.

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass
from typing import Optional


@dataclass
class AccountRecord:
    """Account record structure matching COBOL FD-ACCTFILE-REC."""
    acct_id: str  # PIC 9(11) - 11 digits
    acct_data: str  # PIC X(289) - 289 characters
    
    # Parsed fields from acct_data (inferred from DISPLAY statements)
    acct_active_status: str = ""
    acct_curr_bal: str = ""
    acct_credit_limit: str = ""
    acct_cash_credit_limit: str = ""
    acct_open_date: str = ""
    acct_expiration_date: str = ""
    acct_reissue_date: str = ""
    acct_curr_cyc_credit: str = ""
    acct_curr_cyc_debit: str = ""
    acct_group_id: str = ""


class AcctFileHandler:
    """Handler for account file operations."""
    
    def __init__(self, filename: str = "ACCTFILE"):
        self.filename = filename
        self.file_handle: Optional[object] = None
        self.file_status = "00"
        self.end_of_file = False
    
    def open_file(self) -> int:
        """Open the account file for reading.
        
        Returns:
            APPL-RESULT: 0 if successful, 12 if error
        """
        self.file_status = "00"
        try:
            # In Python, we'll use a simple text file or JSON file
            # For ground truth, assume file exists and can be opened
            self.file_handle = open(self.filename, 'r', encoding='utf-8')
            return 0  # APPL-AOK
        except FileNotFoundError:
            self.file_status = "23"  # File not found
            return 12
        except Exception as e:
            self.file_status = "99"  # General error
            print(f"ERROR OPENING ACCTFILE: {e}")
            return 12
    
    def read_next(self) -> tuple[Optional[AccountRecord], int]:
        """Read next record from file.
        
        Returns:
            tuple: (AccountRecord or None, APPL-RESULT)
                   APPL-RESULT: 0 if successful, 16 if EOF, 12 if error
        """
        if self.end_of_file:
            return None, 16  # APPL-EOF
        
        try:
            line = self.file_handle.readline()
            if not line:
                self.end_of_file = True
                self.file_status = "10"  # EOF
                return None, 16  # APPL-EOF
            
            # Parse the record (assuming fixed format)
            # COBOL: FD-ACCT-ID PIC 9(11), FD-ACCT-DATA PIC X(289)
            line = line.rstrip('\n\r')
            if len(line) >= 11:
                acct_id = line[:11]
                acct_data = line[11:300] if len(line) > 11 else ""
                
                # Parse acct_data into individual fields (simplified)
                # In real implementation, would parse based on COPY CVACT01Y structure
                record = AccountRecord(
                    acct_id=acct_id,
                    acct_data=acct_data
                )
                # Parse fields from acct_data (simplified parsing)
                self._parse_account_data(record, acct_data)
                
                self.file_status = "00"
                return record, 0  # APPL-AOK
            else:
                self.file_status = "04"  # Invalid record
                return None, 12
                
        except Exception as e:
            self.file_status = "99"
            print(f"ERROR READING ACCOUNT FILE: {e}")
            return None, 12
    
    def _parse_account_data(self, record: AccountRecord, data: str):
        """Parse account data fields from the data string.
        
        This is a simplified parser. In real COBOL, fields are defined
        in COPY CVACT01Y with specific positions and lengths.
        """
        # Simplified: assume fields are space-separated or in fixed positions
        # In practice, would parse based on exact COPY book structure
        parts = data.split() if data else []
        
        # Assign fields if available (simplified)
        if len(parts) > 0:
            record.acct_active_status = parts[0] if len(parts) > 0 else ""
        if len(parts) > 1:
            record.acct_curr_bal = parts[1]
        if len(parts) > 2:
            record.acct_credit_limit = parts[2]
        if len(parts) > 3:
            record.acct_cash_credit_limit = parts[3]
        if len(parts) > 4:
            record.acct_open_date = parts[4]
        if len(parts) > 5:
            record.acct_expiration_date = parts[5]
        if len(parts) > 6:
            record.acct_reissue_date = parts[6]
        if len(parts) > 7:
            record.acct_curr_cyc_credit = parts[7]
        if len(parts) > 8:
            record.acct_curr_cyc_debit = parts[8]
        if len(parts) > 9:
            record.acct_group_id = parts[9]
    
    def close_file(self) -> int:
        """Close the account file.
        
        Returns:
            APPL-RESULT: 0 if successful, 12 if error
        """
        try:
            if self.file_handle:
                self.file_handle.close()
                self.file_status = "00"
                return 0  # APPL-AOK
            return 0
        except Exception as e:
            self.file_status = "99"
            print(f"ERROR CLOSING ACCOUNT FILE: {e}")
            return 12
    
    def display_io_status(self):
        """Display I/O status information."""
        io_status = self.file_status
        if len(io_status) != 2:
            io_status = "00"
        
        io_stat1 = io_status[0] if len(io_status) > 0 else '0'
        io_stat2 = io_status[1] if len(io_status) > 1 else '0'
        
        if not io_stat1.isdigit() or io_stat1 == '9':
            # Non-numeric or 9xx status
            status_04 = f"{io_stat1}000"
            print(f'FILE STATUS IS: NNNN {status_04}')
        else:
            status_04 = f"00{io_status}"
            print(f'FILE STATUS IS: NNNN {status_04}')


def display_account_record(record: AccountRecord):
    """Display account record details.
    
    Equivalent to COBOL 1100-DISPLAY-ACCT-RECORD.
    """
    print(f'ACCT-ID                 :{record.acct_id}')
    print(f'ACCT-ACTIVE-STATUS      :{record.acct_active_status}')
    print(f'ACCT-CURR-BAL           :{record.acct_curr_bal}')
    print(f'ACCT-CREDIT-LIMIT       :{record.acct_credit_limit}')
    print(f'ACCT-CASH-CREDIT-LIMIT  :{record.acct_cash_credit_limit}')
    print(f'ACCT-OPEN-DATE          :{record.acct_open_date}')
    print(f'ACCT-EXPIRAION-DATE     :{record.acct_expiration_date}')
    print(f'ACCT-REISSUE-DATE       :{record.acct_reissue_date}')
    print(f'ACCT-CURR-CYC-CREDIT    :{record.acct_curr_cyc_credit}')
    print(f'ACCT-CURR-CYC-DEBIT     :{record.acct_curr_cyc_debit}')
    print(f'ACCT-GROUP-ID           :{record.acct_group_id}')
    print('-------------------------------------------------')


def abend_program():
    """Abend the program (equivalent to COBOL 9999-ABEND-PROGRAM)."""
    print('ABENDING PROGRAM')
    sys.exit(999)


def main():
    """Main procedure equivalent to COBOL PROCEDURE DIVISION."""
    print('START OF EXECUTION OF PROGRAM CBACT01C')
    
    # Initialize file handler
    acct_file = AcctFileHandler()
    
    # Open file
    appl_result = acct_file.open_file()
    if appl_result != 0:  # Not APPL-AOK
        print('ERROR OPENING ACCTFILE')
        acct_file.display_io_status()
        abend_program()
    
    # Read and display records
    while not acct_file.end_of_file:
        record, appl_result = acct_file.read_next()
        
        if appl_result == 0:  # APPL-AOK
            display_account_record(record)
            print(record)  # Display full record (equivalent to DISPLAY ACCOUNT-RECORD)
        elif appl_result == 16:  # APPL-EOF
            acct_file.end_of_file = True
        else:  # Error
            print('ERROR READING ACCOUNT FILE')
            acct_file.display_io_status()
            abend_program()
    
    # Close file
    appl_result = acct_file.close_file()
    if appl_result != 0:  # Not APPL-AOK
        print('ERROR CLOSING ACCOUNT FILE')
        acct_file.display_io_status()
        abend_program()
    
    print('END OF EXECUTION OF PROGRAM CBACT01C')
    return 0


if __name__ == '__main__':
    sys.exit(main())

