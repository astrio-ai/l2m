"""
Ground Truth Python Equivalent for CBTRN01C.cbl

Program: CBTRN01C
Application: CardDemo
Type: BATCH COBOL Program
Function: Post the records from daily transaction file.

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass
from typing import Optional, Dict
from pathlib import Path


@dataclass
class DailyTranRecord:
    """Daily transaction record (FD-TRAN-RECORD)."""
    tran_id: str = ""  # PIC X(16) - 16 chars
    tran_data: str = ""  # PIC X(334) - 334 chars
    
    @property
    def dalytran_id(self) -> str:
        return self.tran_id
    
    @property
    def dalytran_card_num(self) -> str:
        # Extract card number from tran_data (simplified)
        if len(self.tran_data) >= 16:
            return self.tran_data[:16]
        return ""


@dataclass
class CardXrefRecord:
    """Card cross-reference record (CARD-XREF-RECORD)."""
    card_num: str = ""  # 16 chars
    cust_num: str = ""  # 9 chars
    acct_id: str = ""  # 11 chars
    
    @property
    def xref_card_num(self) -> str:
        return self.card_num
    
    @property
    def xref_cust_id(self) -> str:
        return self.cust_num
    
    @property
    def xref_acct_id(self) -> str:
        return self.acct_id


@dataclass
class AccountRecord:
    """Account record (ACCOUNT-RECORD)."""
    acct_id: str = ""  # 11 chars
    acct_data: str = ""  # 289 chars
    
    @property
    def acct_id_prop(self) -> str:
        return self.acct_id


class SequentialFileHandler:
    """Handler for sequential files."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.file_handle: Optional[object] = None
        self.file_status = "00"
        self.end_of_file = False
    
    def open_file(self, mode: str = "r") -> int:
        """Open file."""
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
            return 12
    
    def read_next(self) -> tuple[Optional[str], int]:
        """Read next record."""
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
    
    def close_file(self) -> int:
        """Close file."""
        try:
            if self.file_handle:
                self.file_handle.close()
                self.file_handle = None
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


class IndexedFileHandler:
    """Handler for indexed files (simulated with dictionary)."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.file_handle: Optional[object] = None
        self.file_status = "00"
        self._data: Dict[str, str] = {}
    
    def open_file(self, mode: str = "r") -> int:
        """Open file and load data."""
        try:
            if mode == "r":
                with open(self.filename, "r", encoding='utf-8') as f:
                    for line in f:
                        line = line.rstrip('\n\r')
                        if len(line) > 0:
                            key = self._extract_key(line)
                            if key:
                                self._data[key] = line
            self.file_status = "00"
            return 0
        except FileNotFoundError:
            self.file_status = "23"
            return 12
        except Exception as e:
            self.file_status = "99"
            return 12
    
    def read_record(self, key: str) -> tuple[Optional[str], int]:
        """Read record by key."""
        if key in self._data:
            self.file_status = "00"
            return self._data[key], 0
        else:
            self.file_status = "23"
            return None, 12
    
    def close_file(self) -> int:
        """Close file."""
        try:
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
    
    def _extract_key(self, line: str) -> str:
        """Extract key from line (override in subclasses)."""
        return line[:16].strip() if len(line) >= 16 else ""


class XrefFileHandler(IndexedFileHandler):
    """Handler for XREF file."""
    
    def _extract_key(self, line: str) -> str:
        """Extract card number key (16 chars)."""
        if len(line) >= 16:
            return line[:16]
        return ""


class CustFileHandler(IndexedFileHandler):
    """Handler for CUSTFILE."""
    
    def _extract_key(self, line: str) -> str:
        """Extract customer ID key (9 chars)."""
        if len(line) >= 9:
            return line[:9].strip()
        return ""


class CardFileHandler(IndexedFileHandler):
    """Handler for CARDFILE."""
    
    def _extract_key(self, line: str) -> str:
        """Extract card number key (16 chars)."""
        if len(line) >= 16:
            return line[:16]
        return ""


class AcctFileHandler(IndexedFileHandler):
    """Handler for ACCTFILE."""
    
    def _extract_key(self, line: str) -> str:
        """Extract account ID key (11 chars)."""
        if len(line) >= 11:
            return line[:11].strip()
        return ""


class TranFileHandler(IndexedFileHandler):
    """Handler for TRANFILE."""
    
    def _extract_key(self, line: str) -> str:
        """Extract transaction ID key (16 chars)."""
        if len(line) >= 16:
            return line[:16]
        return ""


def parse_daily_tran_record(line: str) -> DailyTranRecord:
    """Parse daily transaction record."""
    if len(line) < 16:
        return DailyTranRecord()
    
    return DailyTranRecord(
        tran_id=line[:16],
        tran_data=line[16:350] if len(line) > 16 else ""
    )


def parse_xref_record(line: str) -> CardXrefRecord:
    """Parse XREF record."""
    if len(line) < 36:
        return CardXrefRecord()
    
    return CardXrefRecord(
        card_num=line[:16],
        cust_num=line[16:25] if len(line) > 16 else "",
        acct_id=line[25:36] if len(line) > 25 else ""
    )


def parse_account_record(line: str) -> AccountRecord:
    """Parse account record."""
    if len(line) < 11:
        return AccountRecord()
    
    return AccountRecord(
        acct_id=line[:11].strip(),
        acct_data=line[11:300] if len(line) > 11 else ""
    )


def abend_program():
    """Abend the program."""
    print('ABENDING PROGRAM')
    sys.exit(999)


def main():
    """Main procedure equivalent to COBOL PROCEDURE DIVISION."""
    print('START OF EXECUTION OF PROGRAM CBTRN01C')
    
    # File handlers
    dalytran_file = SequentialFileHandler("DALYTRAN")
    custfile = CustFileHandler("CUSTFILE")
    xreffile = XrefFileHandler("XREFFILE")
    cardfile = CardFileHandler("CARDFILE")
    acctfile = AcctFileHandler("ACCTFILE")
    tranfile = TranFileHandler("TRANFILE")
    
    # Working variables
    end_of_daily_trans_file = 'N'
    ws_xref_read_status = 0
    ws_acct_read_status = 0
    
    # Current records
    dalytran_record: Optional[DailyTranRecord] = None
    xref_record: Optional[CardXrefRecord] = None
    account_record: Optional[AccountRecord] = None
    
    try:
        # Open files
        if dalytran_file.open_file("r") != 0:
            print('ERROR OPENING DAILY TRANSACTION FILE')
            dalytran_file.display_io_status()
            abend_program()
        
        if custfile.open_file("r") != 0:
            print('ERROR OPENING CUSTOMER FILE')
            custfile.display_io_status()
            abend_program()
        
        if xreffile.open_file("r") != 0:
            print('ERROR OPENING CROSS REF FILE')
            xreffile.display_io_status()
            abend_program()
        
        if cardfile.open_file("r") != 0:
            print('ERROR OPENING CARD FILE')
            cardfile.display_io_status()
            abend_program()
        
        if acctfile.open_file("r") != 0:
            print('ERROR OPENING ACCOUNT FILE')
            acctfile.display_io_status()
            abend_program()
        
        if tranfile.open_file("r") != 0:
            print('ERROR OPENING TRANSACTION FILE')
            tranfile.display_io_status()
            abend_program()
        
        # Main processing loop
        while end_of_daily_trans_file != 'Y':
            if end_of_daily_trans_file == 'N':
                # Read next daily transaction record
                line, appl_result = dalytran_file.read_next()
                
                if appl_result == 0 and line:
                    dalytran_record = parse_daily_tran_record(line)
                    print(f"{dalytran_record.dalytran_id}{dalytran_record.tran_data}")
                    
                    # Lookup XREF
                    ws_xref_read_status = 0
                    xref_key = dalytran_record.dalytran_card_num
                    
                    xref_line, appl_result = xreffile.read_record(xref_key)
                    if appl_result == 0 and xref_line:
                        xref_record = parse_xref_record(xref_line)
                        print('SUCCESSFUL READ OF XREF')
                        print(f'CARD NUMBER: {xref_record.xref_card_num}')
                        print(f'ACCOUNT ID : {xref_record.xref_acct_id}')
                        print(f'CUSTOMER ID: {xref_record.xref_cust_id}')
                    else:
                        print('INVALID CARD NUMBER FOR XREF')
                        ws_xref_read_status = 4
                    
                    # If XREF found, read account
                    if ws_xref_read_status == 0:
                        ws_acct_read_status = 0
                        acct_key = xref_record.xref_acct_id
                        
                        acct_line, appl_result = acctfile.read_record(acct_key)
                        if appl_result == 0 and acct_line:
                            account_record = parse_account_record(acct_line)
                            print('SUCCESSFUL READ OF ACCOUNT FILE')
                        else:
                            print(f'INVALID ACCOUNT NUMBER FOUND')
                            ws_acct_read_status = 4
                    else:
                        print(f'CARD NUMBER {dalytran_record.dalytran_card_num} COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-{dalytran_record.dalytran_id}')
                
                elif appl_result == 16:
                    end_of_daily_trans_file = 'Y'
                else:
                    print('ERROR READING DAILY TRANSACTION FILE')
                    dalytran_file.display_io_status()
                    abend_program()
        
        # Close files
        if dalytran_file.close_file() != 0:
            print('ERROR CLOSING CUSTOMER FILE')
            dalytran_file.display_io_status()
            abend_program()
        
        if custfile.close_file() != 0:
            print('ERROR CLOSING CUSTOMER FILE')
            custfile.display_io_status()
            abend_program()
        
        if xreffile.close_file() != 0:
            print('ERROR CLOSING CROSS REF FILE')
            xreffile.display_io_status()
            abend_program()
        
        if cardfile.close_file() != 0:
            print('ERROR CLOSING CARD FILE')
            cardfile.display_io_status()
            abend_program()
        
        if acctfile.close_file() != 0:
            print('ERROR CLOSING ACCOUNT FILE')
            acctfile.display_io_status()
            abend_program()
        
        if tranfile.close_file() != 0:
            print('ERROR CLOSING TRANSACTION FILE')
            tranfile.display_io_status()
            abend_program()
        
        print('END OF EXECUTION OF PROGRAM CBTRN01C')
        return 0
        
    except Exception as e:
        print(f'ERROR: {e}')
        abend_program()


if __name__ == '__main__':
    sys.exit(main())

