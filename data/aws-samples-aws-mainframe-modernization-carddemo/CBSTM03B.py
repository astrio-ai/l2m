"""
Ground Truth Python Equivalent for CBSTM03B.cbl

Program: CBSTM03B
Application: CardDemo
Type: BATCH COBOL Subroutine
Function: Does file processing related to Transact Report

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass
from typing import Optional, Dict, TextIO


@dataclass
class M03BArea:
    """M03B-AREA parameter area (LINKAGE SECTION equivalent)."""
    dd_name: str = ""  # LK-M03B-DD: PIC X(08)
    operation: str = ""  # LK-M03B-OPER: PIC X(01) - 'O', 'C', 'R', 'K', 'W', 'Z'
    return_code: str = "00"  # LK-M03B-RC: PIC X(02)
    key: str = ""  # LK-M03B-KEY: PIC X(25)
    key_len: int = 0  # LK-M03B-KEY-LN: PIC S9(4)
    field_data: str = ""  # LK-M03B-FLDT: PIC X(1000)
    
    @property
    def is_open(self) -> bool:
        return self.operation == 'O'
    
    @property
    def is_close(self) -> bool:
        return self.operation == 'C'
    
    @property
    def is_read(self) -> bool:
        return self.operation == 'R'
    
    @property
    def is_read_key(self) -> bool:
        return self.operation == 'K'
    
    @property
    def is_write(self) -> bool:
        return self.operation == 'W'
    
    @property
    def is_rewrite(self) -> bool:
        return self.operation == 'Z'


class FileHandler:
    """Base file handler for indexed files."""
    
    def __init__(self, filename: str, is_sequential: bool = True):
        self.filename = filename
        self.file_handle: Optional[TextIO] = None
        self.file_status = "00"
        self.is_sequential = is_sequential
        self._data: Dict[str, str] = {}  # For random access
        self._sequential_position = 0
        self._sequential_data: list = []
    
    def open_file(self) -> str:
        """Open file for reading."""
        try:
            if self.is_sequential:
                self.file_handle = open(self.filename, 'r', encoding='utf-8')
                self._sequential_data = self.file_handle.readlines()
                self._sequential_position = 0
            else:
                # For random access, load all data into dictionary
                with open(self.filename, 'r', encoding='utf-8') as f:
                    for line in f:
                        line = line.rstrip('\n\r')
                        if len(line) > 0:
                            key = self._extract_key(line)
                            if key:
                                self._data[key] = line
            self.file_status = "00"
            return "00"
        except FileNotFoundError:
            self.file_status = "23"
            return "23"
        except Exception as e:
            self.file_status = "99"
            return "99"
    
    def close_file(self) -> str:
        """Close the file."""
        try:
            if self.file_handle:
                self.file_handle.close()
                self.file_handle = None
            self.file_status = "00"
            return "00"
        except Exception as e:
            self.file_status = "99"
            return "99"
    
    def read_sequential(self) -> tuple[str, str]:
        """Read next record sequentially. Returns (status, data)."""
        try:
            if not self.file_handle and self._sequential_data:
                # Use in-memory data
                if self._sequential_position < len(self._sequential_data):
                    line = self._sequential_data[self._sequential_position]
                    self._sequential_position += 1
                    self.file_status = "00"
                    return ("00", line.rstrip('\n\r'))
                else:
                    self.file_status = "10"
                    return ("10", "")
            elif self.file_handle:
                line = self.file_handle.readline()
                if not line:
                    self.file_status = "10"
                    return ("10", "")
                self.file_status = "00"
                return ("00", line.rstrip('\n\r'))
            else:
                self.file_status = "23"
                return ("23", "")
        except Exception as e:
            self.file_status = "99"
            return ("99", "")
    
    def read_key(self, key: str, key_len: int) -> tuple[str, str]:
        """Read record by key. Returns (status, data)."""
        try:
            if key_len > 0:
                search_key = key[:key_len].strip()
            else:
                search_key = key.strip()
            
            if search_key in self._data:
                self.file_status = "00"
                return ("00", self._data[search_key])
            else:
                self.file_status = "23"
                return ("23", "")
        except Exception as e:
            self.file_status = "99"
            return ("99", "")
    
    def _extract_key(self, line: str) -> str:
        """Extract key from line based on file type."""
        # Default: first 25 characters (would be customized per file type)
        if len(line) > 0:
            return line[:25].strip()
        return ""


class TrnxFileHandler(FileHandler):
    """Handler for TRNXFILE (indexed sequential)."""
    
    def __init__(self, filename: str = "TRNXFILE"):
        super().__init__(filename, is_sequential=True)
    
    def _extract_key(self, line: str) -> str:
        """Extract key from TRNXFILE record (16 chars card + 16 chars trans ID)."""
        if len(line) >= 32:
            return line[:32]
        return line[:16] if len(line) >= 16 else ""


class XrefFileHandler(FileHandler):
    """Handler for XREFFILE (indexed sequential)."""
    
    def __init__(self, filename: str = "XREFFILE"):
        super().__init__(filename, is_sequential=True)
    
    def _extract_key(self, line: str) -> str:
        """Extract key from XREFFILE record (16 chars card number)."""
        if len(line) >= 16:
            return line[:16]
        return ""


class CustFileHandler(FileHandler):
    """Handler for CUSTFILE (indexed random)."""
    
    def __init__(self, filename: str = "CUSTFILE"):
        super().__init__(filename, is_sequential=False)
    
    def _extract_key(self, line: str) -> str:
        """Extract key from CUSTFILE record (9 chars customer ID)."""
        if len(line) >= 9:
            return line[:9].strip()
        return ""


class AcctFileHandler(FileHandler):
    """Handler for ACCTFILE (indexed random)."""
    
    def __init__(self, filename: str = "ACCTFILE"):
        super().__init__(filename, is_sequential=False)
    
    def _extract_key(self, line: str) -> str:
        """Extract key from ACCTFILE record (11 chars account ID)."""
        if len(line) >= 11:
            return line[:11].strip()
        return ""


class CBSTM03BSubroutine:
    """Main subroutine class - equivalent to CBSTM03B COBOL program."""
    
    def __init__(self):
        self.trnx_file: Optional[TrnxFileHandler] = None
        self.xref_file: Optional[XrefFileHandler] = None
        self.cust_file: Optional[CustFileHandler] = None
        self.acct_file: Optional[AcctFileHandler] = None
    
    def process(self, m03b_area: M03BArea) -> None:
        """Process file operation based on M03B-AREA parameter.
        
        Equivalent to COBOL PROCEDURE DIVISION USING LK-M03B-AREA.
        """
        dd_name = m03b_area.dd_name
        
        if dd_name == 'TRNXFILE':
            self._process_trnxfile(m03b_area)
        elif dd_name == 'XREFFILE':
            self._process_xreffile(m03b_area)
        elif dd_name == 'CUSTFILE':
            self._process_custfile(m03b_area)
        elif dd_name == 'ACCTFILE':
            self._process_acctfile(m03b_area)
        else:
            m03b_area.return_code = "12"
    
    def _process_trnxfile(self, m03b_area: M03BArea) -> None:
        """Process TRNXFILE operations."""
        if m03b_area.is_open:
            if self.trnx_file is None:
                self.trnx_file = TrnxFileHandler()
            m03b_area.return_code = self.trnx_file.open_file()
        elif m03b_area.is_read:
            if self.trnx_file is None:
                m03b_area.return_code = "12"
                m03b_area.field_data = ""
            else:
                status, data = self.trnx_file.read_sequential()
                m03b_area.return_code = status
                m03b_area.field_data = data
        elif m03b_area.is_close:
            if self.trnx_file is None:
                m03b_area.return_code = "00"  # Already closed
            else:
                m03b_area.return_code = self.trnx_file.close_file()
                self.trnx_file = None
    
    def _process_xreffile(self, m03b_area: M03BArea) -> None:
        """Process XREFFILE operations."""
        if m03b_area.is_open:
            if self.xref_file is None:
                self.xref_file = XrefFileHandler()
            m03b_area.return_code = self.xref_file.open_file()
        elif m03b_area.is_read:
            if self.xref_file is None:
                m03b_area.return_code = "12"
                m03b_area.field_data = ""
            else:
                status, data = self.xref_file.read_sequential()
                m03b_area.return_code = status
                m03b_area.field_data = data
        elif m03b_area.is_close:
            if self.xref_file is None:
                m03b_area.return_code = "00"
            else:
                m03b_area.return_code = self.xref_file.close_file()
                self.xref_file = None
    
    def _process_custfile(self, m03b_area: M03BArea) -> None:
        """Process CUSTFILE operations."""
        if m03b_area.is_open:
            if self.cust_file is None:
                self.cust_file = CustFileHandler()
            m03b_area.return_code = self.cust_file.open_file()
        elif m03b_area.is_read_key:
            if self.cust_file is None:
                m03b_area.return_code = "12"
                m03b_area.field_data = ""
            else:
                key = m03b_area.key[:m03b_area.key_len] if m03b_area.key_len > 0 else m03b_area.key
                status, data = self.cust_file.read_key(key, m03b_area.key_len)
                m03b_area.return_code = status
                m03b_area.field_data = data
        elif m03b_area.is_close:
            if self.cust_file is None:
                m03b_area.return_code = "00"
            else:
                m03b_area.return_code = self.cust_file.close_file()
                self.cust_file = None
    
    def _process_acctfile(self, m03b_area: M03BArea) -> None:
        """Process ACCTFILE operations."""
        if m03b_area.is_open:
            if self.acct_file is None:
                self.acct_file = AcctFileHandler()
            m03b_area.return_code = self.acct_file.open_file()
        elif m03b_area.is_read_key:
            if self.acct_file is None:
                m03b_area.return_code = "12"
                m03b_area.field_data = ""
            else:
                key = m03b_area.key[:m03b_area.key_len] if m03b_area.key_len > 0 else m03b_area.key
                status, data = self.acct_file.read_key(key, m03b_area.key_len)
                m03b_area.return_code = status
                m03b_area.field_data = data
        elif m03b_area.is_close:
            if self.acct_file is None:
                m03b_area.return_code = "00"
            else:
                m03b_area.return_code = self.acct_file.close_file()
                self.acct_file = None


# Global subroutine instance (equivalent to being called as external subroutine)
_subroutine_instance: Optional[CBSTM03BSubroutine] = None


def cbstmt03b_subroutine(m03b_area: M03BArea) -> None:
    """Entry point for CBSTM03B subroutine call.
    
    Equivalent to COBOL CALL 'CBSTM03B' USING WS-M03B-AREA.
    """
    global _subroutine_instance
    if _subroutine_instance is None:
        _subroutine_instance = CBSTM03BSubroutine()
    _subroutine_instance.process(m03b_area)


def reset_subroutine():
    """Reset the subroutine instance (for testing)."""
    global _subroutine_instance
    _subroutine_instance = None

