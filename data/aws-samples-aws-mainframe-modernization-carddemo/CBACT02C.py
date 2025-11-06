"""
Ground Truth Python Equivalent for CBACT02C.cbl

Program: CBACT02C
Application: CardDemo
Type: BATCH Program
Function: Read and print card data file.

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass
from typing import Optional
from pathlib import Path


@dataclass
class CardRecord:
    """Card record structure matching COBOL FD-CARDFILE-REC."""
    card_num: str  # PIC X(16) - 16 characters
    card_data: str  # PIC X(134) - 134 characters
    
    # Parsed fields from card_data (inferred from COPY CVACT02Y)
    # For a true ground truth, this would need to be accurately parsed from the copybook.
    # Here, we'll define a simplified structure based on common COBOL patterns.
    
    def __str__(self):
        """String representation of card record for display."""
        return f"{self.card_num}{self.card_data}"


class CardFileHandler:
    """Handler for card file operations."""
    
    def __init__(self, filename: str = "CARDFILE"):
        """Initialize card file handler."""
        self.filename = filename
        self.file_handle: Optional[object] = None
        self.file_status = "00"
        self.end_of_file = False
    
    def open_file(self) -> int:
        """Open the card file for reading.
        
        Equivalent to COBOL 0000-CARDFILE-OPEN.
        
        Returns:
            APPL-RESULT: 0 if successful, 12 if error
        """
        self.file_status = "00"
        try:
            # In Python, we'll use a simple text file
            # For ground truth, assume file exists and can be opened
            self.file_handle = open(self.filename, 'r', encoding='utf-8')
            return 0  # APPL-AOK
        except FileNotFoundError:
            self.file_status = "23"  # File not found
            return 12
        except Exception as e:
            self.file_status = "99"  # General error
            print(f"ERROR OPENING CARDFILE: {e}")
            return 12
    
    def read_next(self) -> tuple[Optional[CardRecord], int]:
        """Read next record from file.
        
        Equivalent to COBOL 1000-CARDFILE-GET-NEXT.
        
        Returns:
            tuple: (CardRecord or None, APPL-RESULT)
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
            # COBOL: FD-CARD-NUM PIC X(16), FD-CARD-DATA PIC X(134)
            line = line.rstrip('\n\r')
            if len(line) >= 16:
                card_num = line[:16]
                card_data = line[16:150] if len(line) > 16 else ""
                
                record = CardRecord(
                    card_num=card_num,
                    card_data=card_data
                )
                
                self.file_status = "00"
                return record, 0  # APPL-AOK
            else:
                self.file_status = "04"  # Invalid record
                return None, 12
                
        except Exception as e:
            self.file_status = "99"
            print(f"ERROR READING CARDFILE: {e}")
            return None, 12
    
    def close_file(self) -> int:
        """Close the card file.
        
        Equivalent to COBOL 9000-CARDFILE-CLOSE.
        
        Returns:
            APPL-RESULT: 0 if successful, 12 if error
        """
        try:
            if self.file_handle:
                self.file_handle.close()
                self.file_handle = None
                self.file_status = "00"
                return 0  # APPL-AOK
            return 0
        except Exception as e:
            self.file_status = "99"
            print(f"ERROR CLOSING CARDFILE: {e}")
            return 12
    
    def display_io_status(self):
        """Display I/O status information.
        
        Equivalent to COBOL 9910-DISPLAY-IO-STATUS.
        """
        io_status = self.file_status
        if len(io_status) != 2:
            io_status = "00"
        
        io_stat1 = io_status[0] if len(io_status) > 0 else '0'
        io_stat2 = io_status[1] if len(io_status) > 1 else '0'
        
        # Create IO-STATUS-04 equivalent
        if not io_stat1.isdigit() or io_stat1 == '9':
            # Non-numeric or 9xx status
            status_04 = f"{io_stat1}000"
            print(f'FILE STATUS IS: NNNN {status_04}')
        else:
            status_04 = f"00{io_status}"
            print(f'FILE STATUS IS: NNNN {status_04}')


def abend_program():
    """Abend the program (equivalent to COBOL 9999-ABEND-PROGRAM)."""
    print('ABENDING PROGRAM')
    sys.exit(999)


def main():
    """Main procedure equivalent to COBOL PROCEDURE DIVISION."""
    print('START OF EXECUTION OF PROGRAM CBACT02C')
    
    # Initialize file handler
    card_file = CardFileHandler()
    
    # Open file
    appl_result = card_file.open_file()
    if appl_result != 0:  # Not APPL-AOK
        print('ERROR OPENING CARDFILE')
        card_file.display_io_status()
        abend_program()
    
    # Read and display records
    while not card_file.end_of_file:
        record, appl_result = card_file.read_next()
        
        if not card_file.end_of_file and record:
            print(record)  # DISPLAY CARD-RECORD
    
    # Close file
    appl_result = card_file.close_file()
    if appl_result != 0:  # Not APPL-AOK
        print('ERROR CLOSING CARDFILE')
        card_file.display_io_status()
        abend_program()
    
    print('END OF EXECUTION OF PROGRAM CBACT02C')
    return 0


if __name__ == '__main__':
    sys.exit(main())

