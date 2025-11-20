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

from __future__ import annotations

import argparse
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Optional, Sequence, TextIO

APPL_AOK = 0
APPL_EOF = 16
APPL_ERROR = 12

CARD_NUM_LENGTH = 16
RECORD_TOTAL_LENGTH = 150


@dataclass
class CardRecord:
    """Card record structure matching COBOL FD-CARDFILE-REC."""
    card_num: str  # PIC X(16)
    card_data: str  # PIC X(134)

    def __str__(self) -> str:
        """String representation of card record for DISPLAY."""
        return f"{self.card_num}{self.card_data}"


class CardFileHandler:
    """Handler for card file operations."""

    def __init__(self, filename: str | Path = "CARDFILE"):
        self.path = Path(filename)
        self.file_handle: Optional[TextIO] = None
        self.file_status = "00"
        self.end_of_file = False

    def open_file(self) -> int:
        """Open the card file for reading (COBOL paragraph 0000)."""
        self.file_status = "00"
        self.end_of_file = False
        try:
            self.file_handle = self.path.open("r", encoding="utf-8")
            return APPL_AOK
        except FileNotFoundError:
            self.file_status = "35"
        except PermissionError:
            self.file_status = "37"
        except OSError:
            self.file_status = "99"
        return APPL_ERROR

    def read_next(self) -> tuple[Optional[CardRecord], int]:
        """Read the next record (COBOL paragraph 1000)."""
        if self.end_of_file:
            return None, APPL_EOF
        if self.file_handle is None:
            self.file_status = "90"
            return None, APPL_ERROR

        line = self.file_handle.readline()
        if line == "":
            self.end_of_file = True
            self.file_status = "10"
            return None, APPL_EOF

        line = line.rstrip("\r\n")
        if len(line) < CARD_NUM_LENGTH:
            self.file_status = "04"
            return None, APPL_ERROR

        trimmed = line[:RECORD_TOTAL_LENGTH]
        padded = trimmed.ljust(RECORD_TOTAL_LENGTH)

        record = CardRecord(
            card_num=padded[:CARD_NUM_LENGTH],
            card_data=padded[CARD_NUM_LENGTH:RECORD_TOTAL_LENGTH],
        )
        self.file_status = "00"
        return record, APPL_AOK

    def close_file(self) -> int:
        """Close the card file (COBOL paragraph 9000)."""
        try:
            if self.file_handle:
                self.file_handle.close()
                self.file_handle = None
            self.file_status = "00"
            return APPL_AOK
        except OSError:
            self.file_status = "99"
            return APPL_ERROR

    def display_io_status(self) -> None:
        """Display I/O status information (COBOL paragraph 9910)."""
        io_status = (self.file_status or "00").ljust(2)[:2]
        io_stat1, io_stat2 = io_status[0], io_status[1]

        if (not io_status.isdigit()) or io_stat1 == "9":
            # Best-effort recreation of the COBOL DISPLAY logic.
            ascii_value = ord(io_stat2) if io_stat2 else 0
            io_status_04 = f"{io_stat1}{ascii_value:03d}"
        else:
            io_status_04 = f"00{io_status}"

        print(f"FILE STATUS IS: NNNN {io_status_04}")


def abend_program() -> None:
    """Abend the program (equivalent to COBOL 9999-ABEND-PROGRAM)."""
    print("ABENDING PROGRAM")
    sys.exit(999)


def parse_args(argv: Optional[Sequence[str]] = None) -> argparse.Namespace:
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(description="Read and display card records.")
    parser.add_argument(
        "cardfile",
        nargs="?",
        default="CARDFILE",
        help="Path to the card data file (default: %(default)s)",
    )
    return parser.parse_args(argv)


def main(argv: Optional[Sequence[str]] = None) -> int:
    """Main procedure equivalent to the COBOL PROCEDURE DIVISION."""
    args = parse_args(argv)

    print("START OF EXECUTION OF PROGRAM CBACT02C")

    card_file = CardFileHandler(args.cardfile)

    if card_file.open_file() != APPL_AOK:
        print("ERROR OPENING CARDFILE")
        card_file.display_io_status()
        abend_program()

    while True:
        record, appl_result = card_file.read_next()

        if appl_result == APPL_AOK and record:
            print(record)
        elif appl_result == APPL_EOF:
            break
        else:
            print("ERROR READING CARDFILE")
            card_file.display_io_status()
            card_file.close_file()
            abend_program()

    if card_file.close_file() != APPL_AOK:
        print("ERROR CLOSING CARDFILE")
        card_file.display_io_status()
        abend_program()

    print("END OF EXECUTION OF PROGRAM CBACT02C")
    return 0


if __name__ == "__main__":
    sys.exit(main())
