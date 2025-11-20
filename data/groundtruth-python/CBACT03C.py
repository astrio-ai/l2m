"""
Ground Truth Python Equivalent for CBACT03C.cbl

Program: CBACT03C
Application: CardDemo
Type: BATCH Program
Function: Read and print account cross reference data file.

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

# card_xref.py
from dataclasses import dataclass
from typing import Optional, TextIO, Union
import io


@dataclass
class CardXrefRecord:
    card_num: str  # 16 chars
    data: str      # 34 chars

    def __str__(self) -> str:
        return f"{self.card_num}{self.data}"


class FileStatusError(Exception):
    """Represent an error reading or opening the XREF file."""
    def __init__(self, status: str, message: str = ""):
        super().__init__(message or f"File status: {status}")
        self.status = status


class XRefReader:
    """
    A small file-like reader that expects fixed-width records of 50 characters:
      - 16 chars card number
      - 34 chars data
    """

    RECORD_WIDTH = 16 + 34

    def __init__(self, fileobj: TextIO):
        self._file = fileobj
        self._closed = False

    @classmethod
    def open_from_path(cls, path: str) -> 'XRefReader':
        try:
            f = open(path, "r", encoding="utf-8")
            return cls(f)
        except OSError as e:
            # Map to COBOL-like non-'00' status
            raise FileStatusError("12", f"Error opening file: {e}") from e

    def read_next(self) -> CardXrefRecord:
        """
        Read next fixed-width record. If EOF, raise EOFError.
        If a partial record is found, treat as error (FileStatusError).
        """
        if self._closed:
            raise FileStatusError("12", "Attempt to read closed file")

        raw = self._file.read(self.RECORD_WIDTH)
        if raw == "":
            # EOF
            raise EOFError("EOF")
        if len(raw) < self.RECORD_WIDTH:
            # partial record -> error
            raise FileStatusError("12", "Partial record encountered")

        # strip trailing newlines/spaces only beyond the fixed widths (be conservative)
        card_num = raw[:16]
        data = raw[16:50]
        return CardXrefRecord(card_num=card_num, data=data)

    def close(self) -> None:
        try:
            self._file.close()
        except OSError as e:
            raise FileStatusError("12", f"Error closing file: {e}")
        finally:
            self._closed = True


def format_io_status(status: str) -> str:
    """
    Emulate COBOL display of IO-STATUS-04 (NNNN).
    If status is not numeric (two chars expected), show 'N' style handling.
    We'll return a 4-character string similar to the COBOL display.
    Examples:
      '00' -> '0000' (COBOL moved status into right-most two positions)
      '12' -> '0012'
      '9x' -> treat as non-numeric first char => emulate COBOL's path: e.g. '9A' -> '9???' (but we'll show '9' followed by 3 chars)
    """
    if len(status) != 2:
        status = status[:2].ljust(2, '0')

    if status[0].isdigit() and status[1].isdigit():
        # place as right-most two digits
        return "00" + status
    else:
        # non-numeric or leading 9 -> emulate putting the first char into left-most N (COBOL special handling)
        left = status[0] if status[0].isdigit() else status[0]
        right = status[1] if len(status) > 1 else '0'
        # produce 4 chars: left, then three-digit numeric built from right (best-effort)
        # For simplicity we return left then '00' then right
        return f"{left}00{right}"


def process_xref_file(source: Union[str, TextIO]) -> None:
    """
    Main program flow: prints start, reads and displays each record, handles EOF and errors,
    then closes the file and prints end.
    Accepts either a path (str) or a file-like object.
    """
    print("START OF EXECUTION OF PROGRAM CBACT03C")

    reader: Optional[XRefReader] = None
    try:
        # open or wrap provided file object
        if isinstance(source, str):
            reader = XRefReader.open_from_path(source)
        elif isinstance(source, io.TextIOBase) or hasattr(source, "read"):
            reader = XRefReader(source)  # wrap a file-like object
        else:
            raise FileStatusError("12", "Invalid source provided to process_xref_file")

        # read loop
        while True:
            try:
                rec = reader.read_next()
                # COBOL displays entire CARD-XREF-RECORD; here we print a representation
                # we simply print the fixed-width record string (no trimming)
                print(str(rec))
            except EOFError:
                # set END-OF-FILE, break out
                break
            except FileStatusError as fe:
                # emulate COBOL error handling: display status and abort
                io_status = fe.status
                print("ERROR READING XREFFILE")
                print("FILE STATUS IS: ", format_io_status(io_status))
                # In COBOL, they ABEND; here we raise to allow caller/tests to observe
                raise

    except FileStatusError as fe_open:
        # opening error handling
        print("ERROR OPENING XREFFILE")
        print("FILE STATUS IS: ", format_io_status(fe_open.status))
        # raise or return — we re-raise for test visibility
        raise
    finally:
        # try to close if opened
        if reader is not None:
            try:
                reader.close()
            except FileStatusError as fe_close:
                print("ERROR CLOSING XREFFILE")
                print("FILE STATUS IS: ", format_io_status(fe_close.status))
                raise

    print("END OF EXECUTION OF PROGRAM CBACT03C")
