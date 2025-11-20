from __future__ import annotations

import argparse
import sys
from dataclasses import dataclass
from enum import IntEnum
from pathlib import Path
from typing import Dict, Optional, TextIO


class AppResult(IntEnum):
    """Application-level return codes mirroring the COBOL program."""
    OK = 0
    ERROR = 12
    EOF = 16


@dataclass
class DailyTranRecord:
    tran_id: str = ""
    tran_data: str = ""

    @property
    def raw(self) -> str:
        return f"{self.tran_id}{self.tran_data}"

    @property
    def card_number(self) -> str:
        if len(self.tran_data) >= 16:
            return self.tran_data[:16]
        return ""


@dataclass
class CardXrefRecord:
    card_num: str = ""
    cust_num: str = ""
    acct_id: str = ""

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
    acct_id: str = ""
    acct_data: str = ""


class FileHandler:
    """Base class for simulated COBOL file handlers."""

    def __init__(self, logical_name: str, path: Path):
        self.logical_name = logical_name
        self.path = path
        self.file_status = "00"

    def display_io_status(self):
        """Emit IO status using the same formatting as Z-DISPLAY-IO-STATUS."""
        io_status = self.file_status if len(self.file_status) == 2 else "00"
        if io_status.isdigit() and not io_status.startswith("9"):
            status_04 = f"00{io_status}"
        else:
            left = io_status[0] if io_status else "0"
            right = io_status[1] if len(io_status) > 1 else "0"
            status_04 = f"{left}00{right}"
        print(f'FILE STATUS IS: NNNN {status_04}')


class SequentialFileHandler(FileHandler):
    """Handler that simulates COBOL sequential file access."""

    def __init__(self, logical_name: str, path: Path):
        super().__init__(logical_name, path)
        self.file_handle: Optional[TextIO] = None
        self.end_of_file = False

    def open_file(self, mode: str = "r") -> AppResult:
        try:
            self.file_handle = open(self.path, mode, encoding="utf-8")
            self.file_status = "00"
            self.end_of_file = False
            return AppResult.OK
        except FileNotFoundError:
            self.file_status = "23"
            return AppResult.ERROR
        except Exception:
            self.file_status = "99"
            return AppResult.ERROR

    def read_next(self) -> tuple[Optional[str], AppResult]:
        if self.end_of_file:
            return None, AppResult.EOF
        if not self.file_handle:
            self.file_status = "90"
            return None, AppResult.ERROR
        try:
            line = self.file_handle.readline()
        except Exception:
            self.file_status = "99"
            return None, AppResult.ERROR
        if line == "":
            self.end_of_file = True
            self.file_status = "10"
            return None, AppResult.EOF
        self.file_status = "00"
        return line.rstrip("\n\r"), AppResult.OK

    def close_file(self) -> AppResult:
        try:
            if self.file_handle:
                self.file_handle.close()
                self.file_handle = None
            self.file_status = "00"
            return AppResult.OK
        except Exception:
            self.file_status = "99"
            return AppResult.ERROR


class IndexedFileHandler(FileHandler):
    """Simulated indexed file using an in-memory dictionary."""

    def __init__(self, logical_name: str, path: Path):
        super().__init__(logical_name, path)
        self._data: Dict[str, str] = {}

    def open_file(self, mode: str = "r") -> AppResult:
        if mode != "r":
            raise ValueError("Indexed files only support read mode in this simulation.")
        self._data.clear()
        try:
            with open(self.path, mode, encoding="utf-8") as handle:
                for line in handle:
                    record = line.rstrip("\n\r")
                    key = self._extract_key(record)
                    if key:
                        self._data[key] = record
            self.file_status = "00"
            return AppResult.OK
        except FileNotFoundError:
            self.file_status = "23"
            return AppResult.ERROR
        except Exception:
            self.file_status = "99"
            return AppResult.ERROR

    def read_record(self, key: str) -> tuple[Optional[str], AppResult]:
        if key in self._data:
            self.file_status = "00"
            return self._data[key], AppResult.OK
        self.file_status = "23"
        return None, AppResult.ERROR

    def close_file(self) -> AppResult:
        self.file_status = "00"
        return AppResult.OK

    def _extract_key(self, line: str) -> str:
        return ""


class CustFileHandler(IndexedFileHandler):
    def _extract_key(self, line: str) -> str:
        return line[:9].strip() if len(line) >= 9 else ""


class XrefFileHandler(IndexedFileHandler):
    def _extract_key(self, line: str) -> str:
        return line[:16] if len(line) >= 16 else ""


class CardFileHandler(IndexedFileHandler):
    def _extract_key(self, line: str) -> str:
        return line[:16] if len(line) >= 16 else ""


class AcctFileHandler(IndexedFileHandler):
    def _extract_key(self, line: str) -> str:
        return line[:11].strip() if len(line) >= 11 else ""


class TranFileHandler(IndexedFileHandler):
    def _extract_key(self, line: str) -> str:
        return line[:16] if len(line) >= 16 else ""


def parse_daily_tran_record(line: str) -> DailyTranRecord:
    tran_id = line[:16]
    tran_data = line[16:350] if len(line) > 16 else ""
    return DailyTranRecord(tran_id=tran_id, tran_data=tran_data)


def parse_xref_record(line: str) -> CardXrefRecord:
    card_num = line[:16]
    cust_num = line[16:25] if len(line) > 16 else ""
    acct_id = line[25:36] if len(line) > 25 else ""
    return CardXrefRecord(card_num=card_num, cust_num=cust_num, acct_id=acct_id)


def parse_account_record(line: str) -> AccountRecord:
    acct_id = line[:11]
    acct_data = line[11:300] if len(line) > 11 else ""
    return AccountRecord(acct_id=acct_id, acct_data=acct_data)


def abend_program():
    print('ABENDING PROGRAM')
    sys.exit(999)


class CBTRN01CApp:
    """Python rendition of the CBTRN01C COBOL batch program."""

    def __init__(self, base_dir: Optional[Path] = None):
        self.base_dir = Path(base_dir) if base_dir else None
        self.dalytran_file = SequentialFileHandler("DALYTRAN", self._path("DALYTRAN"))
        self.custfile = CustFileHandler("CUSTFILE", self._path("CUSTFILE"))
        self.xreffile = XrefFileHandler("XREFFILE", self._path("XREFFILE"))
        self.cardfile = CardFileHandler("CARDFILE", self._path("CARDFILE"))
        self.acctfile = AcctFileHandler("ACCTFILE", self._path("ACCTFILE"))
        self.tranfile = TranFileHandler("TRANFILE", self._path("TRANFILE"))
        self.end_of_daily_trans_file = False
        self.ws_xref_read_status = 0
        self.ws_acct_read_status = 0
        self.account_record: Optional[AccountRecord] = None

    def _path(self, filename: str) -> Path:
        return self.base_dir / filename if self.base_dir else Path(filename)

    def run(self) -> int:
        print('START OF EXECUTION OF PROGRAM CBTRN01C')
        self.open_all_files()
        try:
            self._process_daily_transactions()
        finally:
            self.close_all_files()
        print('END OF EXECUTION OF PROGRAM CBTRN01C')
        return 0

    def open_all_files(self):
        self._open_or_abend(self.dalytran_file, 'DAILY TRANSACTION FILE')
        self._open_or_abend(self.custfile, 'CUSTOMER FILE')
        self._open_or_abend(self.xreffile, 'CROSS REF FILE')
        self._open_or_abend(self.cardfile, 'CARD FILE')
        self._open_or_abend(self.acctfile, 'ACCOUNT FILE')
        self._open_or_abend(self.tranfile, 'TRANSACTION FILE')

    def close_all_files(self):
        self._close_or_abend(self.dalytran_file, 'DAILY TRANSACTION FILE')
        self._close_or_abend(self.custfile, 'CUSTOMER FILE')
        self._close_or_abend(self.xreffile, 'CROSS REF FILE')
        self._close_or_abend(self.cardfile, 'CARD FILE')
        self._close_or_abend(self.acctfile, 'ACCOUNT FILE')
        self._close_or_abend(self.tranfile, 'TRANSACTION FILE')

    @staticmethod
    def _open_or_abend(handler: FileHandler, description: str):
        if handler.open_file("r") is not AppResult.OK:
            print(f'ERROR OPENING {description}')
            handler.display_io_status()
            abend_program()

    @staticmethod
    def _close_or_abend(handler: FileHandler, description: str):
        if handler.close_file() is not AppResult.OK:
            print(f'ERROR CLOSING {description}')
            handler.display_io_status()
            abend_program()

    def _process_daily_transactions(self):
        while not self.end_of_daily_trans_file:
            line, result = self.dalytran_file.read_next()
            if result is AppResult.OK and line is not None:
                record = parse_daily_tran_record(line)
                print(record.raw)
                self._handle_transaction_record(record)
            elif result is AppResult.EOF:
                self.end_of_daily_trans_file = True
            else:
                print('ERROR READING DAILY TRANSACTION FILE')
                self.dalytran_file.display_io_status()
                abend_program()

    def _handle_transaction_record(self, record: DailyTranRecord):
        self.ws_xref_read_status = 0
        xref_line, xref_result = self.xreffile.read_record(record.card_number)
        xref_record: Optional[CardXrefRecord] = None

        if xref_result is AppResult.OK and xref_line:
            xref_record = parse_xref_record(xref_line)
            print('SUCCESSFUL READ OF XREF')
            print(f'CARD NUMBER: {xref_record.xref_card_num}')
            print(f'ACCOUNT ID : {xref_record.xref_acct_id}')
            print(f'CUSTOMER ID: {xref_record.xref_cust_id}')
        else:
            print('INVALID CARD NUMBER FOR XREF')
            self.ws_xref_read_status = 4

        if self.ws_xref_read_status == 0 and xref_record:
            self.ws_acct_read_status = 0
            acct_key = xref_record.xref_acct_id
            acct_line, acct_result = self.acctfile.read_record(acct_key)
            if acct_result is AppResult.OK and acct_line:
                self.account_record = parse_account_record(acct_line)
                print('SUCCESSFUL READ OF ACCOUNT FILE')
            else:
                print('INVALID ACCOUNT NUMBER FOUND')
                self.ws_acct_read_status = 4
            if self.ws_acct_read_status != 0:
                print(f'ACCOUNT {acct_key} NOT FOUND')
        else:
            print(
                f'CARD NUMBER {record.card_number} COULD NOT BE VERIFIED. '
                f'SKIPPING TRANSACTION ID-{record.tran_id}'
            )


def build_argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description='Python rendition of CBTRN01C.')
    parser.add_argument(
        '--data-dir',
        type=Path,
        default=None,
        help='Optional base directory where dataset files are located.'
    )
    return parser


def main(argv: Optional[list[str]] = None) -> int:
    parser = build_argument_parser()
    args = parser.parse_args(argv)
    app = CBTRN01CApp(args.data_dir)
    return app.run()


if __name__ == '__main__':
    sys.exit(main())
