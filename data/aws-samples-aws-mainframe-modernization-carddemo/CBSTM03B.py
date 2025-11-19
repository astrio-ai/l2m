"""
Python adaptation of the CBSTM03B COBOL subroutine.

The original subroutine performs basic OPEN/READ/CLOSE operations for four
files (TRNXFILE, XREFFILE, CUSTFILE, ACCTFILE).  This module mirrors that
behavior closely so it can be exercised from Python tests.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Tuple

SUCCESS = "00"
END_OF_FILE = "10"
FILE_NOT_FOUND = "35"
RECORD_NOT_FOUND = "23"
NOT_OPEN = "91"
INVALID_OPERATION = "92"
IO_ERROR = "90"


@dataclass
class M03BArea:
    """Represents the linkage area passed to the subroutine."""

    dd_name: str = ""
    operation: str = ""
    return_code: str = SUCCESS
    key: str = ""
    key_len: int = 0
    field_data: str = ""

    def __post_init__(self) -> None:
        self.dd_name = (self.dd_name or "").strip().upper()
        self.operation = (self.operation or "").strip().upper()

    @property
    def normalized_dd_name(self) -> str:
        return (self.dd_name or "").strip().upper()

    def _op(self) -> str:
        return (self.operation or "").upper()

    @property
    def is_open(self) -> bool:
        return self._op() == "O"

    @property
    def is_close(self) -> bool:
        return self._op() == "C"

    @property
    def is_read(self) -> bool:
        return self._op() == "R"

    @property
    def is_read_key(self) -> bool:
        return self._op() == "K"

    @property
    def is_write(self) -> bool:
        return self._op() == "W"

    @property
    def is_rewrite(self) -> bool:
        return self._op() == "Z"

    @property
    def effective_key(self) -> str:
        """Return the key trimmed to the absolute key length, if supplied."""
        length = abs(self.key_len)
        if length:
            return (self.key or "")[:length]
        return self.key or ""


class BaseFileHandler:
    """Common helper for all data-set handlers."""

    def __init__(self, dd_name: str) -> None:
        self.dd_name = dd_name
        self._records: Optional[List[str]] = None
        self._open: bool = False

    def open_input(self) -> str:
        status, records = self._read_records_from_disk()
        if status == SUCCESS:
            self._records = records
            self._open = True
            self._on_open()
        return status

    def close(self) -> str:
        self._records = None
        self._open = False
        self._on_close()
        return SUCCESS

    def read_next(self) -> Tuple[str, str]:
        return INVALID_OPERATION, ""

    def read_by_key(self, key: str) -> Tuple[str, str]:
        return INVALID_OPERATION, ""

    def _ensure_open(self) -> str:
        return SUCCESS if self._open else NOT_OPEN

    def _read_records_from_disk(self) -> Tuple[str, List[str]]:
        path = Path(self.dd_name)
        try:
            with path.open("r", encoding="utf-8") as handle:
                lines = [line.rstrip("\r\n") for line in handle]
            return SUCCESS, lines
        except FileNotFoundError:
            return FILE_NOT_FOUND, []
        except OSError:
            return IO_ERROR, []

    def _on_open(self) -> None:  # pragma: no cover - hook
        pass

    def _on_close(self) -> None:  # pragma: no cover - hook
        pass


class SequentialFileHandler(BaseFileHandler):
    """Implements sequential READ logic."""

    def __init__(self, dd_name: str) -> None:
        super().__init__(dd_name)
        self._position: int = 0

    def _on_open(self) -> None:
        self._position = 0

    def _on_close(self) -> None:
        self._position = 0

    def read_next(self) -> Tuple[str, str]:
        status = self._ensure_open()
        if status != SUCCESS:
            return status, ""
        assert self._records is not None
        if self._position >= len(self._records):
            return END_OF_FILE, ""
        record = self._records[self._position]
        self._position += 1
        return SUCCESS, record


class KeyedFileHandler(BaseFileHandler):
    """Implements keyed READ logic."""

    def read_by_key(self, key: str) -> Tuple[str, str]:
        status = self._ensure_open()
        if status != SUCCESS:
            return status, ""
        if not key:
            return RECORD_NOT_FOUND, ""
        assert self._records is not None
        for record in self._records:
            if record.startswith(key):
                return SUCCESS, record
        return RECORD_NOT_FOUND, ""


class TrnxFileHandler(SequentialFileHandler):
    """Handler for TRNXFILE (sequential access)."""


class XrefFileHandler(SequentialFileHandler):
    """Handler for XREFFILE (sequential access)."""


class CustFileHandler(KeyedFileHandler):
    """Handler for CUSTFILE (random/keyed access)."""


class AcctFileHandler(KeyedFileHandler):
    """Handler for ACCTFILE (random/keyed access)."""


class CBSTM03BSubroutine:
    """High-level orchestrator mirroring the COBOL PERFORM blocks."""

    def __init__(self) -> None:
        self.handlers: Dict[str, BaseFileHandler] = {
            "TRNXFILE": TrnxFileHandler("TRNXFILE"),
            "XREFFILE": XrefFileHandler("XREFFILE"),
            "CUSTFILE": CustFileHandler("CUSTFILE"),
            "ACCTFILE": AcctFileHandler("ACCTFILE"),
        }

    def process(self, area: M03BArea) -> M03BArea:
        handler = self.handlers.get(area.normalized_dd_name)
        if handler is None:
            area.return_code = IO_ERROR
            return area

        operation = (area.operation or "").upper()
        field_data = area.field_data

        if operation == "O":
            status = handler.open_input()
        elif operation == "R":
            status, field_data = handler.read_next()
        elif operation == "K":
            status, field_data = handler.read_by_key(area.effective_key)
        elif operation == "C":
            status = handler.close()
        else:
            status = INVALID_OPERATION

        area.return_code = status
        area.field_data = field_data
        return area

    def close_all(self) -> None:
        for handler in self.handlers.values():
            handler.close()


_subroutine_instance: Optional[CBSTM03BSubroutine] = None


def _get_subroutine() -> CBSTM03BSubroutine:
    global _subroutine_instance
    if _subroutine_instance is None:
        _subroutine_instance = CBSTM03BSubroutine()
    return _subroutine_instance


def cbstmt03b_subroutine(area: M03BArea) -> M03BArea:
    """Entry-point equivalent to the COBOL PROGRAM-ID."""
    if not isinstance(area, M03BArea):
        raise TypeError("cbstmt03b_subroutine expects an M03BArea instance")
    return _get_subroutine().process(area)


def reset_subroutine() -> None:
    """Utility used by tests to obtain a fresh subroutine instance."""
    global _subroutine_instance
    if _subroutine_instance is not None:
        _subroutine_instance.close_all()
    _subroutine_instance = None
