from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Tuple

FileReadResult = Tuple[str, Optional[str]]


@dataclass
class M03BArea:
    """
    Lightweight representation of the LK-M03B-AREA COBOL linkage structure.
    """

    dd_name: str = ""
    operation: str = ""
    return_code: str = "00"
    key: str = ""
    key_len: int = 0
    field_data: str = ""

    @property
    def normalized_dd_name(self) -> str:
        return (self.dd_name or "").strip().upper()

    @property
    def operation_code(self) -> str:
        return (self.operation or "").strip().upper()[:1]

    @property
    def is_open(self) -> bool:
        return self.operation_code == "O"

    @property
    def is_close(self) -> bool:
        return self.operation_code == "C"

    @property
    def is_read(self) -> bool:
        return self.operation_code == "R"

    @property
    def is_read_key(self) -> bool:
        return self.operation_code == "K"

    @property
    def requested_key_length(self) -> int:
        return abs(self.key_len)

    def effective_key(self) -> str:
        key_value = self.key or ""
        length = self.requested_key_length
        if length:
            return key_value[:length]
        return key_value


class BaseFileHandler:
    """
    Base class for file abstractions used by the modernized subroutine.
    """

    def __init__(self, dd_name: str):
        self.dd_name = dd_name.upper()
        self.is_open = False

    def resolve_file_path(self) -> Path:
        override = os.environ.get(f"{self.dd_name}_PATH")
        if override:
            return Path(override)
        return Path(self.dd_name)

    def open(self) -> str:
        raise NotImplementedError

    def read(self) -> FileReadResult:
        return "12", None

    def read_by_key(self, key: str) -> FileReadResult:
        return "12", None

    def close(self) -> str:
        self.is_open = False
        return "00"


class SequentialFileHandler(BaseFileHandler):
    """
    Handles sequential files (TRNXFILE, XREFFILE).
    """

    def __init__(self, dd_name: str):
        super().__init__(dd_name)
        self.records: Optional[List[str]] = None
        self.cursor: int = 0

    def open(self) -> str:
        path = self.resolve_file_path()
        try:
            with path.open("r", encoding="utf-8") as file_handle:
                self.records = [line.rstrip("\r\n") for line in file_handle]
        except FileNotFoundError:
            self.records = None
            self.cursor = 0
            self.is_open = False
            return "35"

        self.cursor = 0
        self.is_open = True
        return "00"

    def read(self) -> FileReadResult:
        if not self.is_open:
            rc = self.open()
            if rc != "00":
                return rc, None

        if self.records is None:
            return "35", None

        if self.cursor >= len(self.records):
            return "10", None

        record = self.records[self.cursor]
        self.cursor += 1
        return "00", record

    def close(self) -> str:
        self.records = None
        self.cursor = 0
        self.is_open = False
        return "00"


class RandomAccessFileHandler(BaseFileHandler):
    """
    Handles indexed/random access files (CUSTFILE, ACCTFILE).
    """

    def __init__(self, dd_name: str):
        super().__init__(dd_name)
        self.records: Optional[List[str]] = None

    def open(self) -> str:
        path = self.resolve_file_path()
        try:
            with path.open("r", encoding="utf-8") as file_handle:
                self.records = [line.rstrip("\r\n") for line in file_handle]
        except FileNotFoundError:
            self.records = None
            self.is_open = False
            return "35"

        self.is_open = True
        return "00"

    def read_by_key(self, key: str) -> FileReadResult:
        if not key:
            return "23", None

        if not self.is_open:
            rc = self.open()
            if rc != "00":
                return rc, None

        if self.records is None:
            return "35", None

        for record in self.records:
            if record.startswith(key):
                return "00", record

        return "23", None

    def close(self) -> str:
        self.records = None
        self.is_open = False
        return "00"


class TrnxFileHandler(SequentialFileHandler):
    def __init__(self):
        super().__init__("TRNXFILE")


class XrefFileHandler(SequentialFileHandler):
    def __init__(self):
        super().__init__("XREFFILE")


class CustFileHandler(RandomAccessFileHandler):
    def __init__(self):
        super().__init__("CUSTFILE")


class AcctFileHandler(RandomAccessFileHandler):
    def __init__(self):
        super().__init__("ACCTFILE")


class CBSTM03BSubroutine:
    """
    Python rendition of the CBSTM03B COBOL subroutine.
    """

    handler_factories: Dict[str, type[BaseFileHandler]] = {
        "TRNXFILE": TrnxFileHandler,
        "XREFFILE": XrefFileHandler,
        "CUSTFILE": CustFileHandler,
        "ACCTFILE": AcctFileHandler,
    }

    def __init__(self):
        self.handlers: Dict[str, BaseFileHandler] = {}

    def _get_handler(self, dd_name: str) -> Optional[BaseFileHandler]:
        factory = self.handler_factories.get(dd_name)
        if factory is None:
            return None

        if dd_name not in self.handlers:
            self.handlers[dd_name] = factory()
        return self.handlers[dd_name]

    def process(self, area: M03BArea) -> None:
        dd_name = area.normalized_dd_name
        handler = self._get_handler(dd_name)

        if handler is None:
            area.return_code = "12"
            return

        if area.is_open:
            rc = handler.open()
        elif area.is_close:
            rc = handler.close()
        elif area.is_read:
            rc, data = handler.read()
            area.field_data = data or ""
        elif area.is_read_key:
            key = area.effective_key()
            rc, data = handler.read_by_key(key)
            area.field_data = data or ""
        else:
            rc = "12"

        area.return_code = rc


_SUBROUTINE: Optional[CBSTM03BSubroutine] = None


def _get_subroutine() -> CBSTM03BSubroutine:
    global _SUBROUTINE
    if _SUBROUTINE is None:
        _SUBROUTINE = CBSTM03BSubroutine()
    return _SUBROUTINE


def cbstmt03b_subroutine(area: M03BArea) -> M03BArea:
    """
    Entry point compatible with the original COBOL subroutine linkage.
    """
    subroutine = _get_subroutine()
    subroutine.process(area)
    return area


def reset_subroutine() -> None:
    """
    Helper for tests to reset the singleton state.
    """
    global _SUBROUTINE
    _SUBROUTINE = None


__all__ = [
    "M03BArea",
    "CBSTM03BSubroutine",
    "TrnxFileHandler",
    "XrefFileHandler",
    "CustFileHandler",
    "AcctFileHandler",
    "cbstmt03b_subroutine",
    "reset_subroutine",
]
