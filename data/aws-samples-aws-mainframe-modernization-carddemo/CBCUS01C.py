from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator, Optional, TextIO

DEFAULT_DATASET_NAME = "CUSTFILE"
CUSTOMER_ID_LENGTH = 9

ERROR_MESSAGES = {
    "OPEN": "ERROR OPENING CUSTFILE",
    "READ": "ERROR READING CUSTOMER FILE",
    "CLOSE": "ERROR CLOSING CUSTOMER FILE",
}


@dataclass(slots=True)
class CustomerRecord:
    """In-memory representation of FD-CUSTFILE-REC."""
    cust_id: str
    cust_data: str

    @classmethod
    def from_line(cls, line: str) -> "CustomerRecord":
        """Parse a fixed-format record line into a CustomerRecord."""
        stripped_line = line.rstrip("\r\n")
        if len(stripped_line) < CUSTOMER_ID_LENGTH:
            raise ValueError(
                "Record is shorter than the 9-character customer identifier field."
            )

        cust_id = stripped_line[:CUSTOMER_ID_LENGTH]
        cust_data = stripped_line[CUSTOMER_ID_LENGTH:]
        return cls(cust_id=cust_id, cust_data=cust_data)

    def __str__(self) -> str:
        return f"{self.cust_id}{self.cust_data}"


class CustomerFileError(RuntimeError):
    """Represents an error that occurred while handling the customer file."""

    def __init__(self, operation: str, status: str, detail: Optional[str] = None):
        super().__init__(detail or "")
        self.operation = operation
        self.status = status
        self.detail = detail or ""


class CustomerFileReader:
    """High-level equivalent of the COBOL KSDS access routines."""

    def __init__(self, dataset: Path):
        self.dataset = dataset
        self._handle: Optional[TextIO] = None
        self.status = "00"

    @property
    def is_open(self) -> bool:
        return self._handle is not None

    def open(self) -> None:
        """Open the dataset for sequential reading."""
        try:
            self._handle = self.dataset.open("r", encoding="utf-8")
            self.status = "00"
        except FileNotFoundError as exc:
            self.status = "23"
            raise CustomerFileError(
                "OPEN", self.status, f"Customer file '{self.dataset}' was not found."
            ) from exc
        except OSError as exc:
            self.status = "99"
            raise CustomerFileError(
                "OPEN", self.status, f"Unable to open '{self.dataset}': {exc}"
            ) from exc

    def close(self) -> None:
        """Close the dataset."""
        if self._handle is None:
            return

        try:
            self._handle.close()
            self.status = "00"
        except OSError as exc:
            self.status = "99"
            raise CustomerFileError(
                "CLOSE", self.status, f"Unable to close '{self.dataset}': {exc}"
            ) from exc
        finally:
            self._handle = None

    def close_quietly(self) -> None:
        """Attempt to close the dataset while suppressing close errors."""
        try:
            self.close()
        except CustomerFileError:
            pass

    def records(self) -> Iterator[CustomerRecord]:
        """Yield records sequentially, mirroring READ ... NEXT."""
        if self._handle is None:
            raise CustomerFileError(
                "READ", "91", "Customer file has not been opened."
            )

        line_number = 0
        while True:
            try:
                raw_line = self._handle.readline()
            except OSError as exc:
                self.status = "99"
                raise CustomerFileError(
                    "READ",
                    self.status,
                    f"Unable to read from '{self.dataset}': {exc}",
                ) from exc

            if raw_line == "":
                self.status = "10"
                break

            line_number += 1
            try:
                record = CustomerRecord.from_line(raw_line)
            except ValueError as exc:
                self.status = "04"
                raise CustomerFileError(
                    "READ",
                    self.status,
                    f"Invalid record at line {line_number}: {exc}",
                ) from exc

            self.status = "00"
            yield record


def display_io_status(status: str) -> None:
    """Python rendition of Z-DISPLAY-IO-STATUS."""
    io_status = (status or "00")[:2].ljust(2, "0")
    io_stat1, io_stat2 = io_status[0], io_status[1]

    if not io_stat1.isdigit() or io_stat1 == "9":
        status_04 = f"{io_stat1}000"
    else:
        status_04 = f"00{io_stat1}{io_stat2}"

    print(f"FILE STATUS IS: NNNN {status_04}")


def abend_program() -> int:
    """Simulate COBOL's CEE3ABD call by returning a non-zero completion code."""
    print("ABENDING PROGRAM")
    return 999


def handle_file_error(error: CustomerFileError) -> int:
    """Emit COBOL-compatible diagnostics for a file-related failure."""
    print(ERROR_MESSAGES.get(error.operation, "ERROR PROCESSING CUSTOMER FILE"))
    if error.detail:
        print(error.detail)
    display_io_status(error.status)
    return abend_program()


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Modernized Python implementation of CBCUS01C."
    )
    parser.add_argument(
        "customer_file",
        nargs="?",
        default=DEFAULT_DATASET_NAME,
        help="Path to the customer data set (default: %(default)s).",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    dataset = Path(args.customer_file)

    print("START OF EXECUTION OF PROGRAM CBCUS01C")

    reader = CustomerFileReader(dataset)

    try:
        reader.open()
        for record in reader.records():
            print(record)
    except CustomerFileError as exc:
        reader.close_quietly()
        return handle_file_error(exc)

    try:
        reader.close()
    except CustomerFileError as exc:
        return handle_file_error(exc)

    print("END OF EXECUTION OF PROGRAM CBCUS01C")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
