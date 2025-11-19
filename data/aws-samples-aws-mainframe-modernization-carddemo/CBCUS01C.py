#!/usr/bin/env python3
"""Python translation of the CBCUS01C COBOL batch program."""

from __future__ import annotations

import argparse
import os
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator, Sequence

RECORD_ID_LENGTH = 9
RECORD_DATA_LENGTH = 491
RECORD_LENGTH = RECORD_ID_LENGTH + RECORD_DATA_LENGTH
DEFAULT_ENCODING = "utf-8"


@dataclass(frozen=True)
class CustomerRecord:
    """In-memory representation of a single customer record."""

    customer_id: str
    payload: str

    @property
    def display_value(self) -> str:
        """Return the value that should be displayed, matching COBOL output."""
        return f"{self.customer_id}{self.payload}"

    def __str__(self) -> str:
        return self.display_value


def read_customer_records(path: Path, encoding: str) -> Iterator[CustomerRecord]:
    """Yield fixed-length customer records sequentially from the file."""
    with path.open("rb") as source:
        record_number = 0
        while True:
            chunk = source.read(RECORD_LENGTH)
            if not chunk:
                break
            if len(chunk) != RECORD_LENGTH:
                raise ValueError(
                    f"Incomplete record {record_number + 1}: "
                    f"expected {RECORD_LENGTH} bytes, got {len(chunk)}."
                )

            record_number += 1
            text = chunk.decode(encoding, errors="replace")
            customer_id = text[:RECORD_ID_LENGTH]
            payload = text[RECORD_ID_LENGTH:]
            yield CustomerRecord(customer_id=customer_id, payload=payload)


def parse_args(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Read and print customer records (CBCUS01C)."
    )
    parser.add_argument(
        "input_file",
        nargs="?",
        help="Path to the customer data file (defaults to $CUSTFILE).",
    )
    parser.add_argument(
        "--encoding",
        default=DEFAULT_ENCODING,
        help=f"Character encoding of the file (default: {DEFAULT_ENCODING}).",
    )
    return parser.parse_args(argv)


def resolve_input_path(cli_value: str | None) -> Path:
    if cli_value:
        return Path(cli_value)
    env_value = os.getenv("CUSTFILE")
    if env_value:
        return Path(env_value)
    raise ValueError(
        "No input file supplied. Provide a path argument or set the CUSTFILE "
        "environment variable."
    )


def run(argv: Sequence[str] | None = None) -> int:
    args = parse_args(argv)
    try:
        input_path = resolve_input_path(args.input_file)
    except ValueError as exc:
        print(exc, file=sys.stderr)
        return 16

    print("START OF EXECUTION OF PROGRAM CBCUS01C")
    try:
        for record in read_customer_records(input_path, args.encoding):
            print(record)
    except FileNotFoundError as exc:
        print("ERROR OPENING CUSTFILE", file=sys.stderr)
        print(exc, file=sys.stderr)
        return 12
    except ValueError as exc:
        print("ERROR READING CUSTOMER FILE", file=sys.stderr)
        print(exc, file=sys.stderr)
        return 12

    print("END OF EXECUTION OF PROGRAM CBCUS01C")
    return 0


if __name__ == "__main__":
    raise SystemExit(run())
