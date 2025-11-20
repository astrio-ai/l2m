"""Python modernization of the CBACT03C COBOL program."""
from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path
import sys
from typing import Iterator

CARD_NUMBER_WIDTH = 16
XREF_DATA_WIDTH = 34
RECORD_WIDTH = CARD_NUMBER_WIDTH + XREF_DATA_WIDTH
DEFAULT_ENCODING = "utf-8"


class CardXrefFormatError(ValueError):
    """Raised when a line doesn't match the expected fixed-width layout."""


@dataclass(frozen=True)
class CardXrefRecord:
    """Represents a single record in the account cross reference file."""

    card_number: str
    xref_data: str

    def as_display_string(self) -> str:
        """Return a faithful representation suitable for console output."""
        return f"{self.card_number:<{CARD_NUMBER_WIDTH}}{self.xref_data}"


def parse_fixed_width_line(line: str, line_number: int) -> CardXrefRecord:
    """Parse a fixed-width line into a CardXrefRecord."""
    if len(line) < RECORD_WIDTH:
        raise CardXrefFormatError(
            f"Line {line_number} is {len(line)} characters long; expected "
            f"{RECORD_WIDTH} characters."
        )

    card_number = line[:CARD_NUMBER_WIDTH].strip()
    xref_data = line[CARD_NUMBER_WIDTH:RECORD_WIDTH].rstrip()

    return CardXrefRecord(card_number=card_number, xref_data=xref_data)


def read_card_xref_records(file_path: Path) -> Iterator[CardXrefRecord]:
    """Yield records from the fixed-width account cross reference file."""
    with file_path.open("r", encoding=DEFAULT_ENCODING) as source:
        for line_number, raw_line in enumerate(source, start=1):
            stripped_line = raw_line.rstrip("\r\n")
            if not stripped_line:
                continue
            yield parse_fixed_width_line(stripped_line, line_number)


def build_argument_parser() -> argparse.ArgumentParser:
    """Create the CLI argument parser."""
    parser = argparse.ArgumentParser(
        description=(
            "Modernized version of CBACT03C: read and display account cross "
            "reference records from a fixed-width file."
        )
    )
    parser.add_argument(
        "input_file",
        type=Path,
        help=(
            "Path to the fixed-width account cross reference file. Each record "
            "must contain 16 characters for the card number followed by "
            "34 characters of cross reference data."
        ),
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    """Program entry point."""
    parser = build_argument_parser()
    args = parser.parse_args(argv)
    input_path: Path = args.input_file

    print("START OF EXECUTION OF PROGRAM CBACT03C")
    try:
        record_found = False
        for record in read_card_xref_records(input_path):
            record_found = True
            print(record.as_display_string())
    except FileNotFoundError:
        print(f"ERROR OPENING XREFFILE: '{input_path}' was not found.", file=sys.stderr)
        return 12
    except CardXrefFormatError as exc:
        print(f"ERROR READING XREFFILE: {exc}", file=sys.stderr)
        return 12

    if not record_found:
        print("WARNING: No records were found in the supplied file.", file=sys.stderr)

    print("END OF EXECUTION OF PROGRAM CBACT03C")
    return 0


if __name__ == "__main__":
    sys.exit(main())
