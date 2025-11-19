"""Modernized CLI implementation of the COTRN01C COBOL program."""

from __future__ import annotations

import argparse
from datetime import datetime
from decimal import Decimal, InvalidOperation
from pathlib import Path
import sys
from typing import Any, Dict

from COBIL00C import TranRecord, parse_tran_record

DEFAULT_ENCODING = "utf-8"
EXIT_INVALID_INPUT = 8
EXIT_FILE_ERROR = 12
EXIT_RECORD_NOT_FOUND = 16


class TransactionDatasetError(RuntimeError):
    """Raised when the TRANSACT dataset cannot be read or parsed."""


class TransactionRepository:
    """Loads transaction records from the TRANSACT dataset."""

    def __init__(self, dataset_path: Path):
        self.dataset_path = dataset_path
        self._records = self._load_records()

    def _load_records(self) -> Dict[str, TranRecord]:
        records: Dict[str, TranRecord] = {}
        try:
            with self.dataset_path.open("r", encoding=DEFAULT_ENCODING) as dataset:
                for line_no, raw_line in enumerate(dataset, start=1):
                    line = raw_line.rstrip("\r\n")
                    if not line.strip():
                        continue
                    try:
                        record = parse_tran_record(line)
                    except Exception as exc:  # pragma: no cover - defensive
                        raise TransactionDatasetError(
                            f"Line {line_no}: {exc}"
                        ) from exc
                    tran_id = record.tran_id.strip()
                    if tran_id:
                        records[tran_id.upper()] = record
        except FileNotFoundError:
            raise
        except OSError as exc:  # pragma: no cover - platform dependent
            raise TransactionDatasetError(str(exc)) from exc
        return records

    def lookup(self, tran_id: str) -> TranRecord | None:
        if not tran_id:
            return None
        return self._records.get(tran_id.strip().upper())


def build_argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Modernized version of COTRN01C: look up a transaction by ID "
            "from the TRANSACT dataset."
        )
    )
    parser.add_argument(
        "dataset",
        type=Path,
        help="Path to the TRANSACT dataset (fixed-width transaction file).",
    )
    parser.add_argument(
        "transaction_id",
        help="The transaction ID to view (case-insensitive).",
    )
    return parser


def _current_datetime_strings() -> tuple[str, str]:
    now = datetime.now()
    return now.strftime("%m/%d/%y"), now.strftime("%H:%M:%S")


def _format_text(value: Any) -> str:
    if value is None:
        return "-"
    text = str(value).strip()
    return text or "-"


def _format_amount(value: Any) -> str:
    if value is None:
        return "-"
    if isinstance(value, Decimal):
        return f"{value:,.2f}"
    text = str(value).strip()
    if not text:
        return "-"
    try:
        decimal_value = Decimal(text)
    except (InvalidOperation, ValueError):
        return text
    return f"{decimal_value:,.2f}"


def render_transaction(record: TranRecord, dataset_path: Path) -> str:
    date_str, time_str = _current_datetime_strings()
    rows = [
        ("Transaction ID", _format_text(record.tran_id)),
        ("Card Number", _format_text(record.tran_card_num)),
        ("Transaction Type", _format_text(record.tran_type_cd)),
        ("Transaction Category", _format_text(record.tran_cat_cd)),
        ("Source", _format_text(record.tran_source)),
        ("Amount (USD)", _format_amount(record.tran_amt)),
        ("Description", _format_text(record.tran_desc)),
        ("Original Timestamp", _format_text(record.tran_orig_ts)),
        ("Processed Timestamp", _format_text(record.tran_proc_ts)),
        ("Merchant ID", _format_text(record.tran_merchant_id)),
        ("Merchant Name", _format_text(record.tran_merchant_name)),
        ("Merchant City", _format_text(record.tran_merchant_city)),
        ("Merchant ZIP", _format_text(record.tran_merchant_zip)),
    ]
    label_width = max(len(label) for label, _ in rows)
    lines = [
        "CardDemo Application — Transaction View",
        "Program : COTRN01C (modernized)",
        f"Dataset : {dataset_path}",
        f"Date    : {date_str}   Time: {time_str}",
        "",
    ]
    for label, value in rows:
        lines.append(f"{label:<{label_width}} : {value}")
    return "\n".join(lines)


def main(argv: list[str] | None = None) -> int:
    parser = build_argument_parser()
    args = parser.parse_args(argv)

    tran_id = args.transaction_id.strip()
    if not tran_id:
        print("Tran ID can NOT be empty...", file=sys.stderr)
        return EXIT_INVALID_INPUT

    try:
        repository = TransactionRepository(args.dataset)
    except FileNotFoundError:
        print(
            f"ERROR OPENING TRANSACT FILE: '{args.dataset}' was not found.",
            file=sys.stderr,
        )
        return EXIT_FILE_ERROR
    except TransactionDatasetError as exc:
        print(
            f"ERROR READING TRANSACT FILE '{args.dataset}': {exc}",
            file=sys.stderr,
        )
        return EXIT_FILE_ERROR

    record = repository.lookup(tran_id)
    if record is None:
        print("Transaction ID NOT found...", file=sys.stderr)
        return EXIT_RECORD_NOT_FOUND

    print("START OF EXECUTION OF PROGRAM COTRN01C (modernized)")
    print(render_transaction(record, args.dataset))
    print("END OF EXECUTION OF PROGRAM COTRN01C (modernized)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
