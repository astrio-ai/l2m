from __future__ import annotations

import argparse
import csv
import json
import logging
from dataclasses import dataclass
from decimal import Decimal, InvalidOperation
from pathlib import Path
from typing import Any, Dict, Iterable, Iterator, Mapping, Optional, Tuple

LOGGER = logging.getLogger(__name__)

EXIT_SUCCESS = 0
EXIT_FILE_ERROR = 12
EXIT_NOT_FOUND = 16

RECORD_SEPARATOR = "-" * 49


def _normalize_key(key: Any) -> str:
    return str(key).strip().lower().replace("-", "_")


_FIELD_ALIAS_GROUPS_RAW: Dict[str, Tuple[str, ...]] = {
    "acct_id": ("acct_id", "account_id", "fd_acct_id", "acctid"),
    "acct_active_status": ("acct_active_status", "active_status", "acct_status"),
    "acct_curr_bal": ("acct_curr_bal", "acct_current_balance", "current_balance", "acct_curr_bal"),
    "acct_credit_limit": ("acct_credit_limit", "credit_limit"),
    "acct_cash_credit_limit": ("acct_cash_credit_limit", "cash_credit_limit"),
    "acct_open_date": ("acct_open_date", "open_date"),
    "acct_expiration_date": ("acct_expiration_date", "acct_expiraion_date", "expiration_date"),
    "acct_reissue_date": ("acct_reissue_date", "reissue_date"),
    "acct_curr_cyc_credit": ("acct_curr_cyc_credit", "current_cycle_credit"),
    "acct_curr_cyc_debit": ("acct_curr_cyc_debit", "current_cycle_debit"),
    "acct_group_id": ("acct_group_id", "group_id"),
}

FIELD_ALIASES: Dict[str, Tuple[str, ...]] = {
    field: tuple(_normalize_key(alias) for alias in aliases)
    for field, aliases in _FIELD_ALIAS_GROUPS_RAW.items()
}

FIELD_DISPLAY_ORDER = [
    ("acct_id", "ACCT-ID"),
    ("acct_active_status", "ACCT-ACTIVE-STATUS"),
    ("acct_curr_bal", "ACCT-CURR-BAL"),
    ("acct_credit_limit", "ACCT-CREDIT-LIMIT"),
    ("acct_cash_credit_limit", "ACCT-CASH-CREDIT-LIMIT"),
    ("acct_open_date", "ACCT-OPEN-DATE"),
    ("acct_expiration_date", "ACCT-EXPIRAION-DATE"),
    ("acct_reissue_date", "ACCT-REISSUE-DATE"),
    ("acct_curr_cyc_credit", "ACCT-CURR-CYC-CREDIT"),
    ("acct_curr_cyc_debit", "ACCT-CURR-CYC-DEBIT"),
    ("acct_group_id", "ACCT-GROUP-ID"),
]


class AccountFileError(RuntimeError):
    """Raised when an account data file cannot be processed."""


@dataclass
class AccountRecord:
    acct_id: str
    acct_active_status: str = ""
    acct_curr_bal: Decimal = Decimal("0")
    acct_credit_limit: Decimal = Decimal("0")
    acct_cash_credit_limit: Decimal = Decimal("0")
    acct_open_date: str = ""
    acct_expiration_date: str = ""
    acct_reissue_date: str = ""
    acct_curr_cyc_credit: Decimal = Decimal("0")
    acct_curr_cyc_debit: Decimal = Decimal("0")
    acct_group_id: str = ""

    @classmethod
    def from_mapping(cls, raw_record: Mapping[str, Any]) -> "AccountRecord":
        normalized = _normalize_keys(raw_record)
        return cls(
            acct_id=_string_value(normalized, "acct_id", required=True),
            acct_active_status=_string_value(normalized, "acct_active_status"),
            acct_curr_bal=_decimal_value(normalized, "acct_curr_bal"),
            acct_credit_limit=_decimal_value(normalized, "acct_credit_limit"),
            acct_cash_credit_limit=_decimal_value(normalized, "acct_cash_credit_limit"),
            acct_open_date=_string_value(normalized, "acct_open_date"),
            acct_expiration_date=_string_value(normalized, "acct_expiration_date"),
            acct_reissue_date=_string_value(normalized, "acct_reissue_date"),
            acct_curr_cyc_credit=_decimal_value(normalized, "acct_curr_cyc_credit"),
            acct_curr_cyc_debit=_decimal_value(normalized, "acct_curr_cyc_debit"),
            acct_group_id=_string_value(normalized, "acct_group_id"),
        )

    def as_display_lines(self) -> Tuple[str, ...]:
        lines = []
        for attr, label in FIELD_DISPLAY_ORDER:
            value = getattr(self, attr)
            rendered = self._format_value(value)
            lines.append(f"{label:<25}: {rendered}")
        lines.append(RECORD_SEPARATOR)
        return tuple(lines)

    @staticmethod
    def _format_value(value: Any) -> str:
        if isinstance(value, Decimal):
            return f"{value:,.2f}"
        text = "" if value is None else str(value).strip()
        return text


def _normalize_keys(row: Mapping[str, Any]) -> Dict[str, Any]:
    return {_normalize_key(key): value for key, value in row.items()}


def _value_for(field: str, normalized_row: Mapping[str, Any]) -> Optional[Any]:
    aliases = FIELD_ALIASES.get(field, (field,))
    for alias in aliases:
        if alias in normalized_row:
            value = normalized_row[alias]
            if not _is_blank(value):
                return value
    return None


def _string_value(
    normalized_row: Mapping[str, Any],
    field: str,
    default: str = "",
    required: bool = False,
) -> str:
    value = _value_for(field, normalized_row)
    if value is None:
        if required:
            raise ValueError(f"Missing required field '{field}'.")
        return default
    return str(value).strip()


def _decimal_value(
    normalized_row: Mapping[str, Any],
    field: str,
    default: Decimal = Decimal("0"),
) -> Decimal:
    value = _value_for(field, normalized_row)
    if value is None:
        return default
    try:
        return _to_decimal(value)
    except InvalidOperation as exc:
        raise ValueError(f"Invalid numeric value for '{field}': {value!r}") from exc


def _to_decimal(raw_value: Any) -> Decimal:
    if isinstance(raw_value, Decimal):
        return raw_value
    text = str(raw_value).strip().replace(",", "")
    if not text:
        return Decimal("0")
    return Decimal(text)


def _is_blank(value: Any) -> bool:
    if value is None:
        return True
    if isinstance(value, str):
        return not value.strip()
    return False


RecordPayload = Tuple[int, Mapping[str, Any]]


def iter_account_records(path: Path) -> Iterator[AccountRecord]:
    path = path.expanduser()
    if not path.exists():
        raise AccountFileError(f"Account file '{path}' does not exist.")

    suffix = path.suffix.lower()
    if suffix == ".jsonl":
        loader = _iter_json_lines(path)
    elif suffix == ".json":
        loader = _iter_json_document(path)
    elif suffix in {".csv", ".txt"}:
        loader = _iter_csv_rows(path)
    else:
        raise AccountFileError(
            f"Unsupported account file type '{suffix or '<none>'}'. "
            "Use .json, .jsonl, .csv, or .txt files."
        )

    for position, row in loader:
        try:
            yield AccountRecord.from_mapping(row)
        except ValueError as exc:
            raise AccountFileError(
                f"Invalid data in record #{position}: {exc}"
            ) from exc


def _iter_json_lines(path: Path) -> Iterator[RecordPayload]:
    with path.open("r", encoding="utf-8") as handle:
        for line_number, line in enumerate(handle, start=1):
            stripped = line.strip()
            if not stripped:
                continue
            try:
                payload = json.loads(stripped)
            except json.JSONDecodeError as exc:
                raise AccountFileError(
                    f"Failed to decode JSON on line {line_number}: {exc}"
                ) from exc
            if not isinstance(payload, Mapping):
                raise AccountFileError(
                    f"Line {line_number} does not contain a JSON object."
                )
            yield line_number, payload


def _iter_json_document(path: Path) -> Iterator[RecordPayload]:
    with path.open("r", encoding="utf-8") as handle:
        try:
            payload = json.load(handle)
        except json.JSONDecodeError as exc:
            raise AccountFileError(
                f"Failed to decode JSON document '{path}': {exc}"
            ) from exc

    if isinstance(payload, list):
        for index, item in enumerate(payload, start=1):
            if not isinstance(item, Mapping):
                raise AccountFileError(
                    f"Element #{index} in '{path}' is not a JSON object."
                )
            yield index, item
    elif isinstance(payload, Mapping):
        yield 1, payload
    else:
        raise AccountFileError(
            f"JSON root in '{path}' must be an object or array of objects."
        )


def _iter_csv_rows(path: Path) -> Iterator[RecordPayload]:
    with path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        if reader.fieldnames is None:
            raise AccountFileError(f"CSV file '{path}' is missing a header row.")
        for row_number, row in enumerate(reader, start=2):
            if row and all(_is_blank(value) for value in row.values()):
                continue
            yield row_number, row


def parse_args(argv: Optional[Iterable[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Modernized Python equivalent of the CBACT01C COBOL batch program. "
            "Reads an account data file (.json, .jsonl, .csv, .txt) and "
            "prints each record in a human-readable format."
        )
    )
    parser.add_argument(
        "account_file",
        type=Path,
        help="Path to the account data file (.json, .jsonl, .csv, or .txt).",
    )
    parser.add_argument(
        "--log-level",
        default="INFO",
        help="Logging level (default: %(default)s).",
    )
    return parser.parse_args(argv)


def configure_logging(level_name: str) -> None:
    level = getattr(logging, level_name.upper(), logging.INFO)
    logging.basicConfig(level=level, format="%(levelname)s %(message)s")


def main(argv: Optional[Iterable[str]] = None) -> int:
    args = parse_args(argv)
    configure_logging(args.log_level)

    account_file: Path = args.account_file

    print("START OF EXECUTION OF PROGRAM CBACT01C")

    try:
        record_count = 0
        for record in iter_account_records(account_file):
            record_count += 1
            for line in record.as_display_lines():
                print(line)
        if record_count == 0:
            LOGGER.warning("No account records were found in '%s'.", account_file)
    except AccountFileError as exc:
        LOGGER.error("%s", exc)
        return EXIT_FILE_ERROR
    except FileNotFoundError:
        LOGGER.error("Account file '%s' was not found.", account_file)
        return EXIT_NOT_FOUND

    print("END OF EXECUTION OF PROGRAM CBACT01C")
    return EXIT_SUCCESS


if __name__ == "__main__":
    raise SystemExit(main())
