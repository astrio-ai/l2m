#!/usr/bin/env python3
"""
Modern Python implementation of the CardDemo CBACT04C COBOL batch program.

The script reads JSON files that represent the original VSAM/DB2 datasets:

* Transaction category balances (``--tcatbal``)
    [{
        "account_id": "12345678901",
        "transaction_type_code": "01",
        "transaction_category_code": "0005",
        "balance": "1500.00"
    }, ...]

* Account master (``--accounts``)
    [{
        "account_id": "12345678901",
        "group_id": "REVOLVER",
        "current_balance": "2000.00",
        "current_cycle_credit": "0.00",
        "current_cycle_debit": "0.00"
    }, ...]

* Card cross reference (``--xref``)
    [{
        "account_id": "12345678901",
        "card_number": "5555444433332222"
    }, ...]

* Disclosure group interest rates (``--discgrp``)
    [{
        "account_group_id": "REVOLVER",
        "transaction_type_code": "01",
        "transaction_category_code": "0005",
        "interest_rate": "12.99"   # annual percentage rate
    }, ...]

The program writes the generated interest transactions to the path supplied via
``--transactions-out`` and updates the account master file (``--accounts-out``
defaults to ``--accounts``).
"""

from __future__ import annotations

import argparse
import json
import logging
from dataclasses import dataclass
from datetime import datetime
from decimal import Decimal, ROUND_HALF_UP, getcontext
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence, Set, Tuple

getcontext().prec = 28

CENTS = Decimal("0.01")
DEFAULT_DISCLOSURE_GROUP = "DEFAULT"


def decimal_from_value(value: object, default: Decimal = Decimal("0.00")) -> Decimal:
    """Convert an arbitrary value into a Decimal, falling back to ``default``."""
    if isinstance(value, Decimal):
        return value
    if value is None or value == "":
        return default
    return Decimal(str(value))


def decimal_to_str(value: Decimal) -> str:
    """Serialize a Decimal value using fixed-point notation with cents precision."""
    return f"{value.quantize(CENTS, rounding=ROUND_HALF_UP):f}"


def normalize_code(raw_value: object, width: int) -> str:
    """
    Normalize a code field to match COBOL PIC definitions.

    Numeric values are zero-padded to the requested width; alphanumeric values
    are upper-cased and stripped of surrounding whitespace.
    """
    value = str(raw_value).strip()
    if not value:
        return value
    if value.isdecimal():
        return value.zfill(width)
    return value.upper()


@dataclass(frozen=True)
class TransactionCategoryBalance:
    account_id: str
    transaction_type_code: str
    transaction_category_code: str
    balance: Decimal

    @classmethod
    def from_dict(cls, data: dict) -> "TransactionCategoryBalance":
        try:
            account_id = str(data["account_id"]).strip()
            tran_type = normalize_code(data["transaction_type_code"], 2)
            tran_cat = normalize_code(data["transaction_category_code"], 4)
        except KeyError as exc:
            raise KeyError(
                f"Transaction category balance record missing '{exc.args[0]}'"
            ) from exc

        return cls(
            account_id=account_id,
            transaction_type_code=tran_type,
            transaction_category_code=tran_cat,
            balance=decimal_from_value(data.get("balance", "0.00")),
        )


@dataclass
class AccountRecord:
    account_id: str
    group_id: str
    current_balance: Decimal
    current_cycle_credit: Decimal
    current_cycle_debit: Decimal

    @classmethod
    def from_dict(cls, data: dict) -> "AccountRecord":
        try:
            account_id = str(data["account_id"]).strip()
        except KeyError as exc:
            raise KeyError("Account record missing 'account_id'") from exc

        group_id = str(data.get("group_id", DEFAULT_DISCLOSURE_GROUP)).strip() or DEFAULT_DISCLOSURE_GROUP

        return cls(
            account_id=account_id,
            group_id=group_id,
            current_balance=decimal_from_value(data.get("current_balance", "0.00")),
            current_cycle_credit=decimal_from_value(data.get("current_cycle_credit", "0.00")),
            current_cycle_debit=decimal_from_value(data.get("current_cycle_debit", "0.00")),
        )

    def apply_monthly_interest(self, interest_amount: Decimal) -> None:
        self.current_balance = (self.current_balance + interest_amount).quantize(
            CENTS, rounding=ROUND_HALF_UP
        )

    def reset_cycle_totals(self) -> None:
        self.current_cycle_credit = Decimal("0.00")
        self.current_cycle_debit = Decimal("0.00")

    def to_serializable(self) -> dict:
        return {
            "account_id": self.account_id,
            "group_id": self.group_id,
            "current_balance": decimal_to_str(self.current_balance),
            "current_cycle_credit": decimal_to_str(self.current_cycle_credit),
            "current_cycle_debit": decimal_to_str(self.current_cycle_debit),
        }


@dataclass(frozen=True)
class CardXrefRecord:
    account_id: str
    card_number: str

    @classmethod
    def from_dict(cls, data: dict) -> "CardXrefRecord":
        try:
            account_id = str(data["account_id"]).strip()
        except KeyError as exc:
            raise KeyError("Card XREF record missing 'account_id'") from exc

        card_number = str(data.get("card_number", "")).strip()
        return cls(account_id=account_id, card_number=card_number)


@dataclass(frozen=True)
class DisclosureGroupRecord:
    account_group_id: str
    transaction_type_code: str
    transaction_category_code: str
    interest_rate: Decimal

    @classmethod
    def from_dict(cls, data: dict) -> "DisclosureGroupRecord":
        try:
            group_id = str(data["account_group_id"]).strip() or DEFAULT_DISCLOSURE_GROUP
            tran_type = normalize_code(data["transaction_type_code"], 2)
            tran_cat = normalize_code(data["transaction_category_code"], 4)
        except KeyError as exc:
            raise KeyError("Disclosure group record missing required field") from exc

        interest_rate = decimal_from_value(data.get("interest_rate", "0.00"))
        return cls(
            account_group_id=group_id.upper(),
            transaction_type_code=tran_type,
            transaction_category_code=tran_cat,
            interest_rate=interest_rate,
        )


@dataclass
class TransactionRecord:
    transaction_id: str
    transaction_type_code: str
    transaction_category_code: str
    source: str
    description: str
    amount: Decimal
    merchant_id: str
    merchant_name: str
    merchant_city: str
    merchant_zip: str
    card_number: str
    originated_timestamp: str
    processed_timestamp: str

    def to_serializable(self) -> dict:
        return {
            "transaction_id": self.transaction_id,
            "transaction_type_code": self.transaction_type_code,
            "transaction_category_code": self.transaction_category_code,
            "source": self.source,
            "description": self.description,
            "amount": decimal_to_str(self.amount),
            "merchant_id": self.merchant_id,
            "merchant_name": self.merchant_name,
            "merchant_city": self.merchant_city,
            "merchant_zip": self.merchant_zip,
            "card_number": self.card_number,
            "originated_timestamp": self.originated_timestamp,
            "processed_timestamp": self.processed_timestamp,
        }


class InterestCalculator:
    """Encapsulates the modern equivalent of the COBOL processing loop."""

    def __init__(
        self,
        accounts: Dict[str, AccountRecord],
        card_xrefs: Dict[str, CardXrefRecord],
        disclosure_groups: Dict[Tuple[str, str, str], DisclosureGroupRecord],
    ) -> None:
        self.accounts = accounts
        self.card_xrefs = card_xrefs
        self.disclosure_groups = disclosure_groups
        self.transaction_sequence = 0
        self.modified_account_ids: Set[str] = set()
        self._missing_disclosure_keys: Set[Tuple[str, str, str]] = set()
        self._missing_xref_accounts: Set[str] = set()

    def process(
        self,
        balances: Sequence[TransactionCategoryBalance],
        process_date: str,
    ) -> List[TransactionRecord]:
        transactions: List[TransactionRecord] = []
        current_account_id: Optional[str] = None
        current_account: Optional[AccountRecord] = None
        accumulated_interest = Decimal("0.00")
        processed_records = 0

        for balance in balances:
            processed_records += 1
            if balance.account_id != current_account_id:
                if current_account is not None:
                    self._apply_account_updates(current_account, accumulated_interest)
                current_account_id = balance.account_id
                current_account = self.accounts.get(current_account_id)
                if current_account is None:
                    logging.warning("Account %s not found; skipping related balances", current_account_id)
                    accumulated_interest = Decimal("0.00")
                    continue
                accumulated_interest = Decimal("0.00")

            if current_account is None:
                continue

            interest_rate = self._resolve_interest_rate(
                current_account.group_id, balance.transaction_type_code, balance.transaction_category_code
            )
            if interest_rate is None or interest_rate == 0:
                continue

            monthly_interest = self._calculate_monthly_interest(balance.balance, interest_rate)
            if monthly_interest == Decimal("0.00"):
                continue

            accumulated_interest += monthly_interest
            card_number = self._lookup_card_number(balance.account_id)
            transaction = self._build_transaction(
                process_date=process_date,
                account_id=balance.account_id,
                amount=monthly_interest,
                card_number=card_number,
            )
            transactions.append(transaction)

        if current_account is not None:
            self._apply_account_updates(current_account, accumulated_interest)

        logging.info("Processed %s transaction-category balance records.", processed_records)
        logging.info("Generated %s interest transactions.", len(transactions))
        if self._missing_disclosure_keys:
            logging.warning("Missing disclosure entries for %s key(s).", len(self._missing_disclosure_keys))
        if self._missing_xref_accounts:
            logging.warning("Missing card cross-reference entries for %s account(s).", len(self._missing_xref_accounts))
        logging.info("Accounts updated: %s", len(self.modified_account_ids))
        return transactions

    def _apply_account_updates(self, account: AccountRecord, interest_amount: Decimal) -> None:
        account.apply_monthly_interest(interest_amount)
        account.reset_cycle_totals()
        self.modified_account_ids.add(account.account_id)

    def _resolve_interest_rate(
        self, group_id: str, tran_type: str, tran_cat: str
    ) -> Optional[Decimal]:
        key = (group_id.upper(), tran_type, tran_cat)
        record = self.disclosure_groups.get(key)
        if record:
            return record.interest_rate

        default_key = (DEFAULT_DISCLOSURE_GROUP, tran_type, tran_cat)
        default_record = self.disclosure_groups.get(default_key)
        if default_record:
            logging.debug(
                "Falling back to default disclosure group for acct group %s, type %s, cat %s.",
                group_id, tran_type, tran_cat,
            )
            return default_record.interest_rate

        if key not in self._missing_disclosure_keys:
            self._missing_disclosure_keys.add(key)
            logging.warning(
                "Disclosure group entry not found for acct group %s, type %s, cat %s.",
                group_id, tran_type, tran_cat,
            )
        return None

    @staticmethod
    def _calculate_monthly_interest(balance: Decimal, annual_rate: Decimal) -> Decimal:
        return (balance * annual_rate / Decimal("1200")).quantize(CENTS, rounding=ROUND_HALF_UP)

    def _lookup_card_number(self, account_id: str) -> str:
        record = self.card_xrefs.get(account_id)
        if record is None:
            if account_id not in self._missing_xref_accounts:
                self._missing_xref_accounts.add(account_id)
                logging.warning("Card number not found for account %s.", account_id)
            return ""
        return record.card_number

    def _build_transaction(self, process_date: str, account_id: str, amount: Decimal, card_number: str) -> TransactionRecord:
        self.transaction_sequence += 1
        transaction_id = f"{process_date}{self.transaction_sequence:06d}"
        timestamp = format_db2_timestamp()

        return TransactionRecord(
            transaction_id=transaction_id,
            transaction_type_code="01",
            transaction_category_code="05",
            source="System",
            description=f"Int. for a/c {account_id}",
            amount=amount,
            merchant_id="0",
            merchant_name="",
            merchant_city="",
            merchant_zip="",
            card_number=card_number,
            originated_timestamp=timestamp,
            processed_timestamp=timestamp,
        )


def load_json_records(path: Path) -> List[dict]:
    if not path.exists():
        raise FileNotFoundError(f"File not found: {path}")

    with path.open("r", encoding="utf-8") as handle:
        data = json.load(handle)

    if not isinstance(data, list):
        raise ValueError(f"Expected a JSON array in {path}")
    return data


def load_transaction_category_balances(path: Path) -> List[TransactionCategoryBalance]:
    records = [TransactionCategoryBalance.from_dict(item) for item in load_json_records(path)]
    records.sort(key=lambda rec: (rec.account_id, rec.transaction_type_code, rec.transaction_category_code))
    return records


def load_account_records(path: Path) -> Tuple[List[AccountRecord], Dict[str, AccountRecord]]:
    account_list = [AccountRecord.from_dict(item) for item in load_json_records(path)]
    account_index = {record.account_id: record for record in account_list}
    return account_list, account_index


def load_card_xrefs(path: Path) -> Dict[str, CardXrefRecord]:
    records = {}
    for item in load_json_records(path):
        record = CardXrefRecord.from_dict(item)
        records[record.account_id] = record
    return records


def load_disclosure_groups(path: Path) -> Dict[Tuple[str, str, str], DisclosureGroupRecord]:
    groups: Dict[Tuple[str, str, str], DisclosureGroupRecord] = {}
    for item in load_json_records(path):
        record = DisclosureGroupRecord.from_dict(item)
        key = (record.account_group_id.upper(), record.transaction_type_code, record.transaction_category_code)
        groups[key] = record
    return groups


def write_transactions(path: Path, transactions: Sequence[TransactionRecord]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = [transaction.to_serializable() for transaction in transactions]
    with path.open("w", encoding="utf-8") as handle:
        json.dump(payload, handle, indent=2)
    logging.info("Wrote %s transaction(s) to %s.", len(transactions), path)


def save_accounts(path: Path, accounts: Sequence[AccountRecord]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = [account.to_serializable() for account in accounts]
    with path.open("w", encoding="utf-8") as handle:
        json.dump(payload, handle, indent=2)
    logging.info("Persisted %s account record(s) to %s.", len(accounts), path)


def format_db2_timestamp(moment: Optional[datetime] = None) -> str:
    moment = moment or datetime.utcnow()
    milliseconds = moment.microsecond // 1000
    return moment.strftime("%Y-%m-%d-%H.%M.%S.") + f"{milliseconds:03d}0000"


def normalize_process_date(raw_value: str) -> str:
    digits = "".join(ch for ch in raw_value if ch.isdigit())
    if len(digits) != 8:
        raise ValueError(f"Process date must resolve to 8 digits (YYYYMMDD), got '{raw_value}'")
    return digits


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Modernized interest calculator (CBACT04C).")
    parser.add_argument("--tcatbal", type=Path, required=True, help="Path to transaction category balance JSON file.")
    parser.add_argument("--accounts", type=Path, required=True, help="Path to the account master JSON file.")
    parser.add_argument("--accounts-out", type=Path, help="Destination for the updated account master JSON.")
    parser.add_argument("--xref", type=Path, required=True, help="Path to the card cross reference JSON file.")
    parser.add_argument("--discgrp", type=Path, required=True, help="Path to the disclosure group JSON file.")
    parser.add_argument("--transactions-out", type=Path, required=True, help="Output path for generated transactions JSON.")
    parser.add_argument(
        "--process-date",
        default=datetime.utcnow().strftime("%Y%m%d"),
        help="Process date used for transaction IDs (format YYYYMMDD).",
    )
    parser.add_argument(
        "--log-level",
        default="INFO",
        help="Logging level (DEBUG, INFO, WARNING, ERROR). Default is INFO.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    logging.basicConfig(
        level=getattr(logging, args.log_level.upper(), logging.INFO),
        format="%(levelname)s: %(message)s",
    )

    process_date = normalize_process_date(args.process_date)

    tcat_balances = load_transaction_category_balances(args.tcatbal)
    account_records, account_index = load_account_records(args.accounts)
    card_xrefs = load_card_xrefs(args.xref)
    disclosure_groups = load_disclosure_groups(args.discgrp)

    calculator = InterestCalculator(account_index, card_xrefs, disclosure_groups)
    transactions = calculator.process(tcat_balances, process_date)

    accounts_destination = args.accounts_out or args.accounts
    if calculator.modified_account_ids or accounts_destination != args.accounts:
        save_accounts(accounts_destination, account_records)
    else:
        logging.info("No account modifications detected; original account file left unchanged.")

    write_transactions(args.transactions_out, transactions)


if __name__ == "__main__":
    main()
