"""Modernized version of the COACTVWC COBOL program.

This module keeps the spirit of the original account-view transaction that
ran in CICS while embracing a Pythonic design. The service validates an
incoming account identifier, looks up the related card, account, and customer
records, and returns a consolidated view that can be rendered as JSON or as
formatted plain text.

The script can operate on an external JSON data file with the following shape::

    {
        "card_xref": [
            {"account_id": "00000012345", "customer_id": "000000001", "card_number": "5555..."}
        ],
        "accounts": [
            {"account_id": "00000012345", "active_status": "ACTIVE", ...}
        ],
        "customers": [
            {"customer_id": "000000001", "ssn": "123456789", ...}
        ]
    }

If no data file is supplied, an in-memory sample dataset is used so that the
utility remains self-contained and easy to demo.
"""

from __future__ import annotations

import argparse
import json
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Dict, Iterable, Mapping, Optional, Sequence

ACCOUNT_ID_LENGTH = 11
CUSTOMER_ID_LENGTH = 9

ACCOUNT_ID_NOT_PROVIDED_MSG = "Account number not provided"
ACCOUNT_ID_INVALID_MSG = "Account number must be a non zero 11 digit number"
CARD_NOT_FOUND_MSG = "Did not find this account in account card xref file"
ACCOUNT_NOT_FOUND_MSG = "Did not find this account in account master file"
CUSTOMER_NOT_FOUND_MSG = "Did not find associated customer in master file"


class AccountViewError(Exception):
    """Base class for account view errors."""


class InvalidAccountIdError(AccountViewError):
    """Raised when the incoming account identifier fails validation."""


class CardAssociationNotFoundError(AccountViewError):
    """Raised when no card cross-reference entry exists for an account."""


class AccountNotFoundError(AccountViewError):
    """Raised when the account master does not contain the requested account."""


class CustomerNotFoundError(AccountViewError):
    """Raised when the associated customer cannot be located."""


def _clean(value: Any) -> str:
    """Normalize raw data into a trimmed string."""
    if value is None:
        return ""
    return str(value).strip()


def _normalize_digits(value: Any, length: int) -> str:
    """Return a zero-padded numeric string with the desired length."""
    cleaned = _clean(value)
    if not cleaned:
        return cleaned
    if cleaned.isdigit() and len(cleaned) < length:
        return cleaned.zfill(length)
    return cleaned


def _normalize_account_id_value(value: Any) -> str:
    return _normalize_digits(value, ACCOUNT_ID_LENGTH)


def _normalize_customer_id_value(value: Any) -> str:
    return _normalize_digits(value, CUSTOMER_ID_LENGTH)


def format_ssn(ssn: str) -> str:
    """Return an SSN formatted as XXX-XX-XXXX when possible."""
    digits = "".join(ch for ch in ssn if ch.isdigit())
    if len(digits) == 9:
        return f"{digits[:3]}-{digits[3:5]}-{digits[5:]}"
    return ssn


def mask_card_number(card_number: str) -> str:
    """Mask a card number, leaving only the last four digits visible."""
    digits = card_number.replace(" ", "")
    if len(digits) <= 4:
        return digits
    masked = "*" * (len(digits) - 4) + digits[-4:]
    return " ".join(masked[i : i + 4] for i in range(0, len(masked), 4)).strip()


@dataclass(frozen=True)
class CardXrefRecord:
    account_id: str
    customer_id: str
    card_number: str


@dataclass(frozen=True)
class AccountRecord:
    account_id: str
    active_status: str = ""
    current_balance: str = ""
    credit_limit: str = ""
    cash_credit_limit: str = ""
    current_cycle_credit: str = ""
    current_cycle_debit: str = ""
    open_date: str = ""
    expiration_date: str = ""
    reissue_date: str = ""
    group_id: str = ""


@dataclass(frozen=True)
class CustomerRecord:
    customer_id: str
    ssn: str = ""
    fico_score: str = ""
    date_of_birth: str = ""
    first_name: str = ""
    middle_name: str = ""
    last_name: str = ""
    address_line_1: str = ""
    address_line_2: str = ""
    city: str = ""
    state: str = ""
    postal_code: str = ""
    country_code: str = ""
    phone_number_1: str = ""
    phone_number_2: str = ""
    government_id: str = ""
    eft_account_id: str = ""
    primary_card_holder: str = ""


class CardXrefRepository:
    """In-memory repository for card cross-reference records."""

    def __init__(self, records: Iterable[Mapping[str, Any]]):
        self._records: Dict[str, CardXrefRecord] = {}
        for raw in records:
            try:
                account_id = _normalize_account_id_value(raw["account_id"])
                customer_id = _normalize_customer_id_value(raw["customer_id"])
            except KeyError as exc:
                raise AccountViewError(
                    f"Card cross-reference record is missing field {exc.args[0]!r}"
                ) from exc
            card_number = _clean(raw.get("card_number"))
            if not account_id:
                continue
            self._records[account_id] = CardXrefRecord(
                account_id=account_id,
                customer_id=customer_id,
                card_number=card_number,
            )

    def get_by_account_id(self, account_id: str) -> Optional[CardXrefRecord]:
        return self._records.get(account_id)


class AccountRepository:
    """In-memory repository for account master records."""

    def __init__(self, records: Iterable[Mapping[str, Any]]):
        self._records: Dict[str, AccountRecord] = {}
        for raw in records:
            try:
                account_id = _normalize_account_id_value(raw["account_id"])
            except KeyError as exc:
                raise AccountViewError(
                    f"Account record is missing field {exc.args[0]!r}"
                ) from exc
            self._records[account_id] = AccountRecord(
                account_id=account_id,
                active_status=_clean(raw.get("active_status")),
                current_balance=_clean(raw.get("current_balance")),
                credit_limit=_clean(raw.get("credit_limit")),
                cash_credit_limit=_clean(raw.get("cash_credit_limit")),
                current_cycle_credit=_clean(raw.get("current_cycle_credit")),
                current_cycle_debit=_clean(raw.get("current_cycle_debit")),
                open_date=_clean(raw.get("open_date")),
                expiration_date=_clean(raw.get("expiration_date")),
                reissue_date=_clean(raw.get("reissue_date")),
                group_id=_clean(raw.get("group_id")),
            )

    def get(self, account_id: str) -> Optional[AccountRecord]:
        return self._records.get(account_id)


class CustomerRepository:
    """In-memory repository for customer master records."""

    def __init__(self, records: Iterable[Mapping[str, Any]]):
        self._records: Dict[str, CustomerRecord] = {}
        for raw in records:
            try:
                customer_id = _normalize_customer_id_value(raw["customer_id"])
            except KeyError as exc:
                raise AccountViewError(
                    f"Customer record is missing field {exc.args[0]!r}"
                ) from exc
            self._records[customer_id] = CustomerRecord(
                customer_id=customer_id,
                ssn=_clean(raw.get("ssn")),
                fico_score=_clean(raw.get("fico_score")),
                date_of_birth=_clean(raw.get("date_of_birth")),
                first_name=_clean(raw.get("first_name")),
                middle_name=_clean(raw.get("middle_name")),
                last_name=_clean(raw.get("last_name")),
                address_line_1=_clean(raw.get("address_line_1")),
                address_line_2=_clean(raw.get("address_line_2")),
                city=_clean(raw.get("city")),
                state=_clean(raw.get("state")),
                postal_code=_clean(raw.get("postal_code")),
                country_code=_clean(raw.get("country_code")),
                phone_number_1=_clean(raw.get("phone_number_1")),
                phone_number_2=_clean(raw.get("phone_number_2")),
                government_id=_clean(raw.get("government_id")),
                eft_account_id=_clean(raw.get("eft_account_id")),
                primary_card_holder=_clean(raw.get("primary_card_holder")),
            )

    def get(self, customer_id: str) -> Optional[CustomerRecord]:
        return self._records.get(customer_id)


@dataclass(frozen=True)
class AccountView:
    """Composite view that matches what the COBOL screen displayed."""

    account: AccountRecord
    card: CardXrefRecord
    customer: CustomerRecord

    def as_dict(self) -> Dict[str, Any]:
        account_data = asdict(self.account)
        customer_data = asdict(self.customer)
        customer_data["ssn_formatted"] = format_ssn(self.customer.ssn)
        card_data = asdict(self.card)
        card_data["card_number_masked"] = mask_card_number(self.card.card_number)
        return {
            "account": account_data,
            "customer": customer_data,
            "card": card_data,
        }


class AccountViewService:
    """Co-ordinates validation and retrieval of the account view."""

    def __init__(
        self,
        card_repo: CardXrefRepository,
        account_repo: AccountRepository,
        customer_repo: CustomerRepository,
    ):
        self._card_repo = card_repo
        self._account_repo = account_repo
        self._customer_repo = customer_repo

    def fetch_account_view(self, requested_account_id: str) -> AccountView:
        account_id = self._sanitize_requested_account_id(requested_account_id)
        card_link = self._card_repo.get_by_account_id(account_id)
        if card_link is None:
            raise CardAssociationNotFoundError(CARD_NOT_FOUND_MSG)
        account = self._account_repo.get(account_id)
        if account is None:
            raise AccountNotFoundError(ACCOUNT_NOT_FOUND_MSG)
        customer = self._customer_repo.get(card_link.customer_id)
        if customer is None:
            raise CustomerNotFoundError(CUSTOMER_NOT_FOUND_MSG)
        return AccountView(account=account, card=card_link, customer=customer)

    @staticmethod
    def _sanitize_requested_account_id(account_id: Optional[str]) -> str:
        cleaned = _clean(account_id)
        if not cleaned or cleaned == "*":
            raise InvalidAccountIdError(ACCOUNT_ID_NOT_PROVIDED_MSG)
        if (
            len(cleaned) != ACCOUNT_ID_LENGTH
            or not cleaned.isdigit()
            or set(cleaned) == {"0"}
        ):
            raise InvalidAccountIdError(ACCOUNT_ID_INVALID_MSG)
        return cleaned


def format_account_view_text(view: AccountView) -> str:
    """Render a human-friendly multi-line summary."""
    account = view.account
    customer = view.customer
    card = view.card

    address_lines = [
        line
        for line in (
            customer.address_line_1,
            customer.address_line_2,
            " ".join(
                part
                for part in (customer.city, customer.state, customer.postal_code)
                if part
            ).strip(),
            customer.country_code,
        )
        if line
    ]
    phones = ", ".join(
        phone for phone in (customer.phone_number_1, customer.phone_number_2) if phone
    )

    lines = [
        f"Account {account.account_id} (card {mask_card_number(card.card_number)})",
        f"  Status: {account.active_status or 'UNKNOWN'}",
        f"  Current balance: {account.current_balance or 'N/A'}",
        f"  Credit limit: {account.credit_limit or 'N/A'}",
        f"  Cash limit: {account.cash_credit_limit or 'N/A'}",
        f"  Cycle credit/debit: {account.current_cycle_credit or '0'} / {account.current_cycle_debit or '0'}",
        f"  Open date: {account.open_date or 'N/A'}",
        f"  Expires: {account.expiration_date or 'N/A'}",
        f"  Reissue date: {account.reissue_date or 'N/A'}",
        f"Customer {customer.customer_id}",
        f"  Name: {' '.join(part for part in (customer.first_name, customer.middle_name, customer.last_name) if part) or 'N/A'}",
        f"  SSN: {format_ssn(customer.ssn) or 'N/A'}   FICO: {customer.fico_score or 'N/A'}",
        f"  DOB: {customer.date_of_birth or 'N/A'}",
    ]

    if address_lines:
        lines.append("  Address:")
        lines.extend(f"    {line}" for line in address_lines)

    if phones:
        lines.append(f"  Phones: {phones}")

    lines.append(f"  Primary card holder: {customer.primary_card_holder or 'N/A'}")
    lines.append(f"  Govt ID: {customer.government_id or 'N/A'}")
    lines.append(f"  EFT Account: {customer.eft_account_id or 'N/A'}")
    return "\n".join(lines)


def load_raw_data(data_path: Optional[Path]) -> Dict[str, Sequence[Mapping[str, Any]]]:
    """Load JSON data from disk or return the built-in sample dataset."""
    if data_path is None:
        return SAMPLE_DATA

    resolved_path = data_path.expanduser()
    try:
        with resolved_path.open("r", encoding="utf-8") as handle:
            content = json.load(handle)
    except FileNotFoundError as exc:
        raise AccountViewError(f"Data file '{resolved_path}' was not found") from exc
    except json.JSONDecodeError as exc:
        raise AccountViewError(
            f"Data file '{resolved_path}' is not valid JSON: {exc}"
        ) from exc

    for key in ("card_xref", "accounts", "customers"):
        if key not in content:
            raise AccountViewError(
                f"Data file '{resolved_path}' is missing top-level key '{key}'"
            )

    return content


def build_account_view_service(data_path: Optional[Path]) -> AccountViewService:
    """Factory that wires repositories together from the configured data source."""
    raw_data = load_raw_data(data_path)
    return AccountViewService(
        card_repo=CardXrefRepository(raw_data["card_xref"]),
        account_repo=AccountRepository(raw_data["accounts"]),
        customer_repo=CustomerRepository(raw_data["customers"]),
    )


def build_argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Display account and customer details for a given account id."
    )
    parser.add_argument(
        "account_id",
        help="11 digit account identifier (e.g. 00000012345)",
    )
    parser.add_argument(
        "--data",
        type=Path,
        help="Optional path to a JSON data file (defaults to built-in sample data).",
    )
    parser.add_argument(
        "--format",
        dest="output_format",
        choices=("json", "text"),
        default="json",
        help="Choose between machine-readable JSON or plain text output (default: json).",
    )
    return parser


def main(argv: Optional[Sequence[str]] = None) -> None:
    parser = build_argument_parser()
    args = parser.parse_args(argv)

    try:
        service = build_account_view_service(args.data)
        account_view = service.fetch_account_view(args.account_id)
    except AccountViewError as error:
        parser.exit(status=1, message=f"{error}\n")

    if args.output_format == "json":
        print(json.dumps(account_view.as_dict(), indent=2, ensure_ascii=False))
    else:
        print(format_account_view_text(account_view))


SAMPLE_DATA: Dict[str, Sequence[Dict[str, str]]] = {
    "card_xref": [
        {
            "account_id": "00000012345",
            "customer_id": "000000001",
            "card_number": "5555444433332222",
        }
    ],
    "accounts": [
        {
            "account_id": "00000012345",
            "active_status": "ACTIVE",
            "current_balance": "1520.55",
            "credit_limit": "5000",
            "cash_credit_limit": "1000",
            "current_cycle_credit": "320.00",
            "current_cycle_debit": "125.00",
            "open_date": "2018-02-15",
            "expiration_date": "2026-02-28",
            "reissue_date": "2025-12-10",
            "group_id": "GRP001",
        }
    ],
    "customers": [
        {
            "customer_id": "000000001",
            "ssn": "123456789",
            "fico_score": "750",
            "date_of_birth": "1980-09-05",
            "first_name": "Jamie",
            "middle_name": "Lee",
            "last_name": "Taylor",
            "address_line_1": "1234 Main St",
            "address_line_2": "Suite 900",
            "city": "Seattle",
            "state": "WA",
            "postal_code": "98104",
            "country_code": "US",
            "phone_number_1": "+1-206-555-0100",
            "phone_number_2": "+1-206-555-0101",
            "government_id": "DL-SEA-555555",
            "eft_account_id": "US1234567890",
            "primary_card_holder": "Y",
        }
    ],
}


if __name__ == "__main__":
    main()
