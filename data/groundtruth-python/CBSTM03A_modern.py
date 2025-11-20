#!/usr/bin/env python3
"""
Modernized Python implementation of the CBSTM03A COBOL program.

The original COBOL batch program produced customer account statements (both
plain text and HTML) by combining cross-reference, customer, account, and
transaction datasets that were accessed through the CBSTM03B helper program.
This Python module delivers equivalent business functionality while embracing
modern language features such as dataclasses, pathlib, argparse, and Decimal.

Sample usage:

    python CBSTM03A_modern.py \
        --transactions data/transactions.json \
        --xref data/card_xref.json \
        --customers data/customers.json \
        --accounts data/accounts.json \
        --output-dir build/statements

The script expects JSON, JSON Lines, CSV, or pipe-delimited text files in which
the column names roughly match the COBOL field names (hyphens/underscores and
letter casing are ignored).  See the module documentation for additional
details on the expected schema.
"""

from __future__ import annotations

import argparse
import csv
import json
import sys
from collections import defaultdict
from dataclasses import dataclass
from decimal import Decimal, InvalidOperation
from pathlib import Path
from typing import Any, Dict, Iterable, List, Mapping, MutableMapping, Sequence


# ---------------------------------------------------------------------------
# Data model
# ---------------------------------------------------------------------------


@dataclass(slots=True)
class CardXRef:
    """Cross-reference between a card number, a customer, and an account."""
    card_number: str
    customer_id: str
    account_id: str


@dataclass(slots=True)
class Customer:
    """Customer master record."""
    customer_id: str
    first_name: str = ""
    middle_name: str = ""
    last_name: str = ""
    address_line1: str = ""
    address_line2: str = ""
    address_line3: str = ""
    state: str = ""
    country: str = ""
    postal_code: str = ""
    fico_score: str = ""

    @property
    def full_name(self) -> str:
        return " ".join(
            part.strip()
            for part in (self.first_name, self.middle_name, self.last_name)
            if part.strip()
        ).strip()

    @property
    def address_lines(self) -> List[str]:
        lines = [self.address_line1, self.address_line2]
        city_line = " ".join(
            part.strip()
            for part in (
                self.address_line3,
                self.state,
                self.country,
                self.postal_code,
            )
            if part.strip()
        )
        if city_line:
            lines.append(city_line)
        return [line for line in lines if line.strip()]


@dataclass(slots=True)
class Account:
    """Account master record."""
    account_id: str
    current_balance: Decimal = Decimal("0")


@dataclass(slots=True)
class Transaction:
    """Credit card transaction."""
    card_number: str
    transaction_id: str
    description: str
    amount: Decimal
    transaction_date: str | None = None


@dataclass(slots=True)
class StatementResult:
    """Container for generated statement artifacts."""
    card_number: str
    customer: Customer
    account: Account
    transactions: List[Transaction]
    total_amount: Decimal
    text_document: str
    html_fragment: str


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def normalize_key(key: str) -> str:
    """Normalize column names by removing separators and forcing lowercase."""
    return "".join(ch for ch in key.lower() if ch.isalnum())


def fetch_field(
    record: Mapping[str, Any],
    *candidates: str,
    default: str = "",
) -> str:
    """
    Retrieve a value from a record using one or more fallback keys while
    ignoring differences in case, dashes, and underscores.
    """
    normalized_map: Dict[str, Any] = {}
    for raw_key, value in record.items():
        if raw_key is None:
            continue
        nkey = normalize_key(str(raw_key))
        if nkey not in normalized_map:
            normalized_map[nkey] = value

    for candidate in candidates:
        candidate_key = normalize_key(candidate)
        if candidate_key in normalized_map:
            value = normalized_map[candidate_key]
            if value is None:
                continue
            text = str(value).strip()
            if text:
                return text
    return default


def parse_decimal(value: Any) -> Decimal:
    """Convert raw values (strings, ints, floats) into Decimal."""
    if isinstance(value, Decimal):
        return value

    if value is None:
        return Decimal("0")

    if isinstance(value, (int, float)):
        return Decimal(str(value))

    text = str(value).strip()
    if not text:
        return Decimal("0")

    normalized = (
        text.replace(",", "")
        .replace("$", "")
        .replace("+", "")
        .strip()
    )
    # Handle COBOL's trailing minus format.
    if normalized.endswith("-"):
        normalized = "-" + normalized[:-1]

    try:
        return Decimal(normalized)
    except InvalidOperation as exc:
        raise ValueError(f"Unable to parse decimal from {value!r}") from exc


def detect_delimiter(sample_line: str) -> str:
    """Heuristically detect a reasonable delimiter for text files."""
    for delimiter in (",", "|", "\t", ";"):
        if delimiter in sample_line:
            return delimiter
    return ","


def load_structured_records(path: Path, *, encoding: str = "utf-8") -> List[Dict[str, Any]]:
    """
    Load data from JSON, JSON Lines, CSV, or pipe-delimited files and return a
    list of dictionaries suitable for conversion into domain objects.
    """
    suffix = path.suffix.lower()
    if suffix in {".json", ".jsn"}:
        with path.open("r", encoding=encoding) as handle:
            payload = json.load(handle)
        if isinstance(payload, list):
            return payload
        if isinstance(payload, dict):
            for key in ("records", "data", "items"):
                if key in payload and isinstance(payload[key], list):
                    return payload[key]
        raise ValueError(f"Unsupported JSON structure in {path}")
    if suffix in {".jsonl", ".ndjson"}:
        records: List[Dict[str, Any]] = []
        with path.open("r", encoding=encoding) as handle:
            for line in handle:
                line = line.strip()
                if line:
                    records.append(json.loads(line))
        return records
    if suffix in {".csv", ".txt", ".dat"}:
        with path.open("r", newline="", encoding=encoding) as handle:
            first_line = handle.readline()
            handle.seek(0)
            delimiter = detect_delimiter(first_line)
            reader = csv.DictReader(handle, delimiter=delimiter)
            return [
                {key: value for key, value in row.items() if key is not None}
                for row in reader
            ]
    raise ValueError(f"Unsupported file type for {path}")


def load_card_xrefs(path: Path, *, encoding: str) -> List[CardXRef]:
    records = load_structured_records(path, encoding=encoding)
    xrefs: List[CardXRef] = []
    for record in records:
        card_number = fetch_field(record, "card_number", "card_num", "xref_card_num")
        if not card_number:
            continue
        customer_id = fetch_field(record, "customer_id", "cust_id", "xref_cust_id")
        account_id = fetch_field(record, "account_id", "acct_id", "xref_acct_id")
        if not customer_id or not account_id:
            raise ValueError(
                f"Card {card_number} is missing customer/account linkage in {path}"
            )
        xrefs.append(
            CardXRef(
                card_number=card_number,
                customer_id=customer_id,
                account_id=account_id,
            )
        )
    return xrefs


def load_customers(path: Path, *, encoding: str) -> Dict[str, Customer]:
    records = load_structured_records(path, encoding=encoding)
    customers: Dict[str, Customer] = {}
    for record in records:
        customer_id = fetch_field(record, "customer_id", "cust_id", "customer")
        if not customer_id:
            continue
        customers[customer_id] = Customer(
            customer_id=customer_id,
            first_name=fetch_field(record, "first_name", "cust_first_name"),
            middle_name=fetch_field(record, "middle_name", "cust_middle_name"),
            last_name=fetch_field(record, "last_name", "cust_last_name"),
            address_line1=fetch_field(record, "address_line1", "cust_addr_line_1"),
            address_line2=fetch_field(record, "address_line2", "cust_addr_line_2"),
            address_line3=fetch_field(record, "address_line3", "cust_addr_line_3"),
            state=fetch_field(record, "state", "cust_addr_state_cd"),
            country=fetch_field(record, "country", "cust_addr_country_cd"),
            postal_code=fetch_field(record, "postal_code", "cust_addr_zip"),
            fico_score=fetch_field(record, "fico_score", "cust_fico_credit_score"),
        )
    return customers


def load_accounts(path: Path, *, encoding: str) -> Dict[str, Account]:
    records = load_structured_records(path, encoding=encoding)
    accounts: Dict[str, Account] = {}
    for record in records:
        account_id = fetch_field(record, "account_id", "acct_id", "account")
        if not account_id:
            continue
        balance_raw = fetch_field(
            record, "current_balance", "acct_curr_bal", "balance", default="0"
        )
        accounts[account_id] = Account(
            account_id=account_id,
            current_balance=parse_decimal(balance_raw),
        )
    return accounts


def load_transactions(path: Path, *, encoding: str) -> List[Transaction]:
    records = load_structured_records(path, encoding=encoding)
    transactions: List[Transaction] = []
    for record in records:
        card_number = fetch_field(record, "card_number", "card_num", "trnx_card_num")
        transaction_id = fetch_field(record, "transaction_id", "tran_id", "trnx_id")
        if not card_number or not transaction_id:
            continue
        description = fetch_field(
            record,
            "description",
            "transaction_description",
            "details",
            "tran_details",
            "trnx_desc",
            default="Transaction",
        )
        amount_raw = fetch_field(
            record,
            "amount",
            "transaction_amount",
            "tran_amount",
            "trnx_amt",
            default="0",
        )
        transaction_date = fetch_field(
            record,
            "transaction_date",
            "tran_date",
            "trnx_date",
            default="",
        )
        transactions.append(
            Transaction(
                card_number=card_number,
                transaction_id=transaction_id,
                description=description,
                amount=parse_decimal(amount_raw),
                transaction_date=transaction_date or None,
            )
        )
    return transactions


def format_currency(amount: Decimal) -> str:
    quantized = amount.quantize(Decimal("0.01"))
    return f"{quantized:,.2f}"


def truncate(text: str, width: int) -> str:
    text = text.strip()
    if len(text) <= width:
        return text
    return text[: max(0, width - 1)].rstrip() + "…"  # ellipsis


# ---------------------------------------------------------------------------
# Statement generator
# ---------------------------------------------------------------------------


class StatementGenerator:
    """Core engine that assembles statements for each card."""

    def __init__(
        self,
        *,
        card_xrefs: Sequence[CardXRef],
        customers: Mapping[str, Customer],
        accounts: Mapping[str, Account],
        transactions: Sequence[Transaction],
    ) -> None:
        self._card_xrefs = sorted(card_xrefs, key=lambda x: x.card_number)
        self._customers = customers
        self._accounts = accounts
        self._transactions_by_card = self._group_transactions(transactions)

    @staticmethod
    def _group_transactions(
        transactions: Sequence[Transaction],
    ) -> Dict[str, List[Transaction]]:
        grouped: Dict[str, List[Transaction]] = defaultdict(list)
        for txn in transactions:
            grouped[txn.card_number].append(txn)
        for txns in grouped.values():
            txns.sort(key=lambda t: t.transaction_id)
        return grouped

    def generate(self) -> List[StatementResult]:
        results: List[StatementResult] = []
        for xref in self._card_xrefs:
            customer = self._customers.get(xref.customer_id)
            account = self._accounts.get(xref.account_id)
            if not customer or not account:
                raise KeyError(
                    f"Missing customer/account data for card {xref.card_number}"
                )
            transactions = list(self._transactions_by_card.get(xref.card_number, []))
            total_amount = sum((txn.amount for txn in transactions), Decimal("0"))
            text_doc = self._render_text_statement(
                customer=customer,
                account=account,
                transactions=transactions,
                total_amount=total_amount,
            )
            html_fragment = self._render_html_statement(
                card_number=xref.card_number,
                customer=customer,
                account=account,
                transactions=transactions,
                total_amount=total_amount,
            )
            results.append(
                StatementResult(
                    card_number=xref.card_number,
                    customer=customer,
                    account=account,
                    transactions=transactions,
                    total_amount=total_amount,
                    text_document=text_doc,
                    html_fragment=html_fragment,
                )
            )
        return results

    def _render_text_statement(
        self,
        *,
        customer: Customer,
        account: Account,
        transactions: Sequence[Transaction],
        total_amount: Decimal,
    ) -> str:
        lines: List[str] = []
        lines.append("*" * 31 + "START OF STATEMENT".center(18) + "*" * 31)
        name_line = customer.full_name or "Unknown Customer"
        lines.append(name_line.ljust(80))
        address_lines = customer.address_lines or ["Address unavailable"]
        for addr in address_lines:
            lines.append(addr.ljust(80))
        while len(address_lines) < 2:
            lines.append(" ".ljust(80))
            address_lines += [""]

        lines.append("-" * 80)
        lines.append(" " * 33 + "Basic Details" + " " * 33)
        lines.append("-" * 80)
        lines.append(f"Account ID         : {account.account_id:<20}")
        lines.append(
            f"Current Balance    : ${format_currency(account.current_balance):>13}"
        )
        fico = customer.fico_score or "N/A"
        lines.append(f"FICO Score         : {fico}")
        lines.append("-" * 80)
        lines.append(" " * 30 + "TRANSACTION SUMMARY" + " " * 30)
        lines.append("-" * 80)
        lines.append("Tran ID         Tran Details                                    Tran Amount")
        if not transactions:
            lines.append(" " * 5 + "No transactions recorded for this cycle.")
        else:
            for txn in transactions:
                description = truncate(txn.description, 49)
                lines.append(
                    f"{txn.transaction_id:<16} "
                    f"{description:<49} "
                    f"${format_currency(txn.amount):>13}"
                )
        lines.append("-" * 80)
        lines.append(
            f"Total EXP:".ljust(20)
            + " " * 46
            + f"${format_currency(total_amount):>13}"
        )
        lines.append("*" * 32 + "END OF STATEMENT".center(16) + "*" * 32)
        return "\n".join(lines)

    def _render_html_statement(
        self,
        *,
        card_number: str,
        customer: Customer,
        account: Account,
        transactions: Sequence[Transaction],
        total_amount: Decimal,
    ) -> str:
        transaction_rows: List[str] = []
        if transactions:
            for txn in transactions:
                txn_date = txn.transaction_date or ""
                date_markup = f"<div class='tran-date'>{txn_date}</div>" if txn_date else ""
                transaction_rows.append(
                    "            <tr>"
                    f"<td>{txn.transaction_id}</td>"
                    f"<td><div class='tran-desc'>{txn.description}</div>{date_markup}</td>"
                    f"<td class='amount'>${format_currency(txn.amount)}</td>"
                    "</tr>"
                )
        else:
            transaction_rows.append(
                "            <tr><td colspan='3' class='empty'>No transactions found</td></tr>"
            )

        transactions_html = "\n".join(transaction_rows)
        address_html = "".join(
            f"<p>{line}</p>" for line in (customer.address_lines or ["Address unavailable"])
        )
        fico_score = customer.fico_score or "N/A"

        return f"""
<section class="statement">
  <header>
    <h2>Statement for Account {account.account_id}</h2>
    <p class="card-number">Card Number: {card_number}</p>
    <div class="customer">
      <p class="customer-name">{customer.full_name or "Unknown Customer"}</p>
      {address_html}
    </div>
  </header>
  <div class="summary">
    <div><strong>Account ID:</strong> {account.account_id}</div>
    <div><strong>Current Balance:</strong> ${format_currency(account.current_balance)}</div>
    <div><strong>FICO Score:</strong> {fico_score}</div>
  </div>
  <table class="transactions">
    <thead>
      <tr>
        <th>Tran ID</th>
        <th>Tran Details</th>
        <th class="amount">Amount</th>
      </tr>
    </thead>
    <tbody>
{transactions_html}
    </tbody>
    <tfoot>
      <tr>
        <td colspan="2" class="total-label">Total EXP</td>
        <td class="amount">${format_currency(total_amount)}</td>
      </tr>
    </tfoot>
  </table>
</section>
""".strip()


# ---------------------------------------------------------------------------
# Output writers
# ---------------------------------------------------------------------------


def write_text_output(
    statements: Sequence[StatementResult],
    path: Path,
    *,
    encoding: str = "utf-8",
) -> None:
    if not statements:
        path.write_text("", encoding=encoding)
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = "\n\n".join(result.text_document for result in statements)
    path.write_text(payload + "\n", encoding=encoding)


def write_html_output(
    statements: Sequence[StatementResult],
    path: Path,
    *,
    encoding: str = "utf-8",
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    sections = "\n\n".join(result.html_fragment for result in statements)
    html_document = f"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <title>Account Statements</title>
  <style>
    body {{
      font-family: "Segoe UI", Arial, sans-serif;
      margin: 0;
      padding: 1.5rem;
      background: #f5f7fb;
      color: #1a1a1a;
    }}
    .statement {{
      background: #ffffff;
      border: 1px solid #d9e1ec;
      border-radius: 8px;
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.05);
      padding: 1.5rem;
      margin-bottom: 2rem;
    }}
    .statement header {{
      border-bottom: 2px solid #1d1d96b3;
      margin-bottom: 1rem;
      padding-bottom: 1rem;
    }}
    .customer-name {{
      font-size: 1.1rem;
      font-weight: 600;
    }}
    .summary {{
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
      gap: 0.75rem;
      margin-bottom: 1rem;
    }}
    .summary div {{
      background: #f2f5ff;
      border-radius: 6px;
      padding: 0.75rem;
    }}
    .transactions {{
      width: 100%;
      border-collapse: collapse;
    }}
    .transactions th {{
      background: #33FF5E;
      text-align: left;
      padding: 0.5rem;
    }}
    .transactions td {{
      border-bottom: 1px solid #e1e7f0;
      padding: 0.5rem;
      vertical-align: top;
    }}
    .transactions td.amount {{
      text-align: right;
    }}
    .transactions tr:nth-child(even) {{
      background: #f8fafc;
    }}
    .transactions .empty {{
      text-align: center;
      font-style: italic;
      color: #6b7280;
    }}
    .total-label {{
      text-align: right;
      font-weight: 600;
    }}
  </style>
</head>
<body>
{sections}
</body>
</html>
"""
    path.write_text(html_document, encoding=encoding)


# ---------------------------------------------------------------------------
# Command-line interface
# ---------------------------------------------------------------------------


def parse_args(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Generate customer statements from transactional data.",
    )
    parser.add_argument("--transactions", type=Path, required=True, help="Path to the transaction dataset (JSON/CSV).")
    parser.add_argument("--xref", type=Path, required=True, help="Path to the card cross-reference dataset.")
    parser.add_argument("--customers", type=Path, required=True, help="Path to the customer dataset.")
    parser.add_argument("--accounts", type=Path, required=True, help="Path to the account dataset.")
    parser.add_argument("--output-dir", type=Path, default=Path("."), help="Directory to store the generated outputs.")
    parser.add_argument("--text-output", type=Path, default=None, help="Optional custom path for the text statement file.")
    parser.add_argument("--html-output", type=Path, default=None, help="Optional custom path for the HTML statement file.")
    parser.add_argument("--encoding", default="utf-8", help="Character encoding for all I/O operations.")
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_args(argv)

    try:
        card_xrefs = load_card_xrefs(args.xref, encoding=args.encoding)
        customers = load_customers(args.customers, encoding=args.encoding)
        accounts = load_accounts(args.accounts, encoding=args.encoding)
        transactions = load_transactions(args.transactions, encoding=args.encoding)
    except (OSError, ValueError, KeyError) as exc:
        print(f"Error loading input data: {exc}", file=sys.stderr)
        return 1

    generator = StatementGenerator(
        card_xrefs=card_xrefs,
        customers=customers,
        accounts=accounts,
        transactions=transactions,
    )

    try:
        statements = generator.generate()
    except KeyError as exc:
        print(f"Data integrity error: {exc}", file=sys.stderr)
        return 1

    if not statements:
        print("No statements were generated because no cards were found.", file=sys.stderr)
        return 0

    text_path = args.text_output or args.output_dir / "statements.txt"
    html_path = args.html_output or args.output_dir / "statements.html"

    write_text_output(statements, text_path, encoding=args.encoding)
    write_html_output(statements, html_path, encoding=args.encoding)

    print(f"Wrote {len(statements)} statement(s) to:")
    print(f"  - {text_path}")
    print(f"  - {html_path}")
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
