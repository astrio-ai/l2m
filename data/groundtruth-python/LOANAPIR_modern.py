"""Modern Python translation of the LOANAPIR COBOL CICS application.

The original LOANAPIR program collected loan application data from a 3270
screen, invoked an API through the BAQ communication stub, and rendered the
decision along with up to ten explanatory messages.  This module offers a
command-line workflow that replicates the same responsibilities:

* Collect applicant details (name, credit score, yearly income, age, loan
  amount, yearly repayment expectation).
* Call a modern HTTP-based API endpoint.
* Render the approval decision, UID, and any explanatory messages returned by
  the service.

Usage example::

    python data/ibm-wsc-zCONNEE-Wildfire-Workshop/LOANAPIR_modern.py \
        --name "Ada Lovelace" \
        --credit-score 780 \
        --yearly-income 125000 \
        --age 37 \
        --amount 35000 \
        --yearly-repayment 9000

Set the LOAN_API_URL environment variable to override the default endpoint.
"""

from __future__ import annotations

import argparse
import json
import logging
import os
import sys
from collections.abc import Sequence
from dataclasses import dataclass, field
from decimal import Decimal, InvalidOperation
from typing import Any, Optional

try:
    import requests
except ImportError as exc:  # pragma: no cover - handled immediately
    raise SystemExit(
        "The 'requests' package is required to run LOANAPIR_modern. "
        "Install it with `pip install requests`."
    ) from exc


DEFAULT_BASE_URL = os.getenv("LOAN_API_URL", "http://localhost:9080/loanapir")
DEFAULT_TIMEOUT = float(os.getenv("LOAN_API_TIMEOUT", "15"))
MAX_MESSAGES = 10


class LoanApiError(RuntimeError):
    """Represents an error returned by, or while calling, the Loan API."""

    def __init__(self, *, origin: str, status_code: int, message: str) -> None:
        super().__init__(f"[{origin}] status={status_code} message={message}")
        self.origin = origin
        self.status_code = status_code
        self.message = message


@dataclass
class LoanApplication:
    name: str
    credit_score: int
    yearly_income: Decimal
    age: int
    amount: Decimal
    yearly_repayment: Decimal

    def validate(self) -> None:
        if not self.name.strip():
            raise ValueError("The applicant name cannot be blank.")

        if not 0 < self.credit_score <= 999:
            raise ValueError("Credit score must be between 1 and 999.")

        if self.yearly_income <= 0:
            raise ValueError("Yearly income must be greater than zero.")

        if not 18 <= self.age <= 120:
            raise ValueError("Age must be between 18 and 120.")

        if self.amount <= 0:
            raise ValueError("Loan amount must be greater than zero.")

        if self.yearly_repayment <= 0:
            raise ValueError("Yearly repayment must be greater than zero.")

    def to_payload(self) -> dict[str, Any]:
        """Return a dict shaped like the original POST-REQUEST structure."""
        return {
            "name": self.name,
            "creditScore": self.credit_score,
            "yearlyIncome": _decimal_to_api(self.yearly_income),
            "age": self.age,
            "amount": _decimal_to_api(self.amount),
            "yearlyRepayment": _decimal_to_api(self.yearly_repayment),
            "miniLoanCommAreaNum": 1,
            "nameNum": 1,
            "creditScoreNum": 1,
            "yearlyIncomeNum": 1,
            "ageNum": 1,
            "amountNum": 1,
            "yearlyRepaymentNum": 1,
        }


@dataclass
class LoanDecision:
    approved: bool
    effect_date: Optional[str]
    yearly_interest_rate: Optional[Decimal]
    yearly_repayment: Optional[Decimal]
    uid: str
    messages: list[str] = field(default_factory=list)

    @classmethod
    def from_payload(cls, payload: dict[str, Any]) -> "LoanDecision":
        messages = _coerce_messages(payload.get("messages"))
        return cls(
            approved=_to_bool(payload.get("approved")),
            effect_date=_coerce_str(payload.get("effectDate")),
            yearly_interest_rate=_decimal_or_none(payload.get("yearlyInterestRate")),
            yearly_repayment=_decimal_or_none(payload.get("yearlyRepayment")),
            uid=_coerce_str(payload.get("uid")) or "",
            messages=messages[:MAX_MESSAGES],
        )

    def render(self) -> str:
        """Return a multi-line summary similar to the original BMS map."""
        lines = []
        status = "Loan approved" if self.approved else "Loan not approved"
        lines.append(status)

        if self.uid:
            lines.append(f"UID: {self.uid}")

        if self.effect_date:
            lines.append(f"Effect date: {self.effect_date}")

        if self.yearly_interest_rate is not None:
            lines.append(f"Yearly interest rate: {self.yearly_interest_rate}%")

        if self.yearly_repayment is not None:
            lines.append(f"Yearly repayment: {self.yearly_repayment}")

        if self.messages:
            lines.append("Messages:")
            for idx, msg in enumerate(self.messages, 1):
                lines.append(f"  {idx:>2}. {msg}")

        return "\n".join(lines)


class LoanApiClient:
    """HTTP client that emulates the COBOL BAQ communication stub."""

    def __init__(self, *, base_url: str, timeout: float = DEFAULT_TIMEOUT) -> None:
        if not base_url:
            raise ValueError("base_url must be provided.")
        self.base_url = base_url.rstrip("/")
        self.timeout = timeout
        self._session = requests.Session()

    def submit_application(self, application: LoanApplication) -> LoanDecision:
        payload = application.to_payload()
        try:
            response = self._session.post(
                self.base_url,
                json=payload,
                timeout=self.timeout,
                headers={"Content-Type": "application/json"},
            )
        except requests.RequestException as exc:
            raise LoanApiError(
                origin="STUB",
                status_code=-1,
                message=str(exc),
            ) from exc

        if response.status_code >= 400:
            snippet = _truncate(response.text.strip(), 540)
            raise LoanApiError(
                origin="API",
                status_code=response.status_code,
                message=snippet or "Loan API returned an error status.",
            )

        try:
            data = response.json()
        except ValueError as exc:
            raise LoanApiError(
                origin="API",
                status_code=response.status_code,
                message="Loan API returned invalid JSON.",
            ) from exc

        return LoanDecision.from_payload(data)


def _decimal_or_none(value: Any) -> Optional[Decimal]:
    if value is None or value == "":
        return None
    try:
        return Decimal(str(value))
    except (InvalidOperation, TypeError) as exc:
        raise LoanApiError(origin="API", status_code=-1, message=f"Invalid decimal value '{value}'.") from exc


def _decimal_to_api(value: Decimal) -> str:
    return format(value, "f")


def _coerce_messages(messages: Any) -> list[str]:
    if messages is None:
        return []
    if isinstance(messages, str):
        return [messages]
    if isinstance(messages, Sequence):
        return [str(msg) for msg in messages if msg is not None]
    return [str(messages)]


def _coerce_str(value: Any) -> Optional[str]:
    if value is None:
        return None
    stringified = str(value).strip()
    return stringified or None


def _to_bool(value: Any) -> bool:
    if isinstance(value, bool):
        return value
    if isinstance(value, str):
        return value.strip().upper() in {"T", "TRUE", "Y", "YES", "1"}
    return bool(value)


def _truncate(value: str, size: int) -> str:
    if len(value) <= size:
        return value
    return value[: size - 3] + "..."


def _decimal_argument(text: str) -> Decimal:
    try:
        return Decimal(text)
    except InvalidOperation as exc:
        raise argparse.ArgumentTypeError(f"Invalid decimal number '{text}'.") from exc


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Modernized LOANAPIR client that submits loan applications via HTTP.",
    )
    parser.add_argument("--name", required=True, help="Applicant name.")
    parser.add_argument("--credit-score", required=True, type=int, help="Credit score (1-999).")
    parser.add_argument("--yearly-income", required=True, type=_decimal_argument, help="Yearly income (e.g. 95000).")
    parser.add_argument("--age", required=True, type=int, help="Applicant age in years.")
    parser.add_argument("--amount", required=True, type=_decimal_argument, help="Requested loan amount.")
    parser.add_argument(
        "--yearly-repayment",
        required=True,
        type=_decimal_argument,
        help="Expected yearly repayment amount.",
    )
    parser.add_argument(
        "--base-url",
        default=DEFAULT_BASE_URL,
        help=f"Loan API endpoint (default: {DEFAULT_BASE_URL}).",
    )
    parser.add_argument(
        "--timeout",
        default=DEFAULT_TIMEOUT,
        type=float,
        help=f"HTTP timeout in seconds (default: {DEFAULT_TIMEOUT}).",
    )
    parser.add_argument(
        "--log-level",
        default=os.getenv("LOAN_API_LOG_LEVEL", "INFO"),
        choices=["CRITICAL", "ERROR", "WARNING", "INFO", "DEBUG"],
        help="Logging verbosity.",
    )
    parser.add_argument(
        "--print-payload",
        action="store_true",
        help="Print the JSON payload before sending it to the API.",
    )
    return parser


def configure_logging(level_name: str) -> None:
    level = getattr(logging, level_name.upper(), logging.INFO)
    logging.basicConfig(level=level, format="%(levelname)s %(name)s - %(message)s")


def main(argv: Optional[list[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)

    configure_logging(args.log_level)

    application = LoanApplication(
        name=args.name.strip(),
        credit_score=args.credit_score,
        yearly_income=args.yearly_income,
        age=args.age,
        amount=args.amount,
        yearly_repayment=args.yearly_repayment,
    )

    try:
        application.validate()
    except ValueError as exc:
        logging.error("Invalid loan application: %s", exc)
        return 2

    if args.print_payload:
        print(json.dumps(application.to_payload(), indent=2))

    client = LoanApiClient(base_url=args.base_url, timeout=args.timeout)

    try:
        decision = client.submit_application(application)
    except LoanApiError as exc:
        logging.error("Loan API error (%s): %s (status=%s)", exc.origin, exc.message, exc.status_code)
        return 1

    print(decision.render())
    return 0


if __name__ == "__main__":  # pragma: no cover - manual execution entrypoint
    sys.exit(main())
