"""Modernized Python implementation of the CSCVINCI COBOL module.

The original program read an IMS input message, invoked a z/OS Connect EE
(operation ID IMS00) through a communication stub, and then wrote the response
back to IMS. This Python version focuses on the business logic: it accepts an
employee/customer number, calls a REST endpoint exposed by z/OS Connect EE, and
returns a structured response that mirrors the original OUT-BUFFER segment.
"""

from __future__ import annotations

import argparse
import json
import logging
import os
import sys
from dataclasses import dataclass, field
from datetime import datetime, timezone
from typing import Any, Mapping, Sequence, Tuple

import requests
from requests import Session
from requests.exceptions import RequestException


FIELD_ALIASES: dict[str, tuple[str, ...]] = {
    "name": ("name2", "name", "customerName"),
    "address": ("Xaddress2", "address", "address1", "street"),
    "phone": ("phoneNumber2", "phone", "phoneNumber"),
    "date": ("Xdate2", "date", "lastChangeDate"),
    "amount": ("amount2", "amount", "balance"),
    "user_id": ("USERID2", "userId", "userid", "user"),
}


class CustomerInquiryError(Exception):
    """Raised when the downstream API, z/OS Connect EE, or transport fails."""

    def __init__(self, origin: str, http_code: int, message: str) -> None:
        super().__init__(message)
        self.origin = origin
        self.http_code = http_code
        self.message = message


@dataclass(slots=True)
class CustomerInquiryRequest:
    """Represents the IMS input COMMAREA data that matters for this flow."""

    number: str

    def __post_init__(self) -> None:
        cleaned = (self.number or "").strip()
        if not cleaned:
            raise ValueError("NUMB/customer number must not be empty.")
        self.number = cleaned


@dataclass(slots=True)
class CustomerInquiryResponse:
    """Modern equivalent of the COBOL OUT-BUFFER structure."""

    number: str
    name: str = ""
    address: str = ""
    phone: str = ""
    date: str = ""
    amount: str = ""
    user_id: str = ""
    http_code: int = 0
    messages: tuple[str, str, str, str] = field(
        default_factory=lambda: ("", "", "", "")
    )

    def to_dict(self) -> dict[str, Any]:
        msg1, msg2, msg3, msg4 = self.messages
        return {
            "NUMB": self.number,
            "NAME": self.name,
            "ADDRX": self.address,
            "PHONE": self.phone,
            "DATEX": self.date,
            "AMOUNT": self.amount,
            "USERID": self.user_id,
            "HTTPCODE": self.http_code,
            "MSG1": msg1,
            "MSG2": msg2,
            "MSG3": msg3,
            "MSG4": msg4,
        }


def split_message(
    message: str, segment_length: int = 75, segments: int = 4
) -> tuple[str, ...]:
    """Split an error message into fixed-length segments like MSG1-MSG4."""
    cleaned = (message or "").strip()
    chunk_list = [
        cleaned[idx : idx + segment_length]
        for idx in range(0, len(cleaned), segment_length)
    ][:segments]
    while len(chunk_list) < segments:
        chunk_list.append("")
    return tuple(chunk_list[:segments])


def _normalize_string(value: Any) -> str:
    if value is None:
        return ""
    return str(value).strip()


def normalize_response_payload(payload: Mapping[str, Any]) -> dict[str, str]:
    """Map various possible JSON keys into the canonical OUT-BUFFER fields."""
    normalized: dict[str, str] = {}
    for target_key, aliases in FIELD_ALIASES.items():
        normalized[target_key] = ""
        for alias in aliases:
            raw_value = payload.get(alias)
            if raw_value not in (None, ""):
                normalized[target_key] = _normalize_string(raw_value)
                break
    return normalized


def _safe_text(response: requests.Response) -> str:
    try:
        text = response.text
    except Exception:
        return ""
    return text.strip()


class ZceeClient:
    """Thin HTTP client that replaces the BAQCSTUB communication stub."""

    def __init__(
        self,
        endpoint: str,
        session: Session | None = None,
        timeout: float = 10.0,
    ) -> None:
        if not endpoint:
            raise ValueError("An endpoint URL is required for the CSCVINCI client.")
        self.endpoint = endpoint
        self.session = session or requests.Session()
        self.timeout = timeout

    def get_customer(self, employee_number: str) -> tuple[dict[str, str], int]:
        """Invoke the z/OS Connect EE REST API."""
        try:
            response = self.session.get(
                self.endpoint,
                params={"employee": employee_number},
                timeout=self.timeout,
            )
        except RequestException as exc:
            raise CustomerInquiryError("STUB", 0, f"Communication error: {exc}") from exc

        http_code = response.status_code
        text_payload = _safe_text(response)

        try:
            payload = response.json()
        except ValueError:
            payload = None

        if http_code >= 400:
            message = self._derive_error_message(
                payload if isinstance(payload, Mapping) else None,
                text_payload,
                http_code,
            )
            origin = "ZCEE" if http_code >= 500 else "API"
            raise CustomerInquiryError(origin, http_code, message)

        if not isinstance(payload, Mapping):
            raise CustomerInquiryError(
                "API",
                http_code,
                "Unexpected payload shape; expected a JSON object.",
            )

        normalized = normalize_response_payload(payload)
        return normalized, http_code

    @staticmethod
    def _derive_error_message(
        payload: Mapping[str, Any] | None,
        fallback: str,
        http_code: int,
    ) -> str:
        if payload:
            for key in ("message", "errorMessage", "detail", "error"):
                value = payload.get(key)
                if value:
                    return _normalize_string(value)
        if fallback:
            return fallback
        return f"Request failed with HTTP {http_code}."


class CustomerInquiryProcessor:
    """Coordinates the request, logging, and response mapping."""

    def __init__(self, client: ZceeClient, logger: logging.Logger | None = None) -> None:
        self.client = client
        self.log = logger or logging.getLogger(__name__)

    def process(self, request: CustomerInquiryRequest) -> CustomerInquiryResponse:
        timestamp = datetime.now(timezone.utc).strftime("%Y%m%d%H%M%S")
        self.log.info("%s Processing customer %s", timestamp, request.number)

        try:
            payload, http_code = self.client.get_customer(request.number)
        except CustomerInquiryError as exc:
            self.log.error(
                "%s Error origin=%s code=%s message=%s",
                timestamp,
                exc.origin,
                exc.http_code,
                exc.message,
            )
            messages = split_message(exc.message)
            return CustomerInquiryResponse(
                number=request.number,
                http_code=exc.http_code,
                messages=messages,
            )

        response = CustomerInquiryResponse(
            number=request.number,
            name=payload["name"],
            address=payload["address"],
            phone=payload["phone"],
            date=payload["date"],
            amount=payload["amount"],
            user_id=payload["user_id"],
            http_code=http_code,
        )
        self.log.info(
            "%s Successfully retrieved customer %s (HTTP %s)",
            timestamp,
            request.number,
            http_code,
        )
        return response


def parse_args(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Python translation of the CSCVINCI COBOL program."
    )
    parser.add_argument(
        "employee_number",
        help="Employee/customer number previously held in NUMB.",
    )
    parser.add_argument(
        "--endpoint",
        default=os.getenv("CSCVINCI_ENDPOINT"),
        help="z/OS Connect EE REST endpoint. Defaults to CSCVINCI_ENDPOINT env var.",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=float(os.getenv("CSCVINCI_TIMEOUT", "10")),
        help="HTTP timeout in seconds (default: 10, overridable via CSCVINCI_TIMEOUT).",
    )
    parser.add_argument(
        "--log-level",
        default=os.getenv("LOG_LEVEL", "INFO"),
        help="Logging level (default INFO).",
    )
    args = parser.parse_args(argv)
    if not args.endpoint:
        parser.error("The --endpoint option or CSCVINCI_ENDPOINT env var must be provided.")
    return args


def configure_logging(level: str) -> logging.Logger:
    numeric_level = getattr(logging, level.upper(), logging.INFO)
    logging.basicConfig(
        level=numeric_level,
        format="%(asctime)s %(levelname)s %(name)s %(message)s",
    )
    return logging.getLogger("CSCVINCI")


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_args(argv)
    logger = configure_logging(args.log_level)

    try:
        request = CustomerInquiryRequest(args.employee_number)
    except ValueError as exc:
        logger.error("Invalid input: %s", exc)
        return 2

    client = ZceeClient(args.endpoint, timeout=args.timeout)
    processor = CustomerInquiryProcessor(client, logger)
    response = processor.process(request)
    print(json.dumps(response.to_dict(), indent=2))
    return 0


if __name__ == "__main__":
    sys.exit(main())
