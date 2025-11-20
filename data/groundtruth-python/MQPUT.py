"""Modernized Python implementation of the MQPUT COBOL program."""

from __future__ import annotations

import json
import os
import sys
from dataclasses import dataclass
from typing import Any, Callable, Dict, Optional
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen


class CommunicationError(Exception):
    """Raised when the communication stub cannot be reached."""


@dataclass
class MQPutRequest:
    """Represents the data sent to the MQPUT API."""

    message_count: int = 1
    numb2: str = "837367"
    name2: str = "John"
    addrx2: str = "Apex"
    phone2: str = "0065"
    datex2: str = "11 22 65"
    amount2: str = "$1000.65"

    def to_payload(self) -> Dict[str, Any]:
        """Return the payload structure expected by the API."""
        return {
            "MQMESSAGE2-num": self.message_count,
            "numb2": self._field(self.numb2),
            "name2": self._field(self.name2),
            "addrx2": self._field(self.addrx2),
            "phone2": self._field(self.phone2),
            "datex2": self._field(self.datex2),
            "amount2": self._field(self.amount2),
        }

    @staticmethod
    def _field(value: str) -> Dict[str, Any]:
        return {"value": value, "length": len(value)}


@dataclass
class BAQResponse:
    """Normalized response returned by the communication stub."""

    status_code: Optional[int]
    status_message: str
    origin: str
    payload: Any = None

    @property
    def is_success(self) -> bool:
        return self.status_code is not None and 200 <= self.status_code < 300


def determine_origin(status_code: Optional[int]) -> str:
    """Best-effort mapping of status code to the subsystem that failed."""
    if status_code is None:
        return "STUB"
    if 400 <= status_code < 500:
        return "API"
    if status_code >= 500:
        return "ZCEE"
    return "API"


def _parse_json(body: str) -> Any:
    if not body:
        return {}
    try:
        return json.loads(body)
    except json.JSONDecodeError:
        return body


def call_comm_stub(
    payload: Dict[str, Any],
    *,
    endpoint: Optional[str] = None,
    timeout: int = 30,
) -> BAQResponse:
    """Send the payload to the BAQ communication stub endpoint."""
    endpoint = endpoint or os.getenv("BAQ_ENDPOINT")
    if not endpoint:
        raise CommunicationError("BAQ_ENDPOINT environment variable is not set.")

    request = Request(
        endpoint,
        data=json.dumps(payload).encode("utf-8"),
        headers={"Content-Type": "application/json"},
        method="POST",
    )

    try:
        with urlopen(request, timeout=timeout) as response:
            body_bytes = response.read()
            body_text = body_bytes.decode("utf-8").strip()
            response_payload = _parse_json(body_text)
            return BAQResponse(
                status_code=response.getcode(),
                status_message=getattr(response, "reason", "") or "",
                origin="API",
                payload=response_payload,
            )
    except HTTPError as error:
        body = error.read().decode("utf-8").strip()
        return BAQResponse(
            status_code=error.code,
            status_message=body or error.reason or "",
            origin=determine_origin(error.code),
            payload=_parse_json(body),
        )
    except URLError as error:
        raise CommunicationError(f"Unable to reach BAQ endpoint: {error.reason}") from error


def send_put_request(
    communicator: Callable[..., BAQResponse] = call_comm_stub,
    **communicator_kwargs: Any,
) -> BAQResponse:
    """Build the default request and send it via the provided communicator."""
    request = MQPutRequest()
    payload = request.to_payload()
    return communicator(payload, **communicator_kwargs)


def handle_response(response: BAQResponse) -> int:
    """Mirror the COBOL program's reporting logic."""
    status_code = response.status_code or 0

    if response.is_success:
        print(f"HTTP CODE: {status_code}")
        return status_code

    print(f"Error code: {status_code}")
    print(f"Error msg: {response.status_message}")
    print(f"Error origin: {response.origin}")
    return status_code or 1


def main() -> None:
    """Entry point that orchestrates the request/response lifecycle."""
    try:
        response = send_put_request()
        exit_code = handle_response(response)
    except CommunicationError as error:
        print("Error code: 0")
        print(f"Error msg: {error}")
        print("Error origin: STUB")
        exit_code = 1

    sys.exit(exit_code)


if __name__ == "__main__":
    main()
