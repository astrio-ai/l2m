from __future__ import annotations

from datetime import datetime
from typing import Any, Callable, Dict, Mapping, Optional

BAQ_SUCCESS = "BAQ-SUCCESS"
BAQ_ERROR_IN_API = "BAQ-ERROR-IN-API"
BAQ_ERROR_IN_ZCEE = "BAQ-ERROR-IN-ZCEE"
BAQ_ERROR_IN_STUB = "BAQ-ERROR-IN-STUB"

MSG_SEGMENT_LENGTH = 75
MSG_SEGMENT_COUNT = 4

NUMB_LENGTH = 6
NAME_LENGTH = 20
ADDRX_LENGTH = 20
PHONE_LENGTH = 8
DATEX_LENGTH = 8
AMOUNT_LENGTH = 8
USERID_LENGTH = 8
ERROR_DETAIL_LENGTH = 1024

DEFAULT_OUT_LL = 400
DEFAULT_OUT_ZZ = 0
DEFAULT_SEGNO_START = 0


def _current_timestamp() -> str:
    return datetime.utcnow().strftime("%Y%m%d%H%M%S")


def _as_dict(value: Optional[Mapping[str, Any]]) -> Dict[str, Any]:
    if value is None:
        return {}
    if isinstance(value, Mapping):
        return dict(value)
    return dict(value)  # type: ignore[arg-type]


def _extract_commarea(in_buffer: Mapping[str, Any]) -> Dict[str, Any]:
    for key in ("IN-COMMAREA", "commarea", "COMMAREA"):
        if key in in_buffer and isinstance(in_buffer[key], Mapping):
            return dict(in_buffer[key])
    return dict(in_buffer)


def _move_with_padding(value: Any, length: int) -> str:
    text = "" if value is None else str(value)
    return (text + " " * length)[:length]


def _truncate(value: Any, length: int) -> str:
    text = "" if value is None else str(value)
    return text[:length]


def _split_message(message: str) -> list[str]:
    padded = message.ljust(MSG_SEGMENT_LENGTH * MSG_SEGMENT_COUNT)
    return [
        padded[i * MSG_SEGMENT_LENGTH : (i + 1) * MSG_SEGMENT_LENGTH]
        for i in range(MSG_SEGMENT_COUNT)
    ]


def _origin_from_return_code(return_code: str) -> str:
    if return_code == BAQ_ERROR_IN_API:
        return "API"
    if return_code == BAQ_ERROR_IN_ZCEE:
        return "ZCEE"
    if return_code == BAQ_ERROR_IN_STUB:
        return "STUB"
    return "UNKNOWN"


def _build_initial_out_buffer(commarea: Mapping[str, Any]) -> Dict[str, Any]:
    blank_msg = " " * MSG_SEGMENT_LENGTH
    return {
        "OUT-LL": DEFAULT_OUT_LL,
        "OUT-ZZ": DEFAULT_OUT_ZZ,
        "OUT-COMMAREA": {
            "NUMB": _move_with_padding(commarea.get("NUMB", ""), NUMB_LENGTH),
            "NAME": _move_with_padding("", NAME_LENGTH),
            "ADDRX": _move_with_padding("", ADDRX_LENGTH),
            "PHONE": _move_with_padding("", PHONE_LENGTH),
            "DATEX": _move_with_padding("", DATEX_LENGTH),
            "AMOUNT": _move_with_padding("", AMOUNT_LENGTH),
            "USERID": _move_with_padding("", USERID_LENGTH),
            "HTTPCODE": 0,
            "MSG1": blank_msg,
            "MSG2": blank_msg,
            "MSG3": blank_msg,
            "MSG4": blank_msg,
        },
        "OUT-SEGNO": DEFAULT_SEGNO_START,
    }


def _initialize_error_msg(timestamp: str) -> Dict[str, Any]:
    return {
        "EM-ORIGIN": "",
        "EM-CODE": 0,
        "EM-DETAIL": "",
        "TIMESTAMP": timestamp,
    }


def _safe_int(value: Any, default: int = 0) -> int:
    try:
        return int(value)
    except (TypeError, ValueError):
        return default


def _build_request(commarea: Mapping[str, Any]) -> Dict[str, Any]:
    employee = _move_with_padding(commarea.get("NUMB", ""), NUMB_LENGTH)
    return {
        "employee": employee,
        "employee_length": NUMB_LENGTH,
    }


def _invoke_api(
    api_caller: Callable[[Dict[str, Any]], Mapping[str, Any]], request: Dict[str, Any]
) -> Dict[str, Any]:
    try:
        raw_response = api_caller(dict(request))
    except Exception as exc:  # pragma: no cover - defensive path
        return {
            "return_code": BAQ_ERROR_IN_STUB,
            "status_code": 500,
            "status_message": str(exc),
            "payload": {},
        }

    response_dict = dict(raw_response or {})
    payload = response_dict.get("payload")
    if not isinstance(payload, Mapping):
        payload = {}

    return {
        "return_code": response_dict.get("return_code", BAQ_ERROR_IN_STUB),
        "status_code": _safe_int(response_dict.get("status_code"), 0),
        "status_message": "" if response_dict.get("status_message") is None else str(response_dict.get("status_message")),
        "payload": dict(payload),
    }


def _apply_success(out_commarea: Dict[str, Any], payload: Mapping[str, Any]) -> None:
    out_commarea["NAME"] = _move_with_padding(payload.get("name2", ""), NAME_LENGTH)
    out_commarea["ADDRX"] = _move_with_padding(payload.get("Xaddress2", ""), ADDRX_LENGTH)
    out_commarea["PHONE"] = _move_with_padding(payload.get("phoneNumber2", ""), PHONE_LENGTH)
    out_commarea["DATEX"] = _move_with_padding(payload.get("Xdate2", ""), DATEX_LENGTH)
    out_commarea["AMOUNT"] = _move_with_padding(payload.get("amount2", ""), AMOUNT_LENGTH)
    out_commarea["USERID"] = _move_with_padding(payload.get("USERID2", ""), USERID_LENGTH)


def _apply_error(
    out_commarea: Dict[str, Any],
    error_msg: Dict[str, Any],
    status_message: str,
    return_code: str,
    status_code: int,
) -> None:
    segments = _split_message(status_message)
    for idx, key in enumerate(("MSG1", "MSG2", "MSG3", "MSG4")):
        out_commarea[key] = segments[idx]

    error_msg["EM-CODE"] = status_code
    error_msg["EM-DETAIL"] = _truncate(status_message, ERROR_DETAIL_LENGTH)
    error_msg["EM-ORIGIN"] = _origin_from_return_code(return_code)


def cscvgeti(
    in_buffer: Optional[Mapping[str, Any]],
    api_caller: Callable[[Dict[str, Any]], Mapping[str, Any]],
    *,
    iopcb: Optional[Mapping[str, Any]] = None,
    timestamp_provider: Optional[Callable[[], str]] = None,
) -> Dict[str, Any]:
    if api_caller is None:
        raise ValueError("api_caller must be provided")

    timestamp = timestamp_provider() if timestamp_provider else _current_timestamp()
    input_dict = _as_dict(in_buffer)
    iopcb_dict = _as_dict(iopcb)

    modname = iopcb_dict.get("MODNAME", "CSCVGET") or "CSCVGET"
    userid = iopcb_dict.get("USERID", "") or ""
    logs = [
        f"{timestamp} IOPCB ModNAME: {modname}",
        f"{timestamp} IOPCB Userid: {userid}",
        f"{timestamp} IN-LL: {input_dict.get('IN-LL', '')}",
        f"{timestamp} IN-BUFFER: {input_dict}",
    ]

    commarea = _extract_commarea(input_dict)
    out_buffer = _build_initial_out_buffer(commarea)
    request_payload = _build_request(commarea)
    response_data = _invoke_api(api_caller, request_payload)

    status_code = response_data["status_code"]
    status_message = response_data["status_message"]
    return_code = response_data["return_code"]
    payload = response_data["payload"]

    error_msg = _initialize_error_msg(timestamp)
    normalized_modname = "CSCOGET" if modname == "CSCVGET" else modname

    if return_code == BAQ_SUCCESS:
        _apply_success(out_buffer["OUT-COMMAREA"], payload)
    else:
        _apply_error(out_buffer["OUT-COMMAREA"], error_msg, status_message, return_code, status_code)
        logs.append(f"{timestamp} Error code: {status_code}")
        logs.append(f"{timestamp} Error msg: {status_message}")
        logs.append(f"{timestamp} Error origin: {error_msg['EM-ORIGIN']}")

    out_buffer["OUT-COMMAREA"]["HTTPCODE"] = status_code
    out_buffer["OUT-SEGNO"] = DEFAULT_SEGNO_START + 1
    logs.append(f"{timestamp} HTTP CODE: {status_code}")

    return {
        "timestamp": timestamp,
        "modname": normalized_modname,
        "userid": userid,
        "request": request_payload,
        "response": {
            "return_code": return_code,
            "status_code": status_code,
            "status_message": status_message,
        },
        "out_buffer": out_buffer,
        "error_msg": error_msg,
        "logs": logs,
    }
