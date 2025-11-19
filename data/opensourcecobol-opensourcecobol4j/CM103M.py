#!/usr/bin/env python3
"""
Python translation of the CM103M COBOL program.

The original program waited for messages on an inbound communications queue,
echoed them back to an outbound queue, and recorded a detailed log to a
fixed-format report file.  This module keeps the logging behavior, using
plain text input instead of mainframe communication queues.

Each non-empty, non-comment input line represents one message.  The expected
format is:

    message text[|recv_status|send_status|error_key|receipt_timestamp]

Only the message text is required.  Status fields default to "OK", the error
key defaults to a blank, and the timestamp defaults to the time the message is
processed.  Processing stops after logging a message whose first four
characters are "KILL" (matching the COBOL program's sentinel behavior).
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
import sys
from typing import Iterable, Iterator, Optional, TextIO, Tuple

TEST_ID = "CM103M"

CCVS_H1 = (
    " " * 27
    + " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION SYSTEM"
    + " " * 26
)


def build_ccvs_h2(test_id: str) -> str:
    return (
        "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".ljust(52)
        + "TEST RESULTS SET-  "
        + test_id.ljust(9)
        + " " * 40
    )


CCVS_H3 = (
    " FOR OFFICIAL USE ONLY    ".ljust(34)
    + "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      "
    + "  COPYRIGHT   1974 "
)

HYPHEN_LINE = " " + "*" * 119

LOG_HEADER_1 = " " * 54 + "MESSAGE LOG"
LOG_HEADER_2 = (
    " "
    + "MCS RECEIPT".ljust(12)
    + "PROGRAM".ljust(8)
    + "MCS REC".ljust(9)
    + "RECV SEND".ljust(12)
    + "MSG".ljust(38)
    + "MESSAGE".ljust(7)
)
LOG_HEADER_3 = (
    " " * 3
    + "INBOUND".ljust(10)
    + "RECEIPT".ljust(8)
    + "OUTBOUND".ljust(9)
    + "STAT STAT".ljust(11)
    + "LENGTH".ljust(39)
    + "CONTENT".ljust(7)
)
LOG_HEADER_4 = (
    " "
    + "-" * 11
    + " "
    + "-" * 7
    + " "
    + "-" * 7
    + "  "
    + "---- ----"
    + "-" * 5
    + "  "
    + "-" * 72
)

CCVS_E3 = (
    " FOR OFFICIAL USE ONLY".ljust(22)
    + " " * 12
    + "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     "
    + " " * 13
    + " COPYRIGHT 1974"
)


def build_ccvs_e1(test_id: str) -> str:
    return (
        " " * 52
        + "END OF TEST-  "
        + test_id.ljust(9)
        + " NTIS DISTRIBUTION COBOL 74"
    )


def build_ccvs_e2(errors: int) -> str:
    return " " * 52 + f"{errors:>3} " + "ERRORS ENCOUNTERED".ljust(44)


def build_ccvs_e4(executed: int, passed: int) -> str:
    executed = max(executed, 0)
    passed = max(min(passed, executed), 0)
    return f"{passed:>3} OF {executed:>3}  TESTS WERE EXECUTED SUCCESSFULLY"


@dataclass
class Message:
    text: str
    inbound_status: str = "OK"
    outbound_status: str = "OK"
    error_key: str = " "
    receipt_time: Optional[datetime] = None

    @property
    def kill_requested(self) -> bool:
        return self.text[:4].upper() == "KILL"


class ReportWriter:
    """Helper that renders the fixed-format report used by CM103M."""

    def __init__(self, handle: TextIO, test_id: str = TEST_ID) -> None:
        self.handle = handle
        self.test_id = test_id

    def write_line(self, text: str = "") -> None:
        self.handle.write(text.rstrip("\n"))
        self.handle.write("\n")

    def write_header(self) -> None:
        self.write_line(CCVS_H1)
        self.write_line(build_ccvs_h2(self.test_id))
        self.write_line(CCVS_H3)
        self.write_line(HYPHEN_LINE)
        self.write_line()

    def write_log_header(self) -> None:
        self.write_line(LOG_HEADER_1)
        self.write_line(LOG_HEADER_2)
        self.write_line(LOG_HEADER_3)
        self.write_line(LOG_HEADER_4)
        self.write_line()

    def write_footer(self, executed: int, errors: int) -> None:
        self.write_line(HYPHEN_LINE)
        self.write_line()
        self.write_line(build_ccvs_e1(self.test_id))
        self.write_line(build_ccvs_e2(errors))
        self.write_line(CCVS_E3)
        self.write_line(build_ccvs_e4(executed, executed - errors))


def maybe_parse_timestamp(value: str) -> Optional[datetime]:
    cleaned = value.strip()
    if not cleaned:
        return None
    candidate = cleaned if not cleaned.endswith("Z") else f"{cleaned[:-1]}+00:00"
    try:
        return datetime.fromisoformat(candidate)
    except ValueError:
        return None


def parse_message_line(line: str) -> Message:
    parts = line.split("|", 4)
    text = parts[0]
    inbound = parts[1].strip() if len(parts) > 1 else "OK"
    outbound = parts[2].strip() if len(parts) > 2 else "OK"
    error_key = (parts[3].strip() if len(parts) > 3 else "") or " "
    receipt_time = maybe_parse_timestamp(parts[4]) if len(parts) > 4 else None

    inbound = inbound or "OK"
    outbound = outbound or "OK"

    return Message(
        text=text,
        inbound_status=inbound,
        outbound_status=outbound,
        error_key=error_key,
        receipt_time=receipt_time,
    )


def iter_messages(raw_lines: Iterable[str]) -> Iterator[Message]:
    for raw in raw_lines:
        stripped = raw.rstrip("\r\n")
        if not stripped.strip():
            continue
        if stripped.lstrip().startswith("#"):
            continue
        yield parse_message_line(stripped)


def normalise_status(value: str) -> str:
    cleaned = (value or "").upper()
    if not cleaned:
        cleaned = "OK"
    return cleaned.ljust(2)[:2]


def is_status_ok(value: str) -> bool:
    cleaned = (value or "").strip().upper()
    return cleaned in {"", "OK", "0", "00"}


def clamp_seconds(value: float) -> float:
    return max(min(value, 999.99), -999.99)


def diff_seconds(late: datetime, early: datetime) -> float:
    return clamp_seconds((late - early).total_seconds())


def build_log_entry(message: Message) -> Tuple[str, bool, bool]:
    base_time = message.receipt_time or datetime.now()
    in_time = datetime.now()
    send_time = datetime.now()

    prog_time = diff_seconds(in_time, base_time)
    time_sent = diff_seconds(send_time, base_time)

    prog_time_field = f"{prog_time:7.2f}"
    time_sent_field = f"{time_sent:7.2f}"

    recv_status = normalise_status(message.inbound_status)
    send_status = normalise_status(message.outbound_status)
    err_key = (message.error_key or " ")[:1] or " "

    msg_length = min(len(message.text), 999)
    msg_field = message.text[:72].ljust(72)

    time_record = base_time.strftime("%H:%M:%S")
    line = (
        f" {time_record} "
        f"{prog_time_field}"
        f"  {time_sent_field}"
        f"    {recv_status}"
        f"  {send_status}/{err_key}"
        f"   {msg_length:3d}   "
        f"{msg_field}"
    )

    has_error = not (
        is_status_ok(message.inbound_status)
        and is_status_ok(message.outbound_status)
        and err_key.strip() in {"", "0"}
    )

    return line, has_error, message.kill_requested


def log_messages(
    raw_lines: Iterable[str], report_path: str | Path = "report.log"
) -> Tuple[int, int]:
    report_path = Path(report_path)
    report_path.parent.mkdir(parents=True, exist_ok=True)

    record_count = 0
    error_count = 0

    with report_path.open("w", encoding="utf-8") as handle:
        writer = ReportWriter(handle, test_id=TEST_ID)
        writer.write_header()
        writer.write_log_header()

        for message in iter_messages(raw_lines):
            line, has_error, kill = build_log_entry(message)
            writer.write_line(line)

            record_count += 1
            if has_error:
                error_count += 1

            if kill:
                break

        writer.write_footer(record_count, error_count)

    return record_count, error_count


def main(argv: Optional[Iterable[str]] = None) -> int:
    parser = argparse.ArgumentParser(
        description="Generate CM103M-style report.log entries from plain text input."
    )
    parser.add_argument(
        "messages",
        nargs="?",
        help="Path to an input file containing message definitions. "
        "If omitted, stdin is used.",
    )
    parser.add_argument(
        "--report",
        default="report.log",
        help="Destination path for the generated report (default: %(default)s).",
    )
    args = parser.parse_args(list(argv) if argv is not None else None)

    if args.messages:
        with open(args.messages, "r", encoding="utf-8") as source:
            log_messages(source, args.report)
    else:
        log_messages(sys.stdin, args.report)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
