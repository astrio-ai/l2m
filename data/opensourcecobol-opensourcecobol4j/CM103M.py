"""CM103M message logger (Python translation).

The original COBOL program listened to a message queue, echoed inbound
messages, and wrote a formatted log to `report.log`. This Python version
emulates that workflow by accepting newline-delimited messages from
stdin, an optional input file, or command-line arguments. Messages are
logged with timestamps, and processing stops after a message whose first
four characters spell “KILL” (case-insensitive).
"""

from __future__ import annotations

import argparse
from datetime import datetime
from pathlib import Path
import sys
from typing import Iterable, Iterator, Sequence, TextIO

TEST_ID = "CM103M"
DEFAULT_REPORT = Path("report.log")
DEFAULT_ENCODING = "utf-8"
DEFAULT_MAX_LENGTH = 72


def fill(value: str, width: int) -> str:
    """Pad or truncate a value to the desired width."""
    truncated = value[:width]
    padding = width - len(truncated)
    if padding > 0:
        truncated += " " * padding
    return truncated


def compose_line(*segments: tuple[str, int]) -> str:
    """Compose a line from (value, width) segments."""
    return "".join(fill(value, width) for value, width in segments).rstrip()


HYPHEN_LINE = compose_line(
    ("", 1),
    ("*" * 65, 65),
    ("*" * 54, 54),
)

CCVS_H1 = compose_line(
    ("", 27),
    (" FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION SYSTEM", 67),
    ("", 26),
)

CCVS_H3 = compose_line(
    (" FOR OFFICIAL USE ONLY    ", 34),
    ("COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ", 58),
    ("  COPYRIGHT   1974 ", 28),
)

LOG_HDR1 = compose_line(
    ("", 54),
    ("MESSAGE LOG", 11),
)

LOG_HDR2 = compose_line(
    ("", 1),
    ("MCS RECEIPT", 12),
    ("PROGRAM", 8),
    ("MCS REC", 9),
    ("RECV SEND", 12),
    ("MSG", 38),
    ("MESSAGE", 7),
)

LOG_HDR3 = compose_line(
    ("", 3),
    ("INBOUND", 10),
    ("RECEIPT", 8),
    ("OUTBOUND", 9),
    ("STAT STAT", 11),
    ("LENGTH", 39),
    ("CONTENT", 7),
)

LOG_HDR4 = compose_line(
    ("", 1),
    ("-" * 11, 11),
    ("", 1),
    ("-" * 7, 7),
    ("", 1),
    ("-" * 7, 7),
    ("", 2),
    ("---- ----", 11),
    ("-" * 5, 5),
    ("", 2),
    ("-" * 72, 72),
)

CCVS_E2 = compose_line(
    ("", 31),
    ("", 21),
    ("", 3),
    ("", 1),
    ("ERRORS ENCOUNTERED", 44),
)

CCVS_E3 = compose_line(
    (" FOR OFFICIAL USE ONLY", 22),
    ("", 12),
    ("ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ", 58),
    ("", 13),
    (" COPYRIGHT 1974", 15),
)


def build_ccvs_h2(test_id: str) -> str:
    return compose_line(
        ("CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.", 52),
        ("TEST RESULTS SET-  ", 19),
        (test_id, 9),
        ("", 40),
    )


def build_ccvs_e1(test_id: str) -> str:
    return compose_line(
        ("", 52),
        ("END OF TEST-  ", 14),
        (test_id, 9),
        (" NTIS DISTRIBUTION COBOL 74", 45),
    )


def build_header_lines(test_id: str) -> list[str]:
    """Replicate the original HEAD-ROUTINE output."""
    lines = [CCVS_H1]
    lines.extend([""] * 2)
    lines.append(build_ccvs_h2(test_id))
    lines.extend([""] * 5)
    lines.append(CCVS_H3)
    lines.append("")
    lines.append(HYPHEN_LINE)
    return lines


def build_log_header_lines() -> list[str]:
    """Replicate the LOG-HEADER routine output."""
    lines: list[str] = []
    lines.extend([""] * 3)
    lines.append(LOG_HDR1)
    lines.extend([""] * 3)
    lines.append(LOG_HDR2)
    lines.append(LOG_HDR3)
    lines.append("")
    lines.append(LOG_HDR4)
    lines.append("")
    return lines


def build_footer_lines(test_id: str) -> list[str]:
    """Replicate the END-ROUTINE output."""
    lines = [HYPHEN_LINE]
    lines.extend([""] * 4)
    lines.append(build_ccvs_e1(test_id))
    lines.append(CCVS_E2)
    lines.append(CCVS_E3)
    return lines


class ReportWriter:
    """Collects lines and writes them to the report file."""

    def __init__(self, target: Path, *, encoding: str = DEFAULT_ENCODING) -> None:
        self.target = target
        self.encoding = encoding
        self.lines: list[str] = []

    def add_line(self, text: str = "") -> None:
        self.lines.append(text.rstrip("\n"))

    def add_blank(self, count: int = 1) -> None:
        for _ in range(count):
            self.add_line("")

    def extend(self, lines: Iterable[str]) -> None:
        for line in lines:
            self.add_line(line)

    def save(self) -> None:
        self.target.parent.mkdir(parents=True, exist_ok=True)
        content = "\n".join(self.lines)
        if self.lines:
            content += "\n"
        self.target.write_text(content, encoding=self.encoding)


def format_time_rec(moment: datetime) -> str:
    """Format a timestamp as HH:MM:SS.hh (hundredths)."""
    hundredths = int(round(moment.microsecond / 10000))
    second = moment.second
    minute = moment.minute
    hour = moment.hour

    if hundredths == 100:
        hundredths = 0
        second += 1
        if second == 60:
            second = 0
            minute += 1
            if minute == 60:
                minute = 0
                hour = (hour + 1) % 24

    return f"{hour:02}:{minute:02}:{second:02}.{hundredths:02}"


def format_seconds_field(value: float) -> str:
    """Format elapsed time using the PIC ---.99 pattern."""
    constrained = max(-999.99, min(999.99, value))
    return f"{constrained:6.2f}"


def build_log_line(raw_message: str, max_length: int) -> tuple[str, bool]:
    """Create a formatted log line and indicate if the message was the terminator."""
    queue_time = datetime.now()
    in_time = datetime.now()
    out_time = datetime.now()

    message_field = raw_message[:max_length].ljust(max_length)
    msg_length = len(raw_message)
    truncated = msg_length > max_length

    recv_status = "OK"
    send_status = "OV" if truncated else "OK"
    send_error = "T" if truncated else " "

    line = (
        " "
        + format_time_rec(queue_time)
        + " "
        + format_seconds_field((in_time - queue_time).total_seconds())
        + "  "
        + format_seconds_field((out_time - queue_time).total_seconds())
        + "    "
        + recv_status
        + "  "
        + send_status
        + "/"
        + send_error
        + "   "
        + f"{min(msg_length, 999):>3}"
        + "   "
        + message_field
    )

    kill_seen = message_field[:4].upper() == "KILL"
    return line, kill_seen


def read_stream(stream: TextIO) -> Iterator[str]:
    """Yield stripped lines from a text stream."""
    for raw_line in stream:
        yield raw_line.rstrip("\r\n")


def iter_messages(args: argparse.Namespace) -> Iterator[str]:
    """Determine the message source based on CLI arguments."""
    if args.messages:
        for message in args.messages:
            yield message
        return

    input_path: Path | None = args.input
    if input_path is not None:
        if str(input_path) == "-":
            yield from read_stream(sys.stdin)
        else:
            with input_path.open("r", encoding=args.encoding) as source:
                yield from read_stream(source)
        return

    if sys.stdin.isatty():
        sys.stderr.write(
            "Enter messages followed by EOF (Ctrl-D on Unix, Ctrl-Z then Enter on Windows).\n"
        )
    yield from read_stream(sys.stdin)


def positive_int(value: str) -> int:
    ivalue = int(value)
    if ivalue <= 0:
        raise argparse.ArgumentTypeError("value must be a positive integer")
    return ivalue


def parse_args(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Python port of CM103M: log inbound messages to report.log."
    )
    parser.add_argument(
        "-i",
        "--input",
        type=Path,
        default=None,
        help="Read messages from a file (use '-' for stdin).",
    )
    parser.add_argument(
        "-o",
        "--output",
        "--report",
        dest="report",
        type=Path,
        default=DEFAULT_REPORT,
        help="Path to the report file (default: report.log).",
    )
    parser.add_argument(
        "--encoding",
        default=DEFAULT_ENCODING,
        help="Encoding for input files and the report (default: utf-8).",
    )
    parser.add_argument(
        "--max-length",
        type=positive_int,
        default=DEFAULT_MAX_LENGTH,
        help="Maximum message length before truncation (default: 72).",
    )
    parser.add_argument(
        "-q",
        "--quiet",
        action="store_true",
        help="Suppress the completion message.",
    )
    parser.add_argument(
        "messages",
        nargs="*",
        help="Inline messages to process (bypasses --input/stdin when provided).",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_args(argv)

    writer = ReportWriter(args.report, encoding=args.encoding)
    writer.extend(build_header_lines(TEST_ID))
    writer.extend(build_log_header_lines())

    entry_count = 0
    for raw in iter_messages(args):
        log_line, kill_seen = build_log_line(raw, args.max_length)
        writer.add_line(log_line)
        entry_count += 1
        if kill_seen:
            break

    writer.extend(build_footer_lines(TEST_ID))
    writer.save()

    if not args.quiet:
        print(f"{entry_count} message(s) logged to {args.report}", file=sys.stderr)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
