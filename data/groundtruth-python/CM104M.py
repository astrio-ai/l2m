"""Python translation of the COBOL CM104M queue-bridging program.

The module mirrors the original program’s behavior by polling two input queues,
relaying their messages to the opposite output queues, logging every transfer,
and halting when a message beginning with the literal “KILL” is received.
"""

from __future__ import annotations

import argparse
import itertools
import json
from collections import deque
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import (
    Deque,
    Iterable,
    List,
    Mapping,
    Optional,
    Protocol,
    Sequence,
    Union,
)

MAX_PAYLOAD_LENGTH = 72


class KillSignal(RuntimeError):
    """Raised when a KILL message is encountered in the queue."""


@dataclass
class Timestamp:
    """Represents the COBOL TIME format (HH:MM:SS.ss)."""

    hour: int
    minute: int
    second: float

    def __post_init__(self) -> None:
        self.hour = int(self.hour)
        self.minute = int(self.minute)
        self.second = float(self.second)
        self._normalize()

    def _normalize(self) -> None:
        extra_minutes, seconds = divmod(self.second, 60)
        self.second = float(seconds)
        self.minute += int(extra_minutes)
        extra_hours, minutes = divmod(self.minute, 60)
        self.minute = int(minutes)
        self.hour += int(extra_hours)

    @classmethod
    def now(cls) -> Timestamp:
        current = datetime.now()
        secs = current.second + current.microsecond / 1_000_000
        return cls(current.hour, current.minute, secs)

    @classmethod
    def from_string(cls, value: str) -> Timestamp:
        parts = value.strip().split(":")
        if len(parts) != 3:
            raise ValueError(f"Invalid timestamp string: {value!r}")
        hour_part, minute_part, second_part = parts
        return cls(int(hour_part), int(minute_part), float(second_part))

    @classmethod
    def from_value(
        cls,
        value: Optional[
            Union[
                Timestamp,
                str,
                Sequence[Union[int, float]],
                Mapping[str, Union[int, float]],
                int,
                float,
            ]
        ],
    ) -> Timestamp:
        if value is None:
            return cls.now()
        if isinstance(value, Timestamp):
            return cls(value.hour, value.minute, value.second)
        if isinstance(value, str):
            return cls.from_string(value)
        if isinstance(value, Mapping):
            hour = value.get("hour", value.get("hrs", 0))
            minute = value.get("minute", value.get("mins", 0))
            second = value.get("second", value.get("secs", 0.0))
            return cls(int(hour), int(minute), float(second))
        if isinstance(value, Sequence):
            if len(value) < 3:
                raise ValueError("Timestamp sequence must contain 3 values.")
            hour, minute, second = value[:3]
            return cls(int(hour), int(minute), float(second))
        if isinstance(value, (int, float)):
            total_seconds = float(value)
            hours, remainder = divmod(total_seconds, 3600)
            minutes, seconds = divmod(remainder, 60)
            return cls(int(hours), int(minutes), seconds)
        raise TypeError(f"Unsupported timestamp value: {value!r}")

    def to_seconds(self) -> float:
        return self.hour * 3600 + self.minute * 60 + self.second

    def __str__(self) -> str:  # pragma: no cover - formatting helper
        return f"{self.hour:02}:{self.minute:02}:{self.second:05.2f}"


def elapsed_seconds(start: Timestamp, end: Timestamp) -> float:
    """Compute elapsed seconds (wraps across midnight if necessary)."""
    diff = end.to_seconds() - start.to_seconds()
    if diff < 0:
        diff += 24 * 3600
    return diff


@dataclass
class Message:
    """Represents a message passed through the CM104M queues."""

    source: str
    payload: str
    received_time: Timestamp = field(default_factory=Timestamp.now)
    in_status: str = "OK"
    msg_count: int = 0
    length: int = field(init=False)

    def __post_init__(self) -> None:
        self.source = (self.source or "")[:12]
        status = (self.in_status or "").strip() or "OK"
        self.in_status = status[:2].ljust(2)
        try:
            self.msg_count = max(0, int(self.msg_count))
        except (TypeError, ValueError):
            self.msg_count = 0
        self.length = len(self.payload)

    def truncated_payload(self, limit: int = MAX_PAYLOAD_LENGTH) -> str:
        return self.payload[:limit]

    def kill_requested(self) -> bool:
        return self.payload.upper().startswith("KILL")


@dataclass
class SendResult:
    """Information returned by an output queue SEND operation."""

    out_status: str = "OK"
    err_key: str = " "


@dataclass
class LogEntry:
    """Represents the printable LOG-LINE in the original COBOL program."""

    source: str
    received_time: Timestamp
    queue_depth: int
    out_time: float
    message_length: int
    in_status: str
    out_status: str
    err_key: str
    payload: str

    def format_line(self) -> str:
        queue_depth_str = f"{self.queue_depth:>2}" if self.queue_depth else "  "
        out_time_str = f"{self.out_time:6.2f}"
        message_length_str = f"{self.message_length:>3}"
        payload_block = self.payload[:MAX_PAYLOAD_LENGTH].ljust(MAX_PAYLOAD_LENGTH)
        return (
            f" {self.source:<12} "
            f"{self.received_time} "
            f"{queue_depth_str} "
            f"{out_time_str} "
            f"{message_length_str} "
            f"{self.in_status[:2]:<2} "
            f"{self.out_status[:2]:<2}"
            f"/{self.err_key[:1]:<1} "
            f"{payload_block}"
        )


HYPHEN_LINE = " ***********************************************************************************************************************"
CCVS_HEAD_LINES = [
    "                           FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION SYSTEM",
    "                                                    CCVS74 NCC  COPY, NOT FOR DISTRIBUTION. TEST RESULTS SET-  CM104M",
    " FOR OFFICIAL USE ONLY                                 COBOL 85 VERSION 4.2, Apr  1993 SSVG                          COPYRIGHT   1974 ",
]
LOG_HEADER_LINES = [
    "                                                      MESSAGE LOG",
    "   SYMBOLIC        TIME MCS        SEND MSG IN OUT",
    "    SOURCE      RECEIVED  QD COMPLT  LTH  ST  STAT                   MESSAGE CONTENTS",
    " ------------ ------------ -- ------ ---- -- --/ - ------------------------------------------------------------------------",
    "",
]
END_LINES = [
    "                                                  END OF TEST-  CM104M     NTIS DISTRIBUTION COBOL 74",
    "                              ERRORS ENCOUNTERED",
    " FOR OFFICIAL USE ONLY    ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.      COPYRIGHT 1974",
]


class ReportWriter:
    """Accumulates log lines and writes the final report.log output."""

    def __init__(self, path: Optional[Union[str, Path]] = "report.log") -> None:
        self.path = Path(path) if path else None
        self.lines: List[str] = []
        self._closed = False
        self.lines.extend(CCVS_HEAD_LINES)
        self.lines.append(HYPHEN_LINE)
        self.lines.extend(LOG_HEADER_LINES)

    def write_log_entry(self, entry: str) -> None:
        self.lines.append(entry)

    def close(self) -> None:
        if self._closed:
            return
        self.lines.append(HYPHEN_LINE)
        self.lines.extend([""] * 4)
        self.lines.extend(END_LINES)
        text = "\n".join(self.lines) + "\n"
        if self.path is not None:
            self.path.parent.mkdir(parents=True, exist_ok=True)
            self.path.write_text(text, encoding="utf-8")
        self._closed = True

    def as_text(self) -> str:
        return "\n".join(self.lines)


class InputQueue(Protocol):
    def receive(self) -> Optional[Message]:
        ...


class OutputQueue(Protocol):
    def send(self, payload: str) -> SendResult:
        ...


class IterableInputQueue:
    """Simple deque-backed queue used for testing and CLI ingestion."""

    def __init__(self, messages: Optional[Iterable[Message]] = None) -> None:
        self._messages: Deque[Message] = deque(messages or [])

    def receive(self) -> Optional[Message]:
        if not self._messages:
            return None
        return self._messages.popleft()

    def push(self, message: Message) -> None:
        self._messages.append(message)

    def __len__(self) -> int:
        return len(self._messages)


class RecordingOutputQueue:
    """Collects sent payloads and returns configurable SendResult objects."""

    def __init__(
        self,
        default_status: str = "OK",
        default_error_key: str = " ",
        status_script: Optional[Sequence[SendResult]] = None,
    ) -> None:
        self.default_status = (default_status or "OK")[:2]
        self.default_error_key = (default_error_key or " ")[:1]
        self.sent_payloads: List[str] = []
        self._status_iter = (
            itertools.cycle(tuple(status_script)) if status_script else None
        )

    def send(self, payload: str) -> SendResult:
        self.sent_payloads.append(payload)
        if self._status_iter:
            scripted = next(self._status_iter)
            return SendResult(scripted.out_status, scripted.err_key)
        return SendResult(self.default_status, self.default_error_key)


class MessageBridge:
    """Python translation of CM104M's queue-bridging behavior."""

    def __init__(
        self,
        input_queue_1: InputQueue,
        input_queue_2: InputQueue,
        output_queue_1: OutputQueue,
        output_queue_2: OutputQueue,
        *,
        report_writer: Optional[ReportWriter] = None,
        max_payload_length: int = MAX_PAYLOAD_LENGTH,
    ) -> None:
        self.input_queue_1 = input_queue_1
        self.input_queue_2 = input_queue_2
        self.output_queue_1 = output_queue_1
        self.output_queue_2 = output_queue_2
        self.report_writer = report_writer or ReportWriter()
        self.max_payload_length = max_payload_length

    def run(self) -> bool:
        """Run the polling loop until both queues are empty or a KILL message arrives."""
        kill_received = False
        try:
            while True:
                processed_any = False
                processed_any |= self._handle_queue(self.input_queue_1, self.output_queue_2)
                processed_any |= self._handle_queue(self.input_queue_2, self.output_queue_1)
                if not processed_any:
                    break
        except KillSignal:
            kill_received = True
        finally:
            self.report_writer.close()
        return kill_received

    def _handle_queue(self, input_queue: InputQueue, output_queue: OutputQueue) -> bool:
        message = input_queue.receive()
        if message is None:
            return False

        payload = message.truncated_payload(self.max_payload_length)
        send_result = output_queue.send(payload)
        send_timestamp = Timestamp.now()
        out_time = elapsed_seconds(message.received_time, send_timestamp)

        entry = LogEntry(
            source=message.source,
            received_time=message.received_time,
            queue_depth=message.msg_count,
            out_time=out_time,
            message_length=message.length,
            in_status=message.in_status,
            out_status=send_result.out_status,
            err_key=send_result.err_key,
            payload=payload,
        )
        self.report_writer.write_log_entry(entry.format_line())

        if message.kill_requested():
            raise KillSignal("KILL message received")

        return True


def _safe_int(value: object, default: int = 0) -> int:
    try:
        return int(value)  # type: ignore[arg-type]
    except (TypeError, ValueError):
        return default


def load_messages_from_json(path: Union[str, Path]) -> List[Message]:
    """Load a queue description from JSON."""
    raw_text = Path(path).read_text(encoding="utf-8")
    data = json.loads(raw_text)

    if isinstance(data, dict):
        if "messages" in data:
            data = data["messages"]
        elif "queue" in data:
            data = data["queue"]
        else:
            raise ValueError("JSON queue files must include 'messages' or 'queue' keys.")

    if not isinstance(data, list):
        raise TypeError("Queue description must be a list of message objects.")

    messages: List[Message] = []
    for entry in data:
        if not isinstance(entry, Mapping):
            raise TypeError("Each message definition must be a JSON object.")
        received = entry.get("received_time", entry.get("time"))
        messages.append(
            Message(
                source=str(entry.get("source", "UNSPEC")).strip(),
                payload=str(entry.get("payload", "")),
                received_time=Timestamp.from_value(received),
                in_status=str(entry.get("in_status", entry.get("status", "OK"))),
                msg_count=_safe_int(entry.get("msg_count", entry.get("queue_depth", entry.get("depth", 0)))),
            )
        )
    return messages


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Python translation of CM104M – bridges two message queues and writes report.log."
    )
    parser.add_argument(
        "--queue1",
        type=Path,
        help="Path to a JSON file describing the first input queue.",
    )
    parser.add_argument(
        "--queue2",
        type=Path,
        help="Path to a JSON file describing the second input queue.",
    )
    parser.add_argument(
        "--report",
        type=Path,
        default=Path("report.log"),
        help="Location of the generated report.log file.",
    )
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(argv)

    queue1_messages = load_messages_from_json(args.queue1) if args.queue1 else []
    queue2_messages = load_messages_from_json(args.queue2) if args.queue2 else []

    input_queue_1 = IterableInputQueue(queue1_messages)
    input_queue_2 = IterableInputQueue(queue2_messages)
    output_queue_1 = RecordingOutputQueue()
    output_queue_2 = RecordingOutputQueue()

    writer = ReportWriter(args.report)
    bridge = MessageBridge(
        input_queue_1,
        input_queue_2,
        output_queue_1,
        output_queue_2,
        report_writer=writer,
    )
    bridge.run()
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
