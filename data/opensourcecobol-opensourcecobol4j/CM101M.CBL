#!/usr/bin/env python3
"""
Python translation of the historical COBOL validation program CM101M.

The original program exercised a message queue interface, logged all inbound
messages to ``report.log`` and ran a suite of status-code checks to confirm
that the runtime returned the correct diagnostics for invalid combinations of
queue names, passwords and sub-queues.

This module keeps the intent of the original program but implements it with
modern Python code so it can run anywhere without a mainframe message control
system.  A lightweight queue/controller simulation is used to reproduce the
status codes that the COBOL program verified.
"""

from __future__ import annotations

import os
from collections import deque
from dataclasses import dataclass
from datetime import datetime, timedelta
from pathlib import Path
from typing import Deque, Iterable, List, Sequence

REPORT_PATH = Path("report.log")
TEST_ID = "CM101M"

DEFAULT_QUEUE = os.environ.get("CM101M_QUEUE", "CM101M.MAIN.QUEUE")
PASSWORD = os.environ.get("CM101M_PASSWORD", "OPENCICS1")
VALID_SUBQUEUES: Sequence[str] = tuple(
    name.strip().upper()
    for name in os.environ.get("CM101M_SUBQUEUES", "").split(",")
    if name.strip()
)

STATUS_SUCCESS = "00"
STATUS_PARAM_ERROR = "20"
STATUS_INVALID_PASSWORD = "40"

HYPHEN_LINE = " " + "*" * 119
FEATURE_NAME = "MCS STATUS WORD"

LOG_HEADER_1 = "                                                LOG OF INCOMING MESSAGES"
LOG_HEADER_3 = " TIME  RECVD   LOG LAG LENGTH END QD POLL COUNT MESSAGE CONTENTS"


@dataclass
class Message:
    """Represents a simulated inbound queue message."""
    kill_field: str
    contents: str
    end_key: str
    queue_depth: int
    timestamp: datetime

    @property
    def length(self) -> int:
        return len(self.contents)


@dataclass
class StatusTestCase:
    paragraph: str
    remark: str
    expected_status: str
    computed_status: str
    outcome: str
    computed_display: str
    correct_display: str
    feature: str = FEATURE_NAME


class ReportWriter:
    """Accumulates lines for the report and writes them to disk in one shot."""
    def __init__(self, path: Path) -> None:
        self.path = path
        self.lines: List[str] = []

    def write(self, line: str = "") -> None:
        self.lines.append(line.rstrip("\n"))

    def blank_line(self) -> None:
        self.lines.append("")

    def flush(self) -> None:
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self.path.write_text("\n".join(self.lines) + "\n", encoding="utf-8")


class MessageQueueController:
    """
    Lightweight stand-in for the MCS queue interface that the COBOL program
    depended on.  Only the validation logic that produced specific status codes
    has been modelled; no real queueing occurs.
    """
    def __init__(
        self,
        queue_name: str,
        password: str,
        valid_subqueues: Sequence[str] | None = None,
    ) -> None:
        self.queue_name = queue_name.strip().upper()
        self.password = password
        self.valid_subqueues = {name.strip().upper() for name in valid_subqueues or []}
        self.enabled = False
        self._messages: Deque[Message] = deque()

    def load_messages(self, messages: Iterable<Message]) -> None:
        self._messages = deque(messages)

    def dequeue_messages(self) -> Iterable[Message]:
        while self._messages:
            yield self._messages.popleft()

    def enable_input(
        self,
        queue_name: str | None,
        subqueue: str | None = None,
        *,
        key: str | None = None,
    ) -> str:
        status = self._basic_status(queue_name, subqueue, key=key)
        if status == STATUS_SUCCESS:
            self.enabled = True
        return status

    def disable_input(
        self,
        queue_name: str | None,
        subqueue: str | None = None,
        *,
        key: str | None = None,
    ) -> str:
        status = self._basic_status(queue_name, subqueue, key=key)
        if status == STATUS_SUCCESS:
            self.enabled = False
        return status

    def receive_status(self, queue_name: str | None, subqueue: str | None = None) -> str:
        return self._basic_status(queue_name, subqueue, key=None)

    def accept_status(self, queue_name: str | None, subqueue: str | None = None) -> str:
        return self._basic_status(queue_name, subqueue, key=None)

    def _basic_status(
        self,
        queue_name: str | None,
        subqueue: str | None,
        *,
        key: str | None,
    ) -> str:
        if not self._queue_valid(queue_name):
            return STATUS_PARAM_ERROR
        if not self._subqueue_valid(subqueue):
            return STATUS_PARAM_ERROR
        if key is not None and key != self.password:
            return STATUS_INVALID_PASSWORD
        return STATUS_SUCCESS

    def _queue_valid(self, queue_name: str | None) -> bool:
        return bool(
            queue_name and queue_name.strip() and queue_name.strip().upper() == self.queue_name
        )

    def _subqueue_valid(self, subqueue: str | None) -> bool:
        if not subqueue or not subqueue.strip():
            return True
        if not self.valid_subqueues:
            return False
        return subqueue.strip().upper() in self.valid_subqueues


def generate_demo_messages() -> List<Message]:
    """Produces a deterministic batch of messages for the logging portion."""
    now = datetime.now().replace(microsecond=0)
    return [
        Message(
            kill_field="    ",
            contents="Customer maintenance request received from ATM #017",
            end_key="1",
            queue_depth=12,
            timestamp=now - timedelta(seconds=42),
        ),
        Message(
            kill_field="WAIT",
            contents="WAIT request issued to demonstrate timer handling logic",
            end_key="2",
            queue_depth=9,
            timestamp=now - timedelta(seconds=30),
        ),
        Message(
            kill_field="KILL",
            contents="Terminate CM101M logging session and disable queue",
            end_key="3",
            queue_depth=0,
            timestamp=now - timedelta(seconds=12),
        ),
    ]


def pad(text: str, width: int) -> str:
    """Pad or trim text to fit into a fixed-width column."""
    cleaned = text[:width]
    return cleaned.ljust(width)


def format_receipt_time(timestamp: datetime) -> str:
    seconds = timestamp.second + timestamp.microsecond / 1_000_000
    return f"{timestamp.hour:02d}:{timestamp.minute:02d}:{seconds:05.2f}"


def format_idle_count(count: int) -> str:
    return f"{count:>10,}".rjust(10)


def sentinel_from_end_key(end_key: str) -> str:
    mapping = {"3": "EGI", "2": "EMI"}
    return mapping.get(end_key.strip(), end_key.strip().upper()[:3]).ljust(3)


def format_log_line(message: Message, poll_count: int, *, system_time: datetime | None = None) -> str:
    system_time = system_time or datetime.now()
    lag_delta = system_time - message.timestamp
    if lag_delta.total_seconds() < 0:
        lag_delta = timedelta(0)
    receipt = format_receipt_time(message.timestamp)
    lag = f"{lag_delta.total_seconds():8.2f}"
    length = f"{message.length:4}"
    sentinel = sentinel_from_end_key(message.end_key)
    queue_depth = f"{message.queue_depth:>2}"
    idle = format_idle_count(poll_count)
    text = message.contents.replace("\n", " ")[:72].ljust(72)
    return f" {receipt} {lag}  {length}   {sentinel} {queue_depth} {idle}  {text}"


def write_log_header(writer: ReportWriter, source: str, msg_date: str) -> None:
    writer.write(LOG_HEADER_1)
    writer.write(f" SYMBOLIC SOURCE IS {pad(source, 25)} MESSAGE DATE IS {msg_date}")
    writer.write(LOG_HEADER_3)
    writer.write(" -----------  ------- ------ --- -- ---------- --------------------------------------------------------")
    writer.blank_line()


def write_disable_summary(writer: ReportWriter, disable_status: str) -> None:
    writer.write(" -DISABLE COMMAND INITIATED FROM PROGRAM")
    writer.write(f" - STATUS CODE OF {disable_status} AND EXECUTION CONTROL RETURNED FROM MCS")
    writer.blank_line()


def head_routine(writer: ReportWriter) -> None:
    writer.write(" FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION SYSTEM")
    writer.blank_line()
    writer.write(f"CCVS74 NCC  COPY, NOT FOR DISTRIBUTION. TEST RESULTS SET-  {TEST_ID}")
    writer.blank_line()
    writer.write(" FOR OFFICIAL USE ONLY    COBOL 85 VERSION 4.2, Apr  1993 SSVG      COPYRIGHT   1974")
    writer.write(HYPHEN_LINE)
    writer.blank_line()


def log_messages(writer: ReportWriter, controller: MessageQueueController) -> None:
    messages = list(controller.dequeue_messages())
    if not messages:
        writer.write(" NO INCOMING MESSAGES WERE AVAILABLE FOR LOGGING.")
        writer.blank_line()
        return

    source = "SIMULATED SOURCE"
    msg_date = messages[0].timestamp.strftime("%m%d%y")
    write_log_header(writer, source, msg_date)

    poll_count = 0
    for message in messages:
        poll_count += 1
        writer.write(format_log_line(message, poll_count))
        kill_value = message.kill_field.strip().upper()
        if kill_value == "WAIT":
            writer.write("   WAIT message encountered - simulating 30 second idle interval.")
        if kill_value == "KILL":
            disable_status = controller.disable_input(DEFAULT_QUEUE, key=PASSWORD)
            write_disable_summary(writer, disable_status)
            break


def write_status_section(writer: ReportWriter) -> None:
    writer.write(" BEGIN INPUT STATUS TESTS")
    writer.blank_line()
    writer.write("   FEATURE TESTED    RESLT PARAGRAPH NAME      COMPUTED DATA       CORRECT DATA        REMARKS")
    writer.write("   ------------------ ----- -------------------- -------------------- -------------------- ------------------------------")
    writer.blank_line()


def run_status_case(
    paragraph: str,
    remark: str,
    expected: str,
    computed: str,
    *,
    outcome_override: str | None = None,
    correct_note: str | None = None,
) -> StatusTestCase:
    outcome = outcome_override or ("PASS" if expected == computed else "FAIL*")
    computed_display = f"STATUS {computed}"
    correct_display = correct_note or f"STATUS {expected}"
    return StatusTestCase(
        paragraph=paragraph,
        remark=remark,
        expected_status=expected,
        computed_status=computed,
        outcome=outcome,
        computed_display=pad(computed_display, 20),
        correct_display=pad(correct_display, 20),
    )


def run_status_tests() -> List[StatusTestCase]:
    def controller() -> MessageQueueController:
        return MessageQueueController(DEFAULT_QUEUE, PASSWORD, VALID_SUBQUEUES)

    tests: List[StatusTestCase] = []

    tests.append(
        run_status_case(
            "REC-STATUS-TEST-01",
            "QUEUE NAME NOT SPECIFIED",
            STATUS_PARAM_ERROR,
            controller().receive_status(queue_name=""),
        )
    )
    tests.append(
        run_status_case(
            "REC-STATUS-TEST-02",
            "UNKNOWN SUB-QUEUE-1 SPECIFIED",
            STATUS_PARAM_ERROR,
            controller().receive_status(queue_name=DEFAULT_QUEUE, subqueue="DUMMYNAME"),
        )
    )
    tests.append(
        run_status_case(
            "ACCPT-STATUS-TEST-01",
            "QUEUE NAME NOT SPECIFIED",
            STATUS_PARAM_ERROR,
            controller().accept_status(queue_name=""),
        )
    )
    tests.append(
        run_status_case(
            "ACCPT-STATUS-TEST-02",
            "UNKNOWN SUB-QUEUE-1 SPECIFIED",
            STATUS_PARAM_ERROR,
            controller().accept_status(queue_name=DEFAULT_QUEUE, subqueue="DUMMYNAME"),
        )
    )
    tests.append(
        run_status_case(
            "ENABL-STATUS-TEST-01",
            "QUEUE NAME NOT SPECIFIED",
            STATUS_PARAM_ERROR,
            controller().enable_input(queue_name="", key=PASSWORD),
        )
    )
    tests.append(
        run_status_case(
            "ENABL-STATUS-TEST-02",
            "UNKNOWN SUB-QUEUE-1 SPECIFIED",
            STATUS_PARAM_ERROR,
            controller().enable_input(queue_name=DEFAULT_QUEUE, subqueue="DUMMYNAME", key=PASSWORD),
        )
    )
    tests.append(
        run_status_case(
            "ENABL-STATUS-TEST-03",
            "INVALID PASSWORD USED",
            STATUS_INVALID_PASSWORD,
            controller().enable_input(queue_name=DEFAULT_QUEUE, key="LETMEIN"),
        )
    )
    tests.append(
        run_status_case(
            "ENABL-STATUS-TEST-04",
            "NO QUEUE NAME / WRONG PASSWORD",
            STATUS_PARAM_ERROR,
            controller().enable_input(queue_name="", key="LETMEIN"),
            outcome_override="INFO",
            correct_note="  INFO TEST FOR BOTH",
        )
    )
    tests.append(
        run_status_case(
            "DISAB-STATUS-TEST-01",
            "QUEUE NAME NOT SPECIFIED",
            STATUS_PARAM_ERROR,
            controller().disable_input(queue_name="", key=PASSWORD),
        )
    )
    tests.append(
        run_status_case(
            "DISAB-STATUS-TEST-02",
            "UNKNOWN SUB-QUEUE-1 SPECIFIED",
            STATUS_PARAM_ERROR,
            controller().disable_input(queue_name=DEFAULT_QUEUE, subqueue="DUMMYNAME", key=PASSWORD),
        )
    )
    tests.append(
        run_status_case(
            "DISAB-STATUS-TEST-03",
            "INVALID PASSWORD USED",
            STATUS_INVALID_PASSWORD,
            controller().disable_input(queue_name=DEFAULT_QUEUE, key="KILLITNOW"),
        )
    )

    ctrl = controller()
    ctrl.enable_input(DEFAULT_QUEUE, key=PASSWORD)
    ctrl.disable_input(DEFAULT_QUEUE, key=PASSWORD)
    tests.append(
        run_status_case(
            "RENAB-STATUS-TEST-01",
            "RE-ENABLE PREVIOUSLY DISABLED",
            STATUS_SUCCESS,
            ctrl.enable_input(DEFAULT_QUEUE, key=PASSWORD),
        )
    )

    return tests


def write_test_results(writer: ReportWriter, results: List[StatusTestCase]) -> None:
    for case in results:
        line = (
            f"   {pad(case.feature, 18)} "
            f"{pad(case.outcome, 5)} "
            f"{pad(case.paragraph, 20)} "
            f"{case.computed_display} "
            f"{case.correct_display} "
            f"{pad(case.remark, 30)}"
        )
        writer.write(line.rstrip())
    writer.blank_line()


def finalize_report(writer: ReportWriter, results: List[StatusTestCase]) -> None:
    pass_count = sum(1 for case in results if case.outcome == "PASS")
    fail_count = sum(1 for case in results if case.outcome.startswith("FAIL"))
    info_count = sum(1 for case in results if case.outcome == "INFO")
    writer.write(HYPHEN_LINE)
    writer.blank_line()
    writer.write(f" END OF TEST-  {TEST_ID}")
    writer.write(f" TOTAL PASSES : {pass_count:03d}")
    writer.write(f" TOTAL FAILS  : {fail_count:03d}")
    writer.write(f" INFO RESULTS : {info_count:03d}")
    writer.write(" FOR OFFICIAL USE ONLY")
    writer.write(" ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.")
    writer.write(" COPYRIGHT 1974")
    writer.blank_line()


def main() -> None:
    writer = ReportWriter(REPORT_PATH)
    controller = MessageQueueController(DEFAULT_QUEUE, PASSWORD, VALID_SUBQUEUES)
    controller.load_messages(generate_demo_messages())

    head_routine(writer)

    initial_status = controller.enable_input(DEFAULT_QUEUE, key=PASSWORD)
    writer.write(f" INITIAL ENABLE RETURNED STATUS CODE OF {initial_status}")
    writer.write(HYPHEN_LINE)
    writer.blank_line()

    log_messages(writer, controller)
    writer.write(HYPHEN_LINE)
    writer.blank_line()

    write_status_section(writer)
    results = run_status_tests()
    write_test_results(writer, results)
    finalize_report(writer, results)

    writer.flush()


if __name__ == "__main__":
    main()
