from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from textwrap import dedent
from time import perf_counter
from typing import Dict, List, Optional, Tuple

REPORT_PATH = Path(__file__).with_name("CM102M_report.log")


STATUS_DESCRIPTIONS = {
    "00": "Successful completion",
    "10": "Destination disabled",
    "20": "Destination undefined",
    "30": "Invalid destination count",
    "40": "Invalid password",
    "50": "Character count excessive",
    "90": "Combination error",
}


@dataclass(frozen=True)
class Status:
    code: str
    error: str = "0"

    def as_pair(self) -> str:
        return f"{self.code}/{self.error}"

    def describe(self) -> str:
        return STATUS_DESCRIPTIONS.get(self.code, "Unknown status")


@dataclass
class TestCaseResult:
    name: str
    remark: str
    expected: Optional[Status]
    actual: Status
    passed: bool
    detail: str = ""

    @property
    def expected_display(self) -> str:
        return self.expected.as_pair() if self.expected else "n/a"


@dataclass
class MessageLogEntry:
    timestamp: datetime
    elapsed_seconds: float
    status: Status
    length: int
    mode: str
    message: str
    detail: str = ""


class CommunicationQueue:
    """Minimal simulator for the COBOL communication section operations."""

    def __init__(self, destinations: List[str], password: str) -> None:
        self._password = password
        self._destinations: Dict[str, Dict[str, object]] = {
            name: {"enabled": True, "messages": []} for name in destinations
        }

    def disable(self, destination: str, password: str, count: int) -> Tuple[Status, str]:
        issues = self._validate_common(destination, count)
        if password != self._password:
            issues.append((Status("40", "0"), "Password rejected."))
        if issues:
            return self._combine_issues(issues)
        self._destinations[destination]["enabled"] = False
        return Status("00", "0"), "Destination disabled."

    def enable(self, destination: str, password: str, count: int) -> Tuple[Status, str]:
        issues = self._validate_common(destination, count)
        if password != self._password:
            issues.append((Status("40", "0"), "Password rejected."))
        if issues:
            return self._combine_issues(issues)
        self._destinations[destination]["enabled"] = True
        return Status("00", "0"), "Destination enabled."

    def send(
        self,
        destination: str,
        payload: str,
        requested_length: int,
        *,
        dest_count: int,
        mode: str,
    ) -> Tuple[Status, str]:
        issues = self._validate_common(destination, dest_count)
        if not issues:
            dest_state = self._destinations[destination]
            if not dest_state["enabled"]:
                issues.append((Status("10", "0"), f"Destination '{destination}' is disabled."))
        if requested_length < 0:
            issues.append((Status("30", "0"), "Text length cannot be negative."))
        elif requested_length > len(payload or ""):
            issues.append((Status("50", "0"), "Requested length exceeds payload bytes."))
        if issues:
            return self._combine_issues(issues)

        trimmed = (payload or "")[:requested_length]
        record = {"mode": mode, "text": trimmed, "length": requested_length}
        self._destinations[destination]["messages"].append(record)
        return Status("00", "0"), f"Message accepted via {mode}."

    def _validate_common(self, destination: str, count: int) -> List[Tuple[Status, str]]:
        issues: List[Tuple[Status, str]] = []
        if count <= 0:
            issues.append((Status("30", "0"), "Destination count must be greater than zero."))
        if not destination or not destination.strip():
            issues.append((Status("20", "1"), "Destination not provided."))
        elif destination not in self._destinations:
            issues.append((Status("20", "1"), f"Destination '{destination}' is undefined."))
        return issues

    @staticmethod
    def _combine_issues(issues: List[Tuple[Status, str]]) -> Tuple[Status, str]:
        if len(issues) == 1:
            return issues[0]
        detail = "; ".join(msg for _, msg in issues if msg)
        return Status("90", "0"), detail or "Multiple validation errors."


class CM102MTestHarness:
    """Python modernization of the CM102M COBOL validation program."""

    DEFAULT_DESTINATION = "PRIMARY-DEST"
    PASSWORD = "PASSWORD1"

    def __init__(self) -> None:
        self.queue = CommunicationQueue([self.DEFAULT_DESTINATION], self.PASSWORD)
        self.results: List[TestCaseResult] = []
        self.message_log: List[MessageLogEntry] = []

    def run(self) -> List[TestCaseResult]:
        self._run_disable_status_tests()
        self._run_send_status_tests()
        self._run_enable_status_tests()
        self._run_message_walkthrough()
        self._write_report()
        return self.results

    # ------------------------------------------------------------------ #
    # Test suites
    # ------------------------------------------------------------------ #
    def _run_disable_status_tests(self) -> None:
        self._record(
            "DISAB-STATUS-TEST-01",
            "Initial disable against valid destination.",
            Status("00", "0"),
            *self.queue.disable(self.DEFAULT_DESTINATION, self.PASSWORD, 1),
        )
        self.queue.enable(self.DEFAULT_DESTINATION, self.PASSWORD, 1)

        self._record(
            "DISAB-STATUS-TEST-02",
            "Destination symbol unknown.",
            Status("20", "1"),
            *self.queue.disable("GARBAGE", self.PASSWORD, 1),
        )

        self._record(
            "DISAB-STATUS-TEST-03",
            "Invalid password supplied.",
            Status("40", "0"),
            *self.queue.disable(self.DEFAULT_DESTINATION, "BADPASS", 1),
        )

        self._record(
            "DISAB-STATUS-TEST-04",
            "Invalid destination count (0).",
            Status("30", "0"),
            *self.queue.disable(self.DEFAULT_DESTINATION, self.PASSWORD, 0),
        )

        self._record(
            "DISAB-STATUS-TEST-05",
            "Combination error (blank destination, zero count, bad password).",
            None,
            *self.queue.disable("", "BADPASS", 0),
        )

    def _run_send_status_tests(self) -> None:
        self.queue.enable(self.DEFAULT_DESTINATION, self.PASSWORD, 1)
        self.queue.disable(self.DEFAULT_DESTINATION, self.PASSWORD, 1)
        self._record(
            "SEND-STATUS-TEST-01",
            "Attempt to send while destination disabled.",
            Status("10", "0"),
            *self.queue.send(
                self.DEFAULT_DESTINATION,
                "CM102M - I am the first message in queue;",
                45,
                dest_count=1,
                mode="EMI",
            ),
        )
        self.queue.enable(self.DEFAULT_DESTINATION, self.PASSWORD, 1)

        self._record(
            "SEND-STATUS-TEST-02",
            "Combination error (blank destination and zero count).",
            None,
            *self.queue.send(
                "",
                "This message should never be accepted.",
                37,
                dest_count=0,
                mode="EMI",
            ),
        )

        self._record(
            "SEND-STATUS-TEST-03",
            "Destination unknown.",
            Status("20", "1"),
            *self.queue.send(
                "GARBAGE",
                "Unknown destination scenario.",
                30,
                dest_count=1,
                mode="EMI",
            ),
        )

        self._record(
            "SEND-STATUS-TEST-04",
            "Destination count invalid (0).",
            Status("30", "0"),
            *self.queue.send(
                self.DEFAULT_DESTINATION,
                "Count must be positive.",
                24,
                dest_count=0,
                mode="EMI",
            ),
        )

        text = "Character count excessive test."
        self._record(
            "SEND-STATUS-TEST-05",
            "Character count exceeds provided payload.",
            Status("50", "0"),
            *self.queue.send(
                self.DEFAULT_DESTINATION,
                text,
                len(text) + 1,
                dest_count=1,
                mode="EMI",
            ),
        )

    def _run_enable_status_tests(self) -> None:
        self._record(
            "ENABL-STATUS-TEST-01",
            "Destination not specified while enabling.",
            Status("20", "1"),
            *self.queue.enable("", self.PASSWORD, 1),
        )

        self._record(
            "ENABL-STATUS-TEST-02",
            "Invalid destination count during enable.",
            Status("30", "0"),
            *self.queue.enable(self.DEFAULT_DESTINATION, self.PASSWORD, 0),
        )

        self._record(
            "ENABL-STATUS-TEST-03",
            "Invalid password during enable.",
            Status("40", "0"),
            *self.queue.enable(self.DEFAULT_DESTINATION, "BADPASS", 1),
        )

        self.queue.disable(self.DEFAULT_DESTINATION, self.PASSWORD, 1)
        self._record(
            "ENABL-STATUS-TEST-04",
            "Valid enable, no error expected.",
            Status("00", "0"),
            *self.queue.enable(self.DEFAULT_DESTINATION, self.PASSWORD, 1),
        )

    # ------------------------------------------------------------------ #
    # Message scenarios
    # ------------------------------------------------------------------ #
    def _run_message_walkthrough(self) -> None:
        self.queue.enable(self.DEFAULT_DESTINATION, self.PASSWORD, 1)

        for number in range(1, 6):
            message = f"THIS IS MESSAGE NO. {number:04d}. -- Sent with EMI."
            self._send_and_log(message, len(message), "EMI")

        self._send_and_log("", 0, "EGI")

        long_msg = (
            "ON PAGE XIII-21, PARAGRAPH 3.5.4(1)C, THE COBOL STANDARD STATES, "
            '"EXCESS CHARACTERS OF A MESSAGE OR MESSAGE SEGMENT WILL NOT BE TRUNCATED. '
            'CHARACTERS WILL BE PACKED TO A SIZE EQUAL TO THAT OF THE PHYSICAL LINE AND '
            'THEN OUTPUTTED TO THE DEVICE.  THE PROCESS CONTINUES ON THE NEXT LINE WITH '
            'THE EXCESS CHARACTERS."  IF THIS ENTIRE PARAGRAPH WAS RECEIVED BY THE '
            "DESIGNATED DEVICE, THEN THE FOREGOING RULE IS SUPPORTED BY THIS COMPILER."
        )
        self._send_and_log(long_msg, len(long_msg), "EMI_AFTER_PAGE")

        self._send_and_log(
            "EXPECT A PAUSE OF UP TO 30 SECONDS BEFORE TRANSMISSION OF NEXT MESSAGE.",
            72,
            "EMI",
        )

        self.queue.disable(self.DEFAULT_DESTINATION, self.PASSWORD, 1)
        self._send_and_log("****  DEVICE DISABLED  ****", 0, "CONTROL")
        self.queue.enable(self.DEFAULT_DESTINATION, self.PASSWORD, 1)
        self._send_and_log("****  DEVICE NOW RE-ENABLED  ****", 0, "CONTROL")

    # ------------------------------------------------------------------ #
    # Helpers
    # ------------------------------------------------------------------ #
    def _record(
        self,
        name: str,
        remark: str,
        expected: Optional[Status],
        actual: Status,
        detail: str,
    ) -> None:
        passed = True if expected is None else actual == expected
        self.results.append(
            TestCaseResult(
                name=name,
                remark=remark,
                expected=expected,
                actual=actual,
                passed=passed,
                detail=detail,
            )
        )

    def _send_and_log(self, message: str, length: int, mode: str) -> None:
        start = datetime.now()
        perf_start = perf_counter()
        status, detail = self.queue.send(
            self.DEFAULT_DESTINATION,
            message,
            length,
            dest_count=1,
            mode=mode,
        )
        elapsed = perf_counter() - perf_start
        self.message_log.append(
            MessageLogEntry(
                timestamp=start,
                elapsed_seconds=elapsed,
                status=status,
                length=length,
                mode=mode,
                message=(message or "")[:72],
                detail=detail,
            )
        )

    def _write_report(self) -> None:
        lines: List[str] = []

        lines.append("CM102M Python Modernization Report")
        lines.append("=" * 72)
        lines.append("Feature: MCS STATUS WORD")
        lines.append("")

        lines.append("Test Results")
        lines.append("-" * 72)
        header = f"{'Test Name':<24}{'Remark':<40}{'Expected':<10}{'Actual':<10}{'Pass':<6}"
        lines.append(header)
        lines.append("-" * 72)
        for result in self.results:
            lines.append(
                f"{result.name:<24}{result.remark:<40}"
                f"{result.expected_display:<10}{result.actual.as_pair():<10}"
                f"{'YES' if result.passed else 'NO':<6}"
            )
            if result.detail:
                lines.append(f"    -> {result.detail}")
        lines.append("")

        lines.append("Message Activity Log")
        lines.append("-" * 72)
        log_header = (
            f"{'Timestamp':<26}{'Elapsed(ms)':>12}{'Status':>10}"
            f"{'Len':>6}{'Mode':<15}Message"
        )
        lines.append(log_header)
        lines.append("-" * 72)
        for entry in self.message_log:
            elapsed_ms = entry.elapsed_seconds * 1000
            lines.append(
                f"{entry.timestamp.isoformat(sep=' ', timespec='seconds'):<26}"
                f"{elapsed_ms:>12.3f}"
                f"{entry.status.as_pair():>10}"
                f"{entry.length:>6}"
                f"{entry.mode:<15}"
                f"{entry.message}"
            )
            if entry.detail:
                lines.append(f"    -> {entry.detail}")
        lines.append("")

        REPORT_PATH.write_text("\n".join(lines), encoding="utf-8")

    # ------------------------------------------------------------------ #


def main() -> None:
    harness = CM102MTestHarness()
    harness.run()
    print(f"CM102M report written to {REPORT_PATH}")


if __name__ == "__main__":
    main()
