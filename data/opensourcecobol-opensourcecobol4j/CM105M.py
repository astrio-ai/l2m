"""Modernized Python implementation of the CM105M COBOL queue validation program.

The original COBOL code exercised a communications queue by ensuring each queue
held a minimum number of messages, validating that the correct queue key was
returned when messages were received, and finally reporting the number of
messages still present. This module recreates that workflow with a modern,
testable Python implementation.

Running the module will produce an easy-to-read report in ``report.log`` that
mirrors the intent of the COBOL output.
"""
from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Tuple

DEFAULT_REPORT_PATH = Path("report.log")

BASE_QUEUE_NAMES = ("PPPP", "PPPS", "PPSP", "PSPP")

QUEUE_ALIASES = {
    "PPPP": "PPPP",
    "PPPS": "PPPS",
    "PPSP": "PPSP",
    "PSPP": "PSPP",
    "P": "PPPP",
    "PP": "PPPP",
    "PPP": "PPPP",
    "PS": "PSPP",
    "PSP": "PSPP",
    "PPS": "PPSP",
}

QUEUE_TESTS = [
    ("QUEUE SERIES PPPP", "PPPP", "PPPP", "QUEUE-TEST-01"),
    ("QUEUE SERIES PPPS", "PPPS", "PPPS", "QUEUE-TEST-02"),
    ("QUEUE SERIES PPSP", "PPSP", "PPSP", "QUEUE-TEST-03"),
    ("QUEUE SERIES PSPP", "PSPP", "PSPP", "QUEUE-TEST-04"),
    ("QUEUE SERIES P", "P", "PPPP", "QUEUE-TEST-05"),
    ("QUEUE SERIES PP", "PP", "PPPP", "QUEUE-TEST-06"),
    ("QUEUE SERIES PPP", "PPP", "PPPP", "QUEUE-TEST-07"),
    ("QUEUE SERIES PS", "PS", "PSPP", "QUEUE-TEST-08"),
    ("QUEUE SERIES PSP", "PSP", "PSPP", "QUEUE-TEST-09"),
    ("QUEUE SERIES PPS", "PPS", "PPSP", "QUEUE-TEST-10"),
]

ACCEPT_SEQUENCE = ["PPPP", "PPPS", "PPSP", "PSPP", "P", "PP", "PPP", "PS", "PSP", "PPS"]


class QueueEmpty(RuntimeError):
    """Raised when a queue does not contain any messages."""


@dataclass
class QueueMessage:
    """Represents a single message stored in a queue."""

    queue_key: str
    payload: str

    def display(self) -> str:
        """Return a human-readable representation similar to the COBOL remark field."""
        return f"{self.queue_key} {self.payload}"


@dataclass
class TestResult:
    """Container for a single test outcome."""

    feature: str
    paragraph: str
    status: str
    computed: str = ""
    correct: str = ""
    remark: str = ""


class QueueManager:
    """In-memory simulation of the communication queue exercised by CM105M."""

    def __init__(self, queues: Dict[str, List[QueueMessage]] | None = None) -> None:
        self.base_queues: Dict[str, List[QueueMessage]] = {}
        self._sequence_counter: Dict[str, int] = {}

        if queues:
            for name, messages in queues.items():
                self.base_queues[name] = list(messages)
                self._sequence_counter[name] = len(messages)

        for name in BASE_QUEUE_NAMES:
            self.base_queues.setdefault(name, [])
            self._sequence_counter.setdefault(name, len(self.base_queues[name]))

    def ensure_min_depth(self, min_messages: int) -> None:
        """Ensure each base queue has at least ``min_messages`` entries."""
        for name in BASE_QUEUE_NAMES:
            queue = self.base_queues.setdefault(name, [])
            while len(queue) < min_messages:
                queue.append(self._generate_message(name))

    def receive(self, queue_set_name: str) -> QueueMessage:
        """Retrieve the next message for the requested queue (or alias)."""
        base_name = self._resolve_queue_name(queue_set_name)
        queue = self.base_queues.get(base_name, [])
        if not queue:
            raise QueueEmpty(
                f"No data available for queue {queue_set_name} (resolved to {base_name})."
            )
        return queue.pop(0)

    def get_queue_status(self, queue_set_name: str) -> Tuple[str, int]:
        """Return a tuple of (status, message_count) for the requested queue (or alias)."""
        base_name = self._resolve_queue_name(queue_set_name)
        count = len(self.base_queues.get(base_name, []))
        return "00", count

    def _generate_message(self, queue_name: str) -> QueueMessage:
        next_seq = self._sequence_counter.get(queue_name, 0) + 1
        self._sequence_counter[queue_name] = next_seq
        payload = f"Generated message #{next_seq:02d} for queue {queue_name}"
        return QueueMessage(queue_key=queue_name, payload=payload)

    @staticmethod
    def _resolve_queue_name(queue_set_name: str) -> str:
        try:
            return QUEUE_ALIASES[queue_set_name]
        except KeyError as exc:
            raise ValueError(f"Unknown queue name '{queue_set_name}'.") from exc


class ReportWriter:
    """Produces a textual report that mirrors the COBOL print file."""

    HEADER_LINES = [
        "FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION SYSTEM",
        "CM105M Modernized Queue Validation (Python Edition)",
        "---------------------------------------------------------------",
    ]
    COLUMN_LINE_1 = (
        f"{'FEATURE TESTED':<20} {'RESULT':<6} {'PARAGRAPH NAME':<18} "
        f"{'COMPUTED DATA':<20} {'CORRECT':<14} REMARKS"
    )
    COLUMN_LINE_2 = (
        f"{'-' * 20:<20} {'-' * 6:<6} {'-' * 18:<18} "
        f"{'-' * 20:<20} {'-' * 14:<14} {'-' * 30}"
    )

    def __init__(self, path: Path) -> None:
        self.path = path
        self.results: List[TestResult] = []

    def add_pass(self, feature: str, paragraph: str, remark: str) -> None:
        self._add_result(feature, paragraph, "PASS", remark=remark)

    def add_fail(
        self, feature: str, paragraph: str, computed: str, correct: str, remark: str
    ) -> None:
        self._add_result(
            feature,
            paragraph,
            "FAIL",
            computed=computed,
            correct=correct,
            remark=remark,
        )

    def add_info(
        self, feature: str, paragraph: str, computed: str, remark: str
    ) -> None:
        self._add_result(
            feature,
            paragraph,
            "INFO",
            computed=computed,
            remark=remark,
        )

    def finalize(self) -> None:
        """Write the accumulated report to disk."""
        self.path.parent.mkdir(parents=True, exist_ok=True)
        with self.path.open("w", encoding="utf-8") as handle:
            for line in self.HEADER_LINES:
                handle.write(line + "\n")
            handle.write("\n")
            handle.write(self.COLUMN_LINE_1 + "\n")
            handle.write(self.COLUMN_LINE_2 + "\n")

            for result in self.results:
                handle.write(self._format_result(result) + "\n")

            handle.write("-" * 120 + "\n")
            handle.write(self._summary_line() + "\n")

    def _add_result(
        self,
        feature: str,
        paragraph: str,
        status: str,
        computed: str = "",
        correct: str = "",
        remark: str = "",
    ) -> None:
        self.results.append(
            TestResult(
                feature=feature,
                paragraph=paragraph,
                status=status,
                computed=computed,
                correct=correct,
                remark=remark,
            )
        )

    @staticmethod
    def _clip(value: str, width: int) -> str:
        if len(value) <= width:
            return value.ljust(width)
        return value[: width - 1] + "…"

    def _format_result(self, result: TestResult) -> str:
        feature = self._clip(result.feature, 20)
        status = self._clip(result.status, 6)
        paragraph = self._clip(result.paragraph, 18)
        computed = self._clip(result.computed, 20)
        correct = self._clip(result.correct, 14)
        return (
            f"{feature} {status} {paragraph} {computed} {correct} {result.remark or ''}"
        )

    def _summary_line(self) -> str:
        pass_count = sum(1 for r in self.results if r.status == "PASS")
        fail_count = sum(1 for r in self.results if r.status == "FAIL")
        info_count = sum(1 for r in self.results if r.status == "INFO")
        return (
            f"Summary: {pass_count} passed, {fail_count} failed, "
            f"{info_count} informational."
        )


class CM105MTestSuite:
    """Coordinates the queue validation workflow."""

    def __init__(
        self, manager: QueueManager, report: ReportWriter, min_depth: int
    ) -> None:
        self.manager = manager
        self.report = report
        self.min_depth = min_depth

    def run(self) -> None:
        """Execute all queue and acceptance tests."""
        self.manager.ensure_min_depth(self.min_depth)
        self._run_queue_tests()
        self._run_acceptance_checks()
        self.report.finalize()

    def _run_queue_tests(self) -> None:
        for feature, queue_name, expected_key, paragraph in QUEUE_TESTS:
            self._queue_test(feature, queue_name, expected_key, paragraph)

    def _queue_test(
        self, feature: str, queue_name: str, expected_key: str, paragraph: str
    ) -> None:
        try:
            message = self.manager.receive(queue_name)
        except QueueEmpty:
            self.report.add_fail(
                feature=feature,
                paragraph=paragraph,
                computed="NO DATA",
                correct=expected_key,
                remark="Nothing received from queue manager.",
            )
            return
        except ValueError as error:
            self.report.add_fail(
                feature=feature,
                paragraph=paragraph,
                computed="INVALID QUEUE",
                correct=expected_key,
                remark=str(error),
            )
            return

        if message.queue_key == expected_key:
            self.report.add_pass(feature, paragraph, remark=message.display())
        else:
            self.report.add_fail(
                feature=feature,
                paragraph=paragraph,
                computed=message.queue_key,
                correct=expected_key,
                remark=message.display(),
            )

    def _run_acceptance_checks(self) -> None:
        for queue_name in ACCEPT_SEQUENCE:
            status, count = self.manager.get_queue_status(queue_name)
            if status == "00":
                computed = f"{count:02d}"
                remark = f"COUNT FOR {queue_name}"
            else:
                computed = status
                remark = f"BAD STATUS FOR {queue_name}"
            self.report.add_info(
                feature="ACCEPT GROUP QUEUE",
                paragraph="ACCEPT-TEST-01",
                computed=computed,
                remark=remark,
            )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Modernized execution of the CM105M queue validation suite."
    )
    parser.add_argument(
        "--report",
        type=Path,
        default=DEFAULT_REPORT_PATH,
        help="Destination for the generated report (default: report.log).",
    )
    parser.add_argument(
        "--min-depth",
        type=int,
        default=10,
        help="Minimum number of messages seeded per base queue (default: 10).",
    )
    return parser.parse_args()


def run(report_path: Path = DEFAULT_REPORT_PATH, min_depth: int = 10) -> Path:
    """Convenience function for running the suite programmatically."""
    manager = QueueManager()
    report = ReportWriter(report_path)
    suite = CM105MTestSuite(manager, report, min_depth=min_depth)
    suite.run()
    return report_path


def main() -> None:
    args = parse_args()
    run(report_path=args.report, min_depth=args.min_depth)


if __name__ == "__main__":
    main()
