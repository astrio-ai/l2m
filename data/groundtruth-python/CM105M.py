"""Modern Python conversion of the CM105M COBOL communications-queue validation program."""
from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Sequence

DEFAULT_REPORT_PATH = Path("report.log")
DEFAULT_MIN_MESSAGES = 10


def pad_field(value: str, length: int) -> str:
    text = value or ""
    if len(text) > length:
        return text[:length]
    return text.ljust(length)


def format_result_line(
    feature: str,
    status: str,
    paragraph: str,
    computed: str = "",
    correct: str = "",
    remark: str = "",
) -> str:
    return (
        " "
        + pad_field(feature, 18)
        + " "
        + pad_field(status, 5)
        + " "
        + pad_field(paragraph, 20)
        + " "
        + pad_field(computed, 20)
        + " "
        + pad_field(correct, 20)
        + " "
        + pad_field(remark, 30)
    )


def _to_width(text: str, width: int = 120) -> str:
    return text[:width].ljust(width) if len(text) > width else text.ljust(width)


def _center(text: str, width: int = 120) -> str:
    trimmed = text.strip()
    return trimmed[:width] if len(trimmed) >= width else trimmed.center(width)


def build_ccvs_h2(test_id: str) -> str:
    line = "CCVS74 NCC COPY, NOT FOR DISTRIBUTION. TEST RESULTS SET-  "
    return _center(f"{line}{test_id}")


def build_ccvs_e1(test_id: str) -> str:
    prefix = " " * 52
    line = f"{prefix}END OF TEST-  {test_id:<9} NTIS DISTRIBUTION COBOL 74"
    return _to_width(line)


def build_ccvs_e2(count_text: str, description: str) -> str:
    prefix = " " * 52
    number = count_text.rjust(3)
    desc = description[:44].ljust(44)
    return _to_width(f"{prefix}{number} {desc}")


CCVS_H1 = _center(
    "FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION SYSTEM"
)
CCVS_H3 = _center(
    "FOR OFFICIAL USE ONLY    COBOL 85 VERSION 4.2, Apr 1993 SSVG    COPYRIGHT 1974"
)
CCVS_E3 = _center(
    "FOR OFFICIAL USE ONLY - ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH. - COPYRIGHT 1974"
)
HYPHEN_LINE = _to_width(" " + "*" * 118)
COLUMNS_LINE_1 = format_result_line(
    "FEATURE TESTED",
    "RESLT",
    "PARAGRAPH NAME",
    "COMPUTED DATA",
    "CORRECT DATA",
    "REMARKS",
)
COLUMNS_LINE_2 = format_result_line(
    "-" * 18,
    "-" * 5,
    "-" * 20,
    "-" * 20,
    "-" * 20,
    "-" * 30,
)


@dataclass(frozen=True)
class QueueTest:
    feature: str
    queue_set: str
    expected_key: str
    paragraph: str


@dataclass(frozen=True)
class QueueMessage:
    key: str
    contents: str


@dataclass(frozen=True)
class QueueTestResult:
    feature: str
    status: str
    paragraph: str
    computed: str = ""
    correct: str = ""
    remark: str = ""


class ReportWriter:
    """Accumulated output that mirrors the original CM105M report layout."""

    def __init__(self, destination: Path | str) -> None:
        self.path = Path(destination)
        self.lines: List[str] = []

    def line(self, text: str = "") -> None:
        self.lines.append(text)

    def blank_line(self, times: int = 1) -> None:
        for _ in range(times):
            self.line("")

    def write_header(self, test_id: str) -> None:
        self.line(CCVS_H1)
        self.line(build_ccvs_h2(test_id))
        self.line(CCVS_H3)
        self.line(HYPHEN_LINE)

    def write_column_headers(self) -> None:
        self.line(COLUMNS_LINE_1)
        self.line(COLUMNS_LINE_2)
        self.blank_line()

    def write_result(self, result: QueueTestResult) -> None:
        self.line(
            format_result_line(
                result.feature,
                result.status,
                result.paragraph,
                result.computed,
                result.correct,
                result.remark,
            )
        )

    def write_footer(self, test_id: str, errors: int, deletes: int) -> None:
        self.line(HYPHEN_LINE)
        self.blank_line(4)
        self.line(build_ccvs_e1(test_id))
        self.blank_line()
        error_text = " NO" if errors == 0 else f"{errors:3d}"
        self.line(build_ccvs_e2(error_text, "ERRORS ENCOUNTERED"))
        delete_text = " NO" if deletes == 0 else f"{deletes:3d}"
        self.line(build_ccvs_e2(delete_text, "TESTS DELETED"))
        self.line(CCVS_E3)

    def save(self) -> None:
        self.path.parent.mkdir(parents=True, exist_ok=True)
        content = self.content
        self.path.write_text(content, encoding="utf-8")

    @property
    def content(self) -> str:
        if not self.lines:
            return ""
        return "\n".join(self.lines) + "\n"


class MessageQueueSystem:
    """Simple in-memory recreation of the CM105M communication queues."""

    BASE_QUEUES: Sequence[str] = ("PPPP", "PPPS", "PPSP", "PSPP")
    ALIAS_MAP: Dict[str, str] = {
        "P": "PPPP",
        "PP": "PPPP",
        "PPP": "PPPP",
        "PS": "PSPP",
        "PSP": "PSPP",
        "PPS": "PPSP",
    }

    def __init__(self, min_messages: int = DEFAULT_MIN_MESSAGES) -> None:
        self.min_messages = max(1, min_messages)
        self.queues: Dict[str, List[str]] = {name: [] for name in self.BASE_QUEUES}
        self._prime_queues()

    def _prime_queues(self) -> None:
        for name in self.queues:
            self._ensure_min_messages(name)

    def _ensure_min_messages(self, queue_name: str) -> None:
        queue = self.queues[queue_name]
        while len(queue) < self.min_messages:
            index = len(queue) + 1
            queue.append(f"{queue_name}{index:02d} MESSAGE {index:02d}")

    def _resolve_queue(self, queue_name: str) -> str | None:
        normalized = queue_name.strip().upper()
        if normalized in self.queues:
            return normalized
        return self.ALIAS_MAP.get(normalized)

    def receive(self, queue_name: str) -> QueueMessage:
        actual = self._resolve_queue(queue_name)
        if actual is None:
            raise KeyError(f"Unknown queue set '{queue_name.strip()}'")
        queue = self.queues[actual]
        if not queue:
            raise RuntimeError(f"No messages available for queue {actual}")
        payload = queue.pop(0)
        key = payload[:4].strip() or actual
        return QueueMessage(key=key, contents=payload[:30])

    def accept_count(self, queue_name: str) -> tuple[str, int]:
        actual = self._resolve_queue(queue_name)
        if actual is None:
            return "91", 0
        return "00", len(self.queues[actual])


class CM105MRunner:
    """Coordinates the queue tests, accept tests, and report generation."""

    TEST_ID = "CM105M"
    QUEUE_TESTS = [
        QueueTest("QUEUE SERIES PPPP", "PPPP", "PPPP", "QUEUE-TEST-01"),
        QueueTest("QUEUE SERIES PPPS", "PPPS", "PPPS", "QUEUE-TEST-02"),
        QueueTest("QUEUE SERIES PPSP", "PPSP", "PPSP", "QUEUE-TEST-03"),
        QueueTest("QUEUE SERIES PSPP", "PSPP", "PSPP", "QUEUE-TEST-04"),
        QueueTest("QUEUE SERIES P", "P", "PPPP", "QUEUE-TEST-05"),
        QueueTest("QUEUE SERIES PP", "PP", "PPPP", "QUEUE-TEST-06"),
        QueueTest("QUEUE SERIES PPP", "PPP", "PPPP", "QUEUE-TEST-07"),
        QueueTest("QUEUE SERIES PS", "PS", "PSPP", "QUEUE-TEST-08"),
        QueueTest("QUEUE SERIES PSP", "PSP", "PSPP", "QUEUE-TEST-09"),
        QueueTest("QUEUE SERIES PPS", "PPS", "PPSP", "QUEUE-TEST-10"),
    ]
    ACCEPT_QUEUE_ORDER = [
        "PPPP",
        "PPPS",
        "PPSP",
        "PSPP",
        "P",
        "PP",
        "PPP",
        "PS",
        "PSP",
        "PPS",
    ]

    def __init__(
        self,
        report_path: Path | str = DEFAULT_REPORT_PATH,
        min_messages: int = DEFAULT_MIN_MESSAGES,
    ) -> None:
        self.report_path = Path(report_path)
        self.writer = ReportWriter(self.report_path)
        self.queue_system = MessageQueueSystem(min_messages=min_messages)
        self.error_counter = 0
        self.delete_counter = 0
        self.results: List[QueueTestResult] = []

    def run(self, show: bool = False) -> Path:
        self.writer.write_header(self.TEST_ID)
        self.writer.write_column_headers()
        self.run_queue_tests()
        self.run_accept_tests()
        self.writer.write_footer(self.TEST_ID, self.error_counter, self.delete_counter)
        self.writer.save()
        if show:
            print(self.writer.content, end="")
        return self.report_path

    def run_queue_tests(self) -> None:
        for test in self.QUEUE_TESTS:
            self.execute_queue_test(test)

    def execute_queue_test(self, test: QueueTest) -> None:
        try:
            message = self.queue_system.receive(test.queue_set)
            success = message.key == test.expected_key
            remark = message.contents
        except Exception as exc:  # noqa: BLE001
            success = False
            remark = f"{test.queue_set.strip()} {exc}"

        if success:
            result = QueueTestResult(
                feature=test.feature,
                status="PASS",
                paragraph=test.paragraph,
                remark=remark,
            )
        else:
            self.error_counter += 1
            result = QueueTestResult(
                feature=test.feature,
                status="FAIL*",
                paragraph=test.paragraph,
                computed=" SEE REMARKS COLUMN ",
                correct=test.expected_key,
                remark=remark,
            )

        self.record_result(result)

    def run_accept_tests(self) -> None:
        feature = "ACCEPT GROUP QUEUE"
        paragraph = "ACCEPT-TEST-01"
        for queue_name in self.ACCEPT_QUEUE_ORDER:
            status_code, count = self.queue_system.accept_count(queue_name)
            if status_code == "00":
                computed = f"{count:02d}"
                remark = f"COUNT FOR {queue_name}"
            else:
                computed = status_code
                remark = f"BAD STATUS FOR {queue_name}"
            result = QueueTestResult(
                feature=feature,
                status="INFO",
                paragraph=paragraph,
                computed=computed,
                remark=remark,
            )
            self.record_result(result)

    def record_result(self, result: QueueTestResult) -> None:
        self.results.append(result)
        self.writer.write_result(result)


def run(
    report_path: Path | str = DEFAULT_REPORT_PATH,
    *,
    min_messages: int = DEFAULT_MIN_MESSAGES,
    show: bool = False,
) -> Path:
    """Convenience helper for programmatic use."""
    runner = CM105MRunner(report_path=report_path, min_messages=min_messages)
    return runner.run(show=show)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Python translation of the CM105M COBOL queue validation program."
    )
    parser.add_argument(
        "--report",
        type=Path,
        default=DEFAULT_REPORT_PATH,
        help="Destination for the generated report (default: report.log).",
    )
    parser.add_argument(
        "--min-messages",
        type=int,
        default=DEFAULT_MIN_MESSAGES,
        help="Minimum number of messages to seed into each base queue (default: 10).",
    )
    parser.add_argument(
        "--show",
        action="store_true",
        help="Also print the generated report to stdout.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    run(report_path=args.report, min_messages=args.min_messages, show=args.show)


if __name__ == "__main__":
    main()
