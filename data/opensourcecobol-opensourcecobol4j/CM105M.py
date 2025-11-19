"""Python translation of the CM105M COBOL communications-queue validation program."""
from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Tuple

DEFAULT_REPORT_PATH = Path("report.log")


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


class ReportWriter:
    """Accumulates the formatted report lines and writes them to disk."""

    def __init__(self, output_path: str | Path):
        self.path = Path(output_path)
        self.lines: List[str] = []

    def write_line(self, text: str = "") -> None:
        self.lines.append(text.rstrip("\n"))

    def save(self) -> None:
        self.path.parent.mkdir(parents=True, exist_ok=True)
        content = "\n".join(self.lines)
        if content and not content.endswith("\n"):
            content += "\n"
        self.path.write_text(content, encoding="utf-8")


class MessageQueueSystem:
    """Simple in-memory stand-in for the COBOL communications queues."""

    BASE_QUEUES = ("PPPP", "PPPS", "PPSP", "PSPP")
    ALIAS_MAP = {
        "P": "PPPP",
        "PP": "PPPP",
        "PPP": "PPPP",
        "PS": "PSPP",
        "PSP": "PSPP",
        "PPS": "PPSP",
    }

    def __init__(self, min_messages: int = 10) -> None:
        self.min_messages = min_messages
        self.queues: Dict[str, List[str]] = {name: [] for name in self.BASE_QUEUES}
        self.build_up_queues()

    def build_up_queues(self) -> None:
        for name in self.queues:
            self._ensure_min_messages(name)

    def _ensure_min_messages(self, name: str) -> None:
        queue = self.queues[name]
        while len(queue) < self.min_messages:
            index = len(queue) + 1
            queue.append(f"{name}{index:02d} MESSAGE {index:02d}")

    def resolve_queue(self, queue_set: str) -> str | None:
        if queue_set in self.queues:
            return queue_set
        return self.ALIAS_MAP.get(queue_set)

    def receive(self, queue_set: str) -> Tuple[str, str]:
        actual = self.resolve_queue(queue_set)
        if actual is None:
            raise KeyError(f"Unknown queue set '{queue_set}'")
        queue = self.queues[actual]
        if not queue:
            raise RuntimeError(f"No messages available for queue {actual}")
        message = queue.pop(0)
        return message[:4], message[:30]

    def accept_count(self, queue_set: str) -> Tuple[str, int]:
        actual = self.resolve_queue(queue_set)
        if actual is None:
            return "91", 0
        return "00", len(self.queues[actual])


class CM105MReport:
    """Recreates the PROCEDURE DIVISION of CM105M."""

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

    def __init__(self, output_path: str | Path = DEFAULT_REPORT_PATH) -> None:
        self.test_id = "CM105M"
        self.writer = ReportWriter(output_path)
        self.queue_system = MessageQueueSystem()
        self.error_counter = 0
        self.delete_counter = 0

    def run(self) -> None:
        self.write_header()
        self.write_column_names()
        self.run_queue_tests()
        self.run_accept_tests()
        self.write_footer()
        self.writer.save()

    def write_header(self) -> None:
        self.writer.write_line(CCVS_H1)
        self.writer.write_line(build_ccvs_h2(self.test_id))
        self.writer.write_line(CCVS_H3)
        self.writer.write_line(HYPHEN_LINE)

    def write_column_names(self) -> None:
        self.writer.write_line(COLUMNS_LINE_1)
        self.writer.write_line(COLUMNS_LINE_2)
        self.blank_line()

    def run_queue_tests(self) -> None:
        for test in self.QUEUE_TESTS:
            self.execute_queue_test(test)

    def execute_queue_test(self, test: QueueTest) -> None:
        feature = test.feature
        paragraph = test.paragraph
        expected = test.expected_key
        remark: str
        try:
            queue_key, message = self.queue_system.receive(test.queue_set)
            success = queue_key == expected
            remark = message
        except Exception as exc:  # noqa: BLE001
            success = False
            remark = f"{test.queue_set} {exc}"

        if success:
            status = "PASS"
            computed = ""
            correct = ""
        else:
            status = "FAIL*"
            computed = " SEE REMARKS COLUMN "
            correct = expected
            self.error_counter += 1

        self.record_result(feature, status, paragraph, computed, correct, remark)

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
            self.record_result(feature, "INFO", paragraph, computed, "", remark)

    def record_result(
        self,
        feature: str,
        status: str,
        paragraph: str,
        computed: str,
        correct: str,
        remark: str,
    ) -> None:
        line = format_result_line(feature, status, paragraph, computed, correct, remark)
        self.writer.write_line(line)

    def write_footer(self) -> None:
        self.writer.write_line(HYPHEN_LINE)
        self.blank_line(4)
        self.writer.write_line(build_ccvs_e1(self.test_id))
        self.blank_line()
        error_text = " NO" if self.error_counter == 0 else f"{self.error_counter:3d}"
        self.writer.write_line(build_ccvs_e2(error_text, "ERRORS ENCOUNTERED"))
        delete_text = (
            " NO" if self.delete_counter == 0 else f"{self.delete_counter:3d}"
        )
        self.writer.write_line(build_ccvs_e2(delete_text, "TESTS DELETED"))
        self.writer.write_line(CCVS_E3)

    def blank_line(self, times: int = 1) -> None:
        for _ in range(times):
            self.writer.write_line("")


def run(report_path: Path | str = DEFAULT_REPORT_PATH) -> Path:
    """Convenience helper for programmatic use."""
    reporter = CM105MReport(report_path)
    reporter.run()
    return Path(report_path)


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
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    run(args.report)


if __name__ == "__main__":
    main()
