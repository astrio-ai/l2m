from __future__ import annotations

import argparse
from dataclasses import dataclass
from datetime import datetime, timedelta
from pathlib import Path
from typing import Iterable, List, Optional


DEFAULT_QUEUE_NAME = "MAIN.QUEUE"
DEFAULT_PASSWORD = "PASSWORD1"
DEFAULT_SUBQUEUES = ("PRIMARY", "SECONDARY", "TERTIARY")


@dataclass
class MessageLogEntry:
    """Represents one incoming message entry in the log."""

    received_at: datetime
    lag_seconds: float
    length: int
    sentinel: str
    queue_depth: int
    poll_count: int
    contents: str


@dataclass
class Scenario:
    """One status-word validation scenario."""

    name: str
    feature: str
    remark: str
    operation: str
    queue_name: str = ""
    subqueue1: Optional[str] = None
    password: Optional[str] = None
    expected_status: Optional[str] = None
    info: bool = False


@dataclass
class ScenarioResult:
    """Holds the outcome of executing a Scenario."""

    scenario: Scenario
    status: str
    passed: bool
    detail: str

    @property
    def label(self) -> str:
        if self.scenario.info:
            return "INFO"
        return "PASS" if self.passed else "FAIL"


@dataclass
class TestSummary:
    """Aggregated statistics for the executed scenarios."""

    passed: int
    failed: int
    info: int

    @property
    def total(self) -> int:
        return self.passed + self.failed

    @property
    def verdict(self) -> str:
        return "PASS" if self.failed == 0 else "FAIL"


class MessageQueueSimulator:
    """
    Lightweight stand-in for the original MCS queue primitives.

    We model just enough behaviour to validate the status-word logic covered
    by the COBOL test suite.
    """

    def __init__(
        self,
        valid_queue_name: str,
        valid_subqueues: Iterable[str],
        valid_password: str,
    ) -> None:
        self.valid_queue_name = valid_queue_name
        self.valid_subqueues = set(valid_subqueues)
        self.valid_password = valid_password
        self.enabled = False

    def execute(self, scenario: Scenario) -> ScenarioResult:
        status, detail = self._determine_status(scenario)

        if status == "00":
            if scenario.operation in {"enable", "re-enable"}:
                self.enabled = True
            elif scenario.operation == "disable":
                self.enabled = False

        expected = scenario.expected_status
        passed = True

        if not scenario.info:
            passed = expected == status if expected is not None else True

        return ScenarioResult(scenario=scenario, status=status, passed=passed, detail=detail)

    def _determine_status(self, scenario: Scenario) -> tuple[str, str]:
        queue_name = scenario.queue_name.strip()

        if not queue_name:
            return "20", "Queue name not specified."

        if queue_name != self.valid_queue_name:
            return "20", f"Queue '{queue_name}' is unknown."

        if scenario.subqueue1 and scenario.subqueue1 not in self.valid_subqueues:
            return "20", f"Sub-queue '{scenario.subqueue1}' is unknown."

        if scenario.password is not None and scenario.password != self.valid_password:
            return "40", "Invalid password supplied."

        return "00", f"{scenario.operation.capitalize()} request accepted."


class ReportWriter:
    """Accumulates human-readable report lines and flushes them to disk."""

    def __init__(self, destination: Path) -> None:
        self.destination = destination
        self.lines: List[str] = []
        self._rendered: Optional[str] = None

    def line(self, text: str = "") -> None:
        self.lines.append(text)

    def section(self, title: str) -> None:
        if self.lines and self.lines[-1] != "":
            self.line()
        self.line(title)
        self.line("-" * len(title))

    def write_header(self, queue_name: str) -> None:
        now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        self.line("FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION SYSTEM")
        self.line("Module: CM101M (modernized in Python)")
        self.line(f"Run timestamp: {now}")
        self.line(f"Target queue : {queue_name}")
        self.line()

    def write_message_log(self, entries: Iterable[MessageLogEntry]) -> None:
        entries = list(entries)
        if not entries:
            self.line("No incoming messages were recorded during this run.")
            return

        header = (
            f"{'Time':<10} {'Lag(s)':>7} {'Len':>5} {'End':>4} "
            f"{'QD':>4} {'Poll Count':>12}  Message"
        )
        self.line(header)
        self.line("-" * len(header))

        for entry in entries:
            time_str = entry.received_at.strftime("%H:%M:%S")
            lag = f"{entry.lag_seconds:7.2f}"
            length = f"{entry.length:5}"
            sentinel = entry.sentinel[:3].rjust(4)
            queue_depth = f"{entry.queue_depth:4}"
            poll_count = f"{entry.poll_count:12}"
            self.line(
                f"{time_str:<10} {lag} {length} {sentinel} "
                f"{queue_depth} {poll_count}  {entry.contents}"
            )

    def write_test_results(self, results: Iterable[ScenarioResult]) -> None:
        header = (
            f"{'Feature Tested':<20} {'Result':<6} {'Scenario':<24} "
            f"{'Actual':<8} {'Expected':<8} Remarks"
        )
        self.line()
        self.line(header)
        self.line("-" * len(header))

        for result in results:
            scenario = result.scenario
            expected = scenario.expected_status or "--"
            row = (
                f"{scenario.feature:<20} {result.label:<6} "
                f"{scenario.name:<24} {result.status:<8} {expected:<8} "
                f"{scenario.remark}"
            )
            self.line(row)
            if result.detail and result.detail not in scenario.remark:
                detail_row = (
                    f"{'':<20} {'':<6} {'':<24} {'':<8} {'':<8} {result.detail}"
                )
                self.line(detail_row)

    def write_summary(self, summary: TestSummary) -> None:
        self.section("Summary")
        self.line(f"Passed : {summary.passed}")
        self.line(f"Failed : {summary.failed}")
        self.line(f"Info   : {summary.info}")
        self.line(f"Verdict: {summary.verdict}")

    def write(self) -> None:
        self.destination.parent.mkdir(parents=True, exist_ok=True)
        payload = "\n".join(self.lines) + "\n"
        self.destination.write_text(payload, encoding="utf-8")
        self._rendered = payload

    @property
    def content(self) -> str:
        if self._rendered is not None:
            return self._rendered
        return "\n".join(self.lines)


class TestRunner:
    """Coordinates the execution of scenarios and builds a summary."""

    def __init__(self, simulator: MessageQueueSimulator) -> None:
        self.simulator = simulator
        self.results: List[ScenarioResult] = []

    def run(self, scenarios: Iterable[Scenario]) -> List[ScenarioResult]:
        self.results = [self.simulator.execute(scenario) for scenario in scenarios]
        return self.results

    def build_summary(self) -> TestSummary:
        passed = sum(
            1 for result in self.results if not result.scenario.info and result.passed
        )
        failed = sum(
            1 for result in self.results if not result.scenario.info and not result.passed
        )
        info = sum(1 for result in self.results if result.scenario.info)
        return TestSummary(passed=passed, failed=failed, info=info)


def build_sample_log_entries(queue_name: str) -> List[MessageLogEntry]:
    base_time = datetime(2023, 9, 14, 10, 15, 0)
    return [
        MessageLogEntry(
            received_at=base_time,
            lag_seconds=0.32,
            length=48,
            sentinel="EGI",
            queue_depth=1,
            poll_count=24,
            contents=f"Initial enable returned status code 00 for {queue_name}.",
        ),
        MessageLogEntry(
            received_at=base_time + timedelta(minutes=2, seconds=12),
            lag_seconds=12.87,
            length=64,
            sentinel="RUN",
            queue_depth=3,
            poll_count=241,
            contents="Mock payload: account batch posted successfully.",
        ),
    ]


def build_status_scenarios(queue_name: str, password: str) -> List[Scenario]:
    feature = "MCS STATUS WORD"
    return [
        Scenario(
            name="REC-STATUS-TEST-01",
            feature=feature,
            remark="Queue name not specified",
            operation="receive",
            queue_name="",
            expected_status="20",
        ),
        Scenario(
            name="REC-STATUS-TEST-02",
            feature=feature,
            remark="Unknown sub-queue-1 specified",
            operation="receive",
            queue_name=queue_name,
            subqueue1="DUMMYNAME",
            expected_status="20",
        ),
        Scenario(
            name="ACCPT-STATUS-TEST-01",
            feature=feature,
            remark="Queue name not specified",
            operation="accept",
            queue_name="",
            expected_status="20",
        ),
        Scenario(
            name="ACCPT-STATUS-TEST-02",
            feature=feature,
            remark="Unknown sub-queue-1 specified",
            operation="accept",
            queue_name=queue_name,
            subqueue1="DUMMYNAME",
            expected_status="20",
        ),
        Scenario(
            name="ENABL-STATUS-TEST-01",
            feature=feature,
            remark="Queue name not specified",
            operation="enable",
            queue_name="",
            password=password,
            expected_status="20",
        ),
        Scenario(
            name="ENABL-STATUS-TEST-02",
            feature=feature,
            remark="Unknown sub-queue-1 specified",
            operation="enable",
            queue_name=queue_name,
            subqueue1="DUMMYNAME",
            password=password,
            expected_status="20",
        ),
        Scenario(
            name="ENABL-STATUS-TEST-03",
            feature=feature,
            remark="Invalid password used",
            operation="enable",
            queue_name=queue_name,
            password="LETMEIN",
            expected_status="40",
        ),
        Scenario(
            name="ENABL-STATUS-TEST-04",
            feature=feature,
            remark="No queue name / wrong password (informational)",
            operation="enable",
            queue_name="",
            password="LETMEIN",
            expected_status="20",
            info=True,
        ),
        Scenario(
            name="DISAB-STATUS-TEST-01",
            feature=feature,
            remark="Queue name not specified",
            operation="disable",
            queue_name="",
            password=password,
            expected_status="20",
        ),
        Scenario(
            name="DISAB-STATUS-TEST-02",
            feature=feature,
            remark="Unknown sub-queue-1 specified",
            operation="disable",
            queue_name=queue_name,
            subqueue1="DUMMYNAME",
            password=password,
            expected_status="20",
        ),
        Scenario(
            name="DISAB-STATUS-TEST-03",
            feature=feature,
            remark="Invalid password used",
            operation="disable",
            queue_name=queue_name,
            password="KILLITNOW",
            expected_status="40",
        ),
        Scenario(
            name="RENAB-STATUS-TEST-01",
            feature=feature,
            remark="Re-enable previously disabled queue",
            operation="re-enable",
            queue_name=queue_name,
            password=password,
            expected_status="00",
        ),
    ]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Modern Python harness for the CM101M COBOL module. "
            "Generates a status-test report similar to the original program."
        )
    )
    parser.add_argument(
        "--report",
        type=Path,
        default=Path("report.log"),
        help="Destination file for the generated report (default: report.log).",
    )
    parser.add_argument(
        "--queue-name",
        default=DEFAULT_QUEUE_NAME,
        help=f"Logical queue name to test (default: {DEFAULT_QUEUE_NAME}).",
    )
    parser.add_argument(
        "--password",
        default=DEFAULT_PASSWORD,
        help=f"Administrative password (default: {DEFAULT_PASSWORD}).",
    )
    parser.add_argument(
        "--show",
        action="store_true",
        help="Also print the generated report to stdout.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()

    simulator = MessageQueueSimulator(
        valid_queue_name=args.queue_name,
        valid_subqueues=DEFAULT_SUBQUEUES,
        valid_password=args.password,
    )
    report = ReportWriter(args.report)

    report.write_header(args.queue_name)
    report.section("Incoming Message Log")
    report.write_message_log(build_sample_log_entries(args.queue_name))

    report.section("Begin Input Status Tests")
    report.line("MCS status word validation for queue operations.")
    report.line("")

    scenarios = build_status_scenarios(args.queue_name, args.password)
    runner = TestRunner(simulator)
    results = runner.run(scenarios)
    report.write_test_results(results)
    summary = runner.build_summary()
    report.write_summary(summary)
    report.write()

    if args.show:
        print(report.content)


if __name__ == "__main__":
    main()
