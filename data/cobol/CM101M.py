from __future__ import annotations

import datetime as dt
from collections import deque
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Optional

REPORT_FILE = Path(__file__).with_name("report.log")
HYPHEN_LINE = "*" * 119
CCVS_H_1 = (
    "                           FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION SYSTEM"
)
CCVS_H_2 = (
    "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.    TEST RESULTS SET-   CM101M"
)
CCVS_H_3 = (
    " FOR OFFICIAL USE ONLY        COBOL 85 VERSION 4.2, Apr  1993 SSVG            COPYRIGHT   1974"
)
LOG_HDR_1 = "                                                LOG OF INCOMING MESSAGES"
LOG_HDR_3 = " TIME  RECVD    LOG LAG   LENGTH  END QD            POLL COUNT        MESSAGE CONTENTS"
LOG_HDR_4 = " ----------- ----------- ------- --- -- ---------- ---------------------------------------------"
COLUMNS_LINE_1 = (
    "   FEATURE TESTED    RESLT   PARAGRAPH NAME        COMPUTED DATA         CORRECT DATA         REMARKS"
)
COLUMNS_LINE_2 = (
    " ------------------  -----  --------------------  --------------------  --------------------  ------------------------------"
)
CCVS_E_1_TEMPLATE = "                          END OF TEST-  {test_id:<9}NTIS DISTRIBUTION COBOL 74"
CCVS_E_3 = (
    " FOR OFFICIAL USE ONLY     ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     COPYRIGHT 1974"
)
SPEC_LINE_TEMPLATE = " INITIAL ENABLE RETURNED STATUS CODE OF {status}"


class NoDataAvailable(Exception):
    """Raised when no messages are available in the simulated queue."""


@dataclass
class Message:
    kill_field: str
    content: str
    timestamp: dt.datetime
    end_key: str = "0"
    queue_depth: int = 0
    source: str = "SIMULATED.ORIGIN"

    def payload(self) -> str:
        filler = self.content[:68].ljust(68)
        return f"{self.kill_field[:4].ljust(4)}{filler}"

    @property
    def length(self) -> int:
        return len(self.payload().rstrip())


class ReportWriter:
    """Mimics the COBOL report writer that produced 120-column lines."""

    def __init__(self, path: Path):
        self.path = path
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self.lines: list[str] = []

    def write(self, text: str = "", blank_lines_before: int = 0) -> None:
        for _ in range(blank_lines_before):
            self.lines.append("")
        self.lines.append(self._normalize(text))

    def blank(self, count: int = 1) -> None:
        for _ in range(count):
            self.lines.append("")

    def flush(self) -> None:
        self.path.write_text("\n".join(self.lines) + "\n", encoding="utf-8")

    @staticmethod
    def _normalize(text: str) -> str:
        return text[:120].ljust(120)


class SimulatedQueue:
    """A lightweight stand-in for CM-INQUE-1."""

    def __init__(self, password: str, with_messages: bool = False):
        self.password = password
        self.valid_queue_name = "PRIMARY.IN.QUEUE"
        self.status_key = "99"
        self.enabled = False
        self.configured_queue = ""
        self.configured_subqueues = ("", "", "")
        self.messages: deque[Message] = deque()
        if with_messages:
            self._load_sample_messages()
        self.where_from = "SIMULATED.ORIGIN"
        self.date_received = 0
        self.time_received = dt.datetime.now()
        self.msg_length = 0
        self.msg_count = 0
        self.end_key = "0"

    def configure(self, main_queue: str, sub1: str = "", sub2: str = "", sub3: str = "") -> None:
        self.configured_queue = main_queue.strip()
        self.configured_subqueues = (sub1.strip(), sub2.strip(), sub3.strip())

    def enable_input(self, password: Optional[str] = None) -> bool:
        if not self._configuration_valid():
            return False
        if password is not None and password != self.password:
            self.status_key = "40"
            return False
        self.enabled = True
        self.status_key = "00"
        return True

    def disable_input(self, password: Optional[str] = None) -> bool:
        if not self._configuration_valid():
            return False
        if password is not None and password != self.password:
            self.status_key = "40"
            return False
        self.enabled = False
        self.status_key = "00"
        return True

    def receive_message(self) -> Optional[Message]:
        if not self._configuration_valid():
            return None
        if not self.enabled:
            self.status_key = "90"
            raise RuntimeError("Queue is not enabled.")
        if not self.messages:
            raise NoDataAvailable
        message = self.messages.popleft()
        self.status_key = "00"
        self.where_from = message.source
        self.date_received = int(message.timestamp.strftime("%y%m%d"))
        self.time_received = message.timestamp
        self.msg_length = message.length
        self.msg_count = len(self.messages)
        self.end_key = message.end_key
        return message

    def accept_message_count(self) -> int:
        if not self._configuration_valid():
            return 0
        self.status_key = "00"
        self.msg_count = len(self.messages)
        return self.msg_count

    def sentinel(self) -> str:
        if self.end_key == "3":
            return "EGI"
        if self.end_key == "2":
            return "EMI"
        return self.end_key.rjust(3)

    def _configuration_valid(self) -> bool:
        if not self.configured_queue:
            self.status_key = "20"
            return False
        if any(self.configured_subqueues):
            self.status_key = "20"
            return False
        return True

    def _load_sample_messages(self) -> None:
        now = dt.datetime.now()
        self.messages = deque(
            [
                Message(
                    kill_field="    ",
                    content="Initial payload from remote system.",
                    timestamp=now - dt.timedelta(seconds=5),
                    end_key="0",
                    queue_depth=3,
                ),
                Message(
                    kill_field="WAIT",
                    content="WAIT request to pause logging.",
                    timestamp=now - dt.timedelta(seconds=4),
                    end_key="2",
                    queue_depth=2,
                ),
                Message(
                    kill_field="    ",
                    content="Follow-up payload after wait.",
                    timestamp=now - dt.timedelta(seconds=3),
                    end_key="0",
                    queue_depth=1,
                ),
                Message(
                    kill_field="KILL",
                    content="KILL command to stop logging.",
                    timestamp=now - dt.timedelta(seconds=2),
                    end_key="3",
                    queue_depth=0,
                ),
            ]
        )


class CM101MProgram:
    """Python translation of the COBOL CM101M test harness."""

    REPORT_FILE = REPORT_FILE
    TEST_ID = "CM101M"

    def __init__(self) -> None:
        self.writer = ReportWriter(self.REPORT_FILE)
        self.password1 = "PRIMARYPASS"
        self.runtime_queue = SimulatedQueue(self.password1, with_messages=True)
        self.msg_date = 0
        self.sym_source = ""
        self.poll_count = 0
        self.init_time_seconds = 0
        self.comp_time_seconds = 0
        self.init_enable_status = "99"
        self.disable_status = "99"
        self.feature = ""
        self.p_or_f = ""
        self.par_name = ""
        self.computed_status = ""
        self.computed_text = ""
        self.correct_status = ""
        self.correct_text = ""
        self.re_mark = ""
        self.pass_counter = 0
        self.error_counter = 0
        self.delete_counter = 0
        self.total_tests = 12

    def run(self) -> None:
        self.head_routine()
        self.enable_queue_and_log_status()
        self.log_init()
        self.log_messages()
        self.look_for_late_transmissions()
        self.status_tests_init()
        self.run_status_tests()
        self.close_files()

    def head_routine(self) -> None:
        self.writer.write(CCVS_H_1, blank_lines_before=0)
        self.writer.write(CCVS_H_2, blank_lines_before=2)
        self.writer.write(CCVS_H_3, blank_lines_before=2)
        self.writer.write(HYPHEN_LINE, blank_lines_before=1)

    def enable_queue_and_log_status(self) -> None:
        self.configure_queue(self.runtime_queue, self.runtime_queue.valid_queue_name)
        self.runtime_queue.enable_input(self.password1)
        self.init_enable_status = self.runtime_queue.status_key
        self.writer.write(SPEC_LINE_TEMPLATE.format(status=self.init_enable_status), blank_lines_before=2)
        self.writer.write(HYPHEN_LINE, blank_lines_before=2)

    def configure_queue(
        self,
        queue: SimulatedQueue,
        main_queue: str,
        sub1: str = "",
        sub2: str = "",
        sub3: str = "",
    ) -> None:
        queue.configure(main_queue, sub1, sub2, sub3)

    def log_init(self) -> None:
        self.poll_count = 0

    def log_messages(self) -> None:
        while True:
            try:
                message = self.runtime_queue.receive_message()
            except NoDataAvailable:
                self.increment_poll_count()
                continue
            if message is None:
                raise RuntimeError("Queue configuration became invalid during logging.")
            system_time = dt.datetime.now()
            queue_depth = self.runtime_queue.accept_message_count()
            if self.runtime_queue.status_key != "00":
                raise RuntimeError("RUN ABORTED - STATUS KEY WAS " + self.runtime_queue.status_key)
            if self.msg_date == 0:
                self.msg_date = self.runtime_queue.date_received
                self.sym_source = self.runtime_queue.where_from
                self.log_header()
            self.write_log_entry(message, system_time, queue_depth)
            kill_field = message.kill_field.strip().upper()
            if kill_field == "WAIT":
                self.handle_wait_branch()
                continue
            if kill_field == "KILL":
                self.handle_kill_branch()
                break

    def log_header(self) -> None:
        header2 = f" SYMBOLIC SOURCE IS {self.sym_source:<25}MESSAGE DATE IS {self.msg_date:06d}"
        self.writer.write(LOG_HDR_1, blank_lines_before=3)
        self.writer.write(header2, blank_lines_before=0)
        self.writer.write(LOG_HDR_3, blank_lines_before=2)
        self.writer.write(LOG_HDR_4, blank_lines_before=0)
        self.writer.blank()

    def write_log_entry(self, message: Message, system_time: dt.datetime, queue_depth: int) -> None:
        lag_seconds = (system_time - message.timestamp).total_seconds()
        receipt_time = self._format_time(message.timestamp)
        lag_text = f"{lag_seconds:>11.2f}"
        msg_len = f"{message.length:>5}"
        sentinel = self.runtime_queue.sentinel()
        idle = self._format_idle_count()
        line = (
            f" {receipt_time} {lag_text} {msg_len}  {sentinel} "
            f"{queue_depth:>2} {idle}  {message.payload()}"
        )
        self.writer.write(line)

    def handle_wait_branch(self) -> None:
        self.get_initial_time()
        self.delay_for_30_secs()
        self.log_init()

    def handle_kill_branch(self) -> None:
        self.get_initial_time()
        self.runtime_queue.disable_input(self.password1)
        self.disable_status = self.runtime_queue.status_key
        self.get_time_difference()
        self.write_disable_messages()

    def write_disable_messages(self) -> None:
        now_txt = self._format_time(dt.datetime.now())
        line1 = f" {now_txt}  -DISABLE COMMAND INITIATED FROM PROGRAM"
        line2 = f" {now_txt}  - STATUS CODE OF {self.disable_status} AND EXECUTION CONTROL RETURNED FROM MCS"
        self.writer.write(line1, blank_lines_before=2)
        self.writer.write(line2)
        self.writer.blank()

    def look_for_late_transmissions(self) -> None:
        self.get_initial_time()
        self.comp_time_seconds = 15
        self.writer.write(HYPHEN_LINE, blank_lines_before=2)
        self.writer.write(HYPHEN_LINE)

    def status_tests_init(self) -> None:
        self.writer.write(" BEGIN INPUT STATUS TESTS", blank_lines_before=2)
        self.writer.write(COLUMNS_LINE_1, blank_lines_before=2)
        self.writer.write(COLUMNS_LINE_2)
        self.writer.blank()
        self.feature = "MCS STATUS WORD"

    def run_status_tests(self) -> None:
        tests: list[tuple[str, str, Callable[[SimulatedQueue], str], Optional[str], Optional[str]]] = [
            (
                "REC-STATUS-TEST-01",
                "QUEUE NAME NOT SPECIFIED",
                self._rec_status_test_01,
                "20",
                None,
            ),
            (
                "REC-STATUS-TEST-02",
                "UNKNOWN SUB-QUEUE-1 SPECIFIED",
                self._rec_status_test_02,
                "20",
                None,
            ),
            (
                "ACCPT-STATUS-TEST-01",
                "QUEUE NAME NOT SPECIFIED",
                self._accpt_status_test_01,
                "20",
                None,
            ),
            (
                "ACCPT-STATUS-TEST-02",
                "UNKNOWN SUB-QUEUE-1 SPECIFIED",
                self._accpt_status_test_02,
                "20",
                None,
            ),
            (
                "ENABL-STATUS-TEST-01",
                "QUEUE NAME NOT SPECIFIED",
                self._enabl_status_test_01,
                "20",
                None,
            ),
            (
                "ENABL-STATUS-TEST-02",
                "UNKNOWN SUB-QUEUE-1 SPECIFIED",
                self._enabl_status_test_02,
                "20",
                None,
            ),
            (
                "ENABL-STATUS-TEST-03",
                "INVALID PASSWORD USED",
                self._enabl_status_test_03,
                "40",
                None,
            ),
            (
                "ENABL-STATUS-TEST-04",
                "NO QUEUE NAME / WRONG PASSWORD",
                self._enabl_status_test_04,
                None,
                "INFO TEST FOR BOTH",
            ),
            (
                "DISAB-STATUS-TEST-01",
                "QUEUE NAME NOT SPECIFIED",
                self._disab_status_test_01,
                "20",
                None,
            ),
            (
                "DISAB-STATUS-TEST-02",
                "UNKNOWN SUB-QUEUE-1 SPECIFIED",
                self._disab_status_test_02,
                "20",
                None,
            ),
            (
                "DISAB-STATUS-TEST-03",
                "INVALID PASSWORD USED",
                self._disab_status_test_03,
                "40",
                None,
            ),
            (
                "RENAB-STATUS-TEST-01",
                "RE-ENABLE PREVIOUSLY DISABLED",
                self._renab_status_test_01,
                "00",
                None,
            ),
        ]
        self.total_tests = len(tests)
        for name, remark, operation, expected, info_text in tests:
            self.status_test(name, remark, operation, expected, info_text)

    def status_test(
        self,
        par_name: str,
        remark: str,
        operation: Callable[[SimulatedQueue], str],
        expected_status: Optional[str],
        info_text: Optional[str],
    ) -> None:
        queue = SimulatedQueue(self.password1)
        self.configure_queue(queue, "")

        self.clear_test_fields()
        self.par_name = par_name
        self.re_mark = remark
        status = operation(queue)

        if info_text:
            self.p_or_f = "INFO"
            self.computed_status = status
            self.correct_text = info_text
        else:
            if status == expected_status:
                self.pass_test()
            else:
                self.fail_test(expected_status or "--", status)
        self.print_detail()

    def _rec_status_test_01(self, queue: SimulatedQueue) -> str:
        queue.configure("")
        try:
            queue.receive_message()
        except (RuntimeError, NoDataAvailable):
            pass
        return queue.status_key

    def _rec_status_test_02(self, queue: SimulatedQueue) -> str:
        queue.configure(queue.valid_queue_name, "DUMMYNAME")
        try:
            queue.receive_message()
        except (RuntimeError, NoDataAvailable):
            pass
        return queue.status_key

    def _accpt_status_test_01(self, queue: SimulatedQueue) -> str:
        queue.configure("")
        queue.accept_message_count()
        return queue.status_key

    def _accpt_status_test_02(self, queue: SimulatedQueue) -> str:
        queue.configure(queue.valid_queue_name, "DUMMYNAME")
        queue.accept_message_count()
        return queue.status_key

    def _enabl_status_test_01(self, queue: SimulatedQueue) -> str:
        queue.configure("")
        queue.enable_input(self.password1)
        return queue.status_key

    def _enabl_status_test_02(self, queue: SimulatedQueue) -> str:
        queue.configure(queue.valid_queue_name, "DUMMYNAME")
        queue.enable_input(self.password1)
        return queue.status_key

    def _enabl_status_test_03(self, queue: SimulatedQueue) -> str:
        queue.configure(queue.valid_queue_name)
        queue.enable_input("LETMEIN")
        return queue.status_key

    def _enabl_status_test_04(self, queue: SimulatedQueue) -> str:
        queue.configure("")
        queue.enable_input("LETMEIN")
        return queue.status_key

    def _disab_status_test_01(self, queue: SimulatedQueue) -> str:
        queue.configure("")
        queue.disable_input(self.password1)
        return queue.status_key

    def _disab_status_test_02(self, queue: SimulatedQueue) -> str:
        queue.configure(queue.valid_queue_name, "DUMMYNAME")
        queue.disable_input(self.password1)
        return queue.status_key

    def _disab_status_test_03(self, queue: SimulatedQueue) -> str:
        queue.configure(queue.valid_queue_name)
        queue.disable_input("KILLITNOW")
        return queue.status_key

    def _renab_status_test_01(self, queue: SimulatedQueue) -> str:
        queue.configure(queue.valid_queue_name)
        queue.enable_input(self.password1)
        queue.disable_input(self.password1)
        queue.configure(queue.valid_queue_name)
        queue.enable_input(self.password1)
        return queue.status_key

    def pass_test(self) -> None:
        self.p_or_f = "PASS"
        self.pass_counter += 1

    def fail_test(self, expected: str, actual: str) -> None:
        self.p_or_f = "FAIL*"
        self.error_counter += 1
        self.computed_status = actual
        self.correct_status = expected

    def clear_test_fields(self) -> None:
        self.p_or_f = ""
        self.computed_status = ""
        self.computed_text = ""
        self.correct_status = ""
        self.correct_text = ""
        self.re_mark = ""

    def print_detail(self) -> None:
        computed_block = self._format_computed_block()
        correct_block = self._format_correct_block()
        line = (
            f"   {self.feature:<18} {self.p_or_f:<5} {self.par_name:<20} "
            f"{computed_block:<22} {correct_block:<22} {self.re_mark:<30}"
        )
        self.writer.write(line)

    def _format_computed_block(self) -> str:
        if self.computed_status:
            return f"STATUS={self.computed_status}"
        if self.computed_text:
            return self.computed_text
        return ""
    def _format_correct_block(self) -> str:
        if self.correct_status:
            return f"STATUS={self.correct_status}"
        if self.correct_text:
            return self.correct_text
        return ""

    def close_files(self) -> None:
        self.end_routine()
        self.writer.flush()

    def end_routine(self) -> None:
        self.writer.write(HYPHEN_LINE)
        self.writer.blank(4)
        self.writer.write(CCVS_E_1_TEMPLATE.format(test_id=self.TEST_ID))
        error_total = " NO" if self.error_counter == 0 else f"{self.error_counter:>3}"
        delete_total = " NO" if self.delete_counter == 0 else f"{self.delete_counter:>3}"
        self.writer.write(f"{error_total} ERRORS ENCOUNTERED", blank_lines_before=1)
        self.writer.write(f"{delete_total} TESTS DELETED")
        success_count = max(self.total_tests - self.error_counter - self.delete_counter, 0)
        self.writer.write(f"{success_count:>3} OF {self.total_tests:>3} TESTS WERE EXECUTED SUCCESSFULLY")
        self.writer.write(CCVS_E_3, blank_lines_before=1)

    def increment_poll_count(self) -> None:
        self.poll_count = min(self.poll_count + 1, 99_999_999)

    def get_initial_time(self) -> None:
        self.init_time_seconds = self._seconds_since_midnight()

    def get_time_difference(self) -> None:
        self.comp_time_seconds = self._seconds_since_midnight() - self.init_time_seconds

    def delay_for_30_secs(self) -> None:
        self.comp_time_seconds = 30

    @staticmethod
    def _seconds_since_midnight(timestamp: Optional[dt.datetime] = None) -> int:
        timestamp = timestamp or dt.datetime.now()
        return timestamp.hour * 3600 + timestamp.minute * 60 + timestamp.second

    @staticmethod
    def _format_time(timestamp: dt.datetime) -> str:
        hundredths = int(timestamp.microsecond / 10_000)
        return f"{timestamp.hour:02d}:{timestamp.minute:02d}:{timestamp.second:02d}.{hundredths:02d}"

    def _format_idle_count(self) -> str:
        if self.poll_count >= 99_999_999:
            return " OVERFLOW "
        return f"{self.poll_count:>10,}"


def main() -> None:
    CM101MProgram().run()


if __name__ == "__main__":
    main()
