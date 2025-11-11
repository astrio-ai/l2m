"""Interactive CLI inspired by Claude Code, Codex, and Gemini."""

from __future__ import annotations

import argparse
import asyncio
import cmd
import difflib
import json
import os
import shlex
import shutil
import sys
import textwrap
import time
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

from evals.evaluator import Evaluator
from src.config import get_settings
from src.config import settings as settings_module
from src.utils.logger import get_logger
from src.workflows.batch_pipeline import BatchModernizationPipeline, discover_cobol_files
from src.workflows.modernization_pipeline import ModernizationPipeline

logger = get_logger(__name__)


def _expand_path(base: Path, value: str) -> Path:
    """Resolve user input path relative to the current working directory."""
    # Strip whitespace and quotes
    value = value.strip().strip('"').strip("'")
    
    # Handle empty string
    if not value:
        return base
    
    # Normalize backslashes to forward slashes on non-Windows systems
    # This ensures cross-platform consistency
    if os.name != 'nt':
        value = value.replace("\\", "/")
    
    # Create Path and expand user home directory (~) first
    # expanduser() must be called before checking if path is absolute
    path = Path(value).expanduser()
    
    # If absolute, return resolved path
    if path.is_absolute():
        return path.resolve()
    
    # Otherwise, join with base and resolve
    return (base / path).resolve()


def _get_ascii_art() -> str:
    """Generate centered ASCII art with blue color."""
    ascii_lines = [
        "██╗     ██████╗ ███╗   ███╗",
        "██║     ╚════██╗████╗ ████║",
        "██║      █████╔╝██╔████╔██║",
        "██║     ██╔═══╝ ██║╚██╔╝██║",
        "███████╗███████╗██║ ╚═╝ ██║",
        "╚══════╝╚══════╝╚═╝     ╚═╝",
    ]
    
    # Get terminal width, default to 80 if unavailable
    try:
        width = shutil.get_terminal_size().columns
    except (OSError, AttributeError):
        width = 80
    
    # Blue color code (#3B82F6 = RGB 59, 130, 246)
    # blue = "\033[38;2;59;130;246m"
    # reset = "\033[0m"
    
    # Center each line
    centered_lines = []
    for line in ascii_lines:
        # Strip any trailing whitespace from the line
        line = line.rstrip()
        # Calculate padding to center
        padding = (width - len(line)) // 2
        centered_line = " " * padding + line
        centered_lines.append(centered_line)
    
    # Combine with color codes
    # return f"{blue}\n".join(centered_lines) + reset
    return "\n".join(centered_lines)

@dataclass
class CLIContext:
    """Holds mutable state for the interactive session."""

    cwd: Path = field(default_factory=lambda: Path.cwd().resolve())
    history: List[Dict[str, Any]] = field(default_factory=list)
    last_modernization: Optional[Dict[str, Any]] = None
    last_batch: Optional[Dict[str, Any]] = None
    last_evaluation: Optional[Dict[str, Any]] = None
    session_id: str = field(default_factory=lambda: f"cli-session-{int(time.time())}")
    settings_snapshot: Dict[str, Any] = field(default_factory=lambda: get_settings().model_dump())
    notes: Dict[str, Any] = field(default_factory=dict)

    def record(self, command: str, outcome: Optional[str] = None) -> None:
        timestamp = datetime.now().isoformat(timespec="seconds")
        self.history.append({
            "timestamp": timestamp,
            "command": command.strip(),
            "outcome": outcome or "",
        })

    def reset(self) -> None:
        self.cwd = Path.cwd().resolve()
        self.history.clear()
        self.last_modernization = None
        self.last_batch = None
        self.last_evaluation = None
        self.notes.clear()
        self.settings_snapshot = get_settings().model_dump()


class L2MInteractiveShell(cmd.Cmd):
    """Interactive REPL for the Legacy2Modern toolkit."""

    prompt = "l2m> "

    def __init__(self, context: Optional[CLIContext] = None):
        super().__init__()
        self.context = context or CLIContext()
        self.settings = get_settings()
        # Generate intro with centered ASCII art
        self.intro = (
            _get_ascii_art()
            + "\n\n"
            + textwrap.dedent(
                """
                Welcome to the Legacy2Modern interactive shell. Type 'help' or '?' to list commands.
                Use natural language or explicit commands to drive modernizations, batch runs,
                evaluations, and file inspections.
                """
            ).strip()
        )

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------
    def _run_async(self, coro):
        """Run an async coroutine from the synchronous REPL."""
        try:
            return asyncio.run(coro)
        except RuntimeError as exc:  # pragma: no cover - defensive programming
            logger.error("Async runtime error: %s", exc)
            print(f"❌ Async runtime error: {exc}")
            return None

    def _print_box(self, title: str, body: str) -> None:
        border = "=" * 70
        print(f"\n{border}\n{title}\n{border}")
        print(body)

    def _parse_args(self, parser: argparse.ArgumentParser, arg: str):
        try:
            return parser.parse_args(shlex.split(arg))
        except SystemExit:
            return None

    def _format_duration(self, seconds: float) -> str:
        if seconds < 1:
            return f"{seconds*1000:.0f} ms"
        if seconds < 60:
            return f"{seconds:.1f} s"
        minutes, sec = divmod(seconds, 60)
        if minutes < 60:
            return f"{int(minutes)} m {sec:.0f} s"
        hours, minutes = divmod(minutes, 60)
        return f"{int(hours)} h {int(minutes)} m {sec:.0f} s"

    # ------------------------------------------------------------------
    # Core commands
    # ------------------------------------------------------------------
    def do_modernize(self, arg: str) -> None:
        """modernize <cobol_file> [--no-save] [--output DIR]

        Run the modernization pipeline on a single COBOL file.
        """
        parser = argparse.ArgumentParser(prog="modernize", add_help=False)
        parser.add_argument("path", nargs="?")
        parser.add_argument("--no-save", dest="save", action="store_false")
        parser.add_argument("--output", type=str, help="Directory for generated files")
        parser.add_argument("--session", type=str, help="Optional session identifier")
        options = self._parse_args(parser, arg)
        if options is None:
            return
        if not options.path:
            print("Usage: modernize <path/to/file.cbl> [--output DIR]")
            return

        cobol_path = _expand_path(self.context.cwd, options.path)
        if not cobol_path.exists():
            print(f"❌ File not found: {cobol_path}")
            return

        output_dir = _expand_path(self.context.cwd, options.output) if options.output else None
        session_id = options.session or self.context.session_id

        print(f"🚀 Modernizing {cobol_path} (session: {session_id})...")
        start = time.perf_counter()
        pipeline = ModernizationPipeline(session_id=session_id)
        result = self._run_async(pipeline.run(str(cobol_path), save_files=options.save, output_dir=output_dir))
        duration = time.perf_counter() - start

        if not result:
            print("❌ Modernization failed (no result returned).")
            self.context.record(f"modernize {arg}", "failed")
            return

        if result.get("error"):
            print(f"❌ Modernization error: {result['error']}")
            self.context.record(f"modernize {arg}", "error")
            return

        summary_lines = [
            f"COBOL file   : {result.get('cobol_file')}",
            f"Python saved  : {result.get('python_file') or 'not saved'}",
            f"Tests saved   : {result.get('test_file') or 'not saved'}",
            f"Duration      : {self._format_duration(duration)}",
        ]
        self._print_box("MODERNIZATION COMPLETE", "\n".join(summary_lines))

        self.context.last_modernization = result
        self.context.record(f"modernize {arg}", "success")

    def do_batch(self, arg: str) -> None:
        """batch --directory DIR [--pattern PATTERN] [--limit N]

        Run batch modernization with real-time progress tracking.
        """
        parser = argparse.ArgumentParser(prog="batch", add_help=False)
        parser.add_argument("--directory", "-d", type=str)
        parser.add_argument("--pattern", "-p", type=str, help="Glob pattern")
        parser.add_argument("--limit", type=int, help="Limit number of files")
        parser.add_argument("--continue-on-error", action="store_true", dest="continue_on_error")
        parser.add_argument("--output", type=str, help="Custom output directory")
        options = self._parse_args(parser, arg)
        if options is None:
            return
        if not options.directory and not options.pattern:
            print("Usage: batch --directory DIR [--pattern PATTERN]")
            return

        search_root = options.directory or self.context.cwd
        cobol_files = discover_cobol_files(
            base_path=_expand_path(self.context.cwd, search_root),
            pattern=options.pattern,
            limit=options.limit,
        )
        if not cobol_files:
            print("⚠️  No COBOL files found for the given criteria.")
            return

        print(f"🚀 Starting batch modernization for {len(cobol_files)} file(s)...")
        pipeline = BatchModernizationPipeline()
        output_dir = _expand_path(self.context.cwd, options.output) if options.output else None
        start = time.perf_counter()
        result = self._run_async(pipeline.run_batch(cobol_files, output_dir=output_dir))
        duration = time.perf_counter() - start

        if not result:
            print("❌ Batch modernization failed.")
            self.context.record(f"batch {arg}", "failed")
            return

        summary = textwrap.dedent(
            f"""
            Total files : {result.total_files}
            Successful  : {result.successful}
            Failed      : {result.failed}
            Duration    : {self._format_duration(duration)}
            Report file : {result.report_file or 'not generated'}
            """
        ).strip()
        self._print_box("BATCH SUMMARY", summary)

        if result.failed:
            print("Failed files:")
            for item in result.results:
                if not item.success:
                    preview = (item.error or "Unknown error")[:120]
                    print(f"  - {item.file_path.name}: {preview}")

        self.context.last_batch = {
            "summary": summary,
            "result": result,
        }
        self.context.record(f"batch {arg}", "success")

    def do_evaluate(self, arg: str) -> None:
        """evaluate [--program NAME] [--save]

        Run the evaluator on generated programs. Optionally filter by program name.
        """
        parser = argparse.ArgumentParser(prog="evaluate", add_help=False)
        parser.add_argument("--program", "-p", type=str, help="Evaluate a single program")
        parser.add_argument("--save", action="store_true", help="Persist evaluation report")
        options = self._parse_args(parser, arg)
        if options is None:
            return

        evaluator = Evaluator()
        print("🧪 Running evaluation suite...")
        metrics = evaluator.evaluate_all()
        if options.save:
            saved = evaluator.save_results(metrics)
            print(f"💾 Results saved to {saved}")

        summary_lines = [
            f"Programs evaluated : {metrics.total_programs}",
            f"CA@1               : {metrics.ca_at_1:.2f}%",
            f"Compilation errors : {metrics.compilation_error_rate:.2f}%",
            f"Runtime errors     : {metrics.runtime_error_rate:.2f}%",
            f"Timeouts           : {metrics.timeout_rate:.2f}%",
            f"Logical errors     : {metrics.logical_error_rate:.2f}%",
        ]
        self._print_box("EVALUATION SUMMARY", "\n".join(summary_lines))

        failing_programs = [p for p in metrics.program_results if not p.get("success")]
        if failing_programs:
            print("Programs requiring attention:")
            for item in failing_programs[:10]:
                print(f"  - {item['program_name']}: {item.get('error_type') or 'failed tests'}")
            if len(failing_programs) > 10:
                print(f"  ... and {len(failing_programs) - 10} more")

        self.context.last_evaluation = {
            "metrics": metrics,
            "failing": failing_programs,
        }
        self.context.record(f"evaluate {arg}", "success")

    def do_inspect(self, arg: str) -> None:
        """inspect <path> [--lines N]

        Show a file with optional line limit and syntax-aware hints.
        """
        parser = argparse.ArgumentParser(prog="inspect", add_help=False)
        parser.add_argument("path")
        parser.add_argument("--lines", type=int, default=200)
        options = self._parse_args(parser, arg)
        if options is None:
            return

        target = _expand_path(self.context.cwd, options.path)
        if not target.exists():
            print(f"❌ File not found: {target}")
            self.context.record(f"inspect {arg}", "not-found")
            return
        if target.is_dir():
            print(f"📁 {target} (directory)")
            for entry in sorted(target.iterdir()):
                marker = "/" if entry.is_dir() else ""
                print(f"  {entry.name}{marker}")
            self.context.record(f"inspect {arg}", "directory")
            return

        print(f"📄 {target}")
        content = target.read_text(encoding="utf-8", errors="ignore")
        lines = content.splitlines()
        limit = max(1, options.lines)
        for idx, line in enumerate(lines[:limit], start=1):
            print(f"{idx:4d}: {line}")
        if len(lines) > limit:
            print(f"... ({len(lines) - limit} more lines hidden)")
        self.context.record(f"inspect {arg}", "success")

    def do_diff(self, arg: str) -> None:
        """diff <file_a> <file_b>

        Display a unified diff between two files.
        """
        parts = shlex.split(arg)
        if len(parts) != 2:
            print("Usage: diff <file_a> <file_b>")
            return
        file_a = _expand_path(self.context.cwd, parts[0])
        file_b = _expand_path(self.context.cwd, parts[1])
        if not file_a.exists() or not file_b.exists():
            print("❌ Both files must exist.")
            self.context.record(f"diff {arg}", "not-found")
            return

        content_a = file_a.read_text(encoding="utf-8", errors="ignore").splitlines()
        content_b = file_b.read_text(encoding="utf-8", errors="ignore").splitlines()
        diff_lines = list(difflib.unified_diff(
            content_a,
            content_b,
            fromfile=str(file_a),
            tofile=str(file_b),
            lineterm="",
        ))
        if not diff_lines:
            print("✅ Files are identical.")
            self.context.record(f"diff {arg}", "identical")
            return
        for line in diff_lines:
            print(line)
        self.context.record(f"diff {arg}", "diff")

    def do_ls(self, arg: str) -> None:
        """List files in the current or specified directory."""
        target = _expand_path(self.context.cwd, arg or ".")
        if not target.exists():
            print(f"❌ Path not found: {target}")
            self.context.record(f"ls {arg}", "not-found")
            return
        if target.is_file():
            print(target.name)
            self.context.record(f"ls {arg}", "file")
            return
        entries = sorted(target.iterdir())
        for entry in entries:
            marker = "/" if entry.is_dir() else ""
            print(f"{entry.name}{marker}")
        self.context.record(f"ls {arg}", "success")

    def do_cd(self, arg: str) -> None:
        """Change the working directory for the CLI session."""
        target = _expand_path(self.context.cwd, arg or str(Path.home()))
        if not target.exists() or not target.is_dir():
            print(f"❌ Directory not found: {target}")
            self.context.record(f"cd {arg}", "not-found")
            return
        os.chdir(target)
        self.context.cwd = target
        self.prompt = f"l2m:{target.name}> "
        print(f"📂 Current directory: {target}")
        self.context.record(f"cd {arg}", "success")

    def do_history(self, arg: str) -> None:  # pylint: disable=unused-argument
        """Show recent commands and outcomes."""
        if not self.context.history:
            print("(history is empty)")
            return
        for item in self.context.history[-25:]:
            outcome = f" -> {item['outcome']}" if item['outcome'] else ""
            print(f"{item['timestamp']} :: {item['command']}{outcome}")

    def do_summarize(self, arg: str) -> None:  # pylint: disable=unused-argument
        """Summarize the latest modernization, batch, and evaluation results."""
        lines = []
        if self.context.last_modernization:
            result = self.context.last_modernization
            lines.append("Last modernization:")
            lines.append(f"  COBOL: {result.get('cobol_file')}")
            lines.append(f"  Python: {result.get('python_file') or 'not saved'}")
            lines.append(f"  Tests : {result.get('test_file') or 'not saved'}")
        if self.context.last_batch:
            lines.append("\nLast batch summary:")
            lines.append(textwrap.indent(self.context.last_batch.get("summary", ""), "  "))
        if self.context.last_evaluation:
            metrics = self.context.last_evaluation.get("metrics")
            if metrics:
                lines.append("\nLast evaluation:")
                lines.append(f"  Programs : {metrics.total_programs}")
                lines.append(f"  CA@1     : {metrics.ca_at_1:.2f}%")
                lines.append(f"  Runtime errors : {metrics.runtime_error_rate:.2f}%")
        if not lines:
            lines.append("No activity recorded yet. Run 'modernize', 'batch', or 'evaluate'.")
        self._print_box("SESSION SNAPSHOT", "\n".join(lines))

    def do_save_session(self, arg: str) -> None:
        """save_session <file.json>

        Persist the current session context (history and latest results).
        """
        if not arg.strip():
            print("Usage: save_session <file.json>")
            return
        target = _expand_path(self.context.cwd, arg)
        payload = {
            "cwd": str(self.context.cwd),
            "history": self.context.history,
            "last_modernization": self.context.last_modernization,
            "last_batch": self.context.last_batch,
            "last_evaluation": self.context.last_evaluation,
            "notes": self.context.notes,
            "saved_at": datetime.now().isoformat(timespec="seconds"),
        }
        target.write_text(json.dumps(payload, indent=2), encoding="utf-8")
        print(f"💾 Session saved to {target}")
        self.context.record(f"save_session {arg}", "success")

    def do_load_session(self, arg: str) -> None:
        """load_session <file.json>

        Restore a previously saved session snapshot.
        """
        if not arg.strip():
            print("Usage: load_session <file.json>")
            return
        target = _expand_path(self.context.cwd, arg)
        if not target.exists():
            print(f"❌ Session file not found: {target}")
            self.context.record(f"load_session {arg}", "not-found")
            return
        data = json.loads(target.read_text(encoding="utf-8"))
        self.context.cwd = Path(data.get("cwd", str(Path.cwd()))).resolve()
        self.context.history = data.get("history", [])
        self.context.last_modernization = data.get("last_modernization")
        self.context.last_batch = data.get("last_batch")
        self.context.last_evaluation = data.get("last_evaluation")
        self.context.notes = data.get("notes", {})
        print(f"📂 Restored session. Current directory: {self.context.cwd}")
        self.context.record(f"load_session {arg}", "success")

    def do_config(self, arg: str) -> None:
        """config [--refresh]

        Display current configuration (model, rate limits, batch settings).
        """
        parser = argparse.ArgumentParser(prog="config", add_help=False)
        parser.add_argument("--refresh", action="store_true", help="Reload settings from environment")
        options = self._parse_args(parser, arg)
        if options is None:
            return
        if options.refresh:
            # Reset the cached settings instance to force a reload from environment variables
            setattr(settings_module, "_settings", None)
            self.settings = get_settings()
        else:
            self.settings = get_settings()
        settings_dict = self.settings.model_dump()
        lines = ["Active configuration:"]
        for key in sorted(settings_dict):
            lines.append(f"  {key}: {settings_dict[key]}")
        self._print_box("CONFIGURATION", "\n".join(lines))
        self.context.record(f"config {arg}", "success")

    def do_reset(self, arg: str) -> None:  # pylint: disable=unused-argument
        """Reset the interactive session state."""
        self.context.reset()
        self.prompt = "l2m> "
        print("🔄 Session reset.")
        self.context.record("reset", "success")

    def do_exit(self, arg: str) -> bool:  # pylint: disable=unused-argument
        """Exit the CLI."""
        print("👋 Goodbye! The L2M has been terminated. Run 'l2m' again if you need more assistance.")
        self.context.record("exit", "success")
        return True

    def do_quit(self, arg: str) -> bool:  # pylint: disable=unused-argument
        return self.do_exit(arg)

    def do_EOF(self, arg: str) -> bool:  # pylint: disable=unused-argument
        print()
        return self.do_exit(arg)

    # ------------------------------------------------------------------
    # Natural language fallback
    # ------------------------------------------------------------------
    def default(self, line: str) -> None:
        line = line.strip()
        if not line:
            return
        lowered = line.lower()
        if "modernize" in lowered and ".cbl" in lowered:
            # Attempt to extract path heuristically
            parts = [segment for segment in shlex.split(line) if segment.endswith(".cbl")]
            if parts:
                self.do_modernize(parts[0])
                return
        print(f"💬 (Info) I didn't understand '{line}'. Try 'help' for available commands.")

    # ------------------------------------------------------------------
    # Hooks
    # ------------------------------------------------------------------
    def postcmd(self, stop: bool, line: str) -> bool:
        return super().postcmd(stop, line)


def launch_cli() -> None:
    """Launch the interactive CLI."""
    shell = L2MInteractiveShell()
    try:
        shell.cmdloop()
    except KeyboardInterrupt:  # pragma: no cover - interactive behaviour
        print("\n👋 Goodbye!")
