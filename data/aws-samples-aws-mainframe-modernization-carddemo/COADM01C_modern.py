"""Modernized Python equivalent of the COADM01C COBOL admin menu.

This module recreates the admin menu flow that originally lived inside a
CICS/COBOL transaction so it can run as a simple CLI utility or be reused
inside automated tests.
"""
from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime
from typing import Callable, Dict, Sequence

MessageHandler = Callable[["CardDemoContext", "AdminOption"], str | None]
INVALID_OPTION_MESSAGE = "Please enter a valid option number..."


@dataclass
class CardDemoContext:
    """Lightweight stand-in for the CARDDEMO COMMAREA data."""
    current_program: str = "COADM01C"
    current_tranid: str = "CA00"
    from_program: str = "COSGN00C"
    to_program: str = "COSGN00C"
    user_id: str = "ADMIN"
    program_context: int = 0

    def reset_for(self, option: "AdminOption") -> None:
        """Prepare context for a program handoff."""
        self.from_program = self.current_program
        self.to_program = option.program
        self.program_context = 0


@dataclass
class AdminOption:
    """Represents a single selectable admin option."""
    number: int
    name: str
    program: str

    def __post_init__(self) -> None:
        if self.number < 1:
            raise ValueError("Option numbers must be positive.")
        if not self.name.strip():
            raise ValueError("Option name is required.")
        self.program = self.program.upper().strip()


class InvalidOptionError(ValueError):
    """Raised when the user enters an invalid menu option."""


class AdminMenu:
    """Interactive admin menu that mirrors the original COBOL logic."""
    EXIT_KEYS = {"PF3", "EXIT", "QUIT", "Q"}

    def __init__(
        self,
        options: Sequence[AdminOption],
        context: CardDemoContext | None = None,
        program_handlers: Dict[str, MessageHandler] | None = None,
        input_func: Callable[[str], str] = input,
        output_func: Callable[[str], None] = print,
    ) -> None:
        if not options:
            raise ValueError("At least one admin option is required.")
        self.options = sorted(options, key=lambda option: option.number)
        self.context = context or CardDemoContext()
        self.program_handlers = {
            program.upper(): handler
            for program, handler in (program_handlers or {}).items()
        }
        self._input = input_func
        self._output = output_func
        self._status_message: str = ""
        self._running = False

    def run(self) -> None:
        """Start the interactive admin menu loop."""
        self._running = True
        while self._running:
            self._render_menu()
            try:
                raw_choice = self._input("\nEnter option number (or PF3 to exit): ")
            except EOFError:
                self._output("\nReceived EOF. Returning to sign-on screen.")
                break
            choice = raw_choice.strip().upper()
            if choice in self.EXIT_KEYS:
                self.return_to_signon()
                break
            try:
                numeric_choice = self._parse_choice(choice)
            except InvalidOptionError as error:
                self._status_message = str(error)
                continue
            self._handle_option(numeric_choice)

    def _render_menu(self) -> None:
        banner = "=" * 72
        now = datetime.now()
        header = (
            f" CardDemo Admin Console  Tran: {self.context.current_tranid}  "
            f"Program: {self.context.current_program}"
        )
        timestamp = now.strftime("%m/%d/%y %H:%M:%S")
        self._output("\n" + banner)
        self._output(header)
        self._output(f" {timestamp}")
        self._output(banner)
        for option in self.options:
            self._output(f" {option.number:02d}. {option.name}")
        self._output("-" * 72)
        self._output(" PF3=Exit | Quit | Q")
        if self._status_message:
            self._output(f"\n{self._status_message}")
            self._status_message = ""

    def _parse_choice(self, choice: str) -> int:
        if not choice or not choice.isdigit():
            raise InvalidOptionError(INVALID_OPTION_MESSAGE)
        numeric_choice = int(choice)
        if numeric_choice < 1 or numeric_choice > len(self.options):
            raise InvalidOptionError(INVALID_OPTION_MESSAGE)
        return numeric_choice

    def _handle_option(self, numeric_choice: int) -> None:
        option = self.options[numeric_choice - 1]
        self.context.reset_for(option)
        handler = self.program_handlers.get(option.program)
        if handler is None:
            self._status_message = f"This option '{option.name}' is coming soon..."
            return
        result = handler(self.context, option)
        self._status_message = result or (
            f"{option.name} executed via program {option.program}."
        )

    def return_to_signon(self) -> None:
        """Simulate returning to the sign-on screen (PF3)."""
        self._running = False
        self._status_message = ""
        self._output("\nReturning to sign-on screen. Goodbye!")


DEFAULT_ADMIN_OPTIONS = [
    AdminOption(1, "Manage customer profiles", "CBCUS01C"),
    AdminOption(2, "Issue replacement cards", "COCRDUPC"),
    AdminOption(3, "Review pending transactions", "CBTRN01C"),
    AdminOption(4, "Adjust account limits", "CBACT01C"),
    AdminOption(5, "Manage platform users", "COUSR01C"),
    AdminOption(6, "View activity dashboard", "COACTVWC"),
    AdminOption(7, "Statement archive", "CBSTM03A"),
    AdminOption(8, "Operations utilities", "CSUTLDTC"),
    AdminOption(9, "System configuration", "DUMMY1"),
    AdminOption(10, "Upcoming feature", "DUMMY2"),
]


def _default_handler(_: CardDemoContext, option: AdminOption) -> str:
    return (
        f"Handoff to program {option.program} ({option.name}) completed at "
        f"{datetime.now():%H:%M:%S}."
    )


def create_default_handlers() -> Dict[str, MessageHandler]:
    """Provide basic handlers for the non-dummy sample options."""
    handlers: Dict[str, MessageHandler] = {}
    for option in DEFAULT_ADMIN_OPTIONS:
        if not option.program.startswith("DUMMY"):
            handlers[option.program] = _default_handler
    return handlers


def main() -> None:
    menu = AdminMenu(
        options=DEFAULT_ADMIN_OPTIONS,
        program_handlers=create_default_handlers(),
    )
    try:
        menu.run()
    except KeyboardInterrupt:
        print("\nSession cancelled. Returning to sign-on screen.")


if __name__ == "__main__":
    main()
