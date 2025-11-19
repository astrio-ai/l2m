"""Python modernization of the MARBLES_COST (MRBC) COBOL transaction.

The original COBOL program ran as a CICS transaction, allowing users to
create, update, or delete marble inventory rows in the EVENT.MARBLE table.
This Python version offers comparable functionality via the command line
while persisting data in a local SQLite database.

Usage examples (matching the COBOL verbs):

    python MARBLES_COST.py MRBC CRE BLUE 10 4
    python MARBLES_COST.py MRBC UPD BLUE 5 3
    python MARBLES_COST.py MRBC DEL BLUE

You can also omit the MRBC transaction identifier:

    python MARBLES_COST.py CRE BLUE 10 4

The script prints either "SUCCESS" or one of the MRBC00xE error messages
defined by the legacy program.
"""
from __future__ import annotations

import os
import sqlite3
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence

SUCCESS_MESSAGE = "SUCCESS"
ERROR_INVALID_VERB = "USE CRE|UPD|DEL"
ERROR_MARBLE_DNE = "MRBC001E UNKNOWN COLOR, CREate IT"
ERROR_MARBLE_EXISTS = "MRBC002E MARBLE ALREADY EXISTS, UPDate or DELete IT"
VERBS = {"CRE", "UPD", "DEL"}
MAX_COLOR_LENGTH = 10


class UserInputError(Exception):
    """Raised when the provided CLI arguments are invalid."""


@dataclass(frozen=True)
class Command:
    transaction_id: str
    verb: str
    color: str
    inventory: int | None = None
    cost: int | None = None


@dataclass(frozen=True)
class Result:
    success: bool
    message: str


def _default_db_path() -> Path:
    override = os.getenv("MARBLES_DB_PATH")
    if override:
        return Path(override).expanduser()
    return Path(__file__).with_suffix(".db")


class MarbleRepository:
    """SQLite-backed storage that mirrors the EVENT.MARBLE table."""

    def __init__(self, db_path: Path | str | None = None) -> None:
        self._db_path = Path(db_path) if db_path else _default_db_path()
        self._conn = sqlite3.connect(self._db_path)
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS event_marble (
                color TEXT PRIMARY KEY,
                inventory INTEGER NOT NULL,
                cost INTEGER NOT NULL
            )
            """
        )
        self._conn.commit()

    def __enter__(self) -> "MarbleRepository":
        return self

    def __exit__(self, exc_type, exc, exc_tb) -> None:
        self.close()

    def close(self) -> None:
        if self._conn:
            self._conn.close()
            self._conn = None  # type: ignore[assignment]

    def color_exists(self, color: str) -> bool:
        cursor = self._conn.execute(
            "SELECT 1 FROM event_marble WHERE color = ?", (color,)
        )
        return cursor.fetchone() is not None

    def insert_color(self, color: str, inventory: int, cost: int) -> None:
        self._conn.execute(
            "INSERT INTO event_marble (color, inventory, cost) VALUES (?, ?, ?)",
            (color, inventory, cost),
        )
        self._conn.commit()

    def update_color(self, color: str, inventory: int, cost: int) -> None:
        self._conn.execute(
            "UPDATE event_marble SET inventory = ?, cost = ? WHERE color = ?",
            (inventory, cost, color),
        )
        self._conn.commit()

    def delete_color(self, color: str) -> None:
        self._conn.execute("DELETE FROM event_marble WHERE color = ?", (color,))
        self._conn.commit()


def parse_command_line(argv: Sequence[str]) -> Command:
    tokens = list(argv)
    if not tokens:
        raise UserInputError(ERROR_INVALID_VERB)

    first_token = tokens[0].upper()
    if first_token in VERBS:
        transaction_id = "MRBC"
        verb = first_token
        payload = tokens[1:]
    else:
        transaction_id = tokens[0]
        if len(tokens) == 1:
            raise UserInputError(ERROR_INVALID_VERB)
        verb = tokens[1].upper()
        payload = tokens[2:]

    if verb not in VERBS:
        raise UserInputError(ERROR_INVALID_VERB)

    if verb == "DEL":
        if len(payload) != 1:
            raise UserInputError("DEL requires a COLOR argument")
        color = _normalize_color(payload[0])
        return Command(transaction_id=transaction_id, verb=verb, color=color)

    if len(payload) != 3:
        raise UserInputError(f"{verb} requires COLOR, INVENTORY, and COST arguments")

    color = _normalize_color(payload[0])
    inventory = _parse_quantity(payload[1], "INVENTORY")
    cost = _parse_quantity(payload[2], "COST")
    return Command(
        transaction_id=transaction_id,
        verb=verb,
        color=color,
        inventory=inventory,
        cost=cost,
    )


def _normalize_color(raw_color: str) -> str:
    color = raw_color.strip().upper()
    if not color:
        raise UserInputError("COLOR cannot be blank")
    if len(color) > MAX_COLOR_LENGTH:
        raise UserInputError(
            f"COLOR cannot exceed {MAX_COLOR_LENGTH} characters (got '{raw_color}')"
        )
    return color


def _parse_quantity(raw_value: str, field: str) -> int:
    try:
        value = int(raw_value)
    except ValueError as exc:
        raise UserInputError(f"{field} must be an integer (got '{raw_value}')") from exc

    if not 0 <= value <= 9999:
        raise UserInputError(f"{field} must be between 0 and 9999 (got {value})")
    return value


def execute_command(repo: MarbleRepository, command: Command) -> Result:
    verb = command.verb
    color = command.color

    if verb == "CRE":
        if repo.color_exists(color):
            return Result(False, ERROR_MARBLE_EXISTS)
        repo.insert_color(color, command.inventory, command.cost)  # type: ignore[arg-type]
        return Result(True, SUCCESS_MESSAGE)

    if verb == "UPD":
        if not repo.color_exists(color):
            return Result(False, ERROR_MARBLE_DNE)
        repo.update_color(color, command.inventory, command.cost)  # type: ignore[arg-type]
        return Result(True, SUCCESS_MESSAGE)

    if verb == "DEL":
        if not repo.color_exists(color):
            return Result(False, ERROR_MARBLE_DNE)
        repo.delete_color(color)
        return Result(True, SUCCESS_MESSAGE)

    return Result(False, ERROR_INVALID_VERB)


def run_cli(argv: Sequence[str] | None = None) -> int:
    argv = list(argv if argv is not None else sys.argv[1:])
    try:
        command = parse_command_line(argv)
    except UserInputError as exc:
        print(str(exc))
        return 1

    try:
        with MarbleRepository() as repo:
            result = execute_command(repo, command)
    except sqlite3.Error as exc:
        print(f"Database error: {exc}")
        return 2

    print(result.message)
    return 0 if result.success else 1


if __name__ == "__main__":
    raise SystemExit(run_cli())
