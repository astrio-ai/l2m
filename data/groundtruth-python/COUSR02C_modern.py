"""
Modernized Python implementation of the COUSR02C CICS COBOL program.

The original program allowed operators to retrieve and update user security
records stored in the USRSEC file. This module provides an equivalent command
line interface backed by a JSON document so the same workflow can be completed
outside of CICS.

Usage examples:

    # Fetch an existing record
    python COUSR02C_modern.py fetch USER001

    # Update a record
    python COUSR02C_modern.py update USER001 \
        --first-name Jane \
        --last-name Doe \
        --password swordfish \
        --user-type ADMIN
"""
from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Sequence

DEFAULT_STORAGE_PATH = Path(__file__).with_name("usrsec.json")


class UserSecurityError(RuntimeError):
    """Base class for all user security related exceptions."""


class ValidationError(UserSecurityError):
    """Raised when an input field is missing or blank."""


class UserNotFoundError(UserSecurityError):
    """Raised when the requested user ID does not exist."""


class NoChangesDetected(UserSecurityError):
    """Raised when an update is attempted without modifying any fields."""


class StorageError(UserSecurityError):
    """Raised when the backing storage file cannot be read or written."""


@dataclass(eq=True, frozen=True)
class UserSecurityRecord:
    """Represents a single entry in the USRSEC dataset."""

    user_id: str
    first_name: str
    last_name: str
    password: str
    user_type: str

    def to_dict(self) -> Dict[str, str]:
        return {
            "user_id": self.user_id,
            "first_name": self.first_name,
            "last_name": self.last_name,
            "password": self.password,
            "user_type": self.user_type,
        }

    @classmethod
    def from_dict(cls, payload: Dict[str, str]) -> "UserSecurityRecord":
        required_keys = {"user_id", "first_name", "last_name", "password", "user_type"}
        missing = required_keys.difference(payload)
        if missing:
            raise StorageError(f"Record is missing required keys: {sorted(missing)}")

        return cls(
            user_id=str(payload["user_id"]).strip(),
            first_name=str(payload["first_name"]).strip(),
            last_name=str(payload["last_name"]).strip(),
            password=str(payload["password"]).strip(),
            user_type=str(payload["user_type"]).strip(),
        )


class UserSecurityRepository:
    """
    Simple JSON-backed repository that emulates the USRSEC VSAM dataset.

    Records are stored as a list of dictionaries; the list is rewritten on every
    update to keep the implementation straightforward.
    """

    def __init__(self, storage_path: Path = DEFAULT_STORAGE_PATH) -> None:
        self.storage_path = storage_path

    def load_records(self) -> Dict[str, UserSecurityRecord]:
        if not self.storage_path.exists():
            return {}

        try:
            with self.storage_path.open("r", encoding="utf-8") as file:
                raw_records = json.load(file)
        except json.JSONDecodeError as exc:
            raise StorageError(
                f"Unable to read {self.storage_path} (invalid JSON): {exc}"
            ) from exc

        if not isinstance(raw_records, list):
            raise StorageError("Storage file must contain a JSON list of records.")

        records: Dict[str, UserSecurityRecord] = {}
        for entry in raw_records:
            if not isinstance(entry, dict):
                raise StorageError("Every record in storage must be a JSON object.")
            record = UserSecurityRecord.from_dict(entry)
            records[record.user_id] = record
        return records

    def persist_records(self, records: Dict[str, UserSecurityRecord]) -> None:
        self.storage_path.parent.mkdir(parents=True, exist_ok=True)
        payload = [
            record.to_dict()
            for record in sorted(records.values(), key=lambda rec: rec.user_id)
        ]
        with self.storage_path.open("w", encoding="utf-8") as file:
            json.dump(payload, file, indent=2, ensure_ascii=False)


@dataclass(frozen=True)
class UpdateOutcome:
    record: UserSecurityRecord
    changed_fields: Sequence[str]


class UserSecurityService:
    """Domain service that contains the business logic from COUSR02C."""

    FIELD_LABELS = {
        "first_name": "First Name",
        "last_name": "Last Name",
        "password": "Password",
        "user_type": "User Type",
    }

    def __init__(self, repository: UserSecurityRepository) -> None:
        self.repository = repository

    def fetch_user(self, user_id: str) -> UserSecurityRecord:
        normalized_user_id = self._require_input("User ID", user_id)
        records = self.repository.load_records()
        try:
            return records[normalized_user_id]
        except KeyError as exc:
            raise UserNotFoundError("User ID NOT found...") from exc

    def update_user(
        self,
        user_id: str,
        first_name: str,
        last_name: str,
        password: str,
        user_type: str,
    ) -> UpdateOutcome:
        normalized_user_id = self._require_input("User ID", user_id)
        field_values = {
            "first_name": self._require_input("First Name", first_name),
            "last_name": self._require_input("Last Name", last_name),
            "password": self._require_input("Password", password),
            "user_type": self._require_input("User Type", user_type),
        }

        records = self.repository.load_records()
        try:
            current_record = records[normalized_user_id]
        except KeyError as exc:
            raise UserNotFoundError("User ID NOT found...") from exc

        updated_record = UserSecurityRecord(
            user_id=normalized_user_id,
            first_name=field_values["first_name"],
            last_name=field_values["last_name"],
            password=field_values["password"],
            user_type=field_values["user_type"],
        )

        changed_fields = self._detect_changes(current_record, updated_record)
        if not changed_fields:
            raise NoChangesDetected("Please modify to update ...")

        records[normalized_user_id] = updated_record
        self.repository.persist_records(records)
        return UpdateOutcome(record=updated_record, changed_fields=changed_fields)

    def _detect_changes(
        self, current: UserSecurityRecord, updated: UserSecurityRecord
    ) -> List[str]:
        differences: List[str] = []
        for field_name, label in self.FIELD_LABELS.items():
            if getattr(current, field_name) != getattr(updated, field_name):
                differences.append(label)
        return differences

    @staticmethod
    def _require_input(field_label: str, value: str) -> str:
        cleaned_value = (value or "").strip()
        if not cleaned_value:
            raise ValidationError(f"{field_label} can NOT be empty...")
        return cleaned_value


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Modernized COUSR02C user maintenance utility."
    )
    parser.add_argument(
        "--storage-path",
        type=Path,
        default=DEFAULT_STORAGE_PATH,
        help=(
            "Path to the JSON file that stores USRSEC records. "
            f"Defaults to '{DEFAULT_STORAGE_PATH.name}' in the same directory."
        ),
    )

    subparsers = parser.add_subparsers(dest="command", required=True)

    fetch_parser = subparsers.add_parser(
        "fetch", help="Fetch and display a user security record."
    )
    fetch_parser.add_argument("user_id", help="Target user ID to retrieve.")

    update_parser = subparsers.add_parser(
        "update", help="Validate and update a user security record."
    )
    update_parser.add_argument("user_id", help="Target user ID to update.")
    update_parser.add_argument("--first-name", required=True, dest="first_name")
    update_parser.add_argument("--last-name", required=True, dest="last_name")
    update_parser.add_argument("--password", required=True, dest="password")
    update_parser.add_argument("--user-type", required=True, dest="user_type")

    return parser


def render_record(record: UserSecurityRecord) -> str:
    return (
        f"User ID    : {record.user_id}\n"
        f"First Name : {record.first_name}\n"
        f"Last Name  : {record.last_name}\n"
        f"Password   : {record.password}\n"
        f"User Type  : {record.user_type}"
    )


def handle_fetch(service: UserSecurityService, args: argparse.Namespace) -> int:
    try:
        record = service.fetch_user(args.user_id)
    except UserSecurityError as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 1

    print(render_record(record))
    print("\nPress the 'update' command to save your changes ...")
    return 0


def handle_update(service: UserSecurityService, args: argparse.Namespace) -> int:
    try:
        outcome = service.update_user(
            user_id=args.user_id,
            first_name=args.first_name,
            last_name=args.last_name,
            password=args.password,
            user_type=args.user_type,
        )
    except UserSecurityError as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 1

    fields = ", ".join(outcome.changed_fields)
    print(
        f"User {outcome.record.user_id} has been updated ...\n"
        f"Fields updated: {fields}"
    )
    return 0


def main(argv: Sequence[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)

    repository = UserSecurityRepository(args.storage_path)
    service = UserSecurityService(repository)

    if args.command == "fetch":
        return handle_fetch(service, args)
    if args.command == "update":
        return handle_update(service, args)
    parser.error("Unknown command supplied.")
    return 2


if __name__ == "__main__":
    sys.exit(main())
