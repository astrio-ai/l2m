#!/usr/bin/env python3
"""
Modernized Python implementation of the COUSR03C COBOL program.

The original program deletes a user from the USRSEC dataset after
confirming the user exists. This script offers an equivalent command-line
workflow that reads/writes a JSON-based data store while providing clear
status messaging and error handling.
"""

from __future__ import annotations

import argparse
import json
from dataclasses import dataclass, asdict
from pathlib import Path
from typing import Dict, Iterable, Mapping, MutableMapping, Optional

DATA_STORE_PATH = Path(__file__).with_name("USRSEC.json")
MAX_USER_ID_LENGTH = 8


@dataclass
class UserRecord:
    """Represents a single entry in the USRSEC dataset."""
    user_id: str
    first_name: str
    last_name: str
    user_type: str

    @classmethod
    def from_mapping(cls, data: Mapping[str, str]) -> "UserRecord":
        try:
            user_id = data["user_id"]
        except KeyError as exc:  # pragma: no cover - defensive
            raise ValueError("USRSEC entry is missing 'user_id'.") from exc

        return cls(
            user_id=normalize_user_id(user_id),
            first_name=data.get("first_name", "").strip(),
            last_name=data.get("last_name", "").strip(),
            user_type=data.get("user_type", "").strip(),
        )


def normalize_user_id(raw_user_id: Optional[str]) -> str:
    """Trim, uppercase, and validate a user ID."""
    normalized = (raw_user_id or "").strip().upper()
    if not normalized:
        raise ValueError("User ID can NOT be empty.")
    if len(normalized) > MAX_USER_ID_LENGTH:
        raise ValueError(
            f"User ID '{normalized}' exceeds {MAX_USER_ID_LENGTH} characters."
        )
    return normalized


def load_users(path: Path) -> Dict[str, UserRecord]:
    """Load the USRSEC dataset from disk."""
    if not path.exists():
        return {}

    with path.open(encoding="utf-8") as file:
        payload = json.load(file)

    entries: Iterable[Mapping[str, str]]
    if isinstance(payload, list):
        entries = payload
    elif isinstance(payload, Mapping):
        entries = payload.values()
    else:  # pragma: no cover - defensive
        raise ValueError("USRSEC data must be a list or mapping.")

    users: Dict[str, UserRecord] = {}
    for entry in entries:
        record = UserRecord.from_mapping(entry)
        users[record.user_id] = record

    return users


def save_users(users: MutableMapping[str, UserRecord], path: Path) -> None:
    """Persist the USRSEC dataset to disk."""
    path.parent.mkdir(parents=True, exist_ok=True)
    serialized = [
        asdict(user) for user in sorted(users.values(), key=lambda rec: rec.user_id)
    ]
    with path.open("w", encoding="utf-8") as file:
        json.dump(serialized, file, indent=2)
        file.write("\n")


def describe_user(user: UserRecord) -> str:
    """Return a friendly description for console output."""
    name = f"{user.first_name} {user.last_name}".strip()
    if name:
        return f"{name} [{user.user_type or 'UNKNOWN'}]"
    return f"{user.user_id} [{user.user_type or 'UNKNOWN'}]"


def confirm_deletion(user: UserRecord) -> bool:
    """Prompt the operator for confirmation before deleting a user."""
    prompt = (
        f"User {user.user_id} resolves to {describe_user(user)}.\n"
        "Type 'delete' to confirm removal (anything else aborts): "
    )
    return input(prompt).strip().lower() == "delete"


def delete_user(user_id: str, *, force: bool, data_store: Path) -> Optional[UserRecord]:
    """
    Delete a user from the USRSEC dataset.

    Returns the deleted UserRecord or None if the operation was aborted.
    Raises ValueError for invalid IDs and LookupError when the user
    cannot be found.
    """
    normalized_id = normalize_user_id(user_id)
    users = load_users(data_store)

    if normalized_id not in users:
        raise LookupError(f"User ID '{normalized_id}' not found in USRSEC.")

    record = users[normalized_id]

    if not force and not confirm_deletion(record):
        return None

    del users[normalized_id]
    save_users(users, data_store)
    return record


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Delete a user from the USRSEC security dataset."
    )
    parser.add_argument("user_id", help="The user ID to delete (max 8 characters).")
    parser.add_argument(
        "-f",
        "--force",
        action="store_true",
        help="Skip the confirmation prompt and delete immediately.",
    )
    parser.add_argument(
        "--data-store",
        type=Path,
        default=DATA_STORE_PATH,
        help=f"Path to the USRSEC data file (default: {DATA_STORE_PATH}).",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    try:
        deleted = delete_user(
            args.user_id,
            force=args.force,
            data_store=args.data_store,
        )
    except ValueError as exc:
        print(f"ERROR: {exc}")
    except LookupError as exc:
        print(f"ERROR: {exc}")
    else:
        if deleted:
            print(f"User {deleted.user_id} deleted successfully.")
        else:
            print("Deletion aborted by operator.")


if __name__ == "__main__":
    main()
