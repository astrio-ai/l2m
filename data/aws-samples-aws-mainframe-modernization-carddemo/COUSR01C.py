from __future__ import annotations

import argparse
import json
import os
import sys
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Dict, Iterable, Optional, Union


class UserSecurityError(Exception):
    """Base class for user security errors."""


class ValidationError(UserSecurityError):
    """Raised when the input payload contains invalid data."""

    def __init__(self, message: str, field: str) -> None:
        super().__init__(message)
        self.field = field


class DuplicateUserError(UserSecurityError):
    """Raised when attempting to create a user that already exists."""

    def __init__(self, user_id: str) -> None:
        super().__init__("User ID already exist...")
        self.user_id = user_id


@dataclass(frozen=True)
class UserRecord:
    """Represents a record in the USRSEC dataset."""

    user_id: str
    first_name: str
    last_name: str
    password: str
    user_type: str

    def to_dict(self) -> Dict[str, str]:
        return asdict(self)


PathLike = Union[str, os.PathLike[str]]
DEFAULT_STORAGE_PATH = Path(__file__).with_name("usrsec.json")


class UserSecurityRepository:
    """Simple JSON-backed storage that emulates the USRSEC VSAM file."""

    def __init__(self, storage_path: Optional[PathLike] = None) -> None:
        env_path = os.getenv("CARDDEMO_USRSEC_PATH")
        path = storage_path or env_path or DEFAULT_STORAGE_PATH
        self.storage_path = Path(path)
        self.storage_path.parent.mkdir(parents=True, exist_ok=True)

    def _read_all(self) -> Dict[str, Dict[str, str]]:
        if not self.storage_path.exists():
            return {}
        raw_content = self.storage_path.read_text(encoding="utf-8").strip()
        if not raw_content:
            return {}
        try:
            data = json.loads(raw_content)
        except json.JSONDecodeError as exc:
            raise UserSecurityError("Unable to Add User...") from exc
        if not isinstance(data, dict):
            raise UserSecurityError("Unable to Add User...")
        return data

    def _write_all(self, payload: Dict[str, Dict[str, str]]) -> None:
        try:
            self.storage_path.write_text(
                json.dumps(payload, indent=2, sort_keys=True), encoding="utf-8"
            )
        except OSError as exc:
            raise UserSecurityError("Unable to Add User...") from exc

    def add_user(self, record: UserRecord) -> None:
        users = self._read_all()
        key = record.user_id
        if key in users:
            raise DuplicateUserError(record.user_id)
        users[key] = record.to_dict()
        self._write_all(users)


class UserSecurityService:
    """Encapsulates the business logic behind adding users."""

    def __init__(self, repository: Optional[UserSecurityRepository] = None) -> None:
        self.repository = repository or UserSecurityRepository()

    def add_user(
        self,
        *,
        first_name: str,
        last_name: str,
        user_id: str,
        password: str,
        user_type: str,
    ) -> str:
        record = self._build_record(
            first_name=first_name,
            last_name=last_name,
            user_id=user_id,
            password=password,
            user_type=user_type,
        )
        self.repository.add_user(record)
        return f"User {record.user_id} has been added ..."

    @staticmethod
    def _build_record(
        *,
        first_name: str,
        last_name: str,
        user_id: str,
        password: str,
        user_type: str,
    ) -> UserRecord:
        validated = validate_payload(
            first_name=first_name,
            last_name=last_name,
            user_id=user_id,
            password=password,
            user_type=user_type,
        )
        return UserRecord(**validated)


def normalize(value: str) -> str:
    return value.strip()


VALIDATION_ORDER: Iterable[tuple[str, str]] = (
    ("first_name", "First Name can NOT be empty..."),
    ("last_name", "Last Name can NOT be empty..."),
    ("user_id", "User ID can NOT be empty..."),
    ("password", "Password can NOT be empty..."),
    ("user_type", "User Type can NOT be empty..."),
)


def validate_payload(**payload: str) -> Dict[str, str]:
    normalized: Dict[str, str] = {}
    for field, message in VALIDATION_ORDER:
        value = payload.get(field, "")
        if not value or not normalize(value):
            raise ValidationError(message, field)
        normalized[field] = normalize(value)
    return normalized


def add_user(
    *,
    first_name: str,
    last_name: str,
    user_id: str,
    password: str,
    user_type: str,
    repository: Optional[UserSecurityRepository] = None,
) -> str:
    service = UserSecurityService(repository)
    return service.add_user(
        first_name=first_name,
        last_name=last_name,
        user_id=user_id,
        password=password,
        user_type=user_type,
    )


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Modernized version of the COUSR01C program that adds users."
    )
    parser.add_argument("--first-name", required=True, help="User's first name.")
    parser.add_argument("--last-name", required=True, help="User's last name.")
    parser.add_argument("--user-id", required=True, help="Unique user identifier.")
    parser.add_argument("--password", required=True, help="User password.")
    parser.add_argument("--user-type", required=True, help="User type (e.g. ADMIN).")
    parser.add_argument(
        "--storage",
        help="Optional path to the JSON file that acts as the USRSEC dataset.",
    )
    return parser


def main(argv: Optional[Iterable[str]] = None) -> int:
    parser = _build_parser()
    args = parser.parse_args(argv)
    repository = UserSecurityRepository(args.storage) if args.storage else None
    try:
        message = add_user(
            first_name=args.first_name,
            last_name=args.last_name,
            user_id=args.user_id,
            password=args.password,
            user_type=args.user_type,
            repository=repository,
        )
    except ValidationError as exc:
        print(exc, file=sys.stderr)
        return 2
    except DuplicateUserError as exc:
        print(exc, file=sys.stderr)
        return 3
    except UserSecurityError as exc:
        print(exc, file=sys.stderr)
        return 4
    else:
        print(message)
        return 0


if __name__ == "__main__":
    raise SystemExit(main())
