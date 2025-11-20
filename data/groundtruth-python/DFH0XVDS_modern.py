"""Modernized Python equivalent of the DFH0XVDS COBOL program.

The original module managed catalog inquiries and orders via VSAM inside CICS.
This Python translation preserves the request/response semantics while exposing
a repository/service abstraction that can run locally or be embedded into other
applications.
"""

from __future__ import annotations

import argparse
import json
import os
import sys
from bisect import bisect_left
from dataclasses import dataclass, field, replace
from decimal import Decimal, InvalidOperation
from pathlib import Path
from typing import Iterable, Sequence

BROWSE_PAGE_SIZE = 15

REQUEST_INQUIRE_ALL = "01INQC"
REQUEST_INQUIRE_SINGLE = "01INQS"
REQUEST_PLACE_ORDER = "01ORDR"

RETURN_CODE_SUCCESS = 0
RETURN_CODE_ITEM_NOT_FOUND = 20
RETURN_CODE_FILE_ERROR = 21
RETURN_CODE_UPDATE_ERROR = 22
RETURN_CODE_INSUFFICIENT_STOCK = 97
RETURN_CODE_INVALID_QUANTITY = 98
RETURN_CODE_UNKNOWN_REQUEST = 99

DEFAULT_DATASET = [
    {
        "ref": 1001,
        "description": "Acoustic noise dampening panels",
        "department": 101,
        "cost": "249.99",
        "in_stock": 12,
        "on_order": 3,
    },
    {
        "ref": 1005,
        "description": "Smart environmental sensor kit",
        "department": 102,
        "cost": "189.50",
        "in_stock": 23,
        "on_order": 5,
    },
    {
        "ref": 1010,
        "description": "Industrial RFID gateway",
        "department": 103,
        "cost": "799.00",
        "in_stock": 5,
        "on_order": 2,
    },
    {
        "ref": 1015,
        "description": "Predictive maintenance software license",
        "department": 104,
        "cost": "1499.00",
        "in_stock": 2,
        "on_order": 1,
    },
    {
        "ref": 1020,
        "description": "Warehouse robotics controller",
        "department": 105,
        "cost": "3499.00",
        "in_stock": 1,
        "on_order": 0,
    },
]


class CatalogError(Exception):
    """Base error for catalog operations."""


class ItemNotFound(CatalogError):
    """Raised when a catalog item cannot be located."""


class InvalidQuantity(CatalogError):
    """Raised when an order quantity is non-positive."""


class InsufficientStock(CatalogError):
    """Raised when there is not enough inventory to fulfill an order."""


@dataclass(slots=True)
class CatalogItem:
    """Represents a catalog record."""

    ref: int
    description: str
    department: int
    cost: Decimal
    in_stock: int
    on_order: int

    @classmethod
    def from_dict(cls, raw: dict[str, object]) -> CatalogItem:
        """Create a CatalogItem from a mapping."""
        try:
            cost_value = Decimal(str(raw.get("cost", "0")))
        except (InvalidOperation, TypeError):
            cost_value = Decimal("0")

        return cls(
            ref=int(raw["ref"]),
            description=str(raw.get("description", "")),
            department=int(raw.get("department", 0)),
            cost=cost_value,
            in_stock=int(raw.get("in_stock", 0)),
            on_order=int(raw.get("on_order", 0)),
        )

    def to_dict(self) -> dict[str, object]:
        """Serialize the item into JSON-friendly primitives."""
        return {
            "ref": self.ref,
            "description": self.description,
            "department": self.department,
            "cost": format(self.cost, "f"),
            "in_stock": self.in_stock,
            "on_order": self.on_order,
        }


@dataclass(slots=True)
class CatalogRequest:
    """Represents the inputs that were originally passed via DFHCOMMAREA."""

    request_id: str
    list_start_ref: int | None = None
    page_size: int | None = None
    item_ref_req: int | None = None
    item_ref_number: int | None = None
    quantity_req: int | None = None


@dataclass(slots=True)
class CatalogResponse:
    """Structured response mirroring the COBOL program's return values."""

    return_code: int
    message: str
    items: list[CatalogItem] = field(default_factory=list)
    last_item_ref: int | None = None
    item_count: int = 0
    single_item: CatalogItem | None = None

    def to_dict(self) -> dict[str, object]:
        return {
            "return_code": self.return_code,
            "message": self.message,
            "item_count": self.item_count,
            "last_item_ref": self.last_item_ref,
            "items": [item.to_dict() for item in self.items],
            "single_item": self.single_item.to_dict() if self.single_item else None,
        }


@dataclass
class CatalogRepository:
    """Thin repository abstraction for catalog persistence."""

    source_path: Path | None = None
    _items: dict[int, CatalogItem] = field(default_factory=dict, init=False, repr=False)

    def __post_init__(self) -> None:
        if self.source_path:
            resolved = self.source_path.expanduser()
            self.source_path = resolved
            if resolved.exists():
                self._items = self._load_from_json(resolved)
            else:
                self._items = self._load_default()
        else:
            self._items = self._load_default()

    def browse_from(
        self, start_ref: int | None, limit: int = BROWSE_PAGE_SIZE
    ) -> tuple[list[CatalogItem], int | None]:
        """Return up to `limit` items starting at the supplied reference."""
        ordered = self._sorted_items()
        if not ordered or limit <= 0:
            return [], None

        refs = [item.ref for item in ordered]
        effective_ref = refs[0] if not start_ref or start_ref <= 0 else start_ref
        index = bisect_left(refs, effective_ref)
        selection = ordered[index : index + limit]
        last_ref = selection[-1].ref if selection else None
        return [replace(item) for item in selection], last_ref

    def get_item(self, ref: int) -> CatalogItem:
        """Fetch a copy of the catalog item with the given reference."""
        try:
            return replace(self._items[ref])
        except KeyError as exc:
            raise ItemNotFound(f"Item {ref} not found") from exc

    def place_order(self, ref: int, quantity: int) -> CatalogItem:
        """Update inventory counts for an order."""
        if quantity <= 0:
            raise InvalidQuantity("Order quantity must be positive")

        try:
            item = self._items[ref]
        except KeyError as exc:
            raise ItemNotFound(f"Item {ref} not found") from exc

        if quantity > item.in_stock:
            raise InsufficientStock(
                f"Insufficient stock to complete order for {ref} (requested "
                f"{quantity}, available {item.in_stock})"
            )

        item.in_stock -= quantity
        self._items[ref] = item
        self._persist()
        return replace(item)

    def _sorted_items(self) -> list[CatalogItem]:
        return sorted(self._items.values(), key=lambda entry: entry.ref)

    def _load_default(self) -> dict[int, CatalogItem]:
        return self._load_from_iterable(DEFAULT_DATASET)

    def _load_from_iterable(
        self, iterable: Iterable[dict[str, object]]
    ) -> dict[int, CatalogItem]:
        loaded: dict[int, CatalogItem] = {}
        for raw in iterable:
            item = CatalogItem.from_dict(raw)
            loaded[item.ref] = item
        return loaded

    def _load_from_json(self, path: Path) -> dict[int, CatalogItem]:
        data = json.loads(path.read_text())
        if isinstance(data, dict) and "items" in data:
            payload: Sequence[dict[str, object]] = data["items"]  # type: ignore[assignment]
        elif isinstance(data, list):
            payload = data  # type: ignore[assignment]
        else:
            raise ValueError(f"Unrecognized catalog data format in {path}")
        return self._load_from_iterable(payload)

    def _persist(self) -> None:
        if not self.source_path:
            return
        payload = [item.to_dict() for item in self._sorted_items()]
        self.source_path.parent.mkdir(parents=True, exist_ok=True)
        self.source_path.write_text(json.dumps(payload, indent=2))


def _resolve_default_path() -> Path | None:
    """Determine the catalog data source from environment variables or defaults."""
    env_value = os.environ.get("EXMPCAT_PATH")
    if env_value:
        return Path(env_value).expanduser()

    candidate = Path(__file__).with_suffix(".json")
    if candidate.exists():
        return candidate
    return None


class CatalogService:
    """Service layer that reproduces the original COBOL routines."""

    def __init__(self, repository: CatalogRepository | None = None) -> None:
        self.repository = repository or CatalogRepository(_resolve_default_path())

    def handle_request(self, request: CatalogRequest) -> CatalogResponse:
        request_id = (request.request_id or "").strip().upper()
        handler_map = {
            REQUEST_INQUIRE_ALL: self._handle_inquire_multiple,
            REQUEST_INQUIRE_SINGLE: self._handle_inquire_single,
            REQUEST_PLACE_ORDER: self._handle_place_order,
        }
        handler = handler_map.get(request_id)
        if handler is None:
            return CatalogResponse(
                return_code=RETURN_CODE_UNKNOWN_REQUEST,
                message="OPERATION UNKNOWN",
            )
        return handler(request)

    def _handle_inquire_multiple(self, request: CatalogRequest) -> CatalogResponse:
        limit = (
            request.page_size
            if request.page_size and request.page_size > 0
            else BROWSE_PAGE_SIZE
        )
        items, last_ref = self.repository.browse_from(request.list_start_ref, limit)
        message = (
            f"{len(items)} ITEMS RETURNED" if items else "NO ITEMS FOUND FOR SELECTION"
        )
        return CatalogResponse(
            return_code=RETURN_CODE_SUCCESS,
            message=message,
            items=items,
            item_count=len(items),
            last_item_ref=last_ref,
        )

    def _handle_inquire_single(self, request: CatalogRequest) -> CatalogResponse:
        if request.item_ref_req is None:
            return CatalogResponse(
                return_code=RETURN_CODE_INVALID_QUANTITY,
                message="ITEM REFERENCE REQUIRED",
            )
        try:
            item = self.repository.get_item(request.item_ref_req)
        except ItemNotFound:
            return CatalogResponse(
                return_code=RETURN_CODE_ITEM_NOT_FOUND,
                message="ITEM NOT FOUND",
            )

        return CatalogResponse(
            return_code=RETURN_CODE_SUCCESS,
            message=f"RETURNED ITEM: REF = {item.ref}",
            single_item=item,
            item_count=1,
        )

    def _handle_place_order(self, request: CatalogRequest) -> CatalogResponse:
        if request.item_ref_number is None:
            return CatalogResponse(
                return_code=RETURN_CODE_INVALID_QUANTITY,
                message="ITEM NUMBER REQUIRED",
            )
        if request.quantity_req is None:
            return CatalogResponse(
                return_code=RETURN_CODE_INVALID_QUANTITY,
                message="ORDER QUANTITY REQUIRED",
            )

        try:
            item = self.repository.place_order(
                request.item_ref_number, request.quantity_req
            )
        except InvalidQuantity:
            return CatalogResponse(
                return_code=RETURN_CODE_INVALID_QUANTITY,
                message="ORDER QUANTITY MUST BE POSITIVE",
            )
        except InsufficientStock:
            return CatalogResponse(
                return_code=RETURN_CODE_INSUFFICIENT_STOCK,
                message="INSUFFICIENT STOCK TO COMPLETE ORDER",
            )
        except ItemNotFound:
            return CatalogResponse(
                return_code=RETURN_CODE_ITEM_NOT_FOUND,
                message=f"ITEM - {request.item_ref_number} NOT FOUND",
            )
        except CatalogError as exc:
            return CatalogResponse(
                return_code=RETURN_CODE_UPDATE_ERROR,
                message=f"ERROR UPDATING DATA STORE: {exc}",
            )

        return CatalogResponse(
            return_code=RETURN_CODE_SUCCESS,
            message="ORDER SUCCESSFULLY PLACED",
            single_item=item,
            item_count=1,
        )


def _build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Modernized DFH0XVDS catalog service interface."
    )
    parser.add_argument(
        "--data-file",
        type=Path,
        help=(
            "Optional path to a JSON file that represents the catalog data store. "
            "Defaults to EXMPCAT_PATH or sample in-memory data."
        ),
    )

    subparsers = parser.add_subparsers(dest="command", required=True)

    browse_parser = subparsers.add_parser(
        "browse", help="List up to 15 catalog entries starting from a reference."
    )
    browse_parser.add_argument(
        "--start",
        type=int,
        default=None,
        help="Item reference number to start from. Defaults to the first record.",
    )
    browse_parser.add_argument(
        "--limit",
        type=int,
        default=BROWSE_PAGE_SIZE,
        help=f"Maximum number of items to return (default: {BROWSE_PAGE_SIZE}).",
    )

    item_parser = subparsers.add_parser(
        "item", help="Fetch a single catalog item by reference."
    )
    item_parser.add_argument("ref", type=int, help="Catalog reference number.")

    order_parser = subparsers.add_parser("order", help="Place an order for an item.")
    order_parser.add_argument("ref", type=int, help="Catalog reference number.")
    order_parser.add_argument("quantity", type=int, help="Quantity to order.")

    return parser


def _request_from_args(args: argparse.Namespace) -> CatalogRequest:
    if args.command == "browse":
        return CatalogRequest(
            request_id=REQUEST_INQUIRE_ALL,
            list_start_ref=args.start,
            page_size=args.limit,
        )
    if args.command == "item":
        return CatalogRequest(
            request_id=REQUEST_INQUIRE_SINGLE,
            item_ref_req=args.ref,
        )
    if args.command == "order":
        return CatalogRequest(
            request_id=REQUEST_PLACE_ORDER,
            item_ref_number=args.ref,
            quantity_req=args.quantity,
        )
    raise ValueError(f"Unsupported command {args.command}")


def _print_response(response: CatalogResponse) -> None:
    print(json.dumps(response.to_dict(), indent=2))


def main(argv: Sequence[str] | None = None) -> int:
    parser = _build_arg_parser()
    args = parser.parse_args(argv)

    repo_path = args.data_file if args.data_file else _resolve_default_path()
    repository = CatalogRepository(repo_path)
    service = CatalogService(repository)
    request = _request_from_args(args)
    response = service.handle_request(request)

    _print_response(response)
    return 0 if response.return_code == RETURN_CODE_SUCCESS else 1


if __name__ == "__main__":
    raise SystemExit(main())
