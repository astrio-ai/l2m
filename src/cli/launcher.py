"""Entry point for the Legacy2Modern interactive CLI."""

from __future__ import annotations

from .interactive_shell import launch_cli


def main() -> None:
    """Invoke the interactive CLI."""
    launch_cli()
