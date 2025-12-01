"""Footer hints system for Atlas CLI.

Provides context-aware keyboard shortcuts and status information.
"""

import shutil
from typing import List, Optional

from cli.tui_utils import format_footer_hint, format_separator, ANSI_BRAND, ANSI_RESET, ANSI_BOLD


class FooterHints:
    """Manage and display footer hints with keyboard shortcuts."""

    # Common keyboard shortcuts
    SHORTCUTS = {
        "input": [
            ("Ctrl+C", "Cancel"),
            ("Ctrl+D", "Exit"),
            ("Tab", "Autocomplete"),
            ("/help", "Commands"),
        ],
        "editing": [
            ("Ctrl+E", "Editor"),
            ("/add", "Add files"),
            ("/drop", "Remove files"),
            ("/undo", "Undo changes"),
        ],
        "navigation": [
            ("/tokens", "Token usage"),
            ("/settings", "View settings"),
            ("/map", "Repo map"),
            ("/clear", "Clear chat"),
        ],
    }

    @staticmethod
    def get_terminal_width() -> int:
        """Get terminal width, with fallback."""
        try:
            return shutil.get_terminal_size().columns
        except Exception:
            return 80

    @staticmethod
    def format_shortcut(key: str, description: str) -> str:
        """Format a single keyboard shortcut."""
        return f"{ANSI_BOLD}{key}{ANSI_RESET}{ANSI_BRAND}: {description}{ANSI_RESET}"

    @classmethod
    def render_shortcuts(cls, category: str = "input", max_items: int = 4) -> str:
        """Render a set of shortcuts for the footer."""
        shortcuts = cls.SHORTCUTS.get(category, cls.SHORTCUTS["input"])
        
        # Limit to max_items
        shortcuts = shortcuts[:max_items]
        
        # Format each shortcut
        formatted = [cls.format_shortcut(key, desc) for key, desc in shortcuts]
        
        # Join with separator
        return f"{ANSI_BRAND}  │  {ANSI_RESET}".join(formatted)

    @classmethod
    def render_footer(
        cls,
        shortcuts_category: str = "input",
        status: Optional[str] = None,
        show_separator: bool = True,
    ) -> str:
        """Render complete footer with shortcuts and optional status.
        
        Args:
            shortcuts_category: Which category of shortcuts to show
            status: Optional status message to display on the right
            show_separator: Whether to show a separator line above
            
        Returns:
            Formatted footer string
        """
        width = cls.get_terminal_width()
        lines = []
        
        # Add separator if requested
        if show_separator:
            lines.append(format_separator(width))
        
        # Build footer content
        shortcuts = cls.render_shortcuts(shortcuts_category)
        
        if status:
            # Calculate available space for shortcuts
            status_text = f"{ANSI_BRAND}{status}{ANSI_RESET}"
            # Account for ANSI codes not taking visual space
            visible_status_len = len(status)
            available_width = width - visible_status_len - 4  # 4 for spacing
            
            # Truncate shortcuts if needed
            visible_shortcuts_len = sum(len(key) + len(desc) + 2 for key, desc in cls.SHORTCUTS.get(shortcuts_category, [])[:4])
            if visible_shortcuts_len > available_width:
                shortcuts = cls.render_shortcuts(shortcuts_category, max_items=2)
            
            # Combine with status on the right
            footer_line = f"  {shortcuts}  {status_text}"
        else:
            footer_line = f"  {shortcuts}"
        
        lines.append(footer_line)
        
        return "\n".join(lines)

    @classmethod
    def show_input_footer(cls) -> str:
        """Show footer for input mode."""
        return cls.render_footer("input")

    @classmethod
    def show_editing_footer(cls) -> str:
        """Show footer for editing mode."""
        return cls.render_footer("editing")

    @classmethod
    def show_navigation_footer(cls) -> str:
        """Show footer for navigation mode."""
        return cls.render_footer("navigation")

    @classmethod
    def show_footer_with_status(cls, status: str, category: str = "input") -> str:
        """Show footer with a status message."""
        return cls.render_footer(category, status=status)


def demo_footer():
    """Demo the footer hints."""
    print("\n=== Footer Hints Demo ===\n")
    
    print("Input mode:")
    print(FooterHints.show_input_footer())
    print()
    
    print("Editing mode:")
    print(FooterHints.show_editing_footer())
    print()
    
    print("Navigation mode:")
    print(FooterHints.show_navigation_footer())
    print()
    
    print("With status:")
    print(FooterHints.show_footer_with_status("Model: gpt-4o | Cost: $0.05"))
    print()


if __name__ == "__main__":
    demo_footer()

