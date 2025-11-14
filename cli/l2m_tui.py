"""
L2M Full TUI Application with Textual.

This is a complete terminal UI that handles both input and output.
"""

from textual.app import App, ComposeResult
from textual.containers import Container, VerticalScroll
from textual.widgets import TextArea, Static, Footer, RichLog
from textual.binding import Binding
from rich.markdown import Markdown
from rich.text import Text
from queue import Queue


class ChatDisplay(RichLog):
    """Scrollable chat display area."""
    
    DEFAULT_CSS = """
    ChatDisplay {
        height: 1fr;
        background: $background;
        border: none;
    }
    """
    
    def __init__(self, **kwargs):
        super().__init__(
            highlight=True,
            markup=True,
            **kwargs
        )


class ChatInput(TextArea):
    """Text input area at the bottom."""
    
    DEFAULT_CSS = """
    ChatInput {
        height: auto;
        min-height: 3;
        max-height: 10;
        background: #2b2b2b;
        border: none;
        padding: 0 1;
        dock: bottom;
    }
    """


class L2MTUI(App):
    """L2M Terminal UI Application."""
    
    CSS = """
    Screen {
        background: $background;
    }
    """
    
    BINDINGS = [
        Binding("ctrl+c", "quit", "Quit"),
        Binding("ctrl+d", "exit_app", "Exit"),
        Binding("escape", "clear_input", "Clear"),
    ]
    
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.input_queue = Queue()
    
    def compose(self) -> ComposeResult:
        """Create the layout."""
        yield ChatDisplay(id="chat")
        yield ChatInput(id="input")
        yield Footer()
    
    def on_mount(self) -> None:
        """Initialize after mounting."""
        self.query_one("#input", ChatInput).focus()
    
    def on_text_area_submitted(self, event: TextArea.Submitted) -> None:
        """Handle user input submission."""
        text = event.text_area.text.strip()
        if not text:
            return
        
        # Clear input
        event.text_area.clear()
        
        # Display user message
        chat = self.query_one("#chat", ChatDisplay)
        chat.write(Text(f"> {text}", style="#B4B4B4"))
        
        # Send to queue
        self.input_queue.put(text)
        self.waiting_for_input.set()
    
    def action_clear_input(self) -> None:
        """Clear the input field."""
        self.query_one("#input", ChatInput).clear()
    
    def action_exit_app(self) -> None:
        """Exit the application."""
        self.exit()
    
    def write_output(self, text: str, style: str = ""):
        """Write text to the chat display (thread-safe)."""
        def _write():
            chat = self.query_one("#chat", ChatDisplay)
            if style:
                chat.write(Text(text, style=style))
            else:
                chat.write(text)
        
        # Use call_from_thread to make it thread-safe
        self.call_from_thread(_write)
    
    def write_markdown(self, text: str):
        """Write markdown to the chat display (thread-safe)."""
        def _write():
            chat = self.query_one("#chat", ChatDisplay)
            chat.write(Markdown(text))
        
        # Use call_from_thread to make it thread-safe
        self.call_from_thread(_write)
    
    def get_user_input(self) -> str:
        """Get input from user (blocking, thread-safe)."""
        # This will block until user submits input
        return self.input_queue.get()


# Singleton instance
_tui_app: L2MTUI = None


def get_tui_app() -> L2MTUI:
    """Get or create the TUI app instance."""
    global _tui_app
    if _tui_app is None:
        _tui_app = L2MTUI()
    return _tui_app


def run_l2m_tui():
    """Run the L2M TUI."""
    app = get_tui_app()
    app.run()


if __name__ == "__main__":
    # Simple test
    app = L2MTUI()
    
    # Write welcome message before starting
    def on_ready():
        app.write_output("Welcome to L2M!", style="#4EC9B0")
        app.write_output("Type your message below and press Enter.")
        app.write_output("")
    
    # Schedule welcome message
    app.call_later(on_ready)
    
    app.run()

