"""
Textual-based TUI for L2M
"""

from textual.app import App, ComposeResult
from textual.containers import Container, Vertical, VerticalScroll
from textual.widgets import TextArea, Static, Footer
from textual.binding import Binding
from textual.reactive import reactive
from rich.markdown import Markdown
from rich.text import Text
from rich.console import RenderableType
import asyncio


class MessageDisplay(VerticalScroll):
    """Scrollable area for displaying messages."""
    
    DEFAULT_CSS = """
    MessageDisplay {
        height: 1fr;
        background: $background;
        padding: 0 1;
    }
    
    MessageDisplay > Static {
        margin: 0 0 1 0;
    }
    """
    
    def add_message(self, content: RenderableType, message_type: str = "assistant"):
        """Add a message to the display."""
        message = Static(content, classes=message_type)
        self.mount(message)
        self.scroll_end(animate=False)


class InputBox(TextArea):
    """Text input area with background styling."""
    
    DEFAULT_CSS = """
    InputBox {
        height: auto;
        min-height: 3;
        max-height: 10;
        background: #2b2b2b;
        border: none;
        padding: 0 1;
    }
    """
    
    def __init__(self, **kwargs):
        super().__init__(
            text="",
            language="markdown",
            theme="monokai",
            show_line_numbers=False,
            **kwargs
        )
        self.placeholder = "> "


class L2MApp(App):
    """Textual-based L2M TUI application."""
    
    CSS = """
    Screen {
        background: $background;
    }
    
    #input-container {
        height: auto;
        background: #2b2b2b;
        dock: bottom;
    }
    
    .user-message {
        color: #B4B4B4;
    }
    
    .assistant-message {
        color: #787878;
    }
    
    .tool-output {
        color: #B4B4B4;
    }
    
    .tool-error {
        color: #B45A5A;
    }
    
    .tool-warning {
        color: #B4825A;
    }
    """
    
    BINDINGS = [
        Binding("ctrl+c", "quit", "Quit", show=True),
        Binding("ctrl+d", "quit", "Exit", show=True),
        ("escape", "cancel", "Cancel"),
    ]
    
    message_callback = None
    
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.message_queue = asyncio.Queue()
    
    def compose(self) -> ComposeResult:
        """Create the UI layout."""
        yield MessageDisplay(id="messages")
        with Container(id="input-container"):
            yield InputBox(id="input-box")
        yield Footer()
    
    def on_mount(self) -> None:
        """Called when app is mounted."""
        input_box = self.query_one("#input-box", InputBox)
        input_box.focus()
    
    def on_text_area_submitted(self, event: TextArea.Submitted) -> None:
        """Handle when user submits input (Enter key)."""
        text = event.text_area.text.strip()
        if not text:
            return
        
        # Clear the input
        event.text_area.clear()
        
        # Add user message to display
        messages = self.query_one("#messages", MessageDisplay)
        messages.add_message(Text(f"> {text}", style="#B4B4B4"), "user-message")
        
        # Send to callback if set
        if self.message_callback:
            self.message_callback(text)
    
    def action_cancel(self) -> None:
        """Handle escape key."""
        input_box = self.query_one("#input-box", InputBox)
        input_box.clear()
    
    def add_assistant_message(self, content: str) -> None:
        """Add an assistant message to the display."""
        messages = self.query_one("#messages", MessageDisplay)
        
        # Try to render as markdown
        try:
            renderable = Markdown(content)
        except:
            renderable = Text(content)
        
        messages.add_message(renderable, "assistant-message")
    
    def add_tool_output(self, content: str, is_error: bool = False, is_warning: bool = False) -> None:
        """Add tool output to the display."""
        messages = self.query_one("#messages", MessageDisplay)
        
        if is_error:
            msg_class = "tool-error"
        elif is_warning:
            msg_class = "tool-warning"
        else:
            msg_class = "tool-output"
        
        messages.add_message(Text(content), msg_class)
    
    async def get_user_input(self) -> str:
        """Get input from the user (async)."""
        return await self.message_queue.get()
    
    def set_message_callback(self, callback):
        """Set callback for when user submits a message."""
        self.message_callback = callback


def run_textual_ui(message_callback=None):
    """Run the Textual UI."""
    app = L2MApp()
    if message_callback:
        app.set_message_callback(message_callback)
    app.run()


if __name__ == "__main__":
    # Test the UI
    def on_message(text):
        print(f"Received: {text}")
    
    run_textual_ui(on_message)

