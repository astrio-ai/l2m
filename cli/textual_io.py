"""
Textual-based IO adapter for L2M.
Wraps the Textual UI to work with existing Coder interface.
"""

import threading
import queue
from typing import Optional
from cli.textual_ui import L2MApp
from rich.text import Text


class TextualIO:
    """IO adapter that uses Textual instead of prompt_toolkit."""
    
    def __init__(self, **kwargs):
        """Initialize the Textual IO adapter."""
        # Store kwargs for compatibility
        self.pretty = kwargs.get('pretty', True)
        self.user_input_color = kwargs.get('user_input_color', '#B4B4B4')
        self.tool_output_color = kwargs.get('tool_output_color', '#B4B4B4')
        self.tool_error_color = kwargs.get('tool_error_color', '#B45A5A')
        self.tool_warning_color = kwargs.get('tool_warning_color', '#B4825A')
        self.assistant_output_color = kwargs.get('assistant_output_color', '#787878')
        
        # Queue for communication between threads
        self.input_queue = queue.Queue()
        self.output_queue = queue.Queue()
        
        # Textual app instance
        self.app: Optional[L2MApp] = None
        self.app_thread: Optional[threading.Thread] = None
        
        # Compatibility attributes
        self.num_error_outputs = 0
        self.num_user_asks = 0
    
    def start_app(self):
        """Start the Textual app in a separate thread."""
        def run_app():
            self.app = L2MApp()
            self.app.set_message_callback(self._on_user_message)
            self.app.run()
        
        self.app_thread = threading.Thread(target=run_app, daemon=True)
        self.app_thread.start()
        
        # Wait a moment for app to initialize
        import time
        time.sleep(0.5)
    
    def _on_user_message(self, text: str):
        """Callback when user submits a message."""
        self.input_queue.put(text)
    
    def get_input(self, *args, **kwargs) -> str:
        """Get input from user."""
        if not self.app:
            self.start_app()
        
        # Wait for user input from queue
        return self.input_queue.get()
    
    def tool_output(self, *messages, bold: bool = False):
        """Display tool output."""
        if not self.app:
            return
        
        text = " ".join(str(m) for m in messages)
        if text.strip():
            self.app.add_tool_output(text, is_error=False, is_warning=False)
    
    def tool_error(self, message: str):
        """Display error message."""
        if not self.app:
            return
        
        self.num_error_outputs += 1
        self.app.add_tool_output(message, is_error=True)
    
    def tool_warning(self, message: str):
        """Display warning message."""
        if not self.app:
            return
        
        self.app.add_tool_output(message, is_warning=True)
    
    def ai_output(self, content: str):
        """Display AI/assistant output."""
        if not self.app:
            return
        
        self.app.add_assistant_message(content)
    
    def confirm_ask(self, question: str, default: str = "y") -> bool:
        """Ask user for confirmation."""
        # For now, use simple input
        # TODO: Implement proper UI dialog
        response = self.get_input()
        return response.lower() in ['y', 'yes', '']
    
    def prompt_ask(self, question: str, default: str = "") -> str:
        """Ask user a question."""
        if self.app:
            self.app.add_tool_output(question)
        return self.get_input()
    
    # Compatibility methods
    def user_input(self, inp: str, log_only: bool = True):
        """Log user input (for history)."""
        pass  # Textual UI handles display
    
    def rule(self, show_footer: bool = False):
        """Print separator (no-op in Textual)."""
        pass
    
    def append_chat_history(self, text: str, linebreak: bool = False):
        """Append to chat history."""
        pass  # TODO: Implement if needed
    
    def get_rel_fname(self, fname: str) -> str:
        """Get relative filename."""
        return fname  # Simplified for now

