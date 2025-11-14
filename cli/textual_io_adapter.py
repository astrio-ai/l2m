"""
IO Adapter for Textual TUI.
Provides the same interface as io.py but uses Textual for rendering.
"""

import threading
from pathlib import Path
from cli.l2m_tui import L2MTUI
from rich.text import Text
from rich.markdown import Markdown


class TextualIOAdapter:
    """
    IO adapter that mimics the io.InputOutput interface but uses Textual.
    """
    
    def __init__(
        self,
        pretty=True,
        yes=None,
        input_history_file=None,
        chat_history_file=None,
        user_input_color="#B4B4B4",
        tool_output_color="#B4B4B4",
        tool_error_color="#B45A5A",
        tool_warning_color="#B4825A",
        assistant_output_color="#787878",
        **kwargs
    ):
        """Initialize the Textual IO adapter."""
        self.pretty = pretty
        self.yes = yes
        self.user_input_color = user_input_color
        self.tool_output_color = tool_output_color
        self.tool_error_color = tool_error_color
        self.tool_warning_color = tool_warning_color
        self.assistant_output_color = assistant_output_color
        
        # Compatibility attributes
        self.num_error_outputs = 0
        self.num_user_asks = 0
        self.input_history_file = input_history_file
        self.chat_history_file = chat_history_file
        
        # Textual app instance (will be set by main.py)
        self.app: L2MTUI = None
        self.app_thread = None
    
    def set_app(self, app: L2MTUI):
        """Set the Textual app instance."""
        self.app = app
    
    def start_app_thread(self):
        """Start the Textual app in a separate thread."""
        if self.app is None:
            self.app = L2MTUI()
        
        def run():
            self.app.run()
        
        self.app_thread = threading.Thread(target=run, daemon=True)
        self.app_thread.start()
        
        # Wait for app to be ready
        import time
        time.sleep(0.5)
    
    # ========== Output Methods ==========
    
    def tool_output(self, *messages, bold=False):
        """Display tool output."""
        if not messages:
            self.app.write_output("")
            return
        
        text = " ".join(str(m) for m in messages)
        if text.strip():
            style = self.tool_output_color if self.pretty else ""
            if bold and style:
                style = f"bold {style}"
            self.app.write_output(text, style=style)
    
    def tool_error(self, message):
        """Display error message."""
        self.num_error_outputs += 1
        if self.pretty:
            self.app.write_output(f"Error: {message}", style=self.tool_error_color)
        else:
            self.app.write_output(f"Error: {message}")
    
    def tool_warning(self, message):
        """Display warning message."""
        if self.pretty:
            self.app.write_output(f"Warning: {message}", style=self.tool_warning_color)
        else:
            self.app.write_output(f"Warning: {message}")
    
    def ai_output(self, content):
        """Display AI/assistant output."""
        # Try to render as markdown
        try:
            self.app.write_markdown(content)
        except:
            if self.pretty:
                self.app.write_output(content, style=self.assistant_output_color)
            else:
                self.app.write_output(content)
    
    # ========== Input Methods ==========
    
    def get_input(self, *args, **kwargs):
        """Get input from user."""
        if not self.app:
            raise RuntimeError("Textual app not initialized")
        
        return self.app.get_user_input()
    
    def confirm_ask(self, question, default="y"):
        """Ask user for confirmation."""
        self.num_user_asks += 1
        
        if self.yes:
            self.tool_output(f"{question} [assuming yes]")
            return True
        
        self.tool_output(f"{question} (y/n): ", bold=True)
        response = self.get_input().lower().strip()
        
        if not response:
            response = default.lower()
        
        return response in ['y', 'yes']
    
    def prompt_ask(self, question, default=""):
        """Ask user a question."""
        self.num_user_asks += 1
        self.tool_output(f"{question}: ", bold=True)
        response = self.get_input().strip()
        return response if response else default
    
    # ========== Compatibility Methods ==========
    
    def user_input(self, inp, log_only=True):
        """Log user input (for history)."""
        # The TUI already displays user input, so we just log it
        if self.chat_history_file and not log_only:
            try:
                with open(self.chat_history_file, 'a', encoding='utf-8') as f:
                    f.write(f"\n#### {inp}\n")
            except Exception:
                pass
    
    def rule(self, show_footer=False):
        """Print separator (just blank line in TUI)."""
        if self.app:
            self.app.write_output("")
    
    def append_chat_history(self, text, linebreak=False):
        """Append to chat history."""
        if self.chat_history_file:
            try:
                with open(self.chat_history_file, 'a', encoding='utf-8') as f:
                    f.write(text)
                    if linebreak:
                        f.write("\n")
            except Exception:
                pass
    
    def get_rel_fname(self, fname):
        """Get relative filename."""
        try:
            return str(Path(fname).relative_to(Path.cwd()))
        except:
            return str(fname)
    
    # ========== Additional Methods for Compatibility ==========
    
    def write(self, filename, content):
        """Write content to file."""
        # This is handled by the Coder class, just display confirmation
        self.tool_output(f"Writing {self.get_rel_fname(filename)}")
    
    def send_notification(self, message):
        """Send a notification."""
        pass  # Not implemented in TUI yet
    
    def prompt_approval_mode(self, folder_path):
        """Prompt for approval mode."""
        self.tool_output(f"Running L2M in {folder_path}")
        self.tool_output("Since this folder is not version controlled, we recommend requiring approval of all edits and commands.")
        self.tool_output("")
        self.tool_output("1. Allow L2M to work in this folder without asking for approval")
        self.tool_output("2. Require approval of edits and commands")
        self.tool_output("")
        
        response = self.prompt_ask("Choose option (1 or 2)", "2")
        return response == "2"  # True = require approval

