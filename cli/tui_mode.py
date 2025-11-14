"""
TUI Mode - Python backend optimized for Go TUI frontend.

This module provides a simple stdin/stdout interface for the Go TUI to communicate
with the Python L2M backend.
"""

import sys
import json
from io import StringIO


class TUIBackend:
    """Backend interface for Go TUI."""
    
    def __init__(self, coder):
        self.coder = coder
        
    def process_input(self, user_input):
        """Process user input and return response."""
        try:
            # Capture output
            old_stdout = sys.stdout
            sys.stdout = StringIO()
            
            # Process through coder
            for chunk in self.coder.run_stream(user_input):
                # Stream output
                print(chunk, end='', flush=True)
            
            # Get captured output
            output = sys.stdout.getvalue()
            sys.stdout = old_stdout
            
            return output
            
        except Exception as e:
            sys.stdout = old_stdout
            return f"Error: {str(e)}"
    
    def run_loop(self):
        """Main loop - read from stdin, process, write to stdout."""
        print("L2M Backend Ready", flush=True)
        
        while True:
            try:
                # Read line from stdin
                line = sys.stdin.readline()
                if not line:
                    break
                    
                user_input = line.strip()
                if not user_input:
                    continue
                
                # Process and respond
                response = self.process_input(user_input)
                print(response, flush=True)
                print("<<<END>>>", flush=True)  # End marker
                
            except KeyboardInterrupt:
                break
            except Exception as e:
                print(f"Error: {str(e)}", flush=True)
                print("<<<END>>>", flush=True)


def start_tui_backend(coder):
    """Start the TUI backend mode."""
    backend = TUIBackend(coder)
    backend.run_loop()

