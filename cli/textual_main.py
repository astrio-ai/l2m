"""
Textual-based main entry point for L2M.
This runs the Coder logic in a background thread while Textual controls the main thread.
"""

import threading
import sys
from cli.l2m_tui import L2MTUI
from cli.textual_io_adapter import TextualIOAdapter


def run_l2m_with_textual(main_func, *args, **kwargs):
    """
    Run L2M with Textual TUI.
    
    Args:
        main_func: The main() function from cli.main
        *args, **kwargs: Arguments to pass to main_func
    """
    # Create the Textual app
    app = L2MTUI()
    
    # Create IO adapter and set the app
    io_adapter = TextualIOAdapter()
    io_adapter.set_app(app)
    
    # Store result from main thread
    result = {'value': None, 'exception': None}
    
    def run_main():
        """Run the main L2M logic in a background thread."""
        try:
            # Run the main function (it will use our Textual IO)
            result['value'] = main_func(*args, **kwargs)
        except Exception as e:
            result['exception'] = e
            app.write_output(f"Error: {e}", style="#B45A5A")
        finally:
            # Exit the app when done
            app.exit()
    
    # Start the main logic in a background thread
    logic_thread = threading.Thread(target=run_main, daemon=False)
    logic_thread.start()
    
    # Run the Textual app in the main thread
    app.run()
    
    # After app exits, check if there was an exception
    if result['exception']:
        raise result['exception']
    
    return result['value']


if __name__ == "__main__":
    # Test
    from cli.main import main
    
    # Run L2M with Textual
    sys.exit(run_l2m_with_textual(main) or 0)

