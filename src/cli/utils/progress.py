"""
Progress indicator utilities for CLI.

This module provides utilities for showing progress indicators
during long-running operations.
"""

import time
import threading
from typing import Optional
from contextlib import contextmanager

from src.utils.logger import get_logger

logger = get_logger(__name__)


class ProgressIndicator:
    """Progress indicator for CLI operations."""
    
    def __init__(self, message: str, show_percentage: bool = False):
        """Initialize the progress indicator."""
        self.message = message
        self.show_percentage = show_percentage
        self.current_message = message
        self.percentage = 0
        self.running = False
        self.thread: Optional[threading.Thread] = None
        self.logger = get_logger(__name__)
    
    def start(self):
        """Start the progress indicator."""
        self.running = True
        self.thread = threading.Thread(target=self._run)
        self.thread.start()
    
    def stop(self):
        """Stop the progress indicator."""
        self.running = False
        if self.thread:
            self.thread.join()
    
    def update(self, message: str, percentage: Optional[int] = None):
        """Update the progress message and percentage."""
        self.current_message = message
        if percentage is not None:
            self.percentage = percentage
    
    def _run(self):
        """Run the progress indicator animation."""
        while self.running:
            if self.show_percentage:
                print(f"\r{self.current_message} ({self.percentage}%)", end="", flush=True)
            else:
                print(f"\r{self.current_message}", end="", flush=True)
            time.sleep(0.1)
        print()  # New line when done
    
    def __enter__(self):
        """Context manager entry."""
        self.start()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit."""
        self.stop()
