"""User interface components."""

from .copypaste import *  # noqa: F403
from .editor import *  # noqa: F403

# GUI is optional (requires streamlit), so import lazily
# from .gui import *  # noqa: F403

__all__ = [
    "ClipboardWatcher",
    "GUI",
]

# Lazy import for GUI to avoid requiring streamlit at import time
def __getattr__(name):
    if name == "GUI":
        try:
            from .gui import GUI
            return GUI
        except ImportError:
            raise ImportError(
                "GUI requires streamlit. Install it with: pip install streamlit"
            ) from None
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")

