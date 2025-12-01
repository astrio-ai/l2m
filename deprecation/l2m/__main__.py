"""
⚠️ DEPRECATED: This package has been renamed to astrio-atlas.

This wrapper provides the 'l2m' command which now calls 'atlas' from astrio-atlas.
"""

import sys
import warnings

# Show deprecation warning
warnings.warn(
    "⚠️ DEPRECATED: The 'l2m' package has been renamed to 'astrio-atlas'. "
    "Please uninstall l2m and install astrio-atlas instead: "
    "pip uninstall l2m && pip install astrio-atlas",
    DeprecationWarning,
    stacklevel=1
)

# Import from astrio-atlas (required dependency)
# When installed, astrio-atlas provides the cli.main module
try:
    from cli.main import main
except ImportError:
    print(
        "ERROR: The 'l2m' package requires 'astrio-atlas'.\n"
        "Please install it with: pip install astrio-atlas",
        file=sys.stderr
    )
    sys.exit(1)

if __name__ == "__main__":
    sys.exit(main())

