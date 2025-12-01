"""
⚠️ DEPRECATED: This package has been renamed to astrio-atlas.

This package (l2m) is deprecated and will no longer receive updates.
Please migrate to the new package:

    pip uninstall l2m
    pip install astrio-atlas

The CLI command has also changed from 'l2m' to 'atlas'.
"""

import warnings
import sys

# Show deprecation warning on import
warnings.warn(
    "⚠️ DEPRECATED: The 'l2m' package has been renamed to 'astrio-atlas'. "
    "Please uninstall l2m and install astrio-atlas instead: "
    "pip uninstall l2m && pip install astrio-atlas",
    DeprecationWarning,
    stacklevel=2
)

# Print deprecation notice to stderr for visibility
print(
    "\n⚠️  DEPRECATION WARNING ⚠️\n"
    "The 'l2m' package has been renamed to 'astrio-atlas'.\n"
    "Please migrate to the new package:\n"
    "  pip uninstall l2m\n"
    "  pip install astrio-atlas\n"
    "The CLI command has changed from 'l2m' to 'atlas'.\n",
    file=sys.stderr
)

# The package will work because astrio-atlas is a required dependency
# The CLI command is handled by the script entry point in pyproject.toml
__all__ = []

