"""
Modernizers Package

This package contains specialized modernizers for different legacy technologies.
Each modernizer is a self-contained module with its own parser, transformers, and templates.
"""

from .static_site import StaticSiteModernizer
from .cobol_system import CobolSystemModernizer

__version__ = "1.0.0"
__author__ = "Legacy2Modern Team"

__all__ = [
    'StaticSiteModernizer',
    'CobolSystemModernizer'
] 