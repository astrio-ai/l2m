"""
COBOL System Modernizer

Converts legacy COBOL systems into modern Python applications.
Supports Python with SQLAlchemy, FastAPI, Django, and Flask.
"""

from .transpilers.cobol_transpiler import CobolTranspiler

# Alias for backward compatibility
CobolSystemModernizer = CobolTranspiler

__version__ = "1.0.0"
__author__ = "Legacy2Modern Team"

__all__ = ['CobolTranspiler', 'CobolSystemModernizer'] 