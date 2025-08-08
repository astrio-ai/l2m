"""
COBOL Functionality Mapper Package

This package provides COBOL-specific functionality mapping capabilities
for COBOL to Python modernization.
"""

from .functionality_mapper import (
    COBOLFunctionalityMapper, COBOLFieldMapping, COBOLProgramMapping,
    COBOLDataType
)

__all__ = [
    'COBOLFunctionalityMapper',
    'COBOLFieldMapping',
    'COBOLProgramMapping',
    'COBOLDataType'
] 