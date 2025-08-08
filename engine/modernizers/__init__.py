"""
Modernizers Package

This package contains specialized modernizers for different legacy technologies.
Each modernizer is a self-contained module with its own parser, transformers, templates,
and functionality mapping capabilities.
"""

from .cobol_system import (
    COBOLFunctionalityMapper, COBOLFieldMapping, COBOLProgramMapping, COBOLDataType
)
from .static_site import (
    WebsiteFunctionalityMapper, WebsiteModernizationMapping,
    UIComponentMapping, APIMapping, WebsiteFramework, UIComponentType
)

__version__ = "1.0.0"
__author__ = "Legacy2Modern Team"

__all__ = [
    'COBOLFunctionalityMapper',
    'COBOLFieldMapping',
    'COBOLProgramMapping', 
    'COBOLDataType',
    'WebsiteFunctionalityMapper',
    'WebsiteModernizationMapping',
    'UIComponentMapping',
    'APIMapping',
    'WebsiteFramework',
    'UIComponentType'
] 