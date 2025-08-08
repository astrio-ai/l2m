"""
Website Functionality Mapper Package

This package provides website-specific functionality mapping capabilities
for legacy website modernization.
"""

from .functionality_mapper import (
    WebsiteFunctionalityMapper, WebsiteModernizationMapping,
    UIComponentMapping, APIMapping, WebsiteFramework, UIComponentType
)

__all__ = [
    'WebsiteFunctionalityMapper',
    'WebsiteModernizationMapping',
    'UIComponentMapping',
    'APIMapping',
    'WebsiteFramework',
    'UIComponentType'
] 