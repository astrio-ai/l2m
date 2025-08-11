"""
Functionality Mapper Package

This package provides the core functionality mapping system that can be
extended by domain-specific modernizers (websites, etc.).

The functionality mapper analyzes legacy code and maps it to modern
equivalents, providing a bridge between old and new technologies.
"""

from .base_mapper import (
    FunctionalityMapper, FunctionalityMapping, FunctionalityType,
    InputOutputMapping, BusinessLogicMapping, EquivalenceLevel,
    ValidationStrategy, TestType, TestCase, ValidationResult, TestResult,
    ValidationEngine, TestEngine
)

__version__ = "1.0.0"
__author__ = "Legacy2Modern Team"

__all__ = [
    'FunctionalityMapper',
    'FunctionalityMapping', 
    'FunctionalityType',
    'InputOutputMapping',
    'BusinessLogicMapping',
    'EquivalenceLevel',
    'ValidationStrategy',
    'TestType',
    'TestCase',
    'ValidationResult',
    'TestResult',
    'ValidationEngine',
    'TestEngine'
] 