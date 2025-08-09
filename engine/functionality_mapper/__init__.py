"""
Shared functionality mapping infrastructure for software modernization.

This package provides the base functionality mapping system that can be
extended by domain-specific modernizers (COBOL, websites, etc.).
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