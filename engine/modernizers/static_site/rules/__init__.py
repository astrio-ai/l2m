"""
Rules for website modernization.
Contains transformation rules for HTML, CSS, and JavaScript modernization.
"""

from .jquery_rules import JQueryRules
from .bootstrap_rules import BootstrapRules

__all__ = ['JQueryRules', 'BootstrapRules'] 