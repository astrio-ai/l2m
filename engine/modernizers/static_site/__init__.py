"""
Static Site Modernizer

Converts legacy static websites (HTML, CSS, Bootstrap, jQuery) into modern web applications.
Supports React, Next.js, and Astro with Tailwind CSS.
"""

from .static_site_transpiler import StaticSiteTranspiler

# Alias for backward compatibility
StaticSiteModernizer = StaticSiteTranspiler

__version__ = "1.0.0"
__author__ = "Legacy2Modern Team"

__all__ = ['StaticSiteTranspiler', 'StaticSiteModernizer'] 