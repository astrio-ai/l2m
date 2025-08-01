"""
Templates for website modernization.
Contains template generators for React, Next.js, and Astro frameworks.
"""

from .react.react_generator import ReactTemplateGenerator
from .nextjs.nextjs_generator import NextJSTemplateGenerator
from .astro.astro_generator import AstroTemplateGenerator

__all__ = ['ReactTemplateGenerator', 'NextJSTemplateGenerator', 'AstroTemplateGenerator'] 