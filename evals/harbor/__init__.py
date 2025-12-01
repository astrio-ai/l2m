"""Harbor integration for Atlas agent.

This module provides the Atlas (Atlas) agent for Harbor (Terminal) benchmarking.
Harbor will automatically discover and use the AtlasAgent class from this module.
"""

try:
    # Only import if harbor is available (optional dependency)
    from evals.harbor.atlas_agent import AtlasAgent, ExecInput
    
    __all__ = ["AtlasAgent", "ExecInput"]
except ImportError:
    # Harbor is not installed - this is okay, it's an optional dependency
    __all__ = []

