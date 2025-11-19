"""Harbor integration for L2M agent.

This module provides the L2M (Legacy2Modern) agent for Harbor (Terminal) benchmarking.
Harbor will automatically discover and use the L2MAgent class from this module.
"""

try:
    # Only import if harbor is available (optional dependency)
    from evals.harbor.l2m_agent import L2MAgent, ExecInput
    
    __all__ = ["L2MAgent", "ExecInput"]
except ImportError:
    # Harbor is not installed - this is okay, it's an optional dependency
    __all__ = []

