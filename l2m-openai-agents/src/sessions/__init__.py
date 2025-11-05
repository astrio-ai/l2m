"""Session management for L2M OpenAI Agents."""

# Session management will use OpenAI Agents SDK's built-in sessions
# See: https://openai.github.io/openai-agents-python/guides/sessions/

from agents import SQLiteSession, Session

__all__ = ["SQLiteSession", "Session"]

