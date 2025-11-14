"""Setup and onboarding."""

from .onboarding import *  # noqa: F403
from .versioncheck import *  # noqa: F403

__all__ = [
    "offer_openrouter_oauth",
    "select_default_model",
    "check_version",
    "install_from_main_branch",
    "install_upgrade",
    "VERSION_CHECK_FNAME",
]

